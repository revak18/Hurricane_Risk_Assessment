download_afo_raw <- function(raw_dir, drive_folder) {
  # Define the NC DEQ AFO source and snapshot
  service_url <- paste0("https://services2.arcgis.com/kCu40SDxsCGcuUWO/", "ArcGIS/rest/services/", "Animal_Feed_Operation_Permits_%28View%29/", "FeatureServer/0/query")
  access_date <- format(Sys.Date(), "%Y%m%d")
  file_name <- paste0("nc_deq_afo_access_", access_date, ".csv")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  local_file <- file.path(raw_dir, file_name)
  cat("NC DEQ AFO snapshot defined.\n")

  # Restore today's snapshot from Google Drive
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_match <- drive_files[drive_files$name == file_name, ]

  if (nrow(drive_match) > 1) stop("Multiple copies of today's AFO snapshot found on Google Drive.")

  if (nrow(drive_match) == 1) {
    googledrive::drive_download(drive_match, path = local_file, overwrite = TRUE)
    if (file.exists(local_file) && file.info(local_file)$size > 1000) {
      cat("NC DEQ AFO snapshot restored from Google Drive.\n")
      return(local_file)
    }
  }

  cat("No valid persisted AFO snapshot found.\n")

  # Define a robust ArcGIS JSON reader
  read_arcgis_json <- function(url, flatten = FALSE) {
    temp_file <- tempfile(fileext = ".json")
    on.exit(unlink(temp_file), add = TRUE)
    success <- FALSE

    for (attempt in seq_len(3)) {
      result <- try(utils::download.file(url, destfile = temp_file, mode = "wb", method = "libcurl", quiet = TRUE), silent = TRUE)
      if (!inherits(result, "try-error") && file.exists(temp_file) && file.info(temp_file)$size > 0) {
        success <- TRUE
        break
      }
      Sys.sleep(2 * attempt)
    }

    if (!success) stop("ArcGIS JSON request failed.")
    jsonlite::fromJSON(temp_file, flatten = flatten)
  }

  cat("ArcGIS JSON reader ready.\n")

  # Retrieve all AFO feature IDs
  ids_url <- paste0(service_url, "?where=1%3D1", "&returnIdsOnly=true", "&f=json")
  ids_response <- read_arcgis_json(ids_url)

  if (!is.null(ids_response$error)) stop("NC DEQ AFO feature-ID request failed.")

  object_ids <- sort(unique(ids_response$objectIds))

  if (!length(object_ids)) stop("NC DEQ AFO service returned no feature IDs.")

  cat("NC DEQ AFO feature IDs retrieved:", length(object_ids), "\n")

  # Download AFO records in small ArcGIS batches
  batches <- split(object_ids, ceiling(seq_along(object_ids) / 100))
  batch_rows <- vector("list", length(batches))

  for (batch_index in seq_along(batches)) {
    ids <- batches[[batch_index]]
    query_url <- paste0(service_url, "?objectIds=", paste(ids, collapse = "%2C"), "&outFields=%2A", "&returnGeometry=true", "&outSR=4326", "&f=json")
    response <- read_arcgis_json(query_url, flatten = TRUE)

    if (!is.null(response$error)) stop(paste("NC DEQ AFO batch request failed for batch", batch_index))

    features <- response$features

    if (is.null(features) || !nrow(features)) next

    attribute_columns <- grep("^attributes\\.", names(features), value = TRUE)

    if (!length(attribute_columns)) stop("AFO service returned no attribute fields.")
    if (!"geometry.x" %in% names(features) || !"geometry.y" %in% names(features)) stop("AFO service returned no point geometry.")

    batch <- features[, attribute_columns, drop = FALSE]
    names(batch) <- make.unique(sub("^attributes\\.", "", names(batch)))
    batch$longitude <- suppressWarnings(as.numeric(features$geometry.x))
    batch$latitude <- suppressWarnings(as.numeric(features$geometry.y))
    batch_rows[[batch_index]] <- batch

    cat("AFO batch", batch_index, "of", length(batches), "downloaded.\n")
  }

  cat("NC DEQ AFO batches downloaded.\n")

  # Combine downloaded AFO batches
  batch_rows <- batch_rows[vapply(batch_rows, function(x) !is.null(x) && nrow(x) > 0, logical(1))]

  if (!length(batch_rows)) stop("NC DEQ AFO records could not be downloaded.")

  all_columns <- unique(unlist(lapply(batch_rows, names)))

  for (i in seq_along(batch_rows)) {
    missing_columns <- setdiff(all_columns, names(batch_rows[[i]]))
    for (field in missing_columns) batch_rows[[i]][[field]] <- NA
    batch_rows[[i]] <- batch_rows[[i]][, all_columns, drop = FALSE]
  }

  raw <- do.call(rbind, batch_rows)
  rownames(raw) <- NULL

  cat("NC DEQ AFO batches combined.\n")

  # Validate and persist the raw snapshot
  valid_coordinates <- is.finite(raw$longitude) & is.finite(raw$latitude) & raw$longitude >= -90 & raw$longitude <= -70 & raw$latitude >= 30 & raw$latitude <= 40

  if (!any(valid_coordinates)) stop("NC DEQ AFO snapshot contains no valid coordinates.")

  if (nrow(raw) != length(object_ids)) warning(paste("Feature ID count:", length(object_ids), "downloaded rows:", nrow(raw)))

  write.csv(raw, local_file, row.names = FALSE)
  googledrive::drive_upload(local_file, path = drive_folder, overwrite = TRUE)

  cat("NC DEQ AFO raw snapshot validated and persisted.\n")
  local_file
}

build_afo_tables <- function(raw_file, county_file, target_county_fips, target_crs, output_dir, validation_dir) {
  # Load the AFO snapshot and TIGER counties
  raw <- read.csv(raw_file, stringsAsFactors = FALSE, check.names = FALSE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(raw) || !nrow(counties)) stop("AFO or county input is empty.")

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(raw_file))

  if (!grepl("^[0-9]{8}$", access_date)) stop("Could not determine AFO snapshot access date.")

  cat("AFO snapshot and TIGER counties loaded.\n")

  # Validate AFO coordinates
  raw$longitude <- suppressWarnings(as.numeric(raw$longitude))
  raw$latitude <- suppressWarnings(as.numeric(raw$latitude))
  valid_coordinates <- is.finite(raw$longitude) & is.finite(raw$latitude) & raw$longitude >= -90 & raw$longitude <= -70 & raw$latitude >= 30 & raw$latitude <= 40
  invalid_coordinate_count <- sum(!valid_coordinates)
  valid <- raw[valid_coordinates, , drop = FALSE]

  if (!nrow(valid)) stop("No valid AFO coordinates remain.")

  cat("AFO coordinates validated.\n")

  # Spatially assign NC county FIPS
  points <- sf::st_as_sf(valid, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  counties <- sf::st_transform(counties, target_crs)
  points <- sf::st_transform(points, target_crs)
  county_fields <- counties[, c("GEOID", "NAME")]
  names(county_fields)[names(county_fields) == "GEOID"] <- "county_geoid"
  names(county_fields)[names(county_fields) == "NAME"] <- "census_county"
  joined <- sf::st_join(points, county_fields, join = sf::st_within, left = TRUE)
  joined$county_geoid <- as.character(joined$county_geoid)
  nc_points <- joined[!is.na(joined$county_geoid), ]

  cat("AFO sites spatially assigned to NC county FIPS.\n")

  # Resolve important AFO source fields
  normalize_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

  pick_field <- function(data, candidates) {
    available <- normalize_name(names(data))
    for (candidate in normalize_name(candidates)) {
      hit <- which(available == candidate)
      if (length(hit)) return(names(data)[hit[[1]]])
    }
    NULL
  }

  object_field <- pick_field(nc_points, c("OBJECTID", "FID", "OID", "OBJECT_ID"))
  permit_field <- pick_field(nc_points, c("PERMIT_NO", "PERMIT_NUMBER", "PERMITNUMBER", "PERMITNUM", "PERMIT"))
  name_field <- pick_field(nc_points, c("FACILITY_NAME", "FACILITYNAME", "OPERATION_NAME", "OPERATIONNAME", "FARM_NAME", "FARMNAME", "NAME"))
  animal_field <- pick_field(nc_points, c("ANIMAL_TYPE", "ANIMALTYPE", "ANIMAL", "SPECIES", "OPERATION_TYPE", "OPERATIONTYPE"))
  permit_type_field <- pick_field(nc_points, c("PERMIT_TYPE", "PERMITTYPE", "TYPE_DESC", "TYPEDESC", "TYPE"))
  status_field <- pick_field(nc_points, c("PERMIT_STATUS", "PERMITSTATUS", "STATUS"))
  county_field <- pick_field(nc_points, c("COUNTY", "COUNTY_NAME", "COUNTYNAME"))

  cat("AFO source fields resolved.\n")

  # Standardize source attributes
  nc_table <- sf::st_drop_geometry(nc_points)

  value_or_na <- function(data, field) {
    if (is.null(field)) return(rep(NA_character_, nrow(data)))
    as.character(data[[field]])
  }

  source_object_id <- value_or_na(nc_table, object_field)
  missing_object_id <- is.na(source_object_id) | trimws(source_object_id) == ""
  source_object_id[missing_object_id] <- as.character(which(missing_object_id))

  permit_id <- value_or_na(nc_table, permit_field)
  facility_name <- value_or_na(nc_table, name_field)
  animal_type_raw <- value_or_na(nc_table, animal_field)
  permit_type_raw <- value_or_na(nc_table, permit_type_field)
  permit_status_raw <- value_or_na(nc_table, status_field)
  reported_county <- value_or_na(nc_table, county_field)

  cat("AFO source attributes standardized.\n")

  # Classify animal groups
  search_text <- toupper(paste(animal_type_raw, permit_type_raw, facility_name))
  swine_flag <- grepl("SWINE|HOG|PIG", search_text)
  cattle_flag <- grepl("CATTLE|DAIRY|BOVINE|COW", search_text)
  poultry_flag <- grepl("POULTRY|CHICKEN|TURKEY", search_text)
  horse_flag <- grepl("HORSE|EQUINE", search_text)
  sheep_goat_flag <- grepl("SHEEP|OVINE|GOAT|CAPRINE", search_text)
  animal_matrix <- cbind(swine = swine_flag, cattle = cattle_flag, poultry = poultry_flag, horse = horse_flag, sheep_goat = sheep_goat_flag)
  animal_hits <- rowSums(animal_matrix)
  animal_group <- rep("other_or_unknown", nrow(nc_table))
  single_hit <- animal_hits == 1

  if (any(single_hit)) animal_group[single_hit] <- colnames(animal_matrix)[max.col(animal_matrix[single_hit, , drop = FALSE], ties.method = "first")]

  animal_group[animal_hits > 1] <- "mixed_animals"

  cat("AFO animal groups classified.\n")

  # Classify permit-program context
  permit_search <- toupper(paste(permit_type_raw, permit_status_raw))
  npdes_flag <- grepl("NPDES", permit_search)
  digester_flag <- grepl("DIGESTER|BIOGAS", permit_search)
  individual_flag <- grepl("INDIVIDUAL", permit_search)
  state_flag <- grepl("STATE", permit_search)
  permit_program <- ifelse(npdes_flag, "npdes", ifelse(state_flag, "state", "other_or_unknown"))

  cat("AFO permit context classified.\n")

  # Build standardized statewide and Mecklenburg tables
  standardized <- data.frame(
    afo_record_id = paste0("NCDEQ_AFO_", source_object_id),
    source_object_id = source_object_id,
    permit_id = permit_id,
    facility_name = facility_name,
    animal_type_raw = animal_type_raw,
    animal_group = animal_group,
    permit_type_raw = permit_type_raw,
    permit_status_raw = permit_status_raw,
    permit_program = permit_program,
    npdes_flag = npdes_flag,
    state_permit_flag = state_flag,
    digester_flag = digester_flag,
    individual_permit_flag = individual_flag,
    reported_county = reported_county,
    county_geoid = nc_table$county_geoid,
    census_county = nc_table$census_county,
    latitude = nc_table$latitude,
    longitude = nc_table$longitude,
    snapshot_access_date = access_date,
    stringsAsFactors = FALSE
  )

  mecklenburg <- standardized[standardized$county_geoid == target_county_fips, , drop = FALSE]
  rownames(standardized) <- NULL
  rownames(mecklenburg) <- NULL

  cat("Statewide and Mecklenburg AFO tables built.\n")

  # Audit AFO geography and classification
  normalize_county <- function(x) toupper(gsub("[^A-Z0-9]", "", ifelse(is.na(x), "", x)))
  county_comparable <- !is.na(standardized$reported_county) & trimws(standardized$reported_county) != ""
  county_mismatch <- county_comparable & normalize_county(standardized$reported_county) != normalize_county(standardized$census_county)
  duplicate_ids <- sum(duplicated(standardized$afo_record_id))
  classified_animals <- sum(standardized$animal_group != "other_or_unknown")

  qa <- data.frame(
    check = c(
      "Source AFO records",
      "Records with valid coordinates",
      "Records with invalid coordinates",
      "Spatial NC AFO records",
      "Records outside spatial NC",
      "Duplicate AFO record IDs",
      "Records with classified animal group",
      "Records with reported county available",
      "Reported versus spatial county mismatches",
      "Mecklenburg AFO records",
      "NC NPDES AFO permit records",
      "NC state AFO permit records",
      "NC digester-related records"
    ),
    value = c(
      nrow(raw),
      nrow(valid),
      invalid_coordinate_count,
      nrow(standardized),
      nrow(valid) - nrow(standardized),
      duplicate_ids,
      classified_animals,
      sum(county_comparable),
      sum(county_mismatch),
      nrow(mecklenburg),
      sum(standardized$npdes_flag),
      sum(standardized$state_permit_flag),
      sum(standardized$digester_flag)
    ),
    status = c(
      "INFO",
      "INFO",
      ifelse(invalid_coordinate_count == 0, "PASS", "CHECK"),
      "INFO",
      ifelse(nrow(valid) - nrow(standardized) == 0, "PASS", "CHECK"),
      ifelse(duplicate_ids == 0, "PASS", "FAIL"),
      ifelse(classified_animals == nrow(standardized), "PASS", "CHECK"),
      "INFO",
      ifelse(sum(county_mismatch) == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("AFO geography and classification QA/QC completed.\n")

  # Build AFO source metadata
  metadata <- data.frame(
    field = c("source", "publisher", "snapshot_access_date", "temporal_scope", "historical_2022_inventory", "spatial_assignment", "scope_note", "cafo_note"),
    value = c(
      "Animal Feed Operation Permits",
      "NC DEQ Division of Water Resources",
      access_date,
      "Current service snapshot",
      "FALSE",
      "2022 TIGER county FIPS by point location",
      paste("Permitted animal operations represented by the", "NC DEQ AFO program; not a census of all livestock farms."),
      paste("Do not label every AFO record as a federal CAFO;", "NPDES permit status is retained separately.")
    ),
    stringsAsFactors = FALSE
  )

  cat("AFO source metadata built.\n")

  # Write AFO processed outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  nc_file <- file.path(output_dir, paste0("nc_afo_permits_access_", access_date, ".csv"))
  mecklenburg_file <- file.path(output_dir, paste0("mecklenburg_afo_permits_access_", access_date, ".csv"))
  gpkg_file <- file.path(output_dir, paste0("nc_afo_permits_access_", access_date, ".gpkg"))
  metadata_file <- file.path(output_dir, paste0("nc_afo_metadata_access_", access_date, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_afo_access_", access_date, ".csv"))

  write.csv(standardized, nc_file, row.names = FALSE)
  write.csv(mecklenburg, mecklenburg_file, row.names = FALSE)

  gpkg_points <- nc_points
  gpkg_points$afo_record_id <- standardized$afo_record_id
  gpkg_points$animal_group <- standardized$animal_group
  gpkg_points$permit_program <- standardized$permit_program
  gpkg_points$npdes_flag <- standardized$npdes_flag
  gpkg_points$digester_flag <- standardized$digester_flag

  sf::st_write(gpkg_points, gpkg_file, delete_dsn = TRUE, quiet = TRUE)
  write.csv(metadata, metadata_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("AFO processed outputs written.\n")
  c(nc_file, mecklenburg_file, gpkg_file, metadata_file, qa_file)
}

upload_afo_outputs <- function(files, processed_folder, metadata_folder, validation_folder) {
  # Validate AFO output files
  if (length(files) != 5 || !all(file.exists(files))) stop("Expected five AFO output files.")

  cat("AFO output files validated.\n")

  # Upload processed AFO datasets
  invisible(lapply(files[1:3], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))
  cat("Processed AFO datasets uploaded.\n")

  # Upload AFO metadata
  googledrive::drive_upload(files[[4]], path = metadata_folder, overwrite = TRUE)
  cat("AFO metadata uploaded.\n")

  # Upload AFO QA report
  googledrive::drive_upload(files[[5]], path = validation_folder, overwrite = TRUE)
  cat("AFO QA report uploaded.\n")

  TRUE
}