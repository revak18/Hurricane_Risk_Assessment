download_wastewater_raw <- function(raw_dir, drive_folder) {
  # Define the NC DEQ NPDES wastewater snapshot
  service_url <- paste0(
    "https://services2.arcgis.com/kCu40SDxsCGcuUWO/",
    "ArcGIS/rest/services/NPDES_Wastewater_Discharge_Permits/",
    "FeatureServer/0/query"
  )
  access_date <- format(Sys.Date(), "%Y%m%d")
  file_name <- paste0("nc_deq_npdes_wastewater_access_", access_date, ".geojson")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  local_file <- file.path(raw_dir, file_name)
  cat("NC DEQ NPDES wastewater snapshot defined.\n")

  # Restore today's persisted wastewater snapshot
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_match <- drive_files[drive_files$name == file_name, ]

  if (nrow(drive_match) > 1) {
    stop("Multiple copies of today's wastewater snapshot found on Google Drive.")
  }

  if (nrow(drive_match) == 1) {
    googledrive::drive_download(drive_match, path = local_file, overwrite = TRUE)
    restored <- try(sf::st_read(local_file, quiet = TRUE), silent = TRUE)

    if (!inherits(restored, "try-error") && nrow(restored) > 0) {
      cat("NC DEQ wastewater snapshot restored from Google Drive.\n")
      return(local_file)
    }
  }

  cat("No valid persisted wastewater snapshot found.\n")

  # Define robust ArcGIS readers
  read_arcgis_json <- function(url) {
    temp_file <- tempfile(fileext = ".json")
    result <- try(
      utils::download.file(
        url,
        temp_file,
        mode = "wb",
        method = "libcurl",
        quiet = TRUE
      ),
      silent = TRUE
    )

    if (inherits(result, "try-error") || !file.exists(temp_file) || file.info(temp_file)$size == 0) {
      unlink(temp_file)
      stop("ArcGIS JSON request failed.")
    }

    response <- jsonlite::fromJSON(temp_file)
    unlink(temp_file)
    response
  }

  read_arcgis_geojson <- function(url) {
    temp_file <- tempfile(fileext = ".geojson")
    result_sf <- NULL
    success <- FALSE

    for (attempt in seq_len(3)) {
      result <- try(
        utils::download.file(
          url,
          temp_file,
          mode = "wb",
          method = "libcurl",
          quiet = TRUE
        ),
        silent = TRUE
      )

      if (!inherits(result, "try-error") && file.exists(temp_file) && file.info(temp_file)$size > 100) {
        result_sf <- try(sf::st_read(temp_file, quiet = TRUE), silent = TRUE)

        if (!inherits(result_sf, "try-error") && nrow(result_sf) > 0) {
          success <- TRUE
          break
        }
      }

      Sys.sleep(2 * attempt)
    }

    unlink(temp_file)

    if (!success) {
      stop("ArcGIS GeoJSON request failed.")
    }

    result_sf
  }

  cat("Wastewater ArcGIS readers ready.\n")

  # Retrieve all wastewater permit object IDs
  ids_url <- paste0(service_url, "?where=1%3D1", "&returnIdsOnly=true", "&f=json")
  ids_response <- read_arcgis_json(ids_url)

  if (!is.null(ids_response$error)) {
    stop("NC DEQ wastewater feature-ID request failed.")
  }

  object_ids <- sort(unique(ids_response$objectIds))

  if (!length(object_ids)) {
    stop("NC DEQ wastewater service returned no feature IDs.")
  }

  cat("NC DEQ wastewater feature IDs retrieved:", length(object_ids), "\n")

  # Download wastewater permits in ArcGIS batches
  batches <- split(object_ids, ceiling(seq_along(object_ids) / 100))
  batch_rows <- vector("list", length(batches))

  for (batch_index in seq_along(batches)) {
    ids <- batches[[batch_index]]
    query_url <- paste0(
      service_url,
      "?objectIds=",
      paste(ids, collapse = "%2C"),
      "&outFields=%2A",
      "&returnGeometry=true",
      "&outSR=4326",
      "&f=geojson"
    )
    batch_rows[[batch_index]] <- read_arcgis_geojson(query_url)
    cat("Wastewater batch", batch_index, "of", length(batches), "downloaded.\n")
  }

  cat("NC DEQ wastewater batches downloaded.\n")

  # Combine and validate the raw wastewater snapshot
  raw <- do.call(rbind, batch_rows)
  rownames(raw) <- NULL

  if (!nrow(raw) || nrow(raw) != length(object_ids)) {
    stop(
      paste(
        "Wastewater feature count mismatch:",
        nrow(raw),
        "downloaded versus",
        length(object_ids),
        "expected."
      )
    )
  }

  required_fields <- c(
    "ObjectId",
    "PERMITNUMBER",
    "PERMIT_TYPE",
    "PERMIT_STATUS",
    "FACILITY",
    "FACILITY_ACTIVE",
    "FACILITY_STATUS",
    "MAJOR",
    "COUNTY",
    "ASBUILTFLOWQTYGPD"
  )

  missing_fields <- setdiff(required_fields, names(raw))

  if (length(missing_fields)) {
    stop(paste("Missing wastewater fields:", paste(missing_fields, collapse = ", ")))
  }

  cat("NC DEQ wastewater raw snapshot validated.\n")

  # Persist the raw wastewater snapshot
  if (file.exists(local_file)) {
    file.remove(local_file)
  }

  sf::st_write(raw, local_file, driver = "GeoJSON", quiet = TRUE)
  googledrive::drive_upload(local_file, path = drive_folder, overwrite = TRUE)
  cat("NC DEQ wastewater raw snapshot persisted.\n")
  local_file
}

build_wastewater_tables <- function(
  raw_file,
  county_file,
  target_county_fips,
  target_crs,
  output_dir,
  validation_dir
) {
  # Load wastewater permits and TIGER counties
  sites <- sf::st_read(raw_file, quiet = TRUE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(sites) || !nrow(counties)) {
    stop("Wastewater or county input is empty.")
  }

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(raw_file))

  if (!grepl("^[0-9]{8}$", access_date)) {
    stop("Could not determine wastewater snapshot access date.")
  }

  source_record_count <- nrow(sites)
  cat("Wastewater snapshot and TIGER counties loaded.\n")

  # Validate wastewater point geometries
  empty_geometry <- sf::st_is_empty(sites)
  empty_count <- sum(empty_geometry)
  sites <- sites[!empty_geometry, ]

  if (!nrow(sites)) {
    stop("No wastewater geometries remain.")
  }

  invalid_before <- sum(!sf::st_is_valid(sites))
  sites <- sf::st_make_valid(sites)
  invalid_after <- sum(!sf::st_is_valid(sites))
  geometry_types <- as.character(sf::st_geometry_type(sites))
  nonpoint_count <- sum(!geometry_types %in% c("POINT", "MULTIPOINT"))

  if (invalid_after > 0) {
    stop("Wastewater geometries remain invalid.")
  }

  if (nonpoint_count > 0) {
    stop("Wastewater source contains non-point geometries.")
  }

  cat("Wastewater point geometries validated.\n")

  # Define wastewater field helpers
  clean_text <- function(x) {
    value <- trimws(as.character(x))
    value[is.na(value) | value == ""] <- NA_character_
    value
  }

  get_field <- function(data, field) {
    if (!field %in% names(data)) {
      return(rep(NA_character_, nrow(data)))
    }

    clean_text(data[[field]])
  }

  get_numeric <- function(data, field) {
    if (!field %in% names(data)) {
      return(rep(NA_real_, nrow(data)))
    }

    suppressWarnings(as.numeric(data[[field]]))
  }

  cat("Wastewater field helpers ready.\n")

  # Standardize wastewater permit attributes
  permit_number <- get_field(sites, "PERMITNUMBER")
  object_id <- get_field(sites, "ObjectId")

  base_id <- ifelse(
    !is.na(permit_number),
    paste0("NPDES_", permit_number),
    paste0("OID_", object_id)
  )

  duplicate_base <- duplicated(base_id) | duplicated(base_id, fromLast = TRUE)

  wastewater_site_id <- ifelse(
    duplicate_base,
    paste0(base_id, "_OID_", object_id),
    base_id
  )

  major_value <- get_numeric(sites, "MAJOR")
  active_value <- get_numeric(sites, "FACILITY_ACTIVE")
  flow_gpd <- get_numeric(sites, "ASBUILTFLOWQTYGPD")

  major_flag <- !is.na(major_value) & major_value == 1
  facility_active_flag <- ifelse(is.na(active_value), NA, active_value == 1)
  flow_mgd <- flow_gpd / 1e6

  cat("Wastewater permit attributes standardized.\n")

  # Extract wastewater coordinates
  sites_wgs <- sf::st_transform(sites, 4326)
  coordinates <- sf::st_coordinates(sites_wgs)

  if (nrow(coordinates) != nrow(sites_wgs)) {
    stop("Wastewater geometry does not resolve to one point per permit.")
  }

  longitude <- coordinates[, 1]
  latitude <- coordinates[, 2]

  valid_coordinates <-
    is.finite(longitude) &
    is.finite(latitude) &
    longitude >= -180 &
    longitude <= 180 &
    latitude >= -90 &
    latitude <= 90

  invalid_coordinate_count <- sum(!valid_coordinates)

  if (invalid_coordinate_count > 0) {
    stop("Wastewater source contains invalid coordinates.")
  }

  cat("Wastewater coordinates validated.\n")

  # Build standardized wastewater point layer
  standardized <- data.frame(
    wastewater_site_id = wastewater_site_id,
    npdes_permit_number = permit_number,
    facility_name = get_field(sites, "FACILITY"),
    permit_type = get_field(sites, "PERMIT_TYPE"),
    permit_status = get_field(sites, "PERMIT_STATUS"),
    original_issued_date = get_field(sites, "ORIGINAL_ISSUED_DT"),
    permit_effective_date = get_field(sites, "PERMIT_EFFECTIVE_DATE"),
    permit_expiration_date = get_field(sites, "PERMIT_EXPIRATION_DT"),
    facility_active = facility_active_flag,
    facility_status = get_field(sites, "FACILITY_STATUS"),
    owner = get_field(sites, "OWNER"),
    owner_type = get_field(sites, "OWNER_TYPE"),
    major = major_flag,
    reported_county = get_field(sites, "COUNTY"),
    region = get_field(sites, "REGION"),
    asbuilt_flow_gpd = flow_gpd,
    asbuilt_flow_mgd = flow_mgd,
    source_url = get_field(sites, "URL"),
    longitude = longitude,
    latitude = latitude,
    snapshot_access_date = access_date,
    stringsAsFactors = FALSE
  )

  sites_sf <- sf::st_sf(
    standardized,
    geometry = sf::st_geometry(sites_wgs),
    crs = 4326
  )

  sites_sf <- sf::st_transform(sites_sf, target_crs)
  cat("Standardized wastewater point layer built.\n")

  # Spatially assign wastewater permits to TIGER counties
  counties_projected <- sf::st_transform(counties, target_crs)
  intersections <- sf::st_intersects(sites_sf, counties_projected)

  county_geoids <- vapply(
    intersections,
    function(index) {
      if (!length(index)) {
        return(NA_character_)
      }

      paste(
        sort(unique(as.character(counties_projected$GEOID[index]))),
        collapse = ";"
      )
    },
    character(1)
  )

  census_counties <- vapply(
    intersections,
    function(index) {
      if (!length(index)) {
        return(NA_character_)
      }

      paste(
        sort(unique(as.character(counties_projected$NAME[index]))),
        collapse = ";"
      )
    },
    character(1)
  )

  county_count <- lengths(intersections)
  sites_sf$county_geoids <- county_geoids
  sites_sf$census_counties <- census_counties
  sites_sf$county_count <- county_count
  spatial_nc <- county_count > 0

  cat("Wastewater permits spatially assigned to TIGER counties.\n")

  # Audit reported versus TIGER county geography
  normalize_county <- function(x) {
    value <- toupper(ifelse(is.na(x), "", x))
    value <- gsub("\\bCOUNTY\\b", "", value)
    value <- gsub("\\bCO\\.?\\b", "", value)
    gsub("[^A-Z0-9]", "", value)
  }

  reported_norm <- normalize_county(sites_sf$reported_county)
  reported_available <- reported_norm != ""

  county_match <- vapply(
    seq_len(nrow(sites_sf)),
    function(i) {
      if (!reported_available[[i]] || is.na(sites_sf$census_counties[[i]])) {
        return(NA)
      }

      spatial_names <- strsplit(
        sites_sf$census_counties[[i]],
        ";",
        fixed = TRUE
      )[[1]]

      reported_norm[[i]] %in% normalize_county(spatial_names)
    },
    logical(1)
  )

  county_mismatch <-
    reported_available &
    !is.na(county_match) &
    !county_match

  cat("Wastewater county geography audited.\n")

  # Build statewide and Mecklenburg wastewater layers
  nc_sf <- sites_sf[spatial_nc, ]

  intersects_mecklenburg <- vapply(
    strsplit(
      ifelse(is.na(nc_sf$county_geoids), "", nc_sf$county_geoids),
      ";",
      fixed = TRUE
    ),
    function(x) {
      target_county_fips %in% x
    },
    logical(1)
  )

  nc_sf$intersects_mecklenburg <- intersects_mecklenburg
  mecklenburg_sf <- nc_sf[nc_sf$intersects_mecklenburg, ]
  nc_table <- sf::st_drop_geometry(nc_sf)
  mecklenburg_table <- sf::st_drop_geometry(mecklenburg_sf)
  rownames(nc_table) <- NULL
  rownames(mecklenburg_table) <- NULL

  cat("Statewide and Mecklenburg wastewater layers built.\n")

  # Build wastewater discrepancy table
  discrepancy_type <- rep(NA_character_, nrow(sites_sf))
  discrepancy_type[!spatial_nc] <- "outside_spatial_nc"

  discrepancy_type[county_mismatch] <- ifelse(
    is.na(discrepancy_type[county_mismatch]),
    "reported_spatial_county_mismatch",
    paste0(
      discrepancy_type[county_mismatch],
      ";reported_spatial_county_mismatch"
    )
  )

  discrepancies <- sf::st_drop_geometry(sites_sf[!is.na(discrepancy_type), ])
  discrepancies$discrepancy_type <- discrepancy_type[!is.na(discrepancy_type)]

  cat("Wastewater discrepancy table built.\n")

  # Validate statewide wastewater records
  duplicate_permit_numbers <- sum(
    duplicated(nc_table$npdes_permit_number[!is.na(nc_table$npdes_permit_number)])
  )

  missing_permit_numbers <- sum(is.na(nc_table$npdes_permit_number))
  duplicate_site_ids <- sum(duplicated(nc_table$wastewater_site_id))
  missing_major <- sum(is.na(nc_table$major))
  flow_available <- sum(!is.na(nc_table$asbuilt_flow_gpd))
  negative_flow <- sum(nc_table$asbuilt_flow_gpd < 0, na.rm = TRUE)

  qa <- data.frame(
    check = c(
      "Source wastewater permit records",
      "Valid wastewater point records",
      "Invalid or empty geometries removed",
      "Invalid wastewater coordinates",
      "Spatial NC wastewater permits",
      "Wastewater permits outside spatial NC",
      "Duplicate wastewater site IDs",
      "Duplicate NPDES permit numbers",
      "Missing NPDES permit numbers",
      "Reported county available",
      "Reported versus spatial county mismatches",
      "Mecklenburg spatial wastewater permits",
      "Major NC wastewater permits",
      "Minor NC wastewater permits",
      "Wastewater permits missing major flag",
      "NC permits with as-built flow available",
      "NC permits with negative as-built flow",
      "Mecklenburg major wastewater permits",
      "Mecklenburg permits with as-built flow available"
    ),
    value = c(
      source_record_count,
      nrow(sites),
      empty_count,
      invalid_coordinate_count,
      nrow(nc_table),
      sum(!spatial_nc),
      duplicate_site_ids,
      duplicate_permit_numbers,
      missing_permit_numbers,
      sum(reported_available[spatial_nc]),
      sum(county_mismatch[spatial_nc]),
      nrow(mecklenburg_table),
      sum(nc_table$major %in% TRUE, na.rm = TRUE),
      sum(nc_table$major %in% FALSE, na.rm = TRUE),
      missing_major,
      flow_available,
      negative_flow,
      sum(mecklenburg_table$major %in% TRUE, na.rm = TRUE),
      sum(!is.na(mecklenburg_table$asbuilt_flow_gpd))
    ),
    status = c(
      "INFO",
      "INFO",
      ifelse(empty_count == 0, "PASS", "CHECK"),
      ifelse(invalid_coordinate_count == 0, "PASS", "FAIL"),
      "INFO",
      ifelse(sum(!spatial_nc) == 0, "PASS", "CHECK"),
      ifelse(duplicate_site_ids == 0, "PASS", "FAIL"),
      ifelse(duplicate_permit_numbers == 0, "PASS", "CHECK"),
      ifelse(missing_permit_numbers == 0, "PASS", "CHECK"),
      "INFO",
      ifelse(sum(county_mismatch[spatial_nc]) == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      "INFO",
      ifelse(missing_major == 0, "PASS", "CHECK"),
      "INFO",
      ifelse(negative_flow == 0, "PASS", "FAIL"),
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("Wastewater geography and attribute QA/QC completed.\n")

  # Build wastewater source metadata
  metadata <- data.frame(
    field = c(
      "source",
      "publisher",
      "program",
      "service_item_id",
      "dataset_type",
      "snapshot_access_date",
      "temporal_scope",
      "historical_2022_inventory",
      "geometry",
      "capacity_field",
      "major_field",
      "spatial_assignment",
      "update_note",
      "scope_note"
    ),
    value = c(
      "NPDES Wastewater Discharge Permits",
      "NC DEQ Division of Water Resources",
      "National Pollutant Discharge Elimination System",
      "cd70229f8b1f407caa28c2586857c5f4",
      "Active permitted point-source wastewater discharges",
      access_date,
      "Current NC DEQ permit database snapshot",
      "FALSE",
      "Permit facility point locations",
      "ASBUILTFLOWQTYGPD",
      "MAJOR",
      "2022 TIGER county intersection",
      "NC DEQ states the source is refreshed from the DWR permit database on a recurring schedule.",
      paste(
        "This layer represents active NPDES discharge permits.",
        "It does not represent every sewer line, pump station,",
        "collection system asset, or unpermitted wastewater source."
      )
    ),
    stringsAsFactors = FALSE
  )

  cat("Wastewater source metadata built.\n")

  # Write wastewater processed outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  nc_csv <- file.path(
    output_dir,
    paste0("nc_npdes_wastewater_permits_access_", access_date, ".csv")
  )

  mecklenburg_csv <- file.path(
    output_dir,
    paste0("mecklenburg_npdes_wastewater_permits_access_", access_date, ".csv")
  )

  nc_gpkg <- file.path(
    output_dir,
    paste0("nc_npdes_wastewater_permits_access_", access_date, ".gpkg")
  )

  mecklenburg_gpkg <- file.path(
    output_dir,
    paste0("mecklenburg_npdes_wastewater_permits_access_", access_date, ".gpkg")
  )

  metadata_file <- file.path(
    output_dir,
    paste0("nc_npdes_wastewater_metadata_access_", access_date, ".csv")
  )

  discrepancy_file <- file.path(
    validation_dir,
    paste0("wastewater_geography_discrepancies_access_", access_date, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_wastewater_access_", access_date, ".csv")
  )

  write.csv(nc_table, nc_csv, row.names = FALSE)
  write.csv(mecklenburg_table, mecklenburg_csv, row.names = FALSE)
  sf::st_write(nc_sf, nc_gpkg, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(mecklenburg_sf, mecklenburg_gpkg, delete_dsn = TRUE, quiet = TRUE)
  write.csv(metadata, metadata_file, row.names = FALSE)
  write.csv(discrepancies, discrepancy_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("Wastewater processed outputs written.\n")

  c(
    nc_csv,
    mecklenburg_csv,
    nc_gpkg,
    mecklenburg_gpkg,
    metadata_file,
    discrepancy_file,
    qa_file
  )
}

upload_wastewater_outputs <- function(
  files,
  processed_folder,
  metadata_folder,
  validation_folder
) {
  # Validate wastewater output files
  if (length(files) != 7 || !all(file.exists(files))) {
    stop("Expected seven wastewater output files.")
  }

  cat("Wastewater output files validated.\n")

  # Upload processed wastewater datasets
  invisible(
    lapply(
      files[1:4],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("Processed wastewater datasets uploaded.\n")

  # Upload wastewater metadata
  googledrive::drive_upload(
    files[[5]],
    path = metadata_folder,
    overwrite = TRUE
  )

  cat("Wastewater metadata uploaded.\n")

  # Upload wastewater QA reports
  invisible(
    lapply(
      files[6:7],
      googledrive::drive_upload,
      path = validation_folder,
      overwrite = TRUE
    )
  )

  cat("Wastewater QA reports uploaded.\n")
  TRUE
}