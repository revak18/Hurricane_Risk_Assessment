download_brownfields_raw <- function(raw_dir, drive_folder) {
  # Define the NC DEQ Brownfields polygon snapshot
  service_url <- paste0("https://services2.arcgis.com/kCu40SDxsCGcuUWO/", "ArcGIS/rest/services/NCBP_Feature_Poly_View/", "FeatureServer/0/query")
  access_date <- format(Sys.Date(), "%Y%m%d")
  file_name <- paste0("nc_deq_brownfields_boundaries_access_", access_date, ".geojson")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  local_file <- file.path(raw_dir, file_name)
  cat("NC DEQ Brownfields snapshot defined.\n")

  # Restore today's persisted Brownfields snapshot
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_match <- drive_files[drive_files$name == file_name, ]

  if (nrow(drive_match) > 1) stop("Multiple copies of today's Brownfields snapshot found on Google Drive.")

  if (nrow(drive_match) == 1) {
    googledrive::drive_download(drive_match, path = local_file, overwrite = TRUE)
    restored <- try(sf::st_read(local_file, quiet = TRUE), silent = TRUE)
    if (!inherits(restored, "try-error") && nrow(restored) > 0) {
      cat("NC DEQ Brownfields snapshot restored from Google Drive.\n")
      return(local_file)
    }
  }

  cat("No valid persisted Brownfields snapshot found.\n")

  # Define robust ArcGIS readers
  read_arcgis_json <- function(url) {
    temp_file <- tempfile(fileext = ".json")
    result <- try(utils::download.file(url, temp_file, mode = "wb", method = "libcurl", quiet = TRUE), silent = TRUE)

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
    success <- FALSE
    result_sf <- NULL

    for (attempt in seq_len(3)) {
      result <- try(utils::download.file(url, temp_file, mode = "wb", method = "libcurl", quiet = TRUE), silent = TRUE)

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
    if (!success) stop("ArcGIS GeoJSON request failed.")
    result_sf
  }

  cat("Brownfields ArcGIS readers ready.\n")

  # Retrieve all Brownfields object IDs
  ids_url <- paste0(service_url, "?where=1%3D1", "&returnIdsOnly=true", "&f=json")
  ids_response <- read_arcgis_json(ids_url)

  if (!is.null(ids_response$error)) stop("NC DEQ Brownfields feature-ID request failed.")

  object_ids <- sort(unique(ids_response$objectIds))

  if (!length(object_ids)) stop("NC DEQ Brownfields service returned no feature IDs.")

  cat("NC DEQ Brownfields feature IDs retrieved:", length(object_ids), "\n")

  # Download Brownfields polygons in ArcGIS batches
  batches <- split(object_ids, ceiling(seq_along(object_ids) / 100))
  batch_rows <- vector("list", length(batches))

  for (batch_index in seq_along(batches)) {
    ids <- batches[[batch_index]]
    query_url <- paste0(service_url, "?objectIds=", paste(ids, collapse = "%2C"), "&outFields=%2A", "&returnGeometry=true", "&outSR=4326", "&f=geojson")
    batch_rows[[batch_index]] <- read_arcgis_geojson(query_url)
    cat("Brownfields batch", batch_index, "of", length(batches), "downloaded.\n")
  }

  cat("NC DEQ Brownfields batches downloaded.\n")

  # Combine and validate the raw Brownfields polygons
  raw <- do.call(rbind, batch_rows)
  rownames(raw) <- NULL

  if (!nrow(raw) || nrow(raw) != length(object_ids)) {
    stop(paste("Brownfields feature count mismatch:", nrow(raw), "downloaded versus", length(object_ids), "expected."))
  }

  required_fields <- c("OBJECTID", "BF_Number", "BF_Name", "County", "Status")
  missing_fields <- setdiff(required_fields, names(raw))

  if (length(missing_fields)) stop(paste("Missing Brownfields fields:", paste(missing_fields, collapse = ", ")))
  if (any(sf::st_is_empty(raw))) stop("Raw Brownfields snapshot contains empty geometries.")

  cat("NC DEQ Brownfields raw snapshot validated.\n")

  # Persist the raw Brownfields snapshot
  if (file.exists(local_file)) file.remove(local_file)
  sf::st_write(raw, local_file, driver = "GeoJSON", quiet = TRUE)
  googledrive::drive_upload(local_file, path = drive_folder, overwrite = TRUE)

  cat("NC DEQ Brownfields raw snapshot persisted.\n")
  local_file
}

build_brownfields_tables <- function(raw_file, county_file, target_county_fips, target_crs, output_dir, validation_dir) {
  # Load Brownfields polygons and TIGER counties
  sites <- sf::st_read(raw_file, quiet = TRUE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(sites) || !nrow(counties)) stop("Brownfields or county input is empty.")

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(raw_file))

  if (!grepl("^[0-9]{8}$", access_date)) stop("Could not determine Brownfields snapshot access date.")

  cat("Brownfields snapshot and TIGER counties loaded.\n")

  # Validate and repair Brownfields geometries
  invalid_before <- sum(!sf::st_is_valid(sites))
  sites <- sf::st_make_valid(sites)
  empty_geometry <- sf::st_is_empty(sites)
  empty_count <- sum(empty_geometry)
  sites <- sites[!empty_geometry, ]

  if (!nrow(sites)) stop("No Brownfields geometries remain after validation.")

  invalid_after <- sum(!sf::st_is_valid(sites))

  if (invalid_after > 0) stop("Brownfields geometries remain invalid after repair.")

  cat("Brownfields geometries validated and repaired.\n")

  # Define Brownfields source-field helpers
  clean_text <- function(x) {
    value <- trimws(as.character(x))
    value[is.na(value) | value == ""] <- NA_character_
    value
  }

  get_field <- function(data, field) {
    if (!field %in% names(data)) return(rep(NA_character_, nrow(data)))
    clean_text(data[[field]])
  }

  cat("Brownfields field helpers ready.\n")

  # Standardize Brownfields project identifiers and status
  bf_number <- get_field(sites, "BF_Number")
  bf_id <- get_field(sites, "BF_ID")
  object_id <- get_field(sites, "OBJECTID")
  project_key <- ifelse(!is.na(bf_number), paste0("BF_", bf_number), ifelse(!is.na(bf_id), paste0("BFID_", bf_id), paste0("OID_", object_id)))
  status_raw <- get_field(sites, "Status")
  status_upper <- toupper(ifelse(is.na(status_raw), "", status_raw))

  status_group <- ifelse(
    status_upper == "RECORDED", "recorded",
    ifelse(
      status_upper == "ACTIVE ELIGIBLE", "active_eligible",
      ifelse(
        status_upper == "PENDING", "pending",
        ifelse(
          status_upper == "INACTIVE ELIGIBLE", "inactive_eligible",
          ifelse(
            status_upper == "NO FURTHER INTEREST", "no_further_interest",
            ifelse(
              status_upper == "COMPLETE", "complete",
              ifelse(
                status_upper == "RFR PC COMPLETE", "rfr_pc_complete",
                ifelse(status_upper == "INELIGIBLE", "ineligible", "other_or_unknown")
              )
            )
          )
        )
      )
    )
  )

  cat("Brownfields project identifiers and status standardized.\n")

  # Dissolve source polygons into unique Brownfields projects
  sites_projected <- sf::st_transform(sites, target_crs)
  project_groups <- split(seq_len(nrow(sites_projected)), project_key)

  project_geometry <- sf::st_sfc(
    lapply(project_groups, function(index) sf::st_union(sf::st_geometry(sites_projected[index, ]))[[1]]),
    crs = sf::st_crs(sites_projected)
  )

  first_index <- vapply(project_groups, function(index) index[[1]], integer(1))
  cat("Brownfields source polygons dissolved by project.\n")

  # Build the standardized Brownfields project table
  project_table <- data.frame(
    brownfield_project_id = names(project_groups),
    bf_number = bf_number[first_index],
    bf_id = bf_id[first_index],
    bf_name = get_field(sites, "BF_Name")[first_index],
    reported_address = get_field(sites, "Address")[first_index],
    reported_city = get_field(sites, "City")[first_index],
    reported_county = get_field(sites, "County")[first_index],
    reported_acreage = suppressWarnings(as.numeric(get_field(sites, "BF_Acreage")[first_index])),
    status_raw = status_raw[first_index],
    status_group = status_group[first_index],
    contaminant_of_concern = get_field(sites, "COC")[first_index],
    restricted_media = get_field(sites, "Restricted_Media")[first_index],
    allowed_use = get_field(sites, "Allowed_Use")[first_index],
    contamination_source = get_field(sites, "Source")[first_index],
    instrument_status = get_field(sites, "Instrument_Status")[first_index],
    documents_link = get_field(sites, "Rec_Docs_Link")[first_index],
    source_polygon_count = lengths(project_groups),
    snapshot_access_date = access_date,
    stringsAsFactors = FALSE
  )

  projects_sf <- sf::st_sf(project_table, geometry = project_geometry, crs = sf::st_crs(sites_projected))

  if (any(sf::st_is_empty(projects_sf))) stop("Dissolved Brownfields projects contain empty geometries.")
  if (any(!sf::st_is_valid(projects_sf))) projects_sf <- sf::st_make_valid(projects_sf)

  cat("Unique Brownfields project layer built.\n")

  # Spatially assign Brownfields projects to TIGER counties
  counties_projected <- sf::st_transform(counties, target_crs)
  intersections <- sf::st_intersects(projects_sf, counties_projected)

  county_geoids <- vapply(
    intersections,
    function(index) {
      if (!length(index)) return(NA_character_)
      paste(sort(unique(as.character(counties_projected$GEOID[index]))), collapse = ";")
    },
    character(1)
  )

  census_counties <- vapply(
    intersections,
    function(index) {
      if (!length(index)) return(NA_character_)
      paste(sort(unique(as.character(counties_projected$NAME[index]))), collapse = ";")
    },
    character(1)
  )

  county_count <- lengths(intersections)

  intersects_mecklenburg <- vapply(
    intersections,
    function(index) target_county_fips %in% as.character(counties_projected$GEOID[index]),
    logical(1)
  )

  projects_sf$county_geoids <- county_geoids
  projects_sf$census_counties <- census_counties
  projects_sf$county_count <- county_count
  projects_sf$intersects_mecklenburg <- intersects_mecklenburg
  projects_sf$boundary_area_km2 <- as.numeric(sf::st_area(projects_sf)) / 1e6

  cat("Brownfields projects spatially assigned to TIGER counties.\n")

  # Audit reported versus spatial county geography
  normalize_county <- function(x) {
    value <- toupper(ifelse(is.na(x), "", x))
    value <- gsub("\\bCOUNTY\\b", "", value)
    value <- gsub("\\bCO\\.?\\b", "", value)
    gsub("[^A-Z0-9]", "", value)
  }

  reported_norm <- normalize_county(projects_sf$reported_county)
  reported_available <- reported_norm != ""

  county_match <- vapply(
    seq_len(nrow(projects_sf)),
    function(i) {
      if (!reported_available[[i]] || is.na(projects_sf$census_counties[[i]])) return(NA)
      spatial_names <- strsplit(projects_sf$census_counties[[i]], ";", fixed = TRUE)[[1]]
      reported_norm[[i]] %in% normalize_county(spatial_names)
    },
    logical(1)
  )

  county_mismatch <- reported_available & !is.na(county_match) & !county_match
  discrepancies <- sf::st_drop_geometry(projects_sf[county_mismatch, ])

  cat("Brownfields county geography audited.\n")

  # Build statewide and Mecklenburg Brownfields outputs
  standardized <- sf::st_drop_geometry(projects_sf)
  mecklenburg_sf <- projects_sf[projects_sf$intersects_mecklenburg, ]
  mecklenburg_table <- sf::st_drop_geometry(mecklenburg_sf)
  rownames(standardized) <- NULL
  rownames(mecklenburg_table) <- NULL

  cat("Statewide and Mecklenburg Brownfields layers built.\n")

  # Validate Brownfields project tables
  duplicate_project_ids <- sum(duplicated(standardized$brownfield_project_id))
  missing_project_ids <- sum(is.na(standardized$brownfield_project_id) | standardized$brownfield_project_id == "")
  missing_county_assignment <- sum(standardized$county_count == 0)
  multicounty_projects <- sum(standardized$county_count > 1)
  missing_status <- sum(is.na(standardized$status_raw))
  multi_polygon_projects <- sum(standardized$source_polygon_count > 1)

  qa <- data.frame(
    check = c(
      "Source Brownfields polygon records",
      "Unique Brownfields projects",
      "Projects built from multiple source polygons",
      "Invalid geometries before repair",
      "Invalid geometries after repair",
      "Empty geometries removed",
      "Duplicate Brownfields project IDs",
      "Missing Brownfields project IDs",
      "Projects without TIGER county intersection",
      "Projects spanning multiple counties",
      "Projects with reported county available",
      "Reported versus spatial county mismatches",
      "Mecklenburg-intersecting Brownfields projects",
      "Recorded projects",
      "Active Eligible projects",
      "Pending projects",
      "Inactive Eligible projects",
      "Complete projects",
      "RFR PC Complete projects",
      "No Further Interest projects",
      "Ineligible projects",
      "Other or unknown project status",
      "Projects missing status"
    ),
    value = c(
      nrow(sites),
      nrow(standardized),
      multi_polygon_projects,
      invalid_before,
      invalid_after,
      empty_count,
      duplicate_project_ids,
      missing_project_ids,
      missing_county_assignment,
      multicounty_projects,
      sum(reported_available),
      sum(county_mismatch),
      nrow(mecklenburg_table),
      sum(standardized$status_group == "recorded"),
      sum(standardized$status_group == "active_eligible"),
      sum(standardized$status_group == "pending"),
      sum(standardized$status_group == "inactive_eligible"),
      sum(standardized$status_group == "complete"),
      sum(standardized$status_group == "rfr_pc_complete"),
      sum(standardized$status_group == "no_further_interest"),
      sum(standardized$status_group == "ineligible"),
      sum(standardized$status_group == "other_or_unknown"),
      missing_status
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      ifelse(invalid_before == 0, "PASS", "CHECK"),
      ifelse(invalid_after == 0, "PASS", "FAIL"),
      ifelse(empty_count == 0, "PASS", "CHECK"),
      ifelse(duplicate_project_ids == 0, "PASS", "FAIL"),
      ifelse(missing_project_ids == 0, "PASS", "FAIL"),
      ifelse(missing_county_assignment == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      ifelse(sum(county_mismatch) == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(missing_status == 0, "PASS", "CHECK")
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("Brownfields geography and status QA/QC completed.\n")

  # Build Brownfields source metadata
  metadata <- data.frame(
    field = c(
      "source",
      "publisher",
      "dataset_type",
      "snapshot_access_date",
      "temporal_scope",
      "historical_2022_inventory",
      "geometry",
      "project_dissolve",
      "spatial_assignment",
      "scope_note"
    ),
    value = c(
      "NCBP Feature Polygon View",
      "NC DEQ Brownfields Redevelopment Section",
      "North Carolina Brownfields project boundaries",
      access_date,
      "Current NC DEQ Brownfields database snapshot",
      "FALSE",
      "Brownfields project boundary polygons",
      "Source polygons dissolved by Brownfields project identifier",
      "2022 TIGER county intersection",
      paste("This layer represents projects documented in the", "NC DEQ Brownfields Redevelopment Section database.", "It is not a complete inventory of every contaminated", "property in North Carolina.")
    ),
    stringsAsFactors = FALSE
  )

  cat("Brownfields source metadata built.\n")

  # Write Brownfields processed outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  nc_csv <- file.path(output_dir, paste0("nc_brownfields_projects_access_", access_date, ".csv"))
  mecklenburg_csv <- file.path(output_dir, paste0("mecklenburg_brownfields_projects_access_", access_date, ".csv"))
  nc_gpkg <- file.path(output_dir, paste0("nc_brownfields_boundaries_access_", access_date, ".gpkg"))
  mecklenburg_gpkg <- file.path(output_dir, paste0("mecklenburg_brownfields_boundaries_access_", access_date, ".gpkg"))
  metadata_file <- file.path(output_dir, paste0("nc_brownfields_metadata_access_", access_date, ".csv"))
  discrepancy_file <- file.path(validation_dir, paste0("brownfields_geography_discrepancies_access_", access_date, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_brownfields_access_", access_date, ".csv"))

  write.csv(standardized, nc_csv, row.names = FALSE)
  write.csv(mecklenburg_table, mecklenburg_csv, row.names = FALSE)
  sf::st_write(projects_sf, nc_gpkg, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(mecklenburg_sf, mecklenburg_gpkg, delete_dsn = TRUE, quiet = TRUE)
  write.csv(metadata, metadata_file, row.names = FALSE)
  write.csv(discrepancies, discrepancy_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("Brownfields processed outputs written.\n")
  c(nc_csv, mecklenburg_csv, nc_gpkg, mecklenburg_gpkg, metadata_file, discrepancy_file, qa_file)
}

upload_brownfields_outputs <- function(files, processed_folder, metadata_folder, validation_folder) {
  # Validate Brownfields output files
  if (length(files) != 7 || !all(file.exists(files))) stop("Expected seven Brownfields output files.")

  cat("Brownfields output files validated.\n")

  # Upload processed Brownfields datasets
  invisible(lapply(files[1:4], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))

  cat("Processed Brownfields datasets uploaded.\n")

  # Upload Brownfields metadata
  googledrive::drive_upload(files[[5]], path = metadata_folder, overwrite = TRUE)

  cat("Brownfields metadata uploaded.\n")

  # Upload Brownfields QA reports
  invisible(lapply(files[6:7], googledrive::drive_upload, path = validation_folder, overwrite = TRUE))

  cat("Brownfields QA reports uploaded.\n")
  TRUE
}