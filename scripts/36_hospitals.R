download_hospitals_raw <- function(raw_dir, drive_folder, state_abbr) {
  # Define the HIFLD hospital source
  layer_url <- paste0("https://services.arcgis.com/XG15cJAlne2vxtgt/", "ArcGIS/rest/services/Hospitals_hifld/FeatureServer/0")
  query_url <- paste0(layer_url, "/query")
  access_date <- format(Sys.Date(), "%Y%m%d")
  file_name <- paste0("hifld_hospitals_access_", access_date, ".geojson")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  local_file <- file.path(raw_dir, file_name)
  cat("HIFLD hospital source defined.\n")

  # Restore today's persisted hospital snapshot
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_match <- drive_files[drive_files$name == file_name, ]
  if (nrow(drive_match) > 1) stop("Multiple copies of today's hospital snapshot found on Google Drive.")
  if (nrow(drive_match) == 1) {
    googledrive::drive_download(drive_match, path = local_file, overwrite = TRUE)
    restored <- try(sf::st_read(local_file, quiet = TRUE), silent = TRUE)
    if (!inherits(restored, "try-error") && nrow(restored) > 0) {
      cat("HIFLD hospital snapshot restored from Google Drive.\n")
      return(local_file)
    }
  }
  cat("No valid persisted hospital snapshot found.\n")

  # Define a robust ArcGIS JSON reader
  read_arcgis_json <- function(url) {
    temp_file <- tempfile(fileext = ".json")
    response <- NULL
    success <- FALSE
    for (attempt in seq_len(3)) {
      result <- try(utils::download.file(url, temp_file, mode = "wb", method = "libcurl", quiet = TRUE), silent = TRUE)
      if (!inherits(result, "try-error") && file.exists(temp_file) && file.info(temp_file)$size > 20) {
        response <- try(jsonlite::fromJSON(temp_file, simplifyVector = FALSE), silent = TRUE)
        if (!inherits(response, "try-error")) {
          success <- TRUE
          break
        }
      }
      Sys.sleep(2 * attempt)
    }
    unlink(temp_file)
    if (!success) stop("Hospital ArcGIS JSON request failed.")
    if (!is.null(response$error)) {
      error_message <- if (!is.null(response$error$message)) response$error$message else "Unknown ArcGIS service error"
      stop(paste("Hospital ArcGIS service error:", error_message))
    }
    response
  }
  cat("Hospital ArcGIS JSON reader ready.\n")

  # Resolve the hospital layer schema
  layer_metadata <- read_arcgis_json(paste0(layer_url, "?f=json"))
  if (is.null(layer_metadata$fields) || !length(layer_metadata$fields)) stop("Hospital layer metadata contains no fields.")
  field_names <- vapply(layer_metadata$fields, function(field) if (is.null(field$name)) "" else as.character(field$name), character(1))
  oid_field <- if (!is.null(layer_metadata$objectIdField)) {
    as.character(layer_metadata$objectIdField)
  } else {
    oid_candidates <- vapply(layer_metadata$fields, function(field) {
      if (!is.null(field$type) && identical(field$type, "esriFieldTypeOID")) as.character(field$name) else ""
    }, character(1))
    oid_candidates <- oid_candidates[oid_candidates != ""]
    if (!length(oid_candidates)) stop("Could not determine hospital object-ID field.")
    oid_candidates[[1]]
  }
  state_candidates <- c("STATE", "State", "STATE_ABBR", "ST_ABBREV")
  state_field <- state_candidates[state_candidates %in% field_names][1]
  if (is.na(state_field)) stop("Could not determine hospital state field.")
  cat("Hospital ArcGIS schema resolved.\n")

  # Convert ArcGIS hospital features to sf
  feature_page_to_sf <- function(response, oid_field) {
    features <- response$features
    if (is.null(features) || !length(features)) return(NULL)
    attribute_rows <- lapply(features, function(feature) {
      attributes <- feature$attributes
      if (is.null(attributes)) attributes <- list()
      as.data.frame(attributes, stringsAsFactors = FALSE, check.names = FALSE)
    })
    all_names <- unique(unlist(lapply(attribute_rows, names), use.names = FALSE))
    attribute_rows <- lapply(attribute_rows, function(row) {
      missing_names <- setdiff(all_names, names(row))
      for (name in missing_names) row[[name]] <- NA
      row[, all_names, drop = FALSE]
    })
    attributes <- do.call(rbind, attribute_rows)
    geometry_x <- vapply(features, function(feature) {
      if (is.null(feature$geometry) || is.null(feature$geometry$x)) return(NA_real_)
      suppressWarnings(as.numeric(feature$geometry$x))
    }, numeric(1))
    geometry_y <- vapply(features, function(feature) {
      if (is.null(feature$geometry) || is.null(feature$geometry$y)) return(NA_real_)
      suppressWarnings(as.numeric(feature$geometry$y))
    }, numeric(1))
    if (any(!is.finite(geometry_x)) || any(!is.finite(geometry_y))) stop("Hospital page contains missing or invalid point geometry.")
    if (!oid_field %in% names(attributes)) stop("Hospital page is missing its object-ID field.")
    attributes$source_object_id <- as.character(attributes[[oid_field]])
    attributes$geometry_x <- geometry_x
    attributes$geometry_y <- geometry_y
    sf::st_as_sf(attributes, coords = c("geometry_x", "geometry_y"), crs = 4326, remove = TRUE)
  }
  cat("Hospital ArcGIS feature converter ready.\n")

  # Validate the North Carolina hospital query
  state_full <- state.name[match(toupper(state_abbr), state.abb)]
  state_values <- unique(c(state_abbr, if (!is.na(state_full)) state_full else NULL))
  first_page <- NULL
  selected_state_value <- NULL
  for (state_value in state_values) {
    where <- utils::URLencode(paste0(state_field, "='", state_value, "'"), reserved = TRUE)
    test_url <- paste0(
      query_url, "?where=", where, "&outFields=%2A", "&returnGeometry=true", "&outSR=4326",
      "&orderByFields=", utils::URLencode(oid_field, reserved = TRUE),
      "&resultOffset=0", "&resultRecordCount=2000", "&f=json"
    )
    response <- read_arcgis_json(test_url)
    candidate <- feature_page_to_sf(response, oid_field)
    if (!is.null(candidate) && nrow(candidate) > 0) {
      first_page <- candidate
      selected_state_value <- state_value
      break
    }
  }
  if (is.null(first_page)) stop("HIFLD hospital service returned no North Carolina records.")
  cat("North Carolina hospital query validated.\n")

  # Download all North Carolina hospitals
  page_size <- 2000L
  page_rows <- list(first_page)
  page_index <- 1L
  if (nrow(first_page) == page_size) {
    offset <- page_size
    repeat {
      where <- utils::URLencode(paste0(state_field, "='", selected_state_value, "'"), reserved = TRUE)
      page_url <- paste0(
        query_url, "?where=", where, "&outFields=%2A", "&returnGeometry=true", "&outSR=4326",
        "&orderByFields=", utils::URLencode(oid_field, reserved = TRUE),
        "&resultOffset=", offset, "&resultRecordCount=", page_size, "&f=json"
      )
      response <- read_arcgis_json(page_url)
      page <- feature_page_to_sf(response, oid_field)
      if (is.null(page) || !nrow(page)) break
      page_index <- page_index + 1L
      page_rows[[page_index]] <- page
      cat("Hospital page", page_index, "downloaded:", nrow(page), "records.\n")
      if (nrow(page) < page_size) break
      offset <- offset + page_size
    }
  }
  raw <- do.call(rbind, page_rows)
  rownames(raw) <- NULL
  cat("North Carolina hospital records downloaded:", nrow(raw), "\n")

  # Validate the raw North Carolina hospital snapshot
  if (!nrow(raw)) stop("Combined North Carolina hospital extract is empty.")
  if (anyDuplicated(raw$source_object_id)) stop("Duplicate hospital source object IDs found.")
  if (state_field %in% names(raw)) {
    downloaded_state <- toupper(trimws(as.character(raw[[state_field]])))
    valid_state_values <- toupper(state_values)
    outside_state <- sum(!is.na(downloaded_state) & !downloaded_state %in% valid_state_values)
    if (outside_state > 0) stop("Hospital extract contains records outside North Carolina.")
  }
  geometry_types <- as.character(sf::st_geometry_type(raw))
  if (any(!geometry_types %in% c("POINT", "MULTIPOINT"))) stop("Hospital raw extract contains non-point geometry.")
  cat("North Carolina raw hospital snapshot validated.\n")

  # Persist the raw hospital snapshot
  if (file.exists(local_file)) file.remove(local_file)
  sf::st_write(raw, local_file, driver = "GeoJSON", quiet = TRUE)
  googledrive::drive_upload(local_file, path = drive_folder, overwrite = TRUE)
  cat("North Carolina raw hospital snapshot persisted.\n")
  local_file
}

build_hospital_tables <- function(raw_file, county_file, target_county_fips, target_crs, output_dir, validation_dir) {
  # Load hospitals and TIGER counties
  hospitals <- sf::st_read(raw_file, quiet = TRUE)
  counties <- sf::st_read(county_file, quiet = TRUE)
  if (!nrow(hospitals) || !nrow(counties)) stop("Hospital or county input is empty.")
  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(raw_file))
  if (!grepl("^[0-9]{8}$", access_date)) stop("Could not determine hospital snapshot access date.")
  source_record_count <- nrow(hospitals)
  cat("Hospital snapshot and TIGER counties loaded.\n")

  # Validate hospital point geometries
  empty_geometry <- sf::st_is_empty(hospitals)
  empty_count <- sum(empty_geometry)
  hospitals <- hospitals[!empty_geometry, ]
  if (!nrow(hospitals)) stop("No hospital geometries remain.")
  invalid_before <- sum(!sf::st_is_valid(hospitals))
  hospitals <- sf::st_make_valid(hospitals)
  invalid_after <- sum(!sf::st_is_valid(hospitals))
  geometry_types <- as.character(sf::st_geometry_type(hospitals))
  nonpoint_count <- sum(!geometry_types %in% c("POINT", "MULTIPOINT"))
  if (invalid_after > 0) stop("Hospital geometries remain invalid.")
  if (nonpoint_count > 0) stop("Hospital source contains non-point geometries.")
  cat("Hospital point geometries validated.\n")

  # Define hospital field helpers
  clean_text <- function(x) {
    value <- trimws(as.character(x))
    unavailable <- toupper(value) %in% c("", "NOT AVAILABLE", "N/A", "NA", "NULL")
    value[is.na(value) | unavailable] <- NA_character_
    value
  }
  get_field <- function(data, candidates) {
    field <- candidates[candidates %in% names(data)][1]
    if (is.na(field)) return(rep(NA_character_, nrow(data)))
    clean_text(data[[field]])
  }
  get_numeric <- function(data, candidates) {
    field <- candidates[candidates %in% names(data)][1]
    if (is.na(field)) return(rep(NA_real_, nrow(data)))
    suppressWarnings(as.numeric(data[[field]]))
  }
  normalize_fips <- function(x) {
    value <- clean_text(x)
    value <- gsub("[^0-9]", "", value)
    value[is.na(value) | nchar(value) == 0] <- NA_character_
    needs_pad <- !is.na(value) & nchar(value) < 5
    value[needs_pad] <- sprintf("%05d", as.integer(value[needs_pad]))
    value
  }
  cat("Hospital field helpers ready.\n")

  # Clean hospital capacity sentinels
  beds_raw <- get_numeric(hospitals, c("BEDS"))
  staff_raw <- get_numeric(hospitals, c("TTL_STAFF"))
  population_raw <- get_numeric(hospitals, c("POPULATION"))
  beds_sentinel_count <- sum(beds_raw < 0, na.rm = TRUE)
  staff_sentinel_count <- sum(staff_raw < 0, na.rm = TRUE)
  population_sentinel_count <- sum(population_raw < 0, na.rm = TRUE)
  beds <- beds_raw
  total_staff <- staff_raw
  population <- population_raw
  beds[beds < 0] <- NA_real_
  total_staff[total_staff < 0] <- NA_real_
  population[population < 0] <- NA_real_
  cat("Hospital capacity sentinels converted to missing values.\n")

  # Standardize hospital identifiers and attributes
  hifld_id <- get_field(hospitals, c("ID"))
  source_object_id <- get_field(hospitals, c("source_object_id", "OBJECTID_1", "OBJECTID"))
  base_id <- ifelse(!is.na(hifld_id), paste0("HIFLD_", hifld_id), paste0("OID_", source_object_id))
  duplicate_base <- duplicated(base_id) | duplicated(base_id, fromLast = TRUE)
  hospital_site_id <- ifelse(duplicate_base, paste0(base_id, "_OID_", source_object_id), base_id)
  reported_county_fips <- normalize_fips(get_field(hospitals, c("COUNTYFIPS", "COUNTY_FIPS")))
  status <- get_field(hospitals, c("STATUS"))
  status_upper <- toupper(status)
  hospital_open_flag <- ifelse(is.na(status_upper), NA, status_upper == "OPEN")
  trauma <- get_field(hospitals, c("TRAUMA"))
  trauma_designated <- !is.na(trauma)
  helipad <- get_field(hospitals, c("HELIPAD"))
  helipad_upper <- toupper(helipad)
  helipad_flag <- ifelse(helipad_upper %in% c("Y", "YES"), TRUE, ifelse(helipad_upper %in% c("N", "NO"), FALSE, NA))
  cat("Hospital identifiers and attributes standardized.\n")

  # Extract and validate hospital coordinates
  hospitals_wgs <- sf::st_transform(hospitals, 4326)
  coordinates <- sf::st_coordinates(hospitals_wgs)
  if (nrow(coordinates) != nrow(hospitals_wgs)) stop("Hospital geometry does not resolve to one point per facility.")
  longitude <- coordinates[, 1]
  latitude <- coordinates[, 2]
  valid_coordinates <- is.finite(longitude) & is.finite(latitude) & longitude >= -180 & longitude <= 180 & latitude >= -90 & latitude <= 90
  invalid_coordinate_count <- sum(!valid_coordinates)
  if (invalid_coordinate_count > 0) stop("Hospital source contains invalid coordinates.")
  cat("Hospital coordinates validated.\n")

  # Build the standardized hospital point layer
  standardized <- data.frame(
    hospital_site_id = hospital_site_id,
    hifld_id = hifld_id,
    hospital_name = get_field(hospitals, c("NAME")),
    alternate_name = get_field(hospitals, c("ALT_NAME")),
    address = get_field(hospitals, c("ADDRESS")),
    city = get_field(hospitals, c("CITY")),
    state = get_field(hospitals, c("STATE")),
    zip = get_field(hospitals, c("ZIP")),
    telephone = get_field(hospitals, c("TELEPHONE")),
    hospital_type = get_field(hospitals, c("TYPE")),
    status = status,
    hospital_open_flag = hospital_open_flag,
    population = population,
    beds = beds,
    total_staff = total_staff,
    trauma = trauma,
    trauma_designated = trauma_designated,
    helipad = helipad,
    helipad_flag = helipad_flag,
    owner = get_field(hospitals, c("OWNER")),
    reported_county = get_field(hospitals, c("COUNTY")),
    reported_county_fips = reported_county_fips,
    state_fips = get_field(hospitals, c("ST_FIPS")),
    naics_code = get_field(hospitals, c("NAICS_CODE")),
    naics_description = get_field(hospitals, c("NAICS_DESC")),
    source = get_field(hospitals, c("SOURCE")),
    source_date = get_field(hospitals, c("SOURCEDATE")),
    validation_method = get_field(hospitals, c("VAL_METHOD")),
    validation_date = get_field(hospitals, c("VAL_DATE")),
    website = get_field(hospitals, c("WEBSITE")),
    longitude = longitude,
    latitude = latitude,
    snapshot_access_date = access_date,
    stringsAsFactors = FALSE
  )
  hospitals_sf <- sf::st_sf(standardized, geometry = sf::st_geometry(hospitals_wgs), crs = 4326)
  hospitals_sf <- sf::st_transform(hospitals_sf, target_crs)
  cat("Standardized hospital point layer built.\n")

  # Spatially assign hospitals to TIGER counties
  counties_projected <- sf::st_transform(counties, target_crs)
  intersections <- sf::st_intersects(hospitals_sf, counties_projected)
  county_geoids <- vapply(intersections, function(index) {
    if (!length(index)) return(NA_character_)
    paste(sort(unique(as.character(counties_projected$GEOID[index]))), collapse = ";")
  }, character(1))
  census_counties <- vapply(intersections, function(index) {
    if (!length(index)) return(NA_character_)
    paste(sort(unique(as.character(counties_projected$NAME[index]))), collapse = ";")
  }, character(1))
  county_count <- lengths(intersections)
  hospitals_sf$county_geoids <- county_geoids
  hospitals_sf$census_counties <- census_counties
  hospitals_sf$county_count <- county_count
  spatial_nc <- county_count > 0
  cat("Hospitals spatially assigned to TIGER counties.\n")

  # Audit reported versus spatial county FIPS
  reported_available <- !is.na(hospitals_sf$reported_county_fips)
  county_match <- vapply(seq_len(nrow(hospitals_sf)), function(i) {
    if (!reported_available[[i]] || is.na(hospitals_sf$county_geoids[[i]])) return(NA)
    spatial_fips <- strsplit(hospitals_sf$county_geoids[[i]], ";", fixed = TRUE)[[1]]
    hospitals_sf$reported_county_fips[[i]] %in% spatial_fips
  }, logical(1))
  county_mismatch <- reported_available & !is.na(county_match) & !county_match
  cat("Hospital county geography audited.\n")

  # Build statewide and Mecklenburg hospital layers
  nc_sf <- hospitals_sf[spatial_nc, ]
  intersects_mecklenburg <- vapply(
    strsplit(ifelse(is.na(nc_sf$county_geoids), "", nc_sf$county_geoids), ";", fixed = TRUE),
    function(x) target_county_fips %in% x,
    logical(1)
  )
  nc_sf$intersects_mecklenburg <- intersects_mecklenburg
  mecklenburg_sf <- nc_sf[nc_sf$intersects_mecklenburg, ]
  nc_table <- sf::st_drop_geometry(nc_sf)
  mecklenburg_table <- sf::st_drop_geometry(mecklenburg_sf)
  rownames(nc_table) <- NULL
  rownames(mecklenburg_table) <- NULL
  cat("Statewide and Mecklenburg hospital layers built.\n")

  # Build hospital discrepancy records
  discrepancy_type <- rep(NA_character_, nrow(hospitals_sf))
  discrepancy_type[!spatial_nc] <- "outside_spatial_nc"
  discrepancy_type[county_mismatch] <- ifelse(
    is.na(discrepancy_type[county_mismatch]),
    "reported_spatial_fips_mismatch",
    paste0(discrepancy_type[county_mismatch], ";reported_spatial_fips_mismatch")
  )
  multiple_counties <- hospitals_sf$county_count > 1
  discrepancy_type[multiple_counties] <- ifelse(
    is.na(discrepancy_type[multiple_counties]),
    "multiple_county_intersections",
    paste0(discrepancy_type[multiple_counties], ";multiple_county_intersections")
  )
  discrepancies <- sf::st_drop_geometry(hospitals_sf[!is.na(discrepancy_type), ])
  discrepancies$discrepancy_type <- discrepancy_type[!is.na(discrepancy_type)]
  cat("Hospital discrepancy table built.\n")

  # Validate statewide hospital attributes
  hifld_values <- nc_table$hifld_id[!is.na(nc_table$hifld_id)]
  duplicate_hifld_ids <- sum(duplicated(hifld_values))
  duplicate_site_ids <- sum(duplicated(nc_table$hospital_site_id))
  missing_hifld_ids <- sum(is.na(nc_table$hifld_id))
  negative_beds <- sum(nc_table$beds < 0, na.rm = TRUE)
  negative_staff <- sum(nc_table$total_staff < 0, na.rm = TRUE)
  negative_population <- sum(nc_table$population < 0, na.rm = TRUE)
  invalid_helipad_values <- sum(!is.na(nc_table$helipad) & is.na(nc_table$helipad_flag))
  cat("Statewide hospital attributes validated.\n")

  # Build hospital QA table
  qa <- data.frame(
    check = c(
      "Source NC-reported hospital records",
      "Valid hospital point records",
      "Empty geometries removed",
      "Invalid geometries before repair",
      "Invalid geometries after repair",
      "Invalid hospital coordinates",
      "Spatial NC hospitals",
      "Hospitals outside spatial NC",
      "Duplicate hospital site IDs",
      "Duplicate HIFLD IDs",
      "Missing HIFLD IDs",
      "Reported county FIPS available",
      "Reported versus spatial county mismatches",
      "Facilities intersecting multiple counties",
      "Mecklenburg spatial hospitals",
      "NC hospitals with status available",
      "NC hospitals marked OPEN",
      "NC hospitals marked CLOSED",
      "NC hospital types represented",
      "NC hospitals with bed count available",
      "Bed sentinel or negative values converted",
      "NC hospitals with staff count available",
      "Staff sentinel or negative values converted",
      "NC hospitals with population available",
      "Population sentinel or negative values converted",
      "NC hospitals with trauma information",
      "NC hospitals with helipad information",
      "NC hospitals with helipad",
      "Invalid helipad values",
      "NC hospitals with negative beds",
      "NC hospitals with negative staff",
      "NC hospitals with negative population"
    ),
    value = c(
      source_record_count,
      nrow(hospitals),
      empty_count,
      invalid_before,
      invalid_after,
      invalid_coordinate_count,
      nrow(nc_table),
      sum(!spatial_nc),
      duplicate_site_ids,
      duplicate_hifld_ids,
      missing_hifld_ids,
      sum(reported_available[spatial_nc]),
      sum(county_mismatch[spatial_nc]),
      sum(hospitals_sf$county_count[spatial_nc] > 1),
      nrow(mecklenburg_table),
      sum(!is.na(nc_table$status)),
      sum(nc_table$hospital_open_flag %in% TRUE, na.rm = TRUE),
      sum(nc_table$status == "CLOSED", na.rm = TRUE),
      length(unique(nc_table$hospital_type[!is.na(nc_table$hospital_type)])),
      sum(!is.na(nc_table$beds)),
      beds_sentinel_count,
      sum(!is.na(nc_table$total_staff)),
      staff_sentinel_count,
      sum(!is.na(nc_table$population)),
      population_sentinel_count,
      sum(!is.na(nc_table$trauma)),
      sum(!is.na(nc_table$helipad_flag)),
      sum(nc_table$helipad_flag %in% TRUE, na.rm = TRUE),
      invalid_helipad_values,
      negative_beds,
      negative_staff,
      negative_population
    ),
    status = c(
      "INFO",
      "INFO",
      ifelse(empty_count == 0, "PASS", "CHECK"),
      ifelse(invalid_before == 0, "PASS", "CHECK"),
      ifelse(invalid_after == 0, "PASS", "FAIL"),
      ifelse(invalid_coordinate_count == 0, "PASS", "FAIL"),
      "INFO",
      ifelse(sum(!spatial_nc) == 0, "PASS", "CHECK"),
      ifelse(duplicate_site_ids == 0, "PASS", "FAIL"),
      ifelse(duplicate_hifld_ids == 0, "PASS", "CHECK"),
      ifelse(missing_hifld_ids == 0, "PASS", "CHECK"),
      "INFO",
      ifelse(sum(county_mismatch[spatial_nc]) == 0, "PASS", "CHECK"),
      ifelse(sum(hospitals_sf$county_count[spatial_nc] > 1) == 0, "PASS", "CHECK"),
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
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(invalid_helipad_values == 0, "PASS", "CHECK"),
      ifelse(negative_beds == 0, "PASS", "FAIL"),
      ifelse(negative_staff == 0, "PASS", "FAIL"),
      ifelse(negative_population == 0, "PASS", "FAIL")
    ),
    stringsAsFactors = FALSE
  )
  print(qa, row.names = FALSE)
  cat("Hospital geography and attribute QA/QC completed.\n")

  # Build hospital source metadata
  metadata <- data.frame(
    field = c(
      "source",
      "publisher",
      "program",
      "dataset_type",
      "snapshot_access_date",
      "federal_catalog_updated",
      "arcgis_service_data_last_edit",
      "temporal_interpretation",
      "geometry",
      "state_filter",
      "spatial_assignment",
      "facility_scope",
      "capacity_fields",
      "risk_interpretation"
    ),
    value = c(
      "Hospitals",
      "HIFLD / Department of Homeland Security",
      "Homeland Infrastructure Foundation-Level Data",
      "Hospital point locations and infrastructure attributes",
      access_date,
      "2022-10-24",
      "2021-08-30",
      paste(
        "Federal HIFLD catalog metadata was updated in 2022;",
        "the public ArcGIS layer reports a 2021-08-30 data edit.",
        "Treat as pre-Ian infrastructure context rather than an",
        "exact historical census on the date Hurricane Ian occurred."
      ),
      "Hospital point locations",
      "STATE = NC",
      "2022 TIGER county intersection",
      paste(
        "Hospitals only; HIFLD documentation states that",
        "nursing homes and health centers are excluded."
      ),
      "Beds, total staff, trauma designation and helipad information",
      paste(
        "Hospitals represent critical infrastructure and",
        "emergency-response capacity, not a chemical hazard source."
      )
    ),
    stringsAsFactors = FALSE
  )
  cat("Hospital source metadata built.\n")

  # Write hospital processed outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
  nc_csv <- file.path(output_dir, paste0("nc_hifld_hospitals_access_", access_date, ".csv"))
  mecklenburg_csv <- file.path(output_dir, paste0("mecklenburg_hifld_hospitals_access_", access_date, ".csv"))
  nc_gpkg <- file.path(output_dir, paste0("nc_hifld_hospitals_access_", access_date, ".gpkg"))
  mecklenburg_gpkg <- file.path(output_dir, paste0("mecklenburg_hifld_hospitals_access_", access_date, ".gpkg"))
  metadata_file <- file.path(output_dir, paste0("hifld_hospitals_metadata_access_", access_date, ".csv"))
  discrepancy_file <- file.path(validation_dir, paste0("hospital_geography_discrepancies_access_", access_date, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_hospitals_access_", access_date, ".csv"))

  write.csv(nc_table, nc_csv, row.names = FALSE)
  write.csv(mecklenburg_table, mecklenburg_csv, row.names = FALSE)
  sf::st_write(nc_sf, nc_gpkg, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(mecklenburg_sf, mecklenburg_gpkg, delete_dsn = TRUE, quiet = TRUE)
  write.csv(metadata, metadata_file, row.names = FALSE)
  write.csv(discrepancies, discrepancy_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)
  cat("Hospital processed outputs written.\n")
  c(nc_csv, mecklenburg_csv, nc_gpkg, mecklenburg_gpkg, metadata_file, discrepancy_file, qa_file)
}

upload_hospital_outputs <- function(files, processed_folder, metadata_folder, validation_folder) {
  # Validate hospital output files
  if (length(files) != 7 || !all(file.exists(files))) stop("Expected seven hospital output files.")
  cat("Hospital output files validated.\n")

  # Upload processed hospital datasets
  invisible(lapply(files[1:4], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))
  cat("Processed hospital datasets uploaded.\n")

  # Upload hospital metadata
  googledrive::drive_upload(files[[5]], path = metadata_folder, overwrite = TRUE)
  cat("Hospital metadata uploaded.\n")

  # Upload hospital QA reports
  invisible(lapply(files[6:7], googledrive::drive_upload, path = validation_folder, overwrite = TRUE))
  cat("Hospital QA reports uploaded.\n")
  TRUE
}