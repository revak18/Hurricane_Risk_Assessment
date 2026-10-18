build_mrds_record_reconciliation <- function(mrds_raw_file, mrds_fips_files, county_file, state_abbr, state_fips, target_county_fips, target_crs, output_dir, validation_dir, legacy_reference_count = 12484L) {
  # Define MRDS reconciliation helpers
  clean_text <- function(x) {
    value <- trimws(as.character(x))
    value[is.na(value) | value == ""] <- NA_character_
    value
  }

  first_field <- function(data, candidates) {
    found <- candidates[candidates %in% names(data)]
    if (!length(found)) return(NULL)
    found[[1]]
  }

  get_text <- function(data, candidates) {
    field <- first_field(data, candidates)
    if (is.null(field)) return(rep(NA_character_, nrow(data)))
    clean_text(data[[field]])
  }

  get_numeric <- function(data, candidates) {
    field <- first_field(data, candidates)
    if (is.null(field)) return(rep(NA_real_, nrow(data)))
    suppressWarnings(as.numeric(data[[field]]))
  }

  normalize_state_text <- function(x) {
    value <- toupper(clean_text(x))
    value <- gsub("[^A-Z0-9]", "", value)
    value[is.na(value) | value == ""] <- NA_character_
    value
  }

  normalize_state_fips <- function(x) {
    value <- clean_text(x)
    value <- gsub("[^0-9]", "", value)
    value[is.na(value) | value == ""] <- NA_character_
    valid <- !is.na(value)
    value[valid] <- sprintf("%02d", as.integer(value[valid]))
    value
  }

  read_mrds_file <- function(file) {
    if (length(file) != 1 || is.na(file) || !file.exists(file)) stop("A valid MRDS raw file is required.")
    extension <- tolower(tools::file_ext(file))

    if (extension == "zip") {
      extract_dir <- tempfile("mrds_extract_")
      dir.create(extract_dir, recursive = TRUE)
      archive <- utils::unzip(file, list = TRUE)
      csv_rows <- grepl("\\.csv$", archive$Name, ignore.case = TRUE)
      if (!any(csv_rows)) stop("MRDS ZIP archive contains no CSV file.")

      csv_info <- archive[csv_rows, , drop = FALSE]
      mrds_candidates <- csv_info[grepl("mrds", basename(csv_info$Name), ignore.case = TRUE), , drop = FALSE]

      if (nrow(mrds_candidates)) {
        selected <- mrds_candidates$Name[which.max(mrds_candidates$Length)]
      } else {
        selected <- csv_info$Name[which.max(csv_info$Length)]
      }

      utils::unzip(file, files = selected, exdir = extract_dir)
      csv_file <- file.path(extract_dir, selected)
      data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
      unlink(extract_dir, recursive = TRUE)
      return(data)
    }

    if (extension == "csv") return(read.csv(file, stringsAsFactors = FALSE, check.names = FALSE))

    if (extension == "rds") {
      object <- readRDS(file)

      if (inherits(object, "sf")) {
        object_wgs <- sf::st_transform(object, 4326)
        coordinates <- sf::st_coordinates(object_wgs)
        if (nrow(coordinates) != nrow(object)) stop("Raw MRDS sf object does not contain one point per record.")

        table <- sf::st_drop_geometry(object)
        table$reconciliation_longitude <- coordinates[, 1]
        table$reconciliation_latitude <- coordinates[, 2]
        return(table)
      }

      return(as.data.frame(object))
    }

    if (extension %in% c("gpkg", "geojson", "shp")) {
      object <- sf::st_read(file, quiet = TRUE)
      object_wgs <- sf::st_transform(object, 4326)
      coordinates <- sf::st_coordinates(object_wgs)
      if (nrow(coordinates) != nrow(object)) stop("Raw MRDS spatial file does not contain one point per record.")

      table <- sf::st_drop_geometry(object)
      table$reconciliation_longitude <- coordinates[, 1]
      table$reconciliation_latitude <- coordinates[, 2]
      return(table)
    }

    stop("Unsupported MRDS raw file type.")
  }

  cat("MRDS reconciliation helpers ready.\n")

  # Load raw MRDS and validated statewide FIPS data
  raw_candidates <- mrds_raw_file[file.exists(mrds_raw_file)]
  if (!length(raw_candidates)) stop("Could not locate the raw MRDS input.")

  raw_file <- raw_candidates[[1]]
  fips_file <- mrds_fips_files[basename(mrds_fips_files) == "nc_mrds_sites_fips.csv"][1]
  if (is.na(fips_file) || !file.exists(fips_file)) stop("Could not locate nc_mrds_sites_fips.csv.")

  raw <- read_mrds_file(raw_file)
  validated <- read.csv(fips_file, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(raw) || !nrow(validated)) stop("Raw or validated MRDS input is empty.")

  cat("Raw and validated MRDS tables loaded:", nrow(raw), "raw and", nrow(validated), "validated records.\n")

  # Resolve raw MRDS coordinate and state fields
  longitude_field <- first_field(raw, c("longitude", "LONGITUDE", "long", "LONG", "lon", "LON", "x", "X", "reconciliation_longitude"))
  latitude_field <- first_field(raw, c("latitude", "LATITUDE", "lat", "LAT", "y", "Y", "reconciliation_latitude"))
  state_text_field <- first_field(raw, c("state", "STATE", "state_name", "STATE_NAME", "state_nm", "STATE_NM", "state_abbr", "STATE_ABBR", "st_abbr", "ST_ABBR", "st", "ST", "state_code", "STATE_CODE"))
  state_fips_field <- first_field(raw, c("state_fips", "STATE_FIPS", "statefp", "STATEFP", "state_fp", "STATE_FP", "st_fips", "ST_FIPS", "fips_state", "FIPS_STATE"))

  if (is.null(longitude_field) || is.null(latitude_field)) stop("Could not resolve raw MRDS longitude and latitude fields.")

  longitude <- get_numeric(raw, c(longitude_field))
  latitude <- get_numeric(raw, c(latitude_field))

  raw_state_text <- if (is.null(state_text_field)) {
    rep(NA_character_, nrow(raw))
  } else {
    get_text(raw, c(state_text_field))
  }

  raw_state_fips <- if (is.null(state_fips_field)) {
    rep(NA_character_, nrow(raw))
  } else {
    get_text(raw, c(state_fips_field))
  }

  valid_coordinates <- is.finite(longitude) & is.finite(latitude) & longitude >= -180 & longitude <= 180 & latitude >= -90 & latitude <= 90
  cat("Raw MRDS coordinate and state fields resolved.\n")

  # Spatialize all raw MRDS records with usable coordinates
  raw_valid <- raw[valid_coordinates, , drop = FALSE]
  raw_valid$reconciliation_raw_row <- which(valid_coordinates)
  raw_valid$reconciliation_longitude <- longitude[valid_coordinates]
  raw_valid$reconciliation_latitude <- latitude[valid_coordinates]

  raw_sf <- sf::st_as_sf(raw_valid, coords = c("reconciliation_longitude", "reconciliation_latitude"), crs = 4326, remove = FALSE)
  raw_sf <- sf::st_transform(raw_sf, target_crs)

  cat("Raw MRDS records with usable coordinates spatialized:", nrow(raw_sf), "records.\n")

  # Load TIGER counties and isolate North Carolina
  counties <- sf::st_read(county_file, quiet = TRUE)
  if (!nrow(counties) || !"GEOID" %in% names(counties)) stop("Valid TIGER county input with GEOID is required.")

  counties <- sf::st_transform(counties, target_crs)
  nc_counties <- counties[substr(as.character(counties$GEOID), 1, 2) == state_fips, ]
  if (!nrow(nc_counties)) stop("No North Carolina TIGER county polygons found.")

  if (nrow(nc_counties) != 100) warning(paste("Expected 100 NC counties but found", nrow(nc_counties)))
  cat("North Carolina TIGER counties prepared:", nrow(nc_counties), "counties.\n")

  # Spatially assign raw MRDS records to North Carolina counties
  spatial_membership <- sf::st_intersects(raw_sf, nc_counties)
  spatial_nc <- lengths(spatial_membership) > 0
  spatial_county_count <- lengths(spatial_membership)

  spatial_county_geoid <- vapply(
    spatial_membership,
    function(index) {
      if (!length(index)) return(NA_character_)
      paste(sort(unique(as.character(nc_counties$GEOID[index]))), collapse = ";")
    },
    character(1)
  )

  raw_spatial_nc_count <- sum(spatial_nc)
  cat("Raw MRDS records spatially assigned to North Carolina:", raw_spatial_nc_count, "records.\n")

  # Normalize source state names, abbreviations and FIPS values
  target_state_name <- state.name[match(toupper(state_abbr), state.abb)]
  if (is.na(target_state_name)) stop("Could not resolve full state name from state abbreviation.")

  normalized_state_text <- normalize_state_text(raw_state_text)
  normalized_state_fips <- normalize_state_fips(raw_state_fips)

  target_state_values <- unique(c(
    normalize_state_text(state_abbr),
    normalize_state_text(target_state_name),
    normalize_state_text(state_fips)
  ))

  text_geography_available <- !is.na(normalized_state_text)
  fips_geography_available <- !is.na(normalized_state_fips)
  source_geography_available <- text_geography_available | fips_geography_available

  source_nc_text <- text_geography_available & normalized_state_text %in% target_state_values
  source_nc_fips <- fips_geography_available & normalized_state_fips == state_fips

  source_nc <- rep(NA, nrow(raw))
  source_nc[source_geography_available] <- source_nc_text[source_geography_available] | source_nc_fips[source_geography_available]

  cat("Source-reported North Carolina membership calculated:", sum(source_nc %in% TRUE), "records.\n")

  # Summarize source state values for diagnostics
  state_value_summary <- data.frame(
    source_state_value = character(),
    normalized_state_value = character(),
    records = integer(),
    interpreted_as_nc = logical(),
    stringsAsFactors = FALSE
  )

  available_state_text <- raw_state_text[!is.na(raw_state_text)]

  if (length(available_state_text)) {
    state_table <- sort(table(available_state_text), decreasing = TRUE)

    state_value_summary <- data.frame(
      source_state_value = names(state_table),
      normalized_state_value = normalize_state_text(names(state_table)),
      records = as.integer(state_table),
      interpreted_as_nc = normalize_state_text(names(state_table)) %in% target_state_values,
      stringsAsFactors = FALSE
    )
  }

  cat("Raw MRDS source-state value summary built.\n")

  # Calculate raw and validated Mecklenburg membership
  mecklenburg <- nc_counties[as.character(nc_counties$GEOID) == target_county_fips, ]
  if (nrow(mecklenburg) != 1) stop("Could not resolve exactly one Mecklenburg County polygon.")

  raw_mecklenburg <- lengths(sf::st_intersects(raw_sf, mecklenburg)) > 0

  validated_county_field <- first_field(validated, c("county_geoid", "county_fips", "COUNTY_GEOID", "COUNTY_FIPS", "spatial_county_fips"))

  if (!is.null(validated_county_field)) {
    validated_county <- clean_text(validated[[validated_county_field]])
    validated_county <- gsub("[^0-9]", "", validated_county)

    normalized_county <- rep(NA_character_, length(validated_county))
    valid_county <- !is.na(validated_county) & validated_county != ""
    normalized_county[valid_county] <- sprintf("%05d", as.integer(validated_county[valid_county]))

    validated_mecklenburg_count <- sum(normalized_county == target_county_fips, na.rm = TRUE)
  } else {
    validated_longitude <- get_numeric(validated, c("longitude", "LONGITUDE"))
    validated_latitude <- get_numeric(validated, c("latitude", "LATITUDE"))

    validated_coordinates <- is.finite(validated_longitude) & is.finite(validated_latitude) &
      validated_longitude >= -180 & validated_longitude <= 180 &
      validated_latitude >= -90 & validated_latitude <= 90

    if (!all(validated_coordinates)) stop("Validated MRDS table contains invalid coordinates.")

    validated$reconciliation_longitude <- validated_longitude
    validated$reconciliation_latitude <- validated_latitude

    validated_sf <- sf::st_as_sf(
      validated,
      coords = c("reconciliation_longitude", "reconciliation_latitude"),
      crs = 4326,
      remove = FALSE
    )

    validated_sf <- sf::st_transform(validated_sf, target_crs)
    validated_mecklenburg_count <- sum(lengths(sf::st_intersects(validated_sf, mecklenburg)) > 0)
  }

  validated_nc_count <- nrow(validated)
  cat("Raw and validated Mecklenburg counts calculated:", sum(raw_mecklenburg), "raw spatial and", validated_mecklenburg_count, "validated.\n")

  # Build source-versus-spatial geography audit
  valid_rows <- which(valid_coordinates)

  raw_audit <- data.frame(
    raw_row = valid_rows,
    source_state = raw_state_text[valid_rows],
    normalized_source_state = normalized_state_text[valid_rows],
    source_state_fips = raw_state_fips[valid_rows],
    normalized_source_state_fips = normalized_state_fips[valid_rows],
    source_geography_available = source_geography_available[valid_rows],
    source_reports_nc = source_nc[valid_rows],
    spatial_nc = spatial_nc,
    spatial_county_geoid = spatial_county_geoid,
    spatial_county_count = spatial_county_count,
    spatial_mecklenburg = raw_mecklenburg,
    longitude = longitude[valid_rows],
    latitude = latitude[valid_rows],
    stringsAsFactors = FALSE
  )

  raw_audit$geography_discrepancy <- ifelse(
    raw_audit$source_geography_available & !is.na(raw_audit$source_reports_nc) &
      raw_audit$source_reports_nc != raw_audit$spatial_nc,
    "source_vs_spatial_nc_mismatch",
    NA_character_
  )

  discrepancy_audit <- raw_audit[!is.na(raw_audit$geography_discrepancy), , drop = FALSE]
  cat("MRDS source-versus-spatial geography audit built:", nrow(discrepancy_audit), "mismatches.\n")

  # Separate source false positives and false negatives
  source_false_positive <- sum(raw_audit$source_reports_nc %in% TRUE & !raw_audit$spatial_nc, na.rm = TRUE)
  source_false_negative <- sum(raw_audit$source_reports_nc %in% FALSE & raw_audit$spatial_nc, na.rm = TRUE)

  source_match <- sum(
    !is.na(raw_audit$source_reports_nc) &
      raw_audit$source_reports_nc == raw_audit$spatial_nc,
    na.rm = TRUE
  )

  spatial_nc_missing_source_geography <- sum(raw_audit$spatial_nc & !raw_audit$source_geography_available, na.rm = TRUE)
  cat("MRDS geography disagreement types summarized.\n")

  # Build the MRDS record reconciliation summary
  reconciliation <- data.frame(
    stage = c(
      "Legacy text-query result reported during prototyping",
      "Raw MRDS records in downloaded source",
      "Raw MRDS records with usable coordinates",
      "Raw MRDS records failing coordinate QA",
      "Raw records with source geography available",
      "Raw records source-reported as NC",
      "Raw valid-coordinate records spatially inside NC",
      "Raw valid-coordinate records spatially outside NC",
      "Final QA-clean NC MRDS FIPS records",
      "Raw valid-coordinate records spatially in Mecklenburg",
      "Final QA-clean Mecklenburg MRDS records"
    ),
    records = c(
      legacy_reference_count,
      nrow(raw),
      sum(valid_coordinates),
      sum(!valid_coordinates),
      sum(source_geography_available),
      sum(source_nc %in% TRUE),
      raw_spatial_nc_count,
      sum(!spatial_nc),
      validated_nc_count,
      sum(raw_mecklenburg),
      validated_mecklenburg_count
    ),
    interpretation = c(
      paste("Earlier prototype text-query result retained only", "as a historical reconciliation reference"),
      "All records in the raw MRDS source archive used by the pipeline",
      "Raw records that can be tested spatially using valid coordinates",
      "Raw records excluded because longitude or latitude is unusable",
      paste("Raw records with a usable state name, abbreviation,", "state code, or state FIPS value"),
      paste("Raw records whose source geography identifies", target_state_name),
      "Raw records whose coordinates intersect a North Carolina TIGER county",
      "Valid-coordinate raw records that do not intersect North Carolina",
      "Records retained in the final nc_mrds_sites_fips.csv table",
      "Raw valid-coordinate records whose coordinates intersect Mecklenburg",
      "Records retained in the final validated Mecklenburg subset"
    ),
    stringsAsFactors = FALSE
  )

  cat("MRDS record reconciliation summary built.\n")

  # Validate reconciliation arithmetic and final counts
  raw_coordinate_balance <- sum(valid_coordinates) + sum(!valid_coordinates)
  spatial_balance <- raw_spatial_nc_count + sum(!spatial_nc)
  validated_vs_spatial_difference <- validated_nc_count - raw_spatial_nc_count
  validated_mecklenburg_difference <- validated_mecklenburg_count - sum(raw_mecklenburg)
  legacy_vs_validated_difference <- legacy_reference_count - validated_nc_count

  legacy_retained_percent <- if (legacy_reference_count > 0) {
    100 * validated_nc_count / legacy_reference_count
  } else {
    NA_real_
  }

  qa <- data.frame(
    check = c(
      "Legacy text-query reference count",
      "Raw MRDS records",
      "Raw records with valid coordinates",
      "Raw records with invalid or missing coordinates",
      "Raw coordinate balance difference",
      "Raw records with source geography available",
      "Raw records source-reported as NC",
      "Raw spatial NC records",
      "Raw spatial non-NC records",
      "Raw spatial geography balance difference",
      "Final validated NC records",
      "Difference final validated versus raw spatial NC",
      "Raw spatial Mecklenburg records",
      "Final validated Mecklenburg records",
      "Difference final versus raw spatial Mecklenburg",
      "Source versus spatial NC mismatches",
      "Source NC but spatially outside NC",
      "Source non-NC but spatially inside NC",
      "Source and spatial geography matches",
      "Spatial NC records missing source geography",
      "Valid-coordinate records intersecting multiple NC counties",
      "Legacy reference minus final validated NC",
      "Final validated NC as percent of legacy reference"
    ),
    value = c(
      legacy_reference_count,
      nrow(raw),
      sum(valid_coordinates),
      sum(!valid_coordinates),
      raw_coordinate_balance - nrow(raw),
      sum(source_geography_available),
      sum(source_nc %in% TRUE),
      raw_spatial_nc_count,
      sum(!spatial_nc),
      spatial_balance - sum(valid_coordinates),
      validated_nc_count,
      validated_vs_spatial_difference,
      sum(raw_mecklenburg),
      validated_mecklenburg_count,
      validated_mecklenburg_difference,
      nrow(discrepancy_audit),
      source_false_positive,
      source_false_negative,
      source_match,
      spatial_nc_missing_source_geography,
      sum(spatial_county_count > 1),
      legacy_vs_validated_difference,
      round(legacy_retained_percent, 2)
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(raw_coordinate_balance == nrow(raw), "PASS", "FAIL"),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(spatial_balance == sum(valid_coordinates), "PASS", "FAIL"),
      "INFO",
      ifelse(validated_vs_spatial_difference == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      ifelse(validated_mecklenburg_difference == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(sum(spatial_county_count > 1) == 0, "PASS", "CHECK"),
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(reconciliation, row.names = FALSE)
  print(qa, row.names = FALSE)
  cat("MRDS record reconciliation QA/QC completed.\n")

  # Write MRDS reconciliation outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  reconciliation_file <- file.path(output_dir, "mrds_record_reconciliation.csv")
  state_values_file <- file.path(validation_dir, "mrds_source_state_values.csv")
  discrepancy_file <- file.path(validation_dir, "mrds_raw_geography_discrepancies.csv")
  qa_file <- file.path(validation_dir, "qaqc_mrds_record_reconciliation.csv")

  write.csv(reconciliation, reconciliation_file, row.names = FALSE)
  write.csv(state_value_summary, state_values_file, row.names = FALSE)
  write.csv(discrepancy_audit, discrepancy_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("MRDS reconciliation outputs written.\n")
  c(reconciliation_file, state_values_file, discrepancy_file, qa_file)
}

upload_mrds_record_reconciliation <- function(files, processed_folder, validation_folder) {
  # Validate MRDS reconciliation output files
  if (length(files) != 4 || !all(file.exists(files))) stop("Expected four MRDS reconciliation output files.")
  cat("MRDS reconciliation output files validated.\n")

  # Upload the MRDS reconciliation summary
  googledrive::drive_upload(files[[1]], path = processed_folder, overwrite = TRUE)
  cat("MRDS reconciliation summary uploaded.\n")

  # Upload MRDS reconciliation validation files
  invisible(lapply(files[2:4], googledrive::drive_upload, path = validation_folder, overwrite = TRUE))
  cat("MRDS reconciliation validation files uploaded.\n")

  TRUE
}