assign_mrds_fips <- function(mrds_raw_file, county_file, boundary_year, target_county_fips, output_dir, validation_dir) {
  # Extract and load the national MRDS source
  extract_dir <- file.path(dirname(mrds_raw_file), "mrds_extracted")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(mrds_raw_file, exdir = extract_dir)

  csv_files <- list.files(extract_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

  if (!length(csv_files)) {
    stop("No extracted MRDS CSV was found.")
  }

  source_file <- csv_files[which.max(file.info(csv_files)$size)]

  mrds <- readr::read_csv(
    source_file,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  mrds <- as.data.frame(mrds, stringsAsFactors = FALSE)
  cat("National MRDS records loaded.\n")

  # Standardize MRDS field names
  clean_names <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }

  names(mrds) <- clean_names(names(mrds))
  required <- c("dep_id", "site_name", "latitude", "longitude", "state", "county")
  missing <- setdiff(required, names(mrds))

  if (length(missing)) {
    stop(paste("Missing MRDS fields:", paste(missing, collapse = ", ")))
  }

  mrds$mrds_row_id <- seq_len(nrow(mrds))
  cat("MRDS fields standardized.\n")

  # Parse and validate MRDS coordinates
  to_number <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub(",", "", x)
    x[x == ""] <- NA_character_
    suppressWarnings(as.numeric(x))
  }

  latitude <- to_number(mrds$latitude)
  longitude <- to_number(mrds$longitude)

  valid_coordinates <-
    !is.na(latitude) &
    !is.na(longitude) &
    latitude >= -90 &
    latitude <= 90 &
    longitude >= -180 &
    longitude <= 180

  mrds$latitude <- latitude
  mrds$longitude <- longitude
  mrds$valid_coordinates <- valid_coordinates
  cat("MRDS coordinates parsed and validated.\n")

  # Load the authoritative North Carolina TIGER geography
  counties <- sf::st_read(county_file, quiet = TRUE)
  required_county <- c("STATEFP", "COUNTYFP", "GEOID", "NAME")
  missing_county <- setdiff(required_county, names(counties))

  if (length(missing_county)) {
    stop(paste("Missing county geography fields:", paste(missing_county, collapse = ", ")))
  }

  counties <- sf::st_transform(counties, 4326)

  if (nrow(counties) != 100) {
    stop("Expected 100 North Carolina counties.")
  }

  cat("North Carolina TIGER geography loaded.\n")

  # Prefilter valid MRDS points to the North Carolina bounding box
  bbox <- sf::st_bbox(counties)

  candidate_rows <- which(
    valid_coordinates &
      longitude >= bbox["xmin"] &
      longitude <= bbox["xmax"] &
      latitude >= bbox["ymin"] &
      latitude <= bbox["ymax"]
  )

  candidates <- mrds[candidate_rows, ]

  if (!nrow(candidates)) {
    stop("No MRDS records intersect the North Carolina bounding box.")
  }

  cat("MRDS North Carolina spatial candidates selected.\n")

  # Convert candidate MRDS records to spatial points
  points <- sf::st_as_sf(
    candidates,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  county_join <- counties[, c("STATEFP", "COUNTYFP", "GEOID", "NAME")]
  cat("MRDS candidate points prepared.\n")

  # Spatially assign MRDS points to TIGER counties
  joined <- sf::st_join(points, county_join, join = sf::st_within, left = TRUE)

  if (nrow(joined) != nrow(points)) {
    stop("MRDS county join unexpectedly changed the row count.")
  }

  cat("MRDS sites spatially joined to TIGER counties.\n")

  # Recover points exactly on county boundaries
  unmatched <- which(is.na(joined$GEOID))
  ambiguous_boundary <- 0L
  recovered_boundary <- 0L

  if (length(unmatched)) {
    hits <- sf::st_intersects(points[unmatched, ], county_join)
    single_hit <- lengths(hits) == 1
    multiple_hit <- lengths(hits) > 1
    ambiguous_boundary <- sum(multiple_hit)
    recover_rows <- unmatched[single_hit]

    if (length(recover_rows)) {
      county_rows <- vapply(hits[single_hit], function(x) x[[1]], integer(1))

      joined$STATEFP[recover_rows] <- as.character(county_join$STATEFP[county_rows])
      joined$COUNTYFP[recover_rows] <- as.character(county_join$COUNTYFP[county_rows])
      joined$GEOID[recover_rows] <- as.character(county_join$GEOID[county_rows])
      joined$NAME[recover_rows] <- as.character(county_join$NAME[county_rows])

      recovered_boundary <- length(recover_rows)
    }
  }

  cat("MRDS county-boundary assignments checked.\n")

  # Map spatial FIPS assignments back to the national MRDS table
  assigned_state_fips <- rep(NA_character_, nrow(mrds))
  assigned_county_fips <- rep(NA_character_, nrow(mrds))
  assigned_county_geoid <- rep(NA_character_, nrow(mrds))
  assigned_census_county <- rep(NA_character_, nrow(mrds))

  joined_plain <- sf::st_drop_geometry(joined)
  matched <- !is.na(joined_plain$GEOID)
  source_rows <- joined_plain$mrds_row_id[matched]

  assigned_state_fips[source_rows] <- as.character(joined_plain$STATEFP[matched])
  assigned_county_fips[source_rows] <- as.character(joined_plain$COUNTYFP[matched])
  assigned_county_geoid[source_rows] <- as.character(joined_plain$GEOID[matched])
  assigned_census_county[source_rows] <- as.character(joined_plain$NAME[matched])

  mrds$state_fips <- assigned_state_fips
  mrds$county_fips <- assigned_county_fips
  mrds$county_geoid <- assigned_county_geoid
  mrds$census_county <- assigned_census_county

  cat("MRDS spatial FIPS assignments attached.\n")

  # Create authoritative North Carolina and Mecklenburg subsets
  spatial_nc <- !is.na(mrds$county_geoid)
  nc_sites <- mrds[spatial_nc, ]
  mecklenburg_sites <- nc_sites[nc_sites$county_geoid == target_county_fips, ]

  rownames(nc_sites) <- NULL
  rownames(mecklenburg_sites) <- NULL

  if (!nrow(nc_sites)) {
    stop("Spatial assignment produced zero North Carolina MRDS sites.")
  }

  cat("North Carolina and Mecklenburg MRDS subsets created.\n")

  # Compare reported MRDS geography with spatial geography
  reported_state <- toupper(trimws(as.character(mrds$state)))
  reported_nc <- reported_state %in% c("NC", "NORTH CAROLINA")
  reported_nc_spatial_nc <- sum(reported_nc & spatial_nc)
  reported_nc_outside <- sum(reported_nc & !spatial_nc)
  spatial_nc_not_reported <- sum(!reported_nc & spatial_nc)
  geography_discrepancy <- reported_nc != spatial_nc

  discrepancy_fields <- intersect(
    c(
      "mrds_row_id",
      "dep_id",
      "site_name",
      "site_type",
      "dev_status",
      "dep_type",
      "commod1",
      "commod2",
      "commod3",
      "state",
      "county",
      "latitude",
      "longitude",
      "state_fips",
      "county_fips",
      "county_geoid",
      "census_county"
    ),
    names(mrds)
  )

  discrepancies <- mrds[geography_discrepancy, discrepancy_fields, drop = FALSE]
  rownames(discrepancies) <- NULL

  cat("MRDS reported and spatial geography compared.\n")

  # Build MRDS FIPS QA report
  dep_ids <- trimws(as.character(mrds$dep_id))

  duplicate_dep_ids <- sum(
    duplicated(
      dep_ids[
        !is.na(dep_ids) &
          dep_ids != ""
      ]
    )
  )

  qa <- data.frame(
    check = c(
      "National MRDS records",
      "Valid national coordinates",
      "Invalid national coordinates",
      "NC bounding-box candidates",
      "Spatial NC MRDS sites",
      "Spatial NC sites with county FIPS",
      "Reported NC MRDS records",
      "Reported NC also spatially in NC",
      "Reported NC outside spatial NC",
      "Spatial NC not reported as NC",
      "Boundary points recovered",
      "Ambiguous county-boundary points",
      "Duplicate MRDS deposit IDs",
      paste0("MRDS sites in ", target_county_fips)
    ),
    value = c(
      nrow(mrds),
      sum(valid_coordinates),
      sum(!valid_coordinates),
      nrow(candidates),
      nrow(nc_sites),
      sum(!is.na(nc_sites$county_geoid)),
      sum(reported_nc),
      reported_nc_spatial_nc,
      reported_nc_outside,
      spatial_nc_not_reported,
      recovered_boundary,
      ambiguous_boundary,
      duplicate_dep_ids,
      nrow(mecklenburg_sites)
    ),
    status = c(
      "INFO",
      ifelse(sum(valid_coordinates) > 0, "PASS", "FAIL"),
      "INFO",
      "INFO",
      "INFO",
      ifelse(all(!is.na(nc_sites$county_geoid)), "PASS", "FAIL"),
      "INFO",
      "INFO",
      ifelse(reported_nc_outside == 0, "PASS", "CHECK"),
      ifelse(spatial_nc_not_reported == 0, "PASS", "CHECK"),
      "INFO",
      ifelse(ambiguous_boundary == 0, "PASS", "CHECK"),
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("MRDS spatial FIPS QA/QC completed.\n")

  # Write processed MRDS and validation outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  nc_file <- file.path(output_dir, "nc_mrds_sites_fips.csv")
  mecklenburg_file <- file.path(output_dir, "mecklenburg_mrds_sites.csv")
  qa_file <- file.path(validation_dir, "qaqc_mrds_fips.csv")
  discrepancy_file <- file.path(validation_dir, "mrds_geography_discrepancies.csv")

  write.csv(nc_sites, nc_file, row.names = FALSE)
  write.csv(mecklenburg_sites, mecklenburg_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)
  write.csv(discrepancies, discrepancy_file, row.names = FALSE)

  cat("MRDS FIPS outputs written.\n")
  c(nc_file, mecklenburg_file, qa_file, discrepancy_file)
}

upload_mrds_fips <- function(files, processed_folder, validation_folder) {
  # Validate MRDS FIPS output files
  if (length(files) != 4 || !all(file.exists(files))) {
    stop("Expected four MRDS FIPS output files.")
  }

  cat("MRDS FIPS output files validated.\n")

  # Upload processed North Carolina MRDS tables
  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("Processed MRDS FIPS tables uploaded.\n")

  # Upload MRDS FIPS validation outputs
  invisible(
    lapply(
      files[3:4],
      googledrive::drive_upload,
      path = validation_folder,
      overwrite = TRUE
    )
  )

  cat("MRDS FIPS validation outputs uploaded.\n")
  TRUE
}