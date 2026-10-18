assign_tri_fips <- function(tri_files, county_file, year, target_county_fips, output_dir, validation_dir) {
  # Load statewide TRI tables and county geography
  facilities <- read.csv(tri_files[[1]], stringsAsFactors = FALSE)
  chemicals <- read.csv(tri_files[[2]], stringsAsFactors = FALSE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  required_facility <- c("tri_facility_id", "latitude", "longitude", "valid_coordinates")
  missing <- setdiff(required_facility, names(facilities))
  if (length(missing)) stop(paste("Missing TRI facility fields:", paste(missing, collapse = ", ")))
  if (!"tri_facility_id" %in% names(chemicals)) stop("TRI chemical facility ID is missing.")
  if (anyDuplicated(facilities$tri_facility_id)) stop("Statewide TRI facility IDs are not unique.")
  cat("Statewide TRI and county geography loaded.\n")

  # Prepare valid TRI facility coordinates
  valid <- facilities$valid_coordinates
  if (!is.logical(valid)) valid <- toupper(as.character(valid)) == "TRUE"
  valid[is.na(valid)] <- FALSE

  spatial_rows <- which(valid)
  if (!length(spatial_rows)) stop("No valid TRI facility coordinates are available.")

  points <- sf::st_as_sf(
    facilities[spatial_rows, ],
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  points <- sf::st_transform(points, sf::st_crs(counties))
  cat("Statewide TRI facility points prepared.\n")

  # Spatially assign county FIPS
  matched <- sf::st_join(
    points,
    counties[, c("STATEFP", "COUNTYFP", "GEOID", "NAME")],
    join = sf::st_within,
    left = TRUE
  )

  if (nrow(matched) != length(spatial_rows)) {
    stop("Spatial county join changed the TRI facility row count.")
  }

  matched <- sf::st_drop_geometry(matched)
  facilities$state_fips <- NA_character_
  facilities$county_fips <- NA_character_
  facilities$county_geoid <- NA_character_
  facilities$census_county <- NA_character_

  index <- match(matched$tri_facility_id, facilities$tri_facility_id)
  facilities$state_fips[index] <- as.character(matched$STATEFP)
  facilities$county_fips[index] <- as.character(matched$COUNTYFP)
  facilities$county_geoid[index] <- as.character(matched$GEOID)
  facilities$census_county[index] <- as.character(matched$NAME)
  cat("Statewide TRI facilities assigned to county FIPS.\n")

  # Attach spatial FIPS to chemical records
  facility_index <- match(chemicals$tri_facility_id, facilities$tri_facility_id)
  chemicals$state_fips <- facilities$state_fips[facility_index]
  chemicals$county_fips <- facilities$county_fips[facility_index]
  chemicals$county_geoid <- facilities$county_geoid[facility_index]
  chemicals$census_county <- facilities$census_county[facility_index]
  unmatched_chemicals <- sum(is.na(facility_index))
  cat("Statewide TRI chemical records assigned facility FIPS.\n")

  # Create Mecklenburg subsets using FIPS only
  mecklenburg_facilities <- facilities[
    !is.na(facilities$county_geoid) & facilities$county_geoid == target_county_fips,
  ]

  mecklenburg_chemicals <- chemicals[
    !is.na(chemicals$county_geoid) & chemicals$county_geoid == target_county_fips,
  ]

  rownames(mecklenburg_facilities) <- NULL
  rownames(mecklenburg_chemicals) <- NULL
  cat("Mecklenburg TRI subsets created from county FIPS.\n")

  # Build statewide FIPS QA report
  expected_state_fips <- substr(target_county_fips, 1, 2)
  missing_fips <- sum(is.na(facilities$county_geoid))
  wrong_state <- sum(!is.na(facilities$state_fips) & facilities$state_fips != expected_state_fips)

  qa <- data.frame(
    check = c(
      "Statewide TRI facilities",
      "Facilities with county FIPS",
      "Facilities missing county FIPS",
      "Facilities outside NC FIPS",
      "Chemical records",
      "Chemical records without facility match",
      paste0("Facilities in ", target_county_fips),
      paste0("Chemical records in ", target_county_fips)
    ),
    value = c(
      nrow(facilities),
      sum(!is.na(facilities$county_geoid)),
      missing_fips,
      wrong_state,
      nrow(chemicals),
      unmatched_chemicals,
      nrow(mecklenburg_facilities),
      nrow(mecklenburg_chemicals)
    ),
    status = c(
      "INFO",
      ifelse(missing_fips == 0, "PASS", "CHECK"),
      ifelse(missing_fips == 0, "PASS", "CHECK"),
      ifelse(wrong_state == 0, "PASS", "FAIL"),
      "INFO",
      ifelse(unmatched_chemicals == 0, "PASS", "FAIL"),
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  if (sum(!is.na(facilities$county_geoid)) == 0) {
    stop("No statewide TRI facilities received county FIPS.")
  }

  if (wrong_state > 0) stop("Some TRI facilities were assigned outside North Carolina.")
  if (unmatched_chemicals > 0) stop("Some TRI chemical records do not match a facility.")
  cat("Statewide TRI FIPS assignments validated.\n")

  # Write statewide and Mecklenburg FIPS outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  statewide_facility_file <- file.path(output_dir, paste0("nc_tri_facility_fips_", year, ".csv"))
  statewide_chemical_file <- file.path(output_dir, paste0("nc_tri_chemicals_fips_", year, ".csv"))
  mecklenburg_facility_file <- file.path(output_dir, paste0("mecklenburg_tri_facility_", year, ".csv"))
  mecklenburg_chemical_file <- file.path(output_dir, paste0("mecklenburg_tri_chemicals_", year, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_tri_fips_", year, ".csv"))

  write.csv(facilities, statewide_facility_file, row.names = FALSE)
  write.csv(chemicals, statewide_chemical_file, row.names = FALSE)
  write.csv(mecklenburg_facilities, mecklenburg_facility_file, row.names = FALSE)
  write.csv(mecklenburg_chemicals, mecklenburg_chemical_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)
  cat("Statewide and Mecklenburg TRI FIPS outputs written.\n")

  c(
    statewide_facility_file,
    statewide_chemical_file,
    mecklenburg_facility_file,
    mecklenburg_chemical_file,
    qa_file
  )
}

upload_tri_fips <- function(files, processed_folder, validation_folder) {
  # Validate TRI FIPS outputs
  if (length(files) != 5 || !all(file.exists(files))) {
    stop("Expected five TRI FIPS output files.")
  }
  cat("TRI FIPS output files validated.\n")

  # Upload statewide and Mecklenburg TRI tables
  invisible(lapply(
    files[1:4],
    googledrive::drive_upload,
    path = processed_folder,
    overwrite = TRUE
  ))
  cat("TRI FIPS data tables uploaded.\n")

  # Upload TRI FIPS QA report
  googledrive::drive_upload(files[[5]], path = validation_folder, overwrite = TRUE)
  cat("TRI FIPS QA report uploaded.\n")
  TRUE
}