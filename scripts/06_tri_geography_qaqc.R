audit_tri_geography <- function(tri_fips_files, year, expected_county_fips, output_dir) {
  # Load statewide and Mecklenburg FIPS-tagged TRI facilities
  if (length(tri_fips_files) < 4) stop("TRI FIPS files are missing.")

  facilities <- read.csv(tri_fips_files[[1]], stringsAsFactors = FALSE)
  mecklenburg <- read.csv(tri_fips_files[[3]], stringsAsFactors = FALSE)

  required <- c("tri_facility_id", "city", "county", "state_fips", "county_geoid", "census_county")
  missing <- setdiff(required, names(facilities))

  if (length(missing)) {
    stop(paste("Missing TRI geography fields:", paste(missing, collapse = ", ")))
  }
  cat("Statewide TRI geography data loaded.\n")

  # Standardize reported and Census county names
  clean_county <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- sub("\\s+COUNTY$", "", x)
    x[x == ""] <- NA_character_
    x
  }

  facilities$reported_county_clean <- clean_county(facilities$county)
  facilities$census_county_clean <- clean_county(facilities$census_county)
  facilities$county_name_match <- !is.na(facilities$reported_county_clean) &
    !is.na(facilities$census_county_clean) &
    facilities$reported_county_clean == facilities$census_county_clean
  cat("TRI county names standardized.\n")

  # Audit statewide spatial FIPS assignments
  expected_state_fips <- substr(expected_county_fips, 1, 2)
  missing_fips <- is.na(facilities$county_geoid) | trimws(as.character(facilities$county_geoid)) == ""

  outside_state <- !is.na(facilities$state_fips) &
    as.character(facilities$state_fips) != expected_state_fips

  missing_reported_county <- is.na(facilities$reported_county_clean)

  county_name_mismatch <- !missing_reported_county &
    !is.na(facilities$census_county_clean) &
    !facilities$county_name_match
  cat("Statewide TRI geography assignments audited.\n")

  # Verify the FIPS-derived Mecklenburg subset
  statewide_mecklenburg <- facilities[
    !is.na(facilities$county_geoid) &
      as.character(facilities$county_geoid) == expected_county_fips,
  ]

  statewide_ids <- sort(unique(as.character(statewide_mecklenburg$tri_facility_id)))
  subset_ids <- sort(unique(as.character(mecklenburg$tri_facility_id)))
  subset_missing <- setdiff(statewide_ids, subset_ids)
  subset_extra <- setdiff(subset_ids, statewide_ids)
  subset_consistent <- length(subset_missing) == 0 && length(subset_extra) == 0
  cat("Mecklenburg FIPS subset verified.\n")

  # Build statewide geography QA/QC report
  duplicate_ids <- sum(duplicated(facilities$tri_facility_id))

  qa <- data.frame(
    check = c(
      "Statewide TRI facilities",
      "Facilities with county FIPS",
      "Missing county FIPS",
      "Facilities assigned outside NC",
      "County name matches",
      "County name mismatches",
      "Missing reported county names",
      "Duplicate facility IDs",
      paste0("Statewide facilities in ", expected_county_fips),
      "Mecklenburg subset facilities",
      "Mecklenburg subset missing facilities",
      "Mecklenburg subset extra facilities"
    ),
    value = c(
      nrow(facilities),
      sum(!missing_fips),
      sum(missing_fips),
      sum(outside_state, na.rm = TRUE),
      sum(facilities$county_name_match, na.rm = TRUE),
      sum(county_name_mismatch, na.rm = TRUE),
      sum(missing_reported_county),
      duplicate_ids,
      nrow(statewide_mecklenburg),
      nrow(mecklenburg),
      length(subset_missing),
      length(subset_extra)
    ),
    status = c(
      "INFO",
      ifelse(sum(missing_fips) == 0, "PASS", "CHECK"),
      ifelse(sum(missing_fips) == 0, "PASS", "CHECK"),
      ifelse(sum(outside_state, na.rm = TRUE) == 0, "PASS", "FAIL"),
      "INFO",
      ifelse(sum(county_name_mismatch, na.rm = TRUE) == 0, "PASS", "CHECK"),
      ifelse(sum(missing_reported_county) == 0, "PASS", "CHECK"),
      ifelse(duplicate_ids == 0, "PASS", "FAIL"),
      "INFO",
      "INFO",
      ifelse(subset_consistent, "PASS", "FAIL"),
      ifelse(subset_consistent, "PASS", "FAIL")
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  if (sum(outside_state, na.rm = TRUE) > 0) {
    stop("Some TRI facilities were assigned outside North Carolina.")
  }

  if (duplicate_ids > 0) stop("Duplicate statewide TRI facility IDs detected.")

  if (!subset_consistent) {
    stop("Mecklenburg TRI subset does not match spatial FIPS assignments.")
  }
  cat("Statewide TRI geography QA/QC completed.\n")

  # Build the geography mismatch table
  mismatch_rows <- missing_fips | outside_state | missing_reported_county | county_name_mismatch

  mismatches <- facilities[
    mismatch_rows,
    c(
      "tri_facility_id",
      "facility_name",
      "city",
      "county",
      "reported_county_clean",
      "state_fips",
      "county_fips",
      "county_geoid",
      "census_county",
      "census_county_clean",
      "county_name_match",
      "latitude",
      "longitude"
    )
  ]

  rownames(mismatches) <- NULL
  cat("TRI geography mismatch table built.\n")

  # Write geography QA/QC outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  qa_file <- file.path(output_dir, paste0("qaqc_tri_geography_", year, ".csv"))
  mismatch_file <- file.path(output_dir, paste0("tri_geography_mismatches_", year, ".csv"))

  write.csv(qa, qa_file, row.names = FALSE)
  write.csv(mismatches, mismatch_file, row.names = FALSE)
  cat("TRI geography QA/QC outputs written.\n")
  c(qa_file, mismatch_file)
}

upload_tri_geography_qaqc <- function(files, validation_folder) {
  # Validate TRI geography QA/QC outputs
  if (length(files) != 2 || !all(file.exists(files))) {
    stop("TRI geography QA/QC files are missing.")
  }
  cat("TRI geography QA/QC files validated.\n")

  # Upload TRI geography QA/QC outputs
  invisible(lapply(
    files,
    googledrive::drive_upload,
    path = validation_folder,
    overwrite = TRUE
  ))
  cat("TRI geography QA/QC outputs uploaded.\n")
  TRUE
}