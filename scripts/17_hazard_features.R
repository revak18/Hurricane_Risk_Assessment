build_hazard_features <- function(integrated_exposure_files, ian_wind_exposure_files, year, target_county_fips, output_dir, validation_dir) {
  # Load integrated exposure and Ian wind-feature tables
  integrated <- read.csv(integrated_exposure_files[[2]], stringsAsFactors = FALSE)
  wind <- read.csv(ian_wind_exposure_files[[1]], stringsAsFactors = FALSE)

  if (!nrow(integrated) || !nrow(wind)) {
    stop("Hazard feature input tables are empty.")
  }

  cat("Integrated exposure and Ian wind features loaded.\n")

  # Validate hazard-feature input keys
  required_integrated <- c("tri_facility_id", "county_geoid", "buffer_m", "distance_to_ian_miles", "within_ian_100mi")
  required_wind <- c(
    "tri_facility_id",
    "ian_min_center_distance_miles",
    "ian_inside_34kt",
    "ian_inside_50kt",
    "ian_inside_64kt",
    "ian_max_wind_threshold_kt",
    "ian_max_wind_category",
    "ian_hours_34kt_est",
    "ian_hours_50kt_est",
    "ian_hours_64kt_est"
  )

  missing_integrated <- setdiff(required_integrated, names(integrated))
  missing_wind <- setdiff(required_wind, names(wind))

  if (length(missing_integrated)) {
    stop(paste("Missing integrated fields:", paste(missing_integrated, collapse = ", ")))
  }

  if (length(missing_wind)) {
    stop(paste("Missing Ian wind fields:", paste(missing_wind, collapse = ", ")))
  }

  if (!all(as.character(integrated$county_geoid) == target_county_fips)) {
    stop("Integrated table contains records outside Mecklenburg County.")
  }

  if (anyDuplicated(wind$tri_facility_id)) {
    stop("Ian wind table contains duplicate facility IDs.")
  }

  cat("Hazard-feature input keys validated.\n")

  # Select facility-level Ian wind features
  wind_columns <- c(
    "tri_facility_id",
    "ian_min_center_distance_nm",
    "ian_min_center_distance_miles",
    "ian_inside_34kt",
    "ian_inside_50kt",
    "ian_inside_64kt",
    "ian_max_wind_threshold_kt",
    "ian_max_wind_category",
    "ian_hours_34kt_est",
    "ian_hours_50kt_est",
    "ian_hours_64kt_est",
    "ian_34kt_temporal_coverage_pct",
    "ian_50kt_temporal_coverage_pct",
    "ian_64kt_temporal_coverage_pct"
  )

  wind_columns <- intersect(wind_columns, names(wind))
  wind_features <- wind[, wind_columns, drop = FALSE]

  cat("Ian wind feature columns selected.\n")

  # Merge HURDAT2 wind exposure into the integrated table
  original_order <- paste(integrated$tri_facility_id, integrated$buffer_m, sep = "_")

  hazard <- merge(
    integrated,
    wind_features,
    by = "tri_facility_id",
    all.x = TRUE,
    sort = FALSE
  )

  merged_order <- paste(hazard$tri_facility_id, hazard$buffer_m, sep = "_")
  hazard <- hazard[match(original_order, merged_order), ]
  rownames(hazard) <- NULL

  cat("Ian wind exposure merged into integrated features.\n")

  # Create interpretable hurricane hazard features
  to_logical <- function(x) {
    if (is.logical(x)) {
      return(x)
    }

    toupper(trimws(as.character(x))) %in% c("TRUE", "T", "YES", "Y", "1")
  }

  hazard$within_ian_100mi <- to_logical(hazard$within_ian_100mi)
  hazard$ian_inside_34kt <- to_logical(hazard$ian_inside_34kt)
  hazard$ian_inside_50kt <- to_logical(hazard$ian_inside_50kt)
  hazard$ian_inside_64kt <- to_logical(hazard$ian_inside_64kt)

  hazard$ian_reported_wind_exposure <- hazard$ian_inside_34kt
  hazard$ian_screening_only <- hazard$within_ian_100mi & !hazard$ian_inside_34kt

  hazard$ian_hazard_level <- ifelse(
    hazard$ian_inside_64kt,
    "64kt_hurricane_force",
    ifelse(
      hazard$ian_inside_50kt,
      "50kt_strong_tropical_storm",
      ifelse(
        hazard$ian_inside_34kt,
        "34kt_tropical_storm",
        ifelse(hazard$within_ian_100mi, "100mi_screening_only", "outside_100mi_screening")
      )
    )
  )

  hazard$ian_distance_decay_100mi <- pmax(0, 1 - hazard$distance_to_ian_miles / 100)

  cat("Interpretable hurricane hazard features created.\n")

  # Build one facility-level hazard summary
  facility_hazard <- hazard[!duplicated(hazard$tri_facility_id), ]

  facility_columns <- intersect(
    c(
      "tri_facility_id",
      "facility_name",
      "county_geoid",
      "latitude",
      "longitude",
      "distance_to_ian_miles",
      "within_ian_100mi",
      "ian_min_center_distance_miles",
      "ian_inside_34kt",
      "ian_inside_50kt",
      "ian_inside_64kt",
      "ian_reported_wind_exposure",
      "ian_screening_only",
      "ian_max_wind_threshold_kt",
      "ian_max_wind_category",
      "ian_hazard_level",
      "ian_hours_34kt_est",
      "ian_hours_50kt_est",
      "ian_hours_64kt_est",
      "ian_distance_decay_100mi"
    ),
    names(facility_hazard)
  )

  facility_hazard <- facility_hazard[, facility_columns, drop = FALSE]
  rownames(facility_hazard) <- NULL

  cat("Facility-level hurricane hazard summary built.\n")

  # Validate the final HazardGraph feature tables
  duplicate_rows <- sum(
    duplicated(hazard[, c("tri_facility_id", "buffer_m")])
  )

  missing_wind_rows <- sum(is.na(hazard$ian_min_center_distance_miles))
  facility_count <- length(unique(hazard$tri_facility_id))
  buffer_count <- length(unique(hazard$buffer_m))
  expected_rows <- facility_count * buffer_count

  wind_nesting_64_50 <- sum(
    hazard$ian_inside_64kt & !hazard$ian_inside_50kt,
    na.rm = TRUE
  )

  wind_nesting_50_34 <- sum(
    hazard$ian_inside_50kt & !hazard$ian_inside_34kt,
    na.rm = TRUE
  )

  facility_wind <- facility_hazard[
    !duplicated(facility_hazard$tri_facility_id),
  ]

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "CDL buffer distances",
      "Expected HazardGraph rows",
      "Actual HazardGraph rows",
      "Duplicate facility-buffer rows",
      "Rows missing Ian wind features",
      "Facility hazard summary rows",
      "Facilities within Ian 100-mile screening zone",
      "Facilities with reported 34kt exposure",
      "Facilities with reported 50kt exposure",
      "Facilities with reported 64kt exposure",
      "Facilities classified screening-only",
      "64kt outside 50kt nesting violations",
      "50kt outside 34kt nesting violations"
    ),
    value = c(
      facility_count,
      buffer_count,
      expected_rows,
      nrow(hazard),
      duplicate_rows,
      missing_wind_rows,
      nrow(facility_hazard),
      sum(facility_wind$within_ian_100mi, na.rm = TRUE),
      sum(facility_wind$ian_inside_34kt, na.rm = TRUE),
      sum(facility_wind$ian_inside_50kt, na.rm = TRUE),
      sum(facility_wind$ian_inside_64kt, na.rm = TRUE),
      sum(facility_wind$ian_screening_only, na.rm = TRUE),
      wind_nesting_64_50,
      wind_nesting_50_34
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      ifelse(nrow(hazard) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_wind_rows == 0, "PASS", "FAIL"),
      ifelse(nrow(facility_hazard) == facility_count, "PASS", "FAIL"),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(wind_nesting_64_50 == 0, "PASS", "CHECK"),
      ifelse(wind_nesting_50_34 == 0, "PASS", "CHECK")
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("HazardGraph feature QA/QC completed.\n")

  # Write final HazardGraph feature outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  hazard_file <- file.path(
    output_dir,
    paste0("mecklenburg_hazardgraph_features_", year, ".csv")
  )

  facility_file <- file.path(
    output_dir,
    paste0("mecklenburg_ian_hazard_summary_", year, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_hazard_features_", year, ".csv")
  )

  write.csv(hazard, hazard_file, row.names = FALSE)
  write.csv(facility_hazard, facility_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("HazardGraph feature outputs written.\n")

  c(hazard_file, facility_file, qa_file)
}

upload_hazard_features <- function(files, processed_folder, validation_folder) {
  # Validate HazardGraph feature outputs
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three HazardGraph feature output files.")
  }

  cat("HazardGraph feature output files validated.\n")

  # Upload HazardGraph feature tables
  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("HazardGraph feature tables uploaded.\n")

  # Upload HazardGraph feature QA report
  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("HazardGraph feature QA report uploaded.\n")

  TRUE
}