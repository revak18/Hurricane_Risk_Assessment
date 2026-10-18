build_hazardgraph_wastewater <- function(
  hazardgraph_consolidated_files, wastewater_proximity_files, year, target_county_fips,
  output_dir, validation_dir
) {
  # Load consolidated HazardGraph and wastewater proximity tables
  hazard <- read.csv(hazardgraph_consolidated_files[[1]], stringsAsFactors = FALSE)
  wastewater <- read.csv(wastewater_proximity_files[[1]], stringsAsFactors = FALSE)

  if (!nrow(hazard) || !nrow(wastewater)) {
    stop("HazardGraph or wastewater proximity input is empty.")
  }

  cat("HazardGraph and wastewater proximity tables loaded.\n")

  # Validate integration fields and keys
  required_hazard <- c("tri_facility_id", "county_geoid", "buffer_m")

  required_wastewater <- c(
    "tri_facility_id", "county_geoid", "buffer_m", "wastewater_site_count",
    "wastewater_major_count", "wastewater_minor_count", "wastewater_flow_available_count",
    "wastewater_total_asbuilt_flow_mgd", "nearest_wastewater_distance_m",
    "nearest_major_wastewater_distance_m"
  )

  missing_hazard <- setdiff(required_hazard, names(hazard))
  missing_wastewater <- setdiff(required_wastewater, names(wastewater))

  if (length(missing_hazard)) {
    stop(paste("Missing HazardGraph fields:", paste(missing_hazard, collapse = ", ")))
  }

  if (length(missing_wastewater)) {
    stop(paste("Missing wastewater fields:", paste(missing_wastewater, collapse = ", ")))
  }

  if (!all(as.character(hazard$county_geoid) == target_county_fips)) {
    stop("HazardGraph contains records outside Mecklenburg County.")
  }

  if (!all(as.character(wastewater$county_geoid) == target_county_fips)) {
    stop("Wastewater proximity table contains records outside Mecklenburg County.")
  }

  hazard_key <- paste(hazard$tri_facility_id, hazard$buffer_m, sep = "_")
  wastewater_key <- paste(wastewater$tri_facility_id, wastewater$buffer_m, sep = "_")

  if (anyDuplicated(hazard_key)) {
    stop("HazardGraph contains duplicate facility-buffer keys.")
  }

  if (anyDuplicated(wastewater_key)) {
    stop("Wastewater table contains duplicate facility-buffer keys.")
  }

  cat("HazardGraph and wastewater integration keys validated.\n")

  # Select wastewater features for integration
  wastewater_columns <- intersect(
    c(
      "tri_facility_id", "buffer_m", "wastewater_site_count",
      "wastewater_any_within_buffer", "wastewater_major_count",
      "wastewater_minor_count", "wastewater_flow_available_count",
      "wastewater_total_asbuilt_flow_mgd", "wastewater_major_asbuilt_flow_mgd",
      "wastewater_max_asbuilt_flow_mgd", "nearest_wastewater_distance_m",
      "nearest_wastewater_site_id", "nearest_wastewater_permit_number",
      "nearest_wastewater_name", "nearest_wastewater_major",
      "nearest_wastewater_asbuilt_flow_mgd",
      "nearest_major_wastewater_distance_m",
      "wastewater_snapshot_access_date"
    ),
    names(wastewater)
  )

  wastewater_features <- wastewater[, wastewater_columns, drop = FALSE]
  cat("Wastewater integration features selected.\n")

  # Merge wastewater features into HazardGraph
  original_key <- hazard_key

  integrated <- merge(
    hazard,
    wastewater_features,
    by = c("tri_facility_id", "buffer_m"),
    all.x = TRUE,
    sort = FALSE
  )

  integrated_key <- paste(integrated$tri_facility_id, integrated$buffer_m, sep = "_")
  integrated <- integrated[match(original_key, integrated_key), ]
  rownames(integrated) <- NULL

  cat("Wastewater features merged into HazardGraph.\n")

  # Create interpretable wastewater context features
  integrated$wastewater_any_within_buffer <- as.logical(integrated$wastewater_any_within_buffer)

  integrated$wastewater_none_within_buffer <-
    !integrated$wastewater_any_within_buffer

  integrated$wastewater_nearest_within_300m <-
    integrated$nearest_wastewater_distance_m <= 300

  integrated$wastewater_nearest_within_500m <-
    integrated$nearest_wastewater_distance_m <= 500

  integrated$wastewater_nearest_within_1000m <-
    integrated$nearest_wastewater_distance_m <= 1000

  integrated$wastewater_proximity_class <- ifelse(
    integrated$nearest_wastewater_distance_m <= 300,
    "within_300m",
    ifelse(
      integrated$nearest_wastewater_distance_m <= 500,
      "within_500m",
      ifelse(
        integrated$nearest_wastewater_distance_m <= 1000,
        "within_1000m",
        "beyond_1000m"
      )
    )
  )

  integrated$wastewater_flow_coverage_fraction <- ifelse(
    integrated$wastewater_site_count > 0,
    integrated$wastewater_flow_available_count / integrated$wastewater_site_count,
    NA_real_
  )

  cat("Interpretable wastewater context features created.\n")

  # Validate integrated wastewater features
  expected_rows <- nrow(hazard)

  duplicate_rows <- sum(
    duplicated(integrated[, c("tri_facility_id", "buffer_m")])
  )

  missing_wastewater_rows <- sum(is.na(integrated$wastewater_site_count))

  class_mismatches <- sum(
    integrated$wastewater_major_count +
      integrated$wastewater_minor_count !=
      integrated$wastewater_site_count,
    na.rm = TRUE
  )

  invalid_flow_coverage <- sum(
    integrated$wastewater_flow_coverage_fraction < 0 |
      integrated$wastewater_flow_coverage_fraction > 1,
    na.rm = TRUE
  )

  monotonic_count_violations <- 0L
  monotonic_flow_violations <- 0L

  for (id in unique(integrated$tri_facility_id)) {
    rows <- integrated[integrated$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$wastewater_site_count) < 0)) {
      monotonic_count_violations <- monotonic_count_violations + 1L
    }

    if (any(diff(rows$wastewater_total_asbuilt_flow_mgd) < -1e-10)) {
      monotonic_flow_violations <- monotonic_flow_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$wastewater_site_count > 0, na.rm = TRUE)
  }

  major_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$wastewater_major_count > 0, na.rm = TRUE)
  }

  facility_once <- integrated[!duplicated(integrated$tri_facility_id), ]
  cat("Integrated wastewater features validated.\n")

  # Build HazardGraph wastewater QA table
  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Buffer distances",
      "Expected integrated rows",
      "Actual integrated rows",
      "Duplicate facility-buffer rows",
      "Rows missing wastewater features",
      "Wastewater Major-Minor count mismatches",
      "Invalid wastewater flow coverage fractions",
      "Facilities with nonmonotonic wastewater counts",
      "Facilities with nonmonotonic wastewater capacity",
      "Facilities with wastewater within 300m",
      "Facilities with wastewater within 500m",
      "Facilities with wastewater within 1000m",
      "Facilities with major wastewater within 300m",
      "Facilities with major wastewater within 500m",
      "Facilities with major wastewater within 1000m",
      "Minimum nearest-wastewater distance meters",
      "Minimum nearest-major-wastewater distance meters",
      "Maximum nearest-wastewater distance meters"
    ),
    value = c(
      length(unique(integrated$tri_facility_id)),
      length(unique(integrated$buffer_m)),
      expected_rows,
      nrow(integrated),
      duplicate_rows,
      missing_wastewater_rows,
      class_mismatches,
      invalid_flow_coverage,
      monotonic_count_violations,
      monotonic_flow_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      major_at_buffer(300),
      major_at_buffer(500),
      major_at_buffer(1000),
      round(min(facility_once$nearest_wastewater_distance_m, na.rm = TRUE), 2),
      round(min(facility_once$nearest_major_wastewater_distance_m, na.rm = TRUE), 2),
      round(max(facility_once$nearest_wastewater_distance_m, na.rm = TRUE), 2)
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      ifelse(nrow(integrated) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_wastewater_rows == 0, "PASS", "FAIL"),
      ifelse(class_mismatches == 0, "PASS", "FAIL"),
      ifelse(invalid_flow_coverage == 0, "PASS", "FAIL"),
      ifelse(monotonic_count_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_flow_violations == 0, "PASS", "FAIL"),
      rep("INFO", 9)
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("HazardGraph-wastewater QA/QC completed.\n")

  # Write HazardGraph-wastewater outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0("mecklenburg_hazardgraph_wastewater_features_", year, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_hazardgraph_wastewater_", year, ".csv")
  )

  write.csv(integrated, feature_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("HazardGraph-wastewater outputs written.\n")

  c(feature_file, qa_file)
}

upload_hazardgraph_wastewater <- function(
  files, processed_folder, validation_folder
) {
  # Validate HazardGraph-wastewater output files
  if (length(files) != 2 || !all(file.exists(files))) {
    stop("Expected two HazardGraph-wastewater output files.")
  }

  cat("HazardGraph-wastewater output files validated.\n")

  # Upload HazardGraph-wastewater feature table
  googledrive::drive_upload(
    files[[1]],
    path = processed_folder,
    overwrite = TRUE
  )

  cat("HazardGraph-wastewater feature table uploaded.\n")

  # Upload HazardGraph-wastewater QA report
  googledrive::drive_upload(
    files[[2]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("HazardGraph-wastewater QA report uploaded.\n")

  TRUE
}