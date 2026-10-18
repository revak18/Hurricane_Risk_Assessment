build_hazardgraph_brownfields <- function(hazardgraph_mrds_files, brownfields_proximity_files, year, target_county_fips, output_dir, validation_dir) {
  # Load HazardGraph-MRDS and Brownfields proximity tables
  hazard <- read.csv(hazardgraph_mrds_files[[1]], stringsAsFactors = FALSE)
  brownfields <- read.csv(brownfields_proximity_files[[1]], stringsAsFactors = FALSE)

  if (!nrow(hazard) || !nrow(brownfields)) stop("HazardGraph or Brownfields proximity input is empty.")

  cat("HazardGraph-MRDS and Brownfields proximity tables loaded.\n")

  # Validate integration keys and Mecklenburg geography
  required_hazard <- c("tri_facility_id", "county_geoid", "buffer_m")
  required_brownfields <- c("tri_facility_id", "county_geoid", "buffer_m", "brownfield_project_count", "nearest_brownfield_distance_m")
  missing_hazard <- setdiff(required_hazard, names(hazard))
  missing_brownfields <- setdiff(required_brownfields, names(brownfields))

  if (length(missing_hazard)) stop(paste("Missing HazardGraph fields:", paste(missing_hazard, collapse = ", ")))
  if (length(missing_brownfields)) stop(paste("Missing Brownfields fields:", paste(missing_brownfields, collapse = ", ")))

  if (!all(as.character(hazard$county_geoid) == target_county_fips)) stop("HazardGraph table contains records outside Mecklenburg County.")
  if (!all(as.character(brownfields$county_geoid) == target_county_fips)) stop("Brownfields proximity table contains records outside Mecklenburg County.")

  hazard_key <- paste(hazard$tri_facility_id, hazard$buffer_m, sep = "_")
  brownfields_key <- paste(brownfields$tri_facility_id, brownfields$buffer_m, sep = "_")

  if (anyDuplicated(hazard_key)) stop("HazardGraph table contains duplicate facility-buffer keys.")
  if (anyDuplicated(brownfields_key)) stop("Brownfields table contains duplicate facility-buffer keys.")

  cat("HazardGraph and Brownfields integration keys validated.\n")

  # Select Brownfields features for integration
  brownfields_columns <- intersect(
    c(
      "tri_facility_id", "buffer_m", "brownfield_project_count", "brownfield_any_within_buffer",
      "brownfield_recorded_count", "brownfield_active_eligible_count", "brownfield_pending_count",
      "brownfield_inactive_eligible_count", "brownfield_complete_count", "brownfield_rfr_pc_complete_count",
      "brownfield_no_further_interest_count", "brownfield_ineligible_count", "brownfield_other_unknown_count",
      "inside_brownfield_boundary", "inside_brownfield_project_count", "nearest_brownfield_distance_m",
      "nearest_brownfield_boundary_distance_m", "nearest_brownfield_project_id", "nearest_brownfield_name",
      "nearest_brownfield_status", "brownfields_snapshot_access_date"
    ),
    names(brownfields)
  )

  brownfields_features <- brownfields[, brownfields_columns, drop = FALSE]
  cat("Brownfields integration features selected.\n")

  # Merge Brownfields proximity into HazardGraph
  original_key <- hazard_key

  integrated <- merge(
    hazard, brownfields_features,
    by = c("tri_facility_id", "buffer_m"),
    all.x = TRUE, sort = FALSE
  )

  integrated_key <- paste(integrated$tri_facility_id, integrated$buffer_m, sep = "_")
  integrated <- integrated[match(original_key, integrated_key), ]
  rownames(integrated) <- NULL

  cat("Brownfields proximity features merged into HazardGraph.\n")

  # Create interpretable Brownfields context features
  integrated$brownfield_any_within_buffer <- as.logical(integrated$brownfield_any_within_buffer)
  integrated$inside_brownfield_boundary <- as.logical(integrated$inside_brownfield_boundary)
  integrated$brownfield_none_within_buffer <- !integrated$brownfield_any_within_buffer
  integrated$brownfield_nearest_within_300m <- integrated$nearest_brownfield_distance_m <= 300
  integrated$brownfield_nearest_within_500m <- integrated$nearest_brownfield_distance_m <= 500
  integrated$brownfield_nearest_within_1000m <- integrated$nearest_brownfield_distance_m <= 1000

  integrated$brownfield_proximity_class <- ifelse(
    integrated$inside_brownfield_boundary,
    "inside_boundary",
    ifelse(
      integrated$nearest_brownfield_distance_m <= 300,
      "within_300m",
      ifelse(
        integrated$nearest_brownfield_distance_m <= 500,
        "within_500m",
        ifelse(integrated$nearest_brownfield_distance_m <= 1000, "within_1000m", "beyond_1000m")
      )
    )
  )

  cat("Interpretable Brownfields context features created.\n")

  # Validate the consolidated HazardGraph table
  expected_rows <- nrow(hazard)
  duplicate_rows <- sum(duplicated(integrated[, c("tri_facility_id", "buffer_m")]))
  missing_brownfields_rows <- sum(is.na(integrated$brownfield_project_count))

  status_columns <- intersect(
    c(
      "brownfield_recorded_count", "brownfield_active_eligible_count", "brownfield_pending_count",
      "brownfield_inactive_eligible_count", "brownfield_complete_count", "brownfield_rfr_pc_complete_count",
      "brownfield_no_further_interest_count", "brownfield_ineligible_count", "brownfield_other_unknown_count"
    ),
    names(integrated)
  )

  status_sum <- rowSums(integrated[, status_columns, drop = FALSE], na.rm = TRUE)
  status_mismatches <- sum(status_sum != integrated$brownfield_project_count, na.rm = TRUE)
  monotonic_violations <- 0L

  for (id in unique(integrated$tri_facility_id)) {
    rows <- integrated[integrated$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$brownfield_project_count) < 0)) {
      monotonic_violations <- monotonic_violations + 1L
    }
  }

  facility_300 <- integrated[integrated$buffer_m == 300, ]
  facility_500 <- integrated[integrated$buffer_m == 500, ]
  facility_1000 <- integrated[integrated$buffer_m == 1000, ]
  facility_once <- integrated[!duplicated(integrated$tri_facility_id), ]

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Buffer distances",
      "Expected integrated rows",
      "Actual integrated rows",
      "Duplicate facility-buffer rows",
      "Rows missing Brownfields features",
      "Brownfields status-count mismatches",
      "Facilities with nonmonotonic Brownfields counts",
      "TRI facilities inside Brownfields boundary",
      "Facilities with Brownfields within 300m",
      "Facilities with Brownfields within 500m",
      "Facilities with Brownfields within 1000m",
      "Minimum nearest-Brownfields distance meters",
      "Maximum nearest-Brownfields distance meters"
    ),
    value = c(
      length(unique(integrated$tri_facility_id)),
      length(unique(integrated$buffer_m)),
      expected_rows,
      nrow(integrated),
      duplicate_rows,
      missing_brownfields_rows,
      status_mismatches,
      monotonic_violations,
      sum(facility_once$inside_brownfield_boundary, na.rm = TRUE),
      sum(facility_300$brownfield_project_count > 0, na.rm = TRUE),
      sum(facility_500$brownfield_project_count > 0, na.rm = TRUE),
      sum(facility_1000$brownfield_project_count > 0, na.rm = TRUE),
      round(min(integrated$nearest_brownfield_distance_m, na.rm = TRUE), 2),
      round(max(integrated$nearest_brownfield_distance_m, na.rm = TRUE), 2)
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      ifelse(nrow(integrated) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_brownfields_rows == 0, "PASS", "FAIL"),
      ifelse(status_mismatches == 0, "PASS", "FAIL"),
      ifelse(monotonic_violations == 0, "PASS", "FAIL"),
      "INFO", "INFO", "INFO", "INFO", "INFO", "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("HazardGraph-Brownfields QA/QC completed.\n")

  # Write consolidated HazardGraph outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(output_dir, paste0("mecklenburg_hazardgraph_consolidated_features_", year, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_hazardgraph_brownfields_", year, ".csv"))

  write.csv(integrated, feature_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("Consolidated HazardGraph outputs written.\n")
  c(feature_file, qa_file)
}

upload_hazardgraph_brownfields <- function(files, processed_folder, validation_folder) {
  # Validate consolidated HazardGraph outputs
  if (length(files) != 2 || !all(file.exists(files))) stop("Expected two consolidated HazardGraph output files.")

  cat("Consolidated HazardGraph output files validated.\n")

  # Upload the consolidated HazardGraph feature table
  googledrive::drive_upload(files[[1]], path = processed_folder, overwrite = TRUE)

  cat("Consolidated HazardGraph feature table uploaded.\n")

  # Upload the consolidated HazardGraph QA report
  googledrive::drive_upload(files[[2]], path = validation_folder, overwrite = TRUE)

  cat("Consolidated HazardGraph QA report uploaded.\n")
  TRUE
}