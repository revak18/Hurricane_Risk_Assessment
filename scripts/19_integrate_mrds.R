build_hazardgraph_mrds <- function(hazard_feature_files, mrds_proximity_files, year, target_county_fips, output_dir, validation_dir) {
  # Load HazardGraph and MRDS proximity feature tables
  hazard <- read.csv(hazard_feature_files[[1]], stringsAsFactors = FALSE)
  mrds <- read.csv(mrds_proximity_files[[1]], stringsAsFactors = FALSE)
  if (!nrow(hazard) || !nrow(mrds)) stop("HazardGraph or MRDS proximity input table is empty.")
  cat("HazardGraph and MRDS proximity tables loaded.\n")

  # Validate integration keys and Mecklenburg geography
  required_hazard <- c("tri_facility_id", "county_geoid", "buffer_m")
  required_mrds <- c("tri_facility_id", "county_geoid", "buffer_m", "mrds_site_count", "nearest_mrds_distance_m")
  missing_hazard <- setdiff(required_hazard, names(hazard))
  missing_mrds <- setdiff(required_mrds, names(mrds))
  if (length(missing_hazard)) stop(paste("Missing HazardGraph fields:", paste(missing_hazard, collapse = ", ")))
  if (length(missing_mrds)) stop(paste("Missing MRDS fields:", paste(missing_mrds, collapse = ", ")))
  if (!all(as.character(hazard$county_geoid) == target_county_fips)) stop("HazardGraph table contains records outside Mecklenburg County.")
  if (!all(as.character(mrds$county_geoid) == target_county_fips)) stop("MRDS proximity table contains records outside Mecklenburg County.")

  hazard_key <- paste(hazard$tri_facility_id, hazard$buffer_m, sep = "_")
  mrds_key <- paste(mrds$tri_facility_id, mrds$buffer_m, sep = "_")
  if (anyDuplicated(hazard_key)) stop("HazardGraph table contains duplicate facility-buffer keys.")
  if (anyDuplicated(mrds_key)) stop("MRDS table contains duplicate facility-buffer keys.")
  cat("HazardGraph and MRDS integration keys validated.\n")

  # Select MRDS features for HazardGraph integration
  mrds_columns <- intersect(c(
    "tri_facility_id", "buffer_m", "mrds_site_count", "mrds_any_within_buffer",
    "mrds_precious_metal_count", "mrds_base_metal_count", "mrds_ferrous_metal_count",
    "mrds_construction_aggregate_count", "mrds_industrial_mineral_count",
    "mrds_mixed_metal_count", "mrds_mixed_resource_count", "mrds_other_unknown_count",
    "nearest_mrds_distance_m", "nearest_mrds_dep_id", "nearest_mrds_site_name",
    "nearest_mrds_resource_category"
  ), names(mrds))

  mrds_features <- mrds[, mrds_columns, drop = FALSE]
  cat("MRDS integration features selected.\n")

  # Merge MRDS proximity into the HazardGraph feature table
  original_key <- hazard_key
  integrated <- merge(
    hazard, mrds_features,
    by = c("tri_facility_id", "buffer_m"),
    all.x = TRUE, sort = FALSE
  )

  integrated_key <- paste(integrated$tri_facility_id, integrated$buffer_m, sep = "_")
  integrated <- integrated[match(original_key, integrated_key), ]
  rownames(integrated) <- NULL
  cat("MRDS proximity features merged into HazardGraph.\n")

  # Create interpretable MRDS context features
  integrated$mrds_any_within_buffer <- as.logical(integrated$mrds_any_within_buffer)
  integrated$mrds_none_within_buffer <- !integrated$mrds_any_within_buffer
  integrated$mrds_nearest_within_300m <- integrated$nearest_mrds_distance_m <= 300
  integrated$mrds_nearest_within_500m <- integrated$nearest_mrds_distance_m <= 500
  integrated$mrds_nearest_within_1000m <- integrated$nearest_mrds_distance_m <= 1000
  integrated$mrds_proximity_class <- ifelse(
    integrated$nearest_mrds_distance_m <= 300, "within_300m",
    ifelse(
      integrated$nearest_mrds_distance_m <= 500, "within_500m",
      ifelse(integrated$nearest_mrds_distance_m <= 1000, "within_1000m", "beyond_1000m")
    )
  )

  cat("Interpretable MRDS context features created.\n")

  # Validate the integrated HazardGraph-MRDS table
  expected_rows <- nrow(hazard)
  duplicate_rows <- sum(duplicated(integrated[, c("tri_facility_id", "buffer_m")]))
  missing_mrds_rows <- sum(is.na(integrated$mrds_site_count))

  category_columns <- intersect(c(
    "mrds_precious_metal_count", "mrds_base_metal_count",
    "mrds_ferrous_metal_count", "mrds_construction_aggregate_count",
    "mrds_industrial_mineral_count", "mrds_mixed_metal_count",
    "mrds_mixed_resource_count", "mrds_other_unknown_count"
  ), names(integrated))

  category_sum <- rowSums(integrated[, category_columns, drop = FALSE], na.rm = TRUE)
  category_mismatches <- sum(category_sum != integrated$mrds_site_count, na.rm = TRUE)
  monotonic_violations <- 0L

  for (id in unique(integrated$tri_facility_id)) {
    rows <- integrated[integrated$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]
    if (any(diff(rows$mrds_site_count) < 0)) monotonic_violations <- monotonic_violations + 1L
  }

  facility_300 <- integrated[integrated$buffer_m == 300, ]
  facility_500 <- integrated[integrated$buffer_m == 500, ]
  facility_1000 <- integrated[integrated$buffer_m == 1000, ]

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities", "CDL buffer distances", "Expected integrated rows",
      "Actual integrated rows", "Duplicate facility-buffer rows", "Rows missing MRDS features",
      "MRDS category-count mismatches", "Facilities with nonmonotonic MRDS counts",
      "Facilities with MRDS within 300m", "Facilities with MRDS within 500m",
      "Facilities with MRDS within 1000m", "Minimum nearest-MRDS distance meters",
      "Maximum nearest-MRDS distance meters"
    ),
    value = c(
      length(unique(integrated$tri_facility_id)), length(unique(integrated$buffer_m)),
      expected_rows, nrow(integrated), duplicate_rows, missing_mrds_rows,
      category_mismatches, monotonic_violations,
      sum(facility_300$mrds_site_count > 0, na.rm = TRUE),
      sum(facility_500$mrds_site_count > 0, na.rm = TRUE),
      sum(facility_1000$mrds_site_count > 0, na.rm = TRUE),
      round(min(integrated$nearest_mrds_distance_m, na.rm = TRUE), 2),
      round(max(integrated$nearest_mrds_distance_m, na.rm = TRUE), 2)
    ),
    status = c(
      "INFO", "INFO", "INFO",
      ifelse(nrow(integrated) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_mrds_rows == 0, "PASS", "FAIL"),
      ifelse(category_mismatches == 0, "PASS", "FAIL"),
      ifelse(monotonic_violations == 0, "PASS", "FAIL"),
      "INFO", "INFO", "INFO", "INFO", "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("HazardGraph-MRDS QA/QC completed.\n")

  # Write integrated HazardGraph-MRDS outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(output_dir, paste0("mecklenburg_hazardgraph_mrds_features_", year, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_hazardgraph_mrds_", year, ".csv"))

  write.csv(integrated, feature_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)
  cat("HazardGraph-MRDS outputs written.\n")

  c(feature_file, qa_file)
}

upload_hazardgraph_mrds <- function(files, processed_folder, validation_folder) {
  # Validate HazardGraph-MRDS output files
  if (length(files) != 2 || !all(file.exists(files))) stop("Expected two HazardGraph-MRDS output files.")
  cat("HazardGraph-MRDS output files validated.\n")

  # Upload the integrated HazardGraph feature table
  googledrive::drive_upload(files[[1]], path = processed_folder, overwrite = TRUE)
  cat("HazardGraph-MRDS feature table uploaded.\n")

  # Upload the HazardGraph-MRDS QA report
  googledrive::drive_upload(files[[2]], path = validation_folder, overwrite = TRUE)
  cat("HazardGraph-MRDS QA report uploaded.\n")

  TRUE
}