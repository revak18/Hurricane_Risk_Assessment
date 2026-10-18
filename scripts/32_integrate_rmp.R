build_hazardgraph_rmp <- function(hazardgraph_wastewater_files, rmp_proximity_files, year, target_county_fips, output_dir, validation_dir) {
  # Load HazardGraph and RMP proximity tables
  hazard <- read.csv(hazardgraph_wastewater_files[[1]], stringsAsFactors = FALSE)
  rmp <- read.csv(rmp_proximity_files[[1]], stringsAsFactors = FALSE)

  if (!nrow(hazard) || !nrow(rmp)) {
    stop("HazardGraph or RMP proximity input is empty.")
  }

  cat("HazardGraph and RMP proximity tables loaded.\n")

  # Validate integration fields and keys
  required_hazard <- c("tri_facility_id", "county_geoid", "buffer_m")

  required_rmp <- c(
    "tri_facility_id",
    "county_geoid",
    "buffer_m",
    "rmp_site_count",
    "rmp_any_within_buffer",
    "colocated_rmp_count",
    "colocated_rmp_any",
    "external_rmp_site_count",
    "external_rmp_any_within_buffer",
    "nearest_rmp_distance_m",
    "nearest_external_rmp_distance_m"
  )

  missing_hazard <- setdiff(required_hazard, names(hazard))
  missing_rmp <- setdiff(required_rmp, names(rmp))

  if (length(missing_hazard)) {
    stop(paste("Missing HazardGraph fields:", paste(missing_hazard, collapse = ", ")))
  }

  if (length(missing_rmp)) {
    stop(paste("Missing RMP fields:", paste(missing_rmp, collapse = ", ")))
  }

  if (!all(as.character(hazard$county_geoid) == target_county_fips)) {
    stop("HazardGraph contains records outside Mecklenburg County.")
  }

  if (!all(as.character(rmp$county_geoid) == target_county_fips)) {
    stop("RMP proximity table contains records outside Mecklenburg County.")
  }

  hazard_key <- paste(hazard$tri_facility_id, hazard$buffer_m, sep = "_")
  rmp_key <- paste(rmp$tri_facility_id, rmp$buffer_m, sep = "_")

  if (anyDuplicated(hazard_key)) {
    stop("HazardGraph contains duplicate facility-buffer keys.")
  }

  if (anyDuplicated(rmp_key)) {
    stop("RMP table contains duplicate facility-buffer keys.")
  }

  cat("HazardGraph and RMP integration keys validated.\n")

  # Select RMP features for integration
  rmp_columns <- intersect(
    c(
      "tri_facility_id",
      "buffer_m",
      "rmp_site_count",
      "rmp_any_within_buffer",
      "rmp_tri_count",
      "rmp_rcra_lqg_count",
      "rmp_rcra_tsd_count",
      "colocated_rmp_count",
      "colocated_rmp_any",
      "tri_is_rmp_by_registry",
      "external_rmp_site_count",
      "external_rmp_any_within_buffer",
      "external_rmp_tri_count",
      "external_rmp_rcra_lqg_count",
      "external_rmp_rcra_tsd_count",
      "nearest_rmp_distance_m",
      "nearest_rmp_site_id",
      "nearest_rmp_registry_id",
      "nearest_rmp_name",
      "nearest_rmp_tri",
      "nearest_rmp_rcra_lqg",
      "nearest_rmp_rcra_tsd",
      "nearest_external_rmp_distance_m",
      "nearest_external_rmp_site_id",
      "nearest_external_rmp_name",
      "rmp_colocation_threshold_m",
      "rmp_snapshot_access_date"
    ),
    names(rmp)
  )

  rmp_features <- rmp[, rmp_columns, drop = FALSE]

  cat("RMP integration features selected.\n")

  # Merge RMP features into HazardGraph
  original_key <- hazard_key

  integrated <- merge(
    hazard,
    rmp_features,
    by = c("tri_facility_id", "buffer_m"),
    all.x = TRUE,
    sort = FALSE
  )

  integrated_key <- paste(integrated$tri_facility_id, integrated$buffer_m, sep = "_")
  integrated <- integrated[match(original_key, integrated_key), ]
  rownames(integrated) <- NULL

  cat("RMP features merged into HazardGraph.\n")

  # Create interpretable RMP context features
  integrated$rmp_any_within_buffer <- as.logical(integrated$rmp_any_within_buffer)
  integrated$external_rmp_any_within_buffer <- as.logical(integrated$external_rmp_any_within_buffer)
  integrated$colocated_rmp_any <- as.logical(integrated$colocated_rmp_any)

  integrated$rmp_none_within_buffer <- !integrated$rmp_any_within_buffer
  integrated$external_rmp_none_within_buffer <- !integrated$external_rmp_any_within_buffer
  integrated$rmp_nearest_within_300m <- integrated$nearest_rmp_distance_m <= 300
  integrated$rmp_nearest_within_500m <- integrated$nearest_rmp_distance_m <= 500
  integrated$rmp_nearest_within_1000m <- integrated$nearest_rmp_distance_m <= 1000
  integrated$external_rmp_nearest_within_300m <- integrated$nearest_external_rmp_distance_m <= 300
  integrated$external_rmp_nearest_within_500m <- integrated$nearest_external_rmp_distance_m <= 500
  integrated$external_rmp_nearest_within_1000m <- integrated$nearest_external_rmp_distance_m <= 1000

  integrated$rmp_proximity_class <- ifelse(
    integrated$nearest_rmp_distance_m <= 300,
    "within_300m",
    ifelse(
      integrated$nearest_rmp_distance_m <= 500,
      "within_500m",
      ifelse(
        integrated$nearest_rmp_distance_m <= 1000,
        "within_1000m",
        "beyond_1000m"
      )
    )
  )

  integrated$external_rmp_proximity_class <- ifelse(
    integrated$nearest_external_rmp_distance_m <= 300,
    "within_300m",
    ifelse(
      integrated$nearest_external_rmp_distance_m <= 500,
      "within_500m",
      ifelse(
        integrated$nearest_external_rmp_distance_m <= 1000,
        "within_1000m",
        "beyond_1000m"
      )
    )
  )

  cat("Interpretable RMP context features created.\n")

  # Validate integrated RMP features
  expected_rows <- nrow(hazard)

  duplicate_rows <- sum(
    duplicated(
      integrated[
        ,
        c("tri_facility_id", "buffer_m")
      ]
    )
  )

  missing_rmp_rows <- sum(is.na(integrated$rmp_site_count))

  invalid_external_counts <- sum(
    integrated$external_rmp_site_count >
      integrated$rmp_site_count,
    na.rm = TRUE
  )

  invalid_rmp_program_counts <- sum(
    integrated$rmp_tri_count >
      integrated$rmp_site_count |
      integrated$rmp_rcra_lqg_count >
        integrated$rmp_site_count |
      integrated$rmp_rcra_tsd_count >
        integrated$rmp_site_count,
    na.rm = TRUE
  )

  invalid_external_program_counts <- sum(
    integrated$external_rmp_tri_count >
      integrated$external_rmp_site_count |
      integrated$external_rmp_rcra_lqg_count >
        integrated$external_rmp_site_count |
      integrated$external_rmp_rcra_tsd_count >
        integrated$external_rmp_site_count,
    na.rm = TRUE
  )

  monotonic_count_violations <- 0L
  monotonic_external_violations <- 0L

  for (id in unique(integrated$tri_facility_id)) {
    rows <- integrated[integrated$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$rmp_site_count) < 0)) {
      monotonic_count_violations <- monotonic_count_violations + 1L
    }

    if (any(diff(rows$external_rmp_site_count) < 0)) {
      monotonic_external_violations <- monotonic_external_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$rmp_site_count > 0, na.rm = TRUE)
  }

  external_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$external_rmp_site_count > 0, na.rm = TRUE)
  }

  safe_min <- function(x) {
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    min(x)
  }

  safe_max <- function(x) {
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    max(x)
  }

  facility_once <- integrated[!duplicated(integrated$tri_facility_id), ]

  cat("Integrated RMP features validated.\n")

  # Build HazardGraph RMP QA table
  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Buffer distances",
      "Expected integrated rows",
      "Actual integrated rows",
      "Duplicate facility-buffer rows",
      "Rows missing RMP features",
      "Invalid external RMP counts",
      "Invalid RMP program counts",
      "Invalid external RMP program counts",
      "Facilities with nonmonotonic RMP counts",
      "Facilities with nonmonotonic external RMP counts",
      "Facilities with RMP within 300m",
      "Facilities with RMP within 500m",
      "Facilities with RMP within 1000m",
      "Facilities with external RMP within 300m",
      "Facilities with external RMP within 500m",
      "Facilities with external RMP within 1000m",
      "Facilities with colocated RMP",
      "Minimum nearest-RMP distance meters",
      "Minimum nearest-external-RMP distance meters",
      "Maximum nearest-RMP distance meters",
      "Maximum nearest-external-RMP distance meters"
    ),
    value = c(
      length(unique(integrated$tri_facility_id)),
      length(unique(integrated$buffer_m)),
      expected_rows,
      nrow(integrated),
      duplicate_rows,
      missing_rmp_rows,
      invalid_external_counts,
      invalid_rmp_program_counts,
      invalid_external_program_counts,
      monotonic_count_violations,
      monotonic_external_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      external_at_buffer(300),
      external_at_buffer(500),
      external_at_buffer(1000),
      sum(facility_once$colocated_rmp_any, na.rm = TRUE),
      round(safe_min(facility_once$nearest_rmp_distance_m), 2),
      round(safe_min(facility_once$nearest_external_rmp_distance_m), 2),
      round(safe_max(facility_once$nearest_rmp_distance_m), 2),
      round(safe_max(facility_once$nearest_external_rmp_distance_m), 2)
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      ifelse(nrow(integrated) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_rmp_rows == 0, "PASS", "FAIL"),
      ifelse(invalid_external_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_rmp_program_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_external_program_counts == 0, "PASS", "FAIL"),
      ifelse(monotonic_count_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_external_violations == 0, "PASS", "FAIL"),
      rep("INFO", 11)
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("HazardGraph-RMP QA/QC completed.\n")

  # Write HazardGraph RMP outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_hazardgraph_rmp_features_",
      year,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_hazardgraph_rmp_",
      year,
      ".csv"
    )
  )

  write.csv(integrated, feature_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("HazardGraph-RMP outputs written.\n")

  c(feature_file, qa_file)
}

upload_hazardgraph_rmp <- function(files, processed_folder, validation_folder) {
  # Validate HazardGraph RMP output files
  if (length(files) != 2 || !all(file.exists(files))) {
    stop("Expected two HazardGraph-RMP output files.")
  }

  cat("HazardGraph-RMP output files validated.\n")

  # Upload HazardGraph RMP feature table
  googledrive::drive_upload(
    files[[1]],
    path = processed_folder,
    overwrite = TRUE
  )

  cat("HazardGraph-RMP feature table uploaded.\n")

  # Upload HazardGraph RMP QA report
  googledrive::drive_upload(
    files[[2]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("HazardGraph-RMP QA report uploaded.\n")

  TRUE
}