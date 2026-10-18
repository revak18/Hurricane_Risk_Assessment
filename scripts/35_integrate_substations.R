build_hazardgraph_substations <- function(
  hazardgraph_rmp_files,
  substation_proximity_files,
  year,
  target_county_fips,
  output_dir,
  validation_dir
) {
  # Load the Step 32 HazardGraph table and Step 34 substation features
  hazard_file <- hazardgraph_rmp_files[grepl(
    "mecklenburg_hazardgraph_rmp_features",
    basename(hazardgraph_rmp_files),
    ignore.case = TRUE
  )][1]

  substation_file <- substation_proximity_files[grepl(
    "mecklenburg_tri_substation_proximity_features",
    basename(substation_proximity_files),
    ignore.case = TRUE
  )][1]

  if (is.na(hazard_file) || is.na(substation_file)) {
    stop("Could not locate HazardGraph RMP or substation proximity input.")
  }

  hazard <- read.csv(hazard_file, stringsAsFactors = FALSE)
  substations <- read.csv(substation_file, stringsAsFactors = FALSE)

  if (!nrow(hazard) || !nrow(substations)) {
    stop("HazardGraph or substation proximity input is empty.")
  }

  cat("HazardGraph RMP and substation proximity data loaded.\n")

  # Validate integration keys and required substation fields
  key_fields <- c("tri_facility_id", "county_geoid", "buffer_m")

  required_substation <- c(
    key_fields,
    "substation_site_count",
    "substation_any_within_buffer",
    "substation_voltage_available_count",
    "substation_max_voltage_kv",
    "substation_mean_max_voltage_kv",
    "substation_line_count_available_count",
    "substation_total_lines",
    "substation_max_lines",
    "nearest_substation_distance_m",
    "nearest_substation_site_id",
    "nearest_substation_name",
    "nearest_substation_max_voltage_kv",
    "nearest_substation_min_voltage_kv",
    "nearest_substation_line_count",
    "substation_snapshot_access_date"
  )

  missing_hazard_keys <- setdiff(key_fields, names(hazard))
  missing_substation <- setdiff(required_substation, names(substations))

  if (length(missing_hazard_keys)) {
    stop(paste(
      "Missing HazardGraph keys:",
      paste(missing_hazard_keys, collapse = ", ")
    ))
  }

  if (length(missing_substation)) {
    stop(paste(
      "Missing substation fields:",
      paste(missing_substation, collapse = ", ")
    ))
  }

  hazard$county_geoid <- as.character(hazard$county_geoid)
  substations$county_geoid <- as.character(substations$county_geoid)
  hazard$buffer_m <- as.numeric(hazard$buffer_m)
  substations$buffer_m <- as.numeric(substations$buffer_m)

  hazard <- hazard[hazard$county_geoid == target_county_fips, ]
  substations <- substations[substations$county_geoid == target_county_fips, ]

  if (anyDuplicated(hazard[key_fields])) {
    stop("HazardGraph input contains duplicate facility-buffer keys.")
  }

  if (anyDuplicated(substations[key_fields])) {
    stop("Substation proximity input contains duplicate facility-buffer keys.")
  }

  cat("HazardGraph and substation integration keys validated.\n")

  # Select substation fields for integration
  substation_features <- substations[, required_substation]

  original_order <- paste(
    hazard$tri_facility_id,
    hazard$county_geoid,
    hazard$buffer_m,
    sep = "|"
  )

  cat("Substation integration feature set prepared.\n")

  # Merge substation features into HazardGraph
  integrated <- merge(
    hazard,
    substation_features,
    by = key_fields,
    all.x = TRUE,
    sort = FALSE
  )

  integrated_key <- paste(
    integrated$tri_facility_id,
    integrated$county_geoid,
    integrated$buffer_m,
    sep = "|"
  )

  integrated <- integrated[match(original_order, integrated_key), ]
  rownames(integrated) <- NULL

  cat("Substation features merged into HazardGraph.\n")

  # Add interpretable substation proximity flags
  integrated$substation_none_within_buffer <- integrated$substation_site_count == 0
  integrated$nearest_substation_within_300m <- integrated$nearest_substation_distance_m <= 300
  integrated$nearest_substation_within_500m <- integrated$nearest_substation_distance_m <= 500
  integrated$nearest_substation_within_1000m <- integrated$nearest_substation_distance_m <= 1000

  integrated$substation_proximity_class <- ifelse(
    integrated$nearest_substation_distance_m <= 300,
    "0-300m",
    ifelse(
      integrated$nearest_substation_distance_m <= 500,
      "301-500m",
      ifelse(
        integrated$nearest_substation_distance_m <= 1000,
        "501-1000m",
        ">1000m"
      )
    )
  )

  integrated$substation_voltage_data_within_buffer <- integrated$substation_voltage_available_count > 0
  integrated$substation_line_data_within_buffer <- integrated$substation_line_count_available_count > 0

  cat("Interpretable substation proximity flags added.\n")

  # Validate integrated substation features
  expected_rows <- nrow(hazard)
  duplicate_rows <- sum(duplicated(integrated[key_fields]))
  substation_fields <- setdiff(required_substation, key_fields)

  missing_substation_rows <- sum(apply(
    integrated[, substation_fields, drop = FALSE],
    1,
    function(x) {
      all(is.na(x))
    }
  ))

  invalid_voltage_counts <- sum(
    integrated$substation_voltage_available_count >
      integrated$substation_site_count,
    na.rm = TRUE
  )

  invalid_line_counts <- sum(
    integrated$substation_line_count_available_count >
      integrated$substation_site_count,
    na.rm = TRUE
  )

  negative_substation_counts <- sum(integrated$substation_site_count < 0, na.rm = TRUE)

  negative_voltage_features <- sum(
    integrated$substation_max_voltage_kv < 0 |
      integrated$substation_mean_max_voltage_kv < 0 |
      integrated$nearest_substation_max_voltage_kv < 0 |
      integrated$nearest_substation_min_voltage_kv < 0,
    na.rm = TRUE
  )

  negative_line_features <- sum(
    integrated$substation_total_lines < 0 |
      integrated$substation_max_lines < 0 |
      integrated$nearest_substation_line_count < 0,
    na.rm = TRUE
  )

  monotonic_count_violations <- 0L
  monotonic_voltage_count_violations <- 0L
  monotonic_line_count_violations <- 0L

  for (id in unique(integrated$tri_facility_id)) {
    rows <- integrated[integrated$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$substation_site_count) < 0)) {
      monotonic_count_violations <- monotonic_count_violations + 1L
    }

    if (any(diff(rows$substation_voltage_available_count) < 0)) {
      monotonic_voltage_count_violations <-
        monotonic_voltage_count_violations + 1L
    }

    if (any(diff(rows$substation_line_count_available_count) < 0)) {
      monotonic_line_count_violations <-
        monotonic_line_count_violations + 1L
    }
  }

  cat("Integrated substation features validated.\n")

  # Summarize substation signal in the integrated table
  count_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$substation_site_count > 0, na.rm = TRUE)
  }

  voltage_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$substation_voltage_available_count > 0, na.rm = TRUE)
  }

  line_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$substation_line_count_available_count > 0, na.rm = TRUE)
  }

  facility_once <- integrated[!duplicated(integrated$tri_facility_id), ]

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

  cat("Integrated substation signal summarized.\n")

  # Build Step 35 integration QA table
  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Buffer distances",
      "Expected integrated rows",
      "Actual integrated rows",
      "Duplicate facility-buffer rows",
      "Rows missing substation features",
      "Negative substation counts",
      "Invalid voltage-availability counts",
      "Invalid line-availability counts",
      "Negative voltage features",
      "Negative line features",
      "Facilities with nonmonotonic substation counts",
      "Facilities with nonmonotonic voltage-availability counts",
      "Facilities with nonmonotonic line-availability counts",
      "TRI facilities with substation within 300m",
      "TRI facilities with substation within 500m",
      "TRI facilities with substation within 1000m",
      "TRI facilities with voltage data within 300m",
      "TRI facilities with voltage data within 500m",
      "TRI facilities with voltage data within 1000m",
      "TRI facilities with line data within 300m",
      "TRI facilities with line data within 500m",
      "TRI facilities with line data within 1000m",
      "Minimum nearest-substation distance meters",
      "Maximum nearest-substation distance meters"
    ),

    value = c(
      length(unique(integrated$tri_facility_id)),
      length(unique(integrated$buffer_m)),
      expected_rows,
      nrow(integrated),
      duplicate_rows,
      missing_substation_rows,
      negative_substation_counts,
      invalid_voltage_counts,
      invalid_line_counts,
      negative_voltage_features,
      negative_line_features,
      monotonic_count_violations,
      monotonic_voltage_count_violations,
      monotonic_line_count_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      voltage_at_buffer(300),
      voltage_at_buffer(500),
      voltage_at_buffer(1000),
      line_at_buffer(300),
      line_at_buffer(500),
      line_at_buffer(1000),
      round(safe_min(facility_once$nearest_substation_distance_m), 2),
      round(safe_max(facility_once$nearest_substation_distance_m), 2)
    ),

    status = c(
      "INFO",
      "INFO",

      ifelse(
        expected_rows == nrow(integrated),
        "PASS",
        "FAIL"
      ),

      ifelse(
        expected_rows == nrow(integrated),
        "PASS",
        "FAIL"
      ),

      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_substation_rows == 0, "PASS", "FAIL"),
      ifelse(negative_substation_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_voltage_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_line_counts == 0, "PASS", "FAIL"),
      ifelse(negative_voltage_features == 0, "PASS", "FAIL"),
      ifelse(negative_line_features == 0, "PASS", "FAIL"),
      ifelse(monotonic_count_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_voltage_count_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_line_count_violations == 0, "PASS", "FAIL"),
      rep("INFO", 11)
    ),

    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("HazardGraph substation integration QA/QC completed.\n")

  # Write Step 35 integrated HazardGraph outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  output_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_hazardgraph_substation_features_",
      year,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_hazardgraph_substation_integration_",
      year,
      ".csv"
    )
  )

  write.csv(integrated, output_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("HazardGraph substation integration outputs written.\n")

  c(output_file, qa_file)
}

upload_hazardgraph_substations <- function(
  files,
  processed_folder,
  validation_folder
) {
  # Validate Step 35 integration output files
  if (length(files) != 2 || !all(file.exists(files))) {
    stop("Expected two HazardGraph substation integration files.")
  }

  cat("HazardGraph substation integration files validated.\n")

  # Upload the integrated HazardGraph table
  googledrive::drive_upload(
    files[[1]],
    path = processed_folder,
    overwrite = TRUE
  )

  cat("Integrated HazardGraph substation table uploaded.\n")

  # Upload the Step 35 QA report
  googledrive::drive_upload(
    files[[2]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("HazardGraph substation integration QA report uploaded.\n")

  TRUE
}