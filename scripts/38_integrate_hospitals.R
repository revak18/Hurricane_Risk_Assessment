build_hazardgraph_hospitals <- function(
  hazardgraph_substation_files,
  hospital_proximity_files,
  year,
  target_county_fips,
  output_dir,
  validation_dir
) {
  # Load Step 35 HazardGraph and Step 37 hospital features
  hazard_file <- hazardgraph_substation_files[
    grepl("mecklenburg_hazardgraph_substation_features", basename(hazardgraph_substation_files), ignore.case = TRUE)
  ][1]

  hospital_file <- hospital_proximity_files[
    grepl("mecklenburg_tri_hospital_proximity_features", basename(hospital_proximity_files), ignore.case = TRUE)
  ][1]

  if (is.na(hazard_file) || is.na(hospital_file)) {
    stop("Could not locate HazardGraph substation or hospital proximity input.")
  }

  hazard <- read.csv(hazard_file, stringsAsFactors = FALSE)
  hospitals <- read.csv(hospital_file, stringsAsFactors = FALSE)

  if (!nrow(hazard) || !nrow(hospitals)) {
    stop("HazardGraph or hospital proximity input is empty.")
  }

  cat("HazardGraph substation and hospital proximity data loaded.\n")

  # Validate integration keys and hospital fields
  key_fields <- c("tri_facility_id", "county_geoid", "buffer_m")

  required_hospital <- c(
    key_fields,
    "hospital_site_count",
    "hospital_any_within_buffer",
    "open_hospital_count",
    "hospital_beds_available_count",
    "hospital_total_beds",
    "hospital_max_beds",
    "trauma_hospital_count",
    "helipad_hospital_count",
    "nearest_hospital_distance_m",
    "nearest_hospital_site_id",
    "nearest_hospital_name",
    "nearest_hospital_type",
    "nearest_hospital_status",
    "nearest_hospital_open_flag",
    "nearest_hospital_beds",
    "nearest_hospital_trauma",
    "nearest_hospital_trauma_designated",
    "nearest_hospital_helipad",
    "nearest_hospital_helipad_flag",
    "hospital_snapshot_access_date"
  )

  missing_hazard_keys <- setdiff(key_fields, names(hazard))
  missing_hospital <- setdiff(required_hospital, names(hospitals))

  if (length(missing_hazard_keys)) {
    stop(paste("Missing HazardGraph keys:", paste(missing_hazard_keys, collapse = ", ")))
  }

  if (length(missing_hospital)) {
    stop(paste("Missing hospital fields:", paste(missing_hospital, collapse = ", ")))
  }

  hazard$county_geoid <- as.character(hazard$county_geoid)
  hospitals$county_geoid <- as.character(hospitals$county_geoid)
  hazard$buffer_m <- as.numeric(hazard$buffer_m)
  hospitals$buffer_m <- as.numeric(hospitals$buffer_m)

  hazard <- hazard[hazard$county_geoid == target_county_fips, ]
  hospitals <- hospitals[hospitals$county_geoid == target_county_fips, ]

  if (anyDuplicated(hazard[key_fields])) {
    stop("HazardGraph input contains duplicate facility-buffer keys.")
  }

  if (anyDuplicated(hospitals[key_fields])) {
    stop("Hospital proximity input contains duplicate facility-buffer keys.")
  }

  cat("HazardGraph and hospital integration keys validated.\n")

  # Standardize hospital integration fields
  numeric_fields <- c(
    "hospital_site_count",
    "open_hospital_count",
    "hospital_beds_available_count",
    "hospital_total_beds",
    "hospital_max_beds",
    "trauma_hospital_count",
    "helipad_hospital_count",
    "nearest_hospital_distance_m",
    "nearest_hospital_beds"
  )

  logical_fields <- c(
    "hospital_any_within_buffer",
    "nearest_hospital_open_flag",
    "nearest_hospital_trauma_designated",
    "nearest_hospital_helipad_flag"
  )

  for (field in numeric_fields) {
    hospitals[[field]] <- suppressWarnings(as.numeric(hospitals[[field]]))
  }

  for (field in logical_fields) {
    hospitals[[field]] <- as.logical(hospitals[[field]])
  }

  cat("Hospital integration fields standardized.\n")

  # Prepare hospital feature set and original row order
  hospital_features <- hospitals[, required_hospital, drop = FALSE]

  original_order <- paste(
    hazard$tri_facility_id,
    hazard$county_geoid,
    hazard$buffer_m,
    sep = "|"
  )

  cat("Hospital integration feature set prepared.\n")

  # Merge hospital features into HazardGraph
  integrated <- merge(hazard, hospital_features, by = key_fields, all.x = TRUE, sort = FALSE)

  integrated_key <- paste(
    integrated$tri_facility_id,
    integrated$county_geoid,
    integrated$buffer_m,
    sep = "|"
  )

  reorder_index <- match(original_order, integrated_key)

  if (any(is.na(reorder_index))) {
    stop("Hospital integration failed to preserve all HazardGraph keys.")
  }

  integrated <- integrated[reorder_index, ]
  rownames(integrated) <- NULL

  cat("Hospital features merged into HazardGraph.\n")

  # Add interpretable hospital proximity flags
  integrated$hospital_none_within_buffer <- integrated$hospital_site_count == 0
  integrated$open_hospital_any_within_buffer <- integrated$open_hospital_count > 0
  integrated$hospital_bed_data_within_buffer <- integrated$hospital_beds_available_count > 0
  integrated$trauma_hospital_any_within_buffer <- integrated$trauma_hospital_count > 0
  integrated$helipad_hospital_any_within_buffer <- integrated$helipad_hospital_count > 0
  integrated$nearest_hospital_within_300m <- integrated$nearest_hospital_distance_m <= 300
  integrated$nearest_hospital_within_500m <- integrated$nearest_hospital_distance_m <= 500
  integrated$nearest_hospital_within_1000m <- integrated$nearest_hospital_distance_m <= 1000

  integrated$hospital_proximity_class <- ifelse(
    integrated$nearest_hospital_distance_m <= 300,
    "0-300m",
    ifelse(
      integrated$nearest_hospital_distance_m <= 500,
      "301-500m",
      ifelse(
        integrated$nearest_hospital_distance_m <= 1000,
        "501-1000m",
        ">1000m"
      )
    )
  )

  cat("Interpretable hospital proximity flags added.\n")

  # Validate integrated hospital features
  expected_rows <- nrow(hazard)
  duplicate_rows <- sum(duplicated(integrated[key_fields]))
  hospital_fields <- setdiff(required_hospital, key_fields)

  missing_hospital_rows <- sum(
    apply(
      integrated[, hospital_fields, drop = FALSE],
      1,
      function(x) {
        all(is.na(x))
      }
    )
  )

  negative_site_counts <- sum(integrated$hospital_site_count < 0, na.rm = TRUE)
  negative_open_counts <- sum(integrated$open_hospital_count < 0, na.rm = TRUE)
  negative_bed_available_counts <- sum(integrated$hospital_beds_available_count < 0, na.rm = TRUE)
  negative_trauma_counts <- sum(integrated$trauma_hospital_count < 0, na.rm = TRUE)
  negative_helipad_counts <- sum(integrated$helipad_hospital_count < 0, na.rm = TRUE)

  invalid_open_counts <- sum(
    integrated$open_hospital_count > integrated$hospital_site_count,
    na.rm = TRUE
  )

  invalid_bed_counts <- sum(
    integrated$hospital_beds_available_count > integrated$hospital_site_count,
    na.rm = TRUE
  )

  invalid_trauma_counts <- sum(
    integrated$trauma_hospital_count > integrated$hospital_site_count,
    na.rm = TRUE
  )

  invalid_helipad_counts <- sum(
    integrated$helipad_hospital_count > integrated$hospital_site_count,
    na.rm = TRUE
  )

  negative_bed_features <- sum(
    integrated$hospital_total_beds < 0 |
      integrated$hospital_max_beds < 0 |
      integrated$nearest_hospital_beds < 0,
    na.rm = TRUE
  )

  cat("Integrated hospital feature values validated.\n")

  # Validate hospital feature monotonicity
  monotonic_site_violations <- 0L
  monotonic_open_violations <- 0L
  monotonic_bed_available_violations <- 0L
  monotonic_total_bed_violations <- 0L
  monotonic_trauma_violations <- 0L
  monotonic_helipad_violations <- 0L

  for (id in unique(integrated$tri_facility_id)) {
    rows <- integrated[integrated$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$hospital_site_count) < 0)) {
      monotonic_site_violations <- monotonic_site_violations + 1L
    }

    if (any(diff(rows$open_hospital_count) < 0)) {
      monotonic_open_violations <- monotonic_open_violations + 1L
    }

    if (any(diff(rows$hospital_beds_available_count) < 0)) {
      monotonic_bed_available_violations <- monotonic_bed_available_violations + 1L
    }

    bed_values <- rows$hospital_total_beds
    bed_values <- bed_values[!is.na(bed_values)]

    if (length(bed_values) > 1 && any(diff(bed_values) < -1e-10)) {
      monotonic_total_bed_violations <- monotonic_total_bed_violations + 1L
    }

    if (any(diff(rows$trauma_hospital_count) < 0)) {
      monotonic_trauma_violations <- monotonic_trauma_violations + 1L
    }

    if (any(diff(rows$helipad_hospital_count) < 0)) {
      monotonic_helipad_violations <- monotonic_helipad_violations + 1L
    }
  }

  cat("Integrated hospital monotonicity validated.\n")

  # Summarize hospital signal in the integrated table
  count_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$hospital_site_count > 0, na.rm = TRUE)
  }

  open_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$open_hospital_count > 0, na.rm = TRUE)
  }

  beds_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$hospital_beds_available_count > 0, na.rm = TRUE)
  }

  trauma_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$trauma_hospital_count > 0, na.rm = TRUE)
  }

  helipad_at_buffer <- function(radius) {
    rows <- integrated[integrated$buffer_m == radius, ]
    sum(rows$helipad_hospital_count > 0, na.rm = TRUE)
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

  cat("Integrated hospital signal summarized.\n")

  # Build Step 38 integration QA table
  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Buffer distances",
      "Expected integrated rows",
      "Actual integrated rows",
      "Duplicate facility-buffer rows",
      "Rows missing hospital features",
      "Negative hospital site counts",
      "Negative open-hospital counts",
      "Negative bed-availability counts",
      "Negative trauma-hospital counts",
      "Negative helipad-hospital counts",
      "Invalid open-hospital counts",
      "Invalid bed-availability counts",
      "Invalid trauma-hospital counts",
      "Invalid helipad-hospital counts",
      "Negative hospital bed features",
      "Facilities with nonmonotonic hospital counts",
      "Facilities with nonmonotonic open-hospital counts",
      "Facilities with nonmonotonic bed-availability counts",
      "Facilities with nonmonotonic known total beds",
      "Facilities with nonmonotonic trauma counts",
      "Facilities with nonmonotonic helipad counts",
      "TRI facilities with hospital within 300m",
      "TRI facilities with hospital within 500m",
      "TRI facilities with hospital within 1000m",
      "TRI facilities with open hospital within 300m",
      "TRI facilities with open hospital within 500m",
      "TRI facilities with open hospital within 1000m",
      "TRI facilities with bed data within 300m",
      "TRI facilities with bed data within 500m",
      "TRI facilities with bed data within 1000m",
      "TRI facilities with trauma hospital within 300m",
      "TRI facilities with trauma hospital within 500m",
      "TRI facilities with trauma hospital within 1000m",
      "TRI facilities with helipad hospital within 300m",
      "TRI facilities with helipad hospital within 500m",
      "TRI facilities with helipad hospital within 1000m",
      "Minimum nearest-hospital distance meters",
      "Maximum nearest-hospital distance meters"
    ),

    value = c(
      length(unique(integrated$tri_facility_id)),
      length(unique(integrated$buffer_m)),
      expected_rows,
      nrow(integrated),
      duplicate_rows,
      missing_hospital_rows,
      negative_site_counts,
      negative_open_counts,
      negative_bed_available_counts,
      negative_trauma_counts,
      negative_helipad_counts,
      invalid_open_counts,
      invalid_bed_counts,
      invalid_trauma_counts,
      invalid_helipad_counts,
      negative_bed_features,
      monotonic_site_violations,
      monotonic_open_violations,
      monotonic_bed_available_violations,
      monotonic_total_bed_violations,
      monotonic_trauma_violations,
      monotonic_helipad_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      open_at_buffer(300),
      open_at_buffer(500),
      open_at_buffer(1000),
      beds_at_buffer(300),
      beds_at_buffer(500),
      beds_at_buffer(1000),
      trauma_at_buffer(300),
      trauma_at_buffer(500),
      trauma_at_buffer(1000),
      helipad_at_buffer(300),
      helipad_at_buffer(500),
      helipad_at_buffer(1000),
      round(safe_min(facility_once$nearest_hospital_distance_m), 2),
      round(safe_max(facility_once$nearest_hospital_distance_m), 2)
    ),

    status = c(
      "INFO",
      "INFO",
      ifelse(expected_rows == nrow(integrated), "PASS", "FAIL"),
      ifelse(expected_rows == nrow(integrated), "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_hospital_rows == 0, "PASS", "FAIL"),
      ifelse(negative_site_counts == 0, "PASS", "FAIL"),
      ifelse(negative_open_counts == 0, "PASS", "FAIL"),
      ifelse(negative_bed_available_counts == 0, "PASS", "FAIL"),
      ifelse(negative_trauma_counts == 0, "PASS", "FAIL"),
      ifelse(negative_helipad_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_open_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_bed_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_trauma_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_helipad_counts == 0, "PASS", "FAIL"),
      ifelse(negative_bed_features == 0, "PASS", "FAIL"),
      ifelse(monotonic_site_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_open_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_bed_available_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_total_bed_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_trauma_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_helipad_violations == 0, "PASS", "FAIL"),
      rep("INFO", 17)
    ),

    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("HazardGraph hospital integration QA/QC completed.\n")

  # Write Step 38 integrated HazardGraph outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  output_file <- file.path(
    output_dir,
    paste0("mecklenburg_hazardgraph_hospital_features_", year, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_hazardgraph_hospital_integration_", year, ".csv")
  )

  write.csv(integrated, output_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("HazardGraph hospital integration outputs written.\n")

  c(output_file, qa_file)
}

upload_hazardgraph_hospitals <- function(
  files,
  processed_folder,
  validation_folder
) {
  # Validate Step 38 hospital integration files
  if (length(files) != 2 || !all(file.exists(files))) {
    stop("Expected two HazardGraph hospital integration files.")
  }

  cat("HazardGraph hospital integration files validated.\n")

  # Upload the integrated HazardGraph table
  googledrive::drive_upload(
    files[[1]],
    path = processed_folder,
    overwrite = TRUE
  )

  cat("Integrated HazardGraph hospital table uploaded.\n")

  # Upload the Step 38 QA report
  googledrive::drive_upload(
    files[[2]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("HazardGraph hospital integration QA report uploaded.\n")

  TRUE
}