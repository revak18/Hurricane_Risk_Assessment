build_hospital_proximity_features <- function(
  tri_fips_files, hospitals_files, year, target_county_fips, buffer_m,
  target_crs, output_dir, validation_dir
) {
  # Load Mecklenburg TRI facilities and statewide hospitals
  tri_file <- tri_fips_files[
    grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)
  ][1]

  hospital_file <- hospitals_files[
    grepl("^nc_hifld_hospitals_access_", basename(hospitals_files), ignore.case = TRUE) &
      grepl("\\.gpkg$", basename(hospitals_files), ignore.case = TRUE)
  ][1]

  if (is.na(tri_file) || is.na(hospital_file)) {
    stop("Could not locate Mecklenburg TRI or statewide hospital input.")
  }

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  hospitals <- sf::st_read(hospital_file, quiet = TRUE)

  if (!nrow(facilities) || !nrow(hospitals)) {
    stop("TRI or hospital input is empty.")
  }

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(hospital_file))

  if (!grepl("^[0-9]{8}$", access_date)) {
    stop("Could not determine hospital snapshot access date.")
  }

  cat("Mecklenburg TRI and statewide hospital data loaded.\n")

  # Validate TRI and hospital fields
  required_tri <- c(
    "tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid"
  )

  required_hospital <- c(
    "hospital_site_id", "hospital_name", "hospital_type", "status",
    "hospital_open_flag", "beds", "trauma", "trauma_designated",
    "helipad", "helipad_flag", "intersects_mecklenburg"
  )

  missing_tri <- setdiff(required_tri, names(facilities))
  missing_hospital <- setdiff(required_hospital, names(hospitals))

  if (length(missing_tri)) {
    stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  }

  if (length(missing_hospital)) {
    stop(paste("Missing hospital fields:", paste(missing_hospital, collapse = ", ")))
  }

  facilities$county_geoid <- as.character(facilities$county_geoid)
  facilities <- facilities[facilities$county_geoid == target_county_fips, ]

  if (!nrow(facilities)) {
    stop("No Mecklenburg TRI facilities remain after FIPS filtering.")
  }

  if (anyDuplicated(hospitals$hospital_site_id)) {
    stop("Hospital layer contains duplicate hospital site IDs.")
  }

  cat("TRI and hospital input fields validated.\n")

  # Standardize hospital capacity and indicator fields
  hospitals$beds <- suppressWarnings(as.numeric(hospitals$beds))
  hospitals$hospital_open_flag <- as.logical(hospitals$hospital_open_flag)
  hospitals$trauma_designated <- as.logical(hospitals$trauma_designated)
  hospitals$helipad_flag <- as.logical(hospitals$helipad_flag)

  if (any(hospitals$beds < 0, na.rm = TRUE)) {
    stop("Hospital layer contains negative bed counts.")
  }

  cat("Hospital capacity and indicator fields standardized.\n")

  # Project TRI facilities and hospitals
  tri_sf <- sf::st_as_sf(
    facilities,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  tri_sf <- sf::st_transform(tri_sf, target_crs)
  hospitals <- sf::st_transform(hospitals, target_crs)

  cat("TRI facilities and hospitals projected.\n")

  # Calculate TRI-to-hospital distances
  distance_matrix <- sf::st_distance(tri_sf, hospitals)

  distance_matrix <- matrix(
    as.numeric(distance_matrix),
    nrow = nrow(tri_sf),
    ncol = nrow(hospitals)
  )

  if (any(!is.finite(distance_matrix))) {
    stop("Invalid TRI-to-hospital distances detected.")
  }

  cat("TRI-to-hospital distances calculated.\n")

  # Build hospital proximity and capacity features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- distance_matrix[i, ]
    nearest_index <- which.min(distances)
    nearest <- hospitals[nearest_index, , drop = FALSE]

    for (radius in sort(buffer_m)) {
      nearby <- which(distances <= radius)

      open_nearby <- nearby[
        !is.na(hospitals$hospital_open_flag[nearby]) &
          hospitals$hospital_open_flag[nearby]
      ]

      beds_available <- nearby[!is.na(hospitals$beds[nearby])]

      trauma_nearby <- nearby[
        !is.na(hospitals$trauma_designated[nearby]) &
          hospitals$trauma_designated[nearby]
      ]

      helipad_nearby <- nearby[
        !is.na(hospitals$helipad_flag[nearby]) &
          hospitals$helipad_flag[nearby]
      ]

      total_beds <- if (!length(nearby)) {
        0
      } else if (length(beds_available)) {
        sum(hospitals$beds[beds_available], na.rm = TRUE)
      } else {
        NA_real_
      }

      max_beds <- if (length(beds_available)) {
        max(hospitals$beds[beds_available], na.rm = TRUE)
      } else {
        NA_real_
      }

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        hospital_site_count = length(nearby),
        hospital_any_within_buffer = length(nearby) > 0,
        open_hospital_count = length(open_nearby),
        hospital_beds_available_count = length(beds_available),
        hospital_total_beds = total_beds,
        hospital_max_beds = max_beds,
        trauma_hospital_count = length(trauma_nearby),
        helipad_hospital_count = length(helipad_nearby),
        nearest_hospital_distance_m = distances[[nearest_index]],
        nearest_hospital_site_id = nearest$hospital_site_id[[1]],
        nearest_hospital_name = nearest$hospital_name[[1]],
        nearest_hospital_type = nearest$hospital_type[[1]],
        nearest_hospital_status = nearest$status[[1]],
        nearest_hospital_open_flag = nearest$hospital_open_flag[[1]],
        nearest_hospital_beds = nearest$beds[[1]],
        nearest_hospital_trauma = nearest$trauma[[1]],
        nearest_hospital_trauma_designated = nearest$trauma_designated[[1]],
        nearest_hospital_helipad = nearest$helipad[[1]],
        nearest_hospital_helipad_flag = nearest$helipad_flag[[1]],
        hospital_snapshot_access_date = access_date,
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL

  cat("TRI-hospital proximity and capacity features calculated.\n")

  # Build detailed TRI-hospital pairs within the largest buffer
  max_buffer <- max(buffer_m)
  pair_rows <- list()
  pair_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    nearby <- which(distance_matrix[i, ] <= max_buffer)

    if (!length(nearby)) {
      next
    }

    for (j in nearby) {
      pair_rows[[pair_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        tri_facility_name = facilities$facility_name[[i]],
        hospital_site_id = hospitals$hospital_site_id[[j]],
        hospital_name = hospitals$hospital_name[[j]],
        hospital_type = hospitals$hospital_type[[j]],
        hospital_status = hospitals$status[[j]],
        hospital_open_flag = hospitals$hospital_open_flag[[j]],
        hospital_beds = hospitals$beds[[j]],
        hospital_trauma = hospitals$trauma[[j]],
        hospital_trauma_designated = hospitals$trauma_designated[[j]],
        hospital_helipad = hospitals$helipad[[j]],
        hospital_helipad_flag = hospitals$helipad_flag[[j]],
        distance_m = distance_matrix[i, j],
        within_300m = distance_matrix[i, j] <= 300,
        within_500m = distance_matrix[i, j] <= 500,
        within_1000m = distance_matrix[i, j] <= 1000,
        stringsAsFactors = FALSE
      )

      pair_index <- pair_index + 1L
    }
  }

  if (length(pair_rows)) {
    pairs <- do.call(rbind, pair_rows)
  } else {
    pairs <- data.frame(
      tri_facility_id = character(),
      tri_facility_name = character(),
      hospital_site_id = character(),
      hospital_name = character(),
      hospital_type = character(),
      hospital_status = character(),
      hospital_open_flag = logical(),
      hospital_beds = numeric(),
      hospital_trauma = character(),
      hospital_trauma_designated = logical(),
      hospital_helipad = character(),
      hospital_helipad_flag = logical(),
      distance_m = numeric(),
      within_300m = logical(),
      within_500m = logical(),
      within_1000m = logical(),
      stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL

  cat("Detailed TRI-hospital proximity pairs built.\n")

  # Validate hospital proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)

  duplicate_rows <- sum(
    duplicated(features[, c("tri_facility_id", "buffer_m")])
  )

  buffers_per_facility <- table(features$tri_facility_id)

  invalid_open_counts <- sum(
    features$open_hospital_count > features$hospital_site_count
  )

  invalid_bed_counts <- sum(
    features$hospital_beds_available_count > features$hospital_site_count
  )

  invalid_trauma_counts <- sum(
    features$trauma_hospital_count > features$hospital_site_count
  )

  invalid_helipad_counts <- sum(
    features$helipad_hospital_count > features$hospital_site_count
  )

  negative_bed_features <- sum(
    features$hospital_total_beds < 0 | features$hospital_max_beds < 0,
    na.rm = TRUE
  )

  monotonic_site_violations <- 0L
  monotonic_open_violations <- 0L
  monotonic_bed_available_violations <- 0L
  monotonic_total_bed_violations <- 0L
  monotonic_trauma_violations <- 0L
  monotonic_helipad_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
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

  cat("Hospital proximity feature structure validated.\n")

  # Summarize hospital signal
  count_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$hospital_site_count > 0, na.rm = TRUE)
  }

  open_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$open_hospital_count > 0, na.rm = TRUE)
  }

  beds_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$hospital_beds_available_count > 0, na.rm = TRUE)
  }

  trauma_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$trauma_hospital_count > 0, na.rm = TRUE)
  }

  helipad_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
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

  facility_once <- features[!duplicated(features$tri_facility_id), ]

  cat("Hospital proximity signal summarized.\n")

  # Build hospital proximity QA table
  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Statewide NC hospitals",
      "Hospitals intersecting Mecklenburg",
      "Buffer distances",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
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
      "Unique hospitals within 1000m of any TRI",
      "Minimum TRI-hospital distance meters",
      "Maximum nearest-hospital distance meters"
    ),

    value = c(
      nrow(facilities),
      nrow(hospitals),
      sum(as.logical(hospitals$intersects_mecklenburg), na.rm = TRUE),
      length(buffer_m),
      expected_rows,
      nrow(features),
      duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
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
      length(unique(pairs$hospital_site_id)),
      round(safe_min(facility_once$nearest_hospital_distance_m), 2),
      round(safe_max(facility_once$nearest_hospital_distance_m), 2)
    ),

    status = c(
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(nrow(features) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(all(buffers_per_facility == length(buffer_m)), "PASS", "FAIL"),
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
      rep("INFO", 18)
    ),

    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("TRI-hospital proximity QA/QC completed.\n")

  # Write hospital proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_hospital_proximity_features_",
      year,
      "_hospitals_",
      access_date,
      ".csv"
    )
  )

  pair_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_hospital_pairs_",
      max_buffer,
      "m_",
      year,
      "_hospitals_",
      access_date,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_hospital_proximity_features_",
      year,
      "_hospitals_",
      access_date,
      ".csv"
    )
  )

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-hospital proximity outputs written.\n")

  c(feature_file, pair_file, qa_file)
}

upload_hospital_proximity_features <- function(
  files, processed_folder, validation_folder
) {
  # Validate hospital proximity output files
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three hospital proximity output files.")
  }

  cat("Hospital proximity output files validated.\n")

  # Upload hospital proximity feature tables
  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("Hospital proximity feature tables uploaded.\n")

  # Upload hospital proximity QA report
  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("Hospital proximity QA report uploaded.\n")

  TRUE
}