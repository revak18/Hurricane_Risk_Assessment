build_substation_proximity_features <- function(tri_fips_files, substations_files, year, target_county_fips, buffer_m, target_crs, output_dir, validation_dir) {
  # Load Mecklenburg TRI facilities and statewide substations
  tri_file <- tri_fips_files[grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)][1]
  substation_file <- substations_files[
    grepl("^nc_hifld_substations_oct2022_access_", basename(substations_files), ignore.case = TRUE) &
      grepl("\\.gpkg$", basename(substations_files), ignore.case = TRUE)
  ][1]

  if (is.na(tri_file) || is.na(substation_file)) stop("Could not locate Mecklenburg TRI or statewide substation input.")

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  substations <- sf::st_read(substation_file, quiet = TRUE)

  if (!nrow(facilities) || !nrow(substations)) stop("TRI or substation input is empty.")

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(substation_file))
  if (!grepl("^[0-9]{8}$", access_date)) stop("Could not determine substation snapshot access date.")

  cat("Mecklenburg TRI and statewide substation data loaded.\n")

  # Validate TRI and substation fields
  required_tri <- c("tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid")
  required_substation <- c("substation_site_id", "facility_name", "max_voltage_kv", "min_voltage_kv", "line_count", "intersects_mecklenburg")

  missing_tri <- setdiff(required_tri, names(facilities))
  missing_substation <- setdiff(required_substation, names(substations))

  if (length(missing_tri)) stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  if (length(missing_substation)) stop(paste("Missing substation fields:", paste(missing_substation, collapse = ", ")))

  facilities <- facilities[as.character(facilities$county_geoid) == target_county_fips, ]

  if (!nrow(facilities)) stop("No Mecklenburg TRI facilities remain after FIPS filtering.")
  if (anyDuplicated(substations$substation_site_id)) stop("Substation layer contains duplicate substation site IDs.")

  cat("TRI and substation input fields validated.\n")

  # Standardize substation voltage and line attributes
  substations$max_voltage_kv <- suppressWarnings(as.numeric(substations$max_voltage_kv))
  substations$min_voltage_kv <- suppressWarnings(as.numeric(substations$min_voltage_kv))
  substations$line_count <- suppressWarnings(as.numeric(substations$line_count))

  if (any(substations$max_voltage_kv < 0, na.rm = TRUE) || any(substations$min_voltage_kv < 0, na.rm = TRUE)) {
    stop("Substation layer contains negative voltage values.")
  }

  if (any(substations$line_count < 0, na.rm = TRUE)) stop("Substation layer contains negative line counts.")

  cat("Substation voltage and line attributes standardized.\n")

  # Project TRI facilities and substations
  tri_sf <- sf::st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  tri_sf <- sf::st_transform(tri_sf, target_crs)
  substations <- sf::st_transform(substations, target_crs)

  cat("TRI facilities and substations projected.\n")

  # Calculate TRI-to-substation distances
  distance_matrix <- sf::st_distance(tri_sf, substations)
  distance_matrix <- matrix(as.numeric(distance_matrix), nrow = nrow(tri_sf), ncol = nrow(substations))

  if (any(!is.finite(distance_matrix))) stop("Invalid TRI-to-substation distances detected.")

  cat("TRI-to-substation distances calculated.\n")

  # Build substation proximity and infrastructure features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- distance_matrix[i, ]
    nearest_index <- which.min(distances)
    nearest <- substations[nearest_index, , drop = FALSE]

    for (radius in sort(buffer_m)) {
      nearby <- which(distances <= radius)
      voltage_available <- nearby[!is.na(substations$max_voltage_kv[nearby])]
      line_available <- nearby[!is.na(substations$line_count[nearby])]

      max_voltage <- if (length(voltage_available)) max(substations$max_voltage_kv[voltage_available], na.rm = TRUE) else NA_real_
      mean_voltage <- if (length(voltage_available)) mean(substations$max_voltage_kv[voltage_available], na.rm = TRUE) else NA_real_

      total_lines <- if (!length(nearby)) {
        0
      } else if (length(line_available)) {
        sum(substations$line_count[line_available], na.rm = TRUE)
      } else {
        NA_real_
      }

      max_lines <- if (length(line_available)) max(substations$line_count[line_available], na.rm = TRUE) else NA_real_

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        substation_site_count = length(nearby),
        substation_any_within_buffer = length(nearby) > 0,
        substation_voltage_available_count = length(voltage_available),
        substation_max_voltage_kv = max_voltage,
        substation_mean_max_voltage_kv = mean_voltage,
        substation_line_count_available_count = length(line_available),
        substation_total_lines = total_lines,
        substation_max_lines = max_lines,
        nearest_substation_distance_m = distances[[nearest_index]],
        nearest_substation_site_id = nearest$substation_site_id[[1]],
        nearest_substation_name = nearest$facility_name[[1]],
        nearest_substation_max_voltage_kv = nearest$max_voltage_kv[[1]],
        nearest_substation_min_voltage_kv = nearest$min_voltage_kv[[1]],
        nearest_substation_line_count = nearest$line_count[[1]],
        substation_snapshot_access_date = access_date,
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL

  cat("TRI-substation proximity and infrastructure features calculated.\n")

  # Build detailed TRI-substation pairs within the largest buffer
  max_buffer <- max(buffer_m)
  pair_rows <- list()
  pair_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    nearby <- which(distance_matrix[i, ] <= max_buffer)
    if (!length(nearby)) next

    for (j in nearby) {
      pair_rows[[pair_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        tri_facility_name = facilities$facility_name[[i]],
        substation_site_id = substations$substation_site_id[[j]],
        substation_name = substations$facility_name[[j]],
        substation_max_voltage_kv = substations$max_voltage_kv[[j]],
        substation_min_voltage_kv = substations$min_voltage_kv[[j]],
        substation_line_count = substations$line_count[[j]],
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
      substation_site_id = character(),
      substation_name = character(),
      substation_max_voltage_kv = numeric(),
      substation_min_voltage_kv = numeric(),
      substation_line_count = numeric(),
      distance_m = numeric(),
      within_300m = logical(),
      within_500m = logical(),
      within_1000m = logical(),
      stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL

  cat("Detailed TRI-substation proximity pairs built.\n")

  # Validate substation proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)
  duplicate_rows <- sum(duplicated(features[, c("tri_facility_id", "buffer_m")]))
  buffers_per_facility <- table(features$tri_facility_id)

  invalid_voltage_counts <- sum(features$substation_voltage_available_count > features$substation_site_count)
  invalid_line_counts <- sum(features$substation_line_count_available_count > features$substation_site_count)

  negative_voltage_features <- sum(
    features$substation_max_voltage_kv < 0 | features$substation_mean_max_voltage_kv < 0,
    na.rm = TRUE
  )

  negative_line_features <- sum(
    features$substation_total_lines < 0 | features$substation_max_lines < 0,
    na.rm = TRUE
  )

  monotonic_count_violations <- 0L
  monotonic_voltage_available_violations <- 0L
  monotonic_line_available_violations <- 0L
  monotonic_max_voltage_violations <- 0L
  monotonic_total_line_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$substation_site_count) < 0)) {
      monotonic_count_violations <- monotonic_count_violations + 1L
    }

    if (any(diff(rows$substation_voltage_available_count) < 0)) {
      monotonic_voltage_available_violations <- monotonic_voltage_available_violations + 1L
    }

    if (any(diff(rows$substation_line_count_available_count) < 0)) {
      monotonic_line_available_violations <- monotonic_line_available_violations + 1L
    }

    voltage_values <- rows$substation_max_voltage_kv
    voltage_values <- voltage_values[!is.na(voltage_values)]

    if (length(voltage_values) > 1 && any(diff(voltage_values) < -1e-10)) {
      monotonic_max_voltage_violations <- monotonic_max_voltage_violations + 1L
    }

    line_values <- rows$substation_total_lines
    line_values <- line_values[!is.na(line_values)]

    if (length(line_values) > 1 && any(diff(line_values) < -1e-10)) {
      monotonic_total_line_violations <- monotonic_total_line_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$substation_site_count > 0, na.rm = TRUE)
  }

  voltage_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$substation_voltage_available_count > 0, na.rm = TRUE)
  }

  safe_min <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    min(x)
  }

  safe_max <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    max(x)
  }

  facility_once <- features[!duplicated(features$tri_facility_id), ]

  cat("Substation proximity feature structure validated.\n")

  # Build substation proximity QA table
  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Statewide NC substations",
      "Substations intersecting Mecklenburg",
      "Buffer distances",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
      "Invalid voltage-availability counts",
      "Invalid line-availability counts",
      "Negative substation voltage features",
      "Negative substation line features",
      "Facilities with nonmonotonic substation counts",
      "Facilities with nonmonotonic voltage-availability counts",
      "Facilities with nonmonotonic line-availability counts",
      "Facilities with nonmonotonic maximum voltage",
      "Facilities with nonmonotonic known total lines",
      "TRI facilities with substation within 300m",
      "TRI facilities with substation within 500m",
      "TRI facilities with substation within 1000m",
      "TRI facilities with voltage data within 300m",
      "TRI facilities with voltage data within 500m",
      "TRI facilities with voltage data within 1000m",
      "Unique substations within 1000m of any TRI",
      "Minimum TRI-substation distance meters",
      "Maximum nearest-substation distance meters"
    ),

    value = c(
      nrow(facilities),
      nrow(substations),
      sum(as.logical(substations$intersects_mecklenburg), na.rm = TRUE),
      length(buffer_m),
      expected_rows,
      nrow(features),
      duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
      invalid_voltage_counts,
      invalid_line_counts,
      negative_voltage_features,
      negative_line_features,
      monotonic_count_violations,
      monotonic_voltage_available_violations,
      monotonic_line_available_violations,
      monotonic_max_voltage_violations,
      monotonic_total_line_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      voltage_at_buffer(300),
      voltage_at_buffer(500),
      voltage_at_buffer(1000),
      length(unique(pairs$substation_site_id)),
      round(safe_min(facility_once$nearest_substation_distance_m), 2),
      round(safe_max(facility_once$nearest_substation_distance_m), 2)
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
      ifelse(invalid_voltage_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_line_counts == 0, "PASS", "FAIL"),
      ifelse(negative_voltage_features == 0, "PASS", "FAIL"),
      ifelse(negative_line_features == 0, "PASS", "FAIL"),
      ifelse(monotonic_count_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_voltage_available_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_line_available_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_max_voltage_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_total_line_violations == 0, "PASS", "FAIL"),
      rep("INFO", 9)
    ),

    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("TRI-substation proximity QA/QC completed.\n")

  # Write substation proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0("mecklenburg_tri_substation_proximity_features_", year, "_substations_", access_date, ".csv")
  )

  pair_file <- file.path(
    output_dir,
    paste0("mecklenburg_tri_substation_pairs_", max_buffer, "m_", year, "_substations_", access_date, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_substation_proximity_features_", year, "_substations_", access_date, ".csv")
  )

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-substation proximity outputs written.\n")

  c(feature_file, pair_file, qa_file)
}

upload_substation_proximity_features <- function(files, processed_folder, validation_folder) {
  # Validate substation proximity output files
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three substation proximity output files.")
  }

  cat("Substation proximity output files validated.\n")

  # Upload substation proximity feature tables
  invisible(lapply(
    files[1:2],
    googledrive::drive_upload,
    path = processed_folder,
    overwrite = TRUE
  ))

  cat("Substation proximity feature tables uploaded.\n")

  # Upload substation proximity QA report
  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("Substation proximity QA report uploaded.\n")

  TRUE
}