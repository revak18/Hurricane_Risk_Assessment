build_brownfields_proximity_features <- function(tri_fips_files, brownfields_files, year, target_county_fips, buffer_m, target_crs, output_dir, validation_dir) {
  # Load Mecklenburg TRI facilities and statewide Brownfields polygons
  tri_file <- tri_fips_files[grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)][1]
  brownfields_file <- brownfields_files[grepl("^nc_brownfields_boundaries_access_", basename(brownfields_files), ignore.case = TRUE) & grepl("\\.gpkg$", basename(brownfields_files), ignore.case = TRUE)][1]

  if (is.na(tri_file) || is.na(brownfields_file)) stop("Could not locate Mecklenburg TRI or statewide Brownfields input.")

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  brownfields <- sf::st_read(brownfields_file, quiet = TRUE)

  if (!nrow(facilities) || !nrow(brownfields)) stop("TRI or Brownfields input is empty.")

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(brownfields_file))
  cat("Mecklenburg TRI and statewide Brownfields data loaded.\n")

  # Validate TRI and Brownfields fields
  required_tri <- c("tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid")
  required_brownfields <- c("brownfield_project_id", "bf_name", "status_raw", "status_group", "intersects_mecklenburg")
  missing_tri <- setdiff(required_tri, names(facilities))
  missing_brownfields <- setdiff(required_brownfields, names(brownfields))

  if (length(missing_tri)) stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  if (length(missing_brownfields)) stop(paste("Missing Brownfields fields:", paste(missing_brownfields, collapse = ", ")))

  facilities <- facilities[as.character(facilities$county_geoid) == target_county_fips, ]
  if (!nrow(facilities)) stop("No Mecklenburg TRI facilities remain after FIPS filtering.")
  if (anyDuplicated(brownfields$brownfield_project_id)) stop("Brownfields layer contains duplicate project IDs.")

  cat("TRI and Brownfields input fields validated.\n")

  # Validate Brownfields geometry and status
  brownfields <- sf::st_make_valid(brownfields)
  if (any(sf::st_is_empty(brownfields))) stop("Brownfields layer contains empty geometries.")
  if (any(!sf::st_is_valid(brownfields))) stop("Brownfields layer contains invalid geometries.")

  valid_status <- c("recorded", "active_eligible", "pending", "inactive_eligible", "complete", "rfr_pc_complete", "no_further_interest", "ineligible", "other_or_unknown")
  brownfields$status_group <- tolower(trimws(as.character(brownfields$status_group)))
  brownfields$status_group[is.na(brownfields$status_group) | !brownfields$status_group %in% valid_status] <- "other_or_unknown"

  cat("Brownfields geometries and status standardized.\n")

  # Project TRI facilities and Brownfields polygons
  tri_sf <- sf::st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  tri_sf <- sf::st_transform(tri_sf, target_crs)
  brownfields <- sf::st_transform(brownfields, target_crs)

  cat("TRI facilities and Brownfields polygons projected.\n")

  # Calculate footprint, boundary, and containment relationships
  footprint_distance <- sf::st_distance(tri_sf, brownfields)
  footprint_distance <- matrix(as.numeric(footprint_distance), nrow = nrow(tri_sf), ncol = nrow(brownfields))

  brownfield_boundaries <- sf::st_boundary(sf::st_geometry(brownfields))
  boundary_distance <- sf::st_distance(sf::st_geometry(tri_sf), brownfield_boundaries)
  boundary_distance <- matrix(as.numeric(boundary_distance), nrow = nrow(tri_sf), ncol = nrow(brownfields))

  inside_list <- sf::st_within(tri_sf, brownfields)

  if (any(!is.finite(footprint_distance)) || any(!is.finite(boundary_distance))) stop("Invalid TRI-to-Brownfields distances detected.")
  cat("TRI-to-Brownfields spatial relationships calculated.\n")

  # Build facility-buffer Brownfields proximity features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- footprint_distance[i, ]
    boundary_distances <- boundary_distance[i, ]
    nearest_index <- which.min(distances)
    nearest_boundary_index <- which.min(boundary_distances)
    nearest <- brownfields[nearest_index, , drop = FALSE]
    inside_indices <- inside_list[[i]]

    for (radius in sort(buffer_m)) {
      nearby <- which(distances <= radius)
      status_counts <- setNames(rep(0L, length(valid_status)), valid_status)

      if (length(nearby)) {
        status_table <- table(brownfields$status_group[nearby])
        status_counts[names(status_table)] <- as.integer(status_table)
      }

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        brownfield_project_count = length(nearby),
        brownfield_any_within_buffer = length(nearby) > 0,
        brownfield_recorded_count = status_counts[["recorded"]],
        brownfield_active_eligible_count = status_counts[["active_eligible"]],
        brownfield_pending_count = status_counts[["pending"]],
        brownfield_inactive_eligible_count = status_counts[["inactive_eligible"]],
        brownfield_complete_count = status_counts[["complete"]],
        brownfield_rfr_pc_complete_count = status_counts[["rfr_pc_complete"]],
        brownfield_no_further_interest_count = status_counts[["no_further_interest"]],
        brownfield_ineligible_count = status_counts[["ineligible"]],
        brownfield_other_unknown_count = status_counts[["other_or_unknown"]],
        inside_brownfield_boundary = length(inside_indices) > 0,
        inside_brownfield_project_count = length(inside_indices),
        nearest_brownfield_distance_m = distances[[nearest_index]],
        nearest_brownfield_boundary_distance_m = boundary_distances[[nearest_boundary_index]],
        nearest_brownfield_project_id = nearest$brownfield_project_id[[1]],
        nearest_brownfield_name = nearest$bf_name[[1]],
        nearest_brownfield_status = nearest$status_group[[1]],
        brownfields_snapshot_access_date = access_date,
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL
  cat("TRI-Brownfields proximity features calculated.\n")

  # Build detailed TRI-Brownfields pairs within the largest buffer
  max_buffer <- max(buffer_m)
  pair_rows <- list()
  pair_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    nearby <- which(footprint_distance[i, ] <= max_buffer)
    if (!length(nearby)) next

    for (j in nearby) {
      pair_rows[[pair_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        brownfield_project_id = brownfields$brownfield_project_id[[j]],
        brownfield_name = brownfields$bf_name[[j]],
        brownfield_status = brownfields$status_group[[j]],
        contaminant_of_concern = if ("contaminant_of_concern" %in% names(brownfields)) brownfields$contaminant_of_concern[[j]] else NA_character_,
        footprint_distance_m = footprint_distance[i, j],
        boundary_distance_m = boundary_distance[i, j],
        inside_brownfield_boundary = j %in% inside_list[[i]],
        within_300m = footprint_distance[i, j] <= 300,
        within_500m = footprint_distance[i, j] <= 500,
        within_1000m = footprint_distance[i, j] <= 1000,
        stringsAsFactors = FALSE
      )

      pair_index <- pair_index + 1L
    }
  }

  if (length(pair_rows)) {
    pairs <- do.call(rbind, pair_rows)
  } else {
    pairs <- data.frame(
      tri_facility_id = character(), facility_name = character(),
      brownfield_project_id = character(), brownfield_name = character(),
      brownfield_status = character(), contaminant_of_concern = character(),
      footprint_distance_m = numeric(), boundary_distance_m = numeric(),
      inside_brownfield_boundary = logical(), within_300m = logical(),
      within_500m = logical(), within_1000m = logical(),
      stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL
  cat("Detailed TRI-Brownfields proximity pairs built.\n")

  # Validate Brownfields proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)
  duplicate_rows <- sum(duplicated(features[, c("tri_facility_id", "buffer_m")]))
  buffers_per_facility <- table(features$tri_facility_id)

  status_columns <- c(
    "brownfield_recorded_count", "brownfield_active_eligible_count",
    "brownfield_pending_count", "brownfield_inactive_eligible_count",
    "brownfield_complete_count", "brownfield_rfr_pc_complete_count",
    "brownfield_no_further_interest_count", "brownfield_ineligible_count",
    "brownfield_other_unknown_count"
  )

  status_sum <- rowSums(features[, status_columns, drop = FALSE])
  status_mismatches <- sum(status_sum != features$brownfield_project_count)
  monotonic_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$brownfield_project_count) < 0)) {
      monotonic_violations <- monotonic_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$brownfield_project_count > 0, na.rm = TRUE)
  }

  facility_once <- features[!duplicated(features$tri_facility_id), ]

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Statewide NC Brownfields projects",
      "Brownfields projects intersecting Mecklenburg",
      "Buffer distances",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
      "Brownfields status-count mismatches",
      "Facilities with nonmonotonic Brownfields counts",
      "TRI facilities inside a Brownfields boundary",
      "TRI facilities with Brownfields within 300m",
      "TRI facilities with Brownfields within 500m",
      "TRI facilities with Brownfields within 1000m",
      "Unique Brownfields within 1000m of any TRI facility",
      "Minimum TRI-Brownfields footprint distance meters",
      "Minimum TRI-Brownfields boundary distance meters",
      "Maximum nearest-Brownfields footprint distance meters"
    ),
    value = c(
      nrow(facilities),
      nrow(brownfields),
      sum(as.logical(brownfields$intersects_mecklenburg), na.rm = TRUE),
      length(buffer_m),
      expected_rows,
      nrow(features),
      duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
      status_mismatches,
      monotonic_violations,
      sum(facility_once$inside_brownfield_boundary, na.rm = TRUE),
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      length(unique(pairs$brownfield_project_id)),
      round(min(features$nearest_brownfield_distance_m, na.rm = TRUE), 2),
      round(min(features$nearest_brownfield_boundary_distance_m, na.rm = TRUE), 2),
      round(max(features$nearest_brownfield_distance_m, na.rm = TRUE), 2)
    ),
    status = c(
      "INFO", "INFO", "INFO", "INFO", "INFO",
      ifelse(nrow(features) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(all(buffers_per_facility == length(buffer_m)), "PASS", "FAIL"),
      ifelse(status_mismatches == 0, "PASS", "FAIL"),
      ifelse(monotonic_violations == 0, "PASS", "FAIL"),
      "INFO", "INFO", "INFO", "INFO", "INFO", "INFO", "INFO", "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("TRI-Brownfields proximity QA/QC completed.\n")

  # Write Brownfields proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(output_dir, paste0("mecklenburg_tri_brownfields_proximity_features_", year, "_brownfields_", access_date, ".csv"))
  pair_file <- file.path(output_dir, paste0("mecklenburg_tri_brownfields_pairs_", max_buffer, "m_", year, "_brownfields_", access_date, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_brownfields_proximity_features_", year, "_brownfields_", access_date, ".csv"))

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-Brownfields proximity outputs written.\n")
  c(feature_file, pair_file, qa_file)
}

upload_brownfields_proximity_features <- function(files, processed_folder, validation_folder) {
  # Validate Brownfields proximity output files
  if (length(files) != 3 || !all(file.exists(files))) stop("Expected three Brownfields proximity output files.")

  cat("Brownfields proximity output files validated.\n")

  # Upload Brownfields proximity feature tables
  invisible(lapply(files[1:2], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))

  cat("Brownfields proximity feature tables uploaded.\n")

  # Upload Brownfields proximity QA report
  googledrive::drive_upload(files[[3]], path = validation_folder, overwrite = TRUE)

  cat("Brownfields proximity QA report uploaded.\n")
  TRUE
}