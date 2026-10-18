build_superfund_proximity_features <- function(tri_fips_files, superfund_files, year, target_county_fips, buffer_m, target_crs, output_dir, validation_dir) {
  # Load Mecklenburg TRI facilities and statewide Superfund polygons
  tri_file <- tri_fips_files[grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)][1]
  superfund_file <- superfund_files[grepl("^nc_superfund_npl_boundaries_access_", basename(superfund_files), ignore.case = TRUE) & grepl("\\.gpkg$", basename(superfund_files), ignore.case = TRUE)][1]

  if (is.na(tri_file) || is.na(superfund_file)) stop("Could not locate Mecklenburg TRI or statewide Superfund input.")

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  superfund <- sf::st_read(superfund_file, quiet = TRUE)

  if (!nrow(facilities) || !nrow(superfund)) stop("TRI or Superfund input is empty.")

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(superfund_file))
  cat("Mecklenburg TRI and statewide Superfund data loaded.\n")

  # Validate TRI and Superfund fields
  required_tri <- c("tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid")
  required_superfund <- c("epa_id", "site_name", "npl_status_raw", "npl_status_group", "intersects_mecklenburg")
  missing_tri <- setdiff(required_tri, names(facilities))
  missing_superfund <- setdiff(required_superfund, names(superfund))

  if (length(missing_tri)) stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  if (length(missing_superfund)) stop(paste("Missing Superfund fields:", paste(missing_superfund, collapse = ", ")))

  facilities <- facilities[as.character(facilities$county_geoid) == target_county_fips, ]

  if (!nrow(facilities)) stop("No Mecklenburg TRI facilities remain after FIPS filtering.")
  if (anyDuplicated(superfund$epa_id)) stop("Superfund layer contains duplicate EPA IDs.")

  cat("TRI and Superfund input fields validated.\n")

  # Validate and standardize Superfund geometries and status
  superfund <- sf::st_make_valid(superfund)

  if (any(sf::st_is_empty(superfund))) stop("Superfund layer contains empty geometries.")
  if (any(!sf::st_is_valid(superfund))) stop("Superfund layer contains invalid geometries.")

  valid_status <- c("final", "proposed", "deleted", "other_or_unknown")
  superfund$npl_status_group <- tolower(trimws(as.character(superfund$npl_status_group)))
  superfund$npl_status_group[is.na(superfund$npl_status_group) | !superfund$npl_status_group %in% valid_status] <- "other_or_unknown"

  cat("Superfund geometries and status standardized.\n")

  # Project TRI facilities and Superfund polygons
  tri_sf <- sf::st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  tri_sf <- sf::st_transform(tri_sf, target_crs)
  superfund <- sf::st_transform(superfund, target_crs)

  cat("TRI facilities and Superfund polygons projected.\n")

  # Calculate footprint, boundary, and containment relationships
  footprint_distance <- sf::st_distance(tri_sf, superfund)
  footprint_distance <- matrix(as.numeric(footprint_distance), nrow = nrow(tri_sf), ncol = nrow(superfund))

  superfund_boundaries <- sf::st_boundary(sf::st_geometry(superfund))
  boundary_distance <- sf::st_distance(sf::st_geometry(tri_sf), superfund_boundaries)
  boundary_distance <- matrix(as.numeric(boundary_distance), nrow = nrow(tri_sf), ncol = nrow(superfund))

  inside_list <- sf::st_within(tri_sf, superfund)

  if (any(!is.finite(footprint_distance)) || any(!is.finite(boundary_distance))) {
    stop("Invalid TRI-to-Superfund distances detected.")
  }

  cat("TRI-to-Superfund spatial relationships calculated.\n")

  # Build facility-buffer Superfund proximity features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- footprint_distance[i, ]
    boundary_distances <- boundary_distance[i, ]
    nearest_index <- which.min(distances)
    nearest_boundary_index <- which.min(boundary_distances)
    nearest <- superfund[nearest_index, , drop = FALSE]
    inside_indices <- inside_list[[i]]

    for (radius in sort(buffer_m)) {
      nearby <- which(distances <= radius)
      status_counts <- setNames(rep(0L, length(valid_status)), valid_status)

      if (length(nearby)) {
        status_table <- table(superfund$npl_status_group[nearby])
        status_counts[names(status_table)] <- as.integer(status_table)
      }

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        superfund_site_count = length(nearby),
        superfund_any_within_buffer = length(nearby) > 0,
        superfund_final_count = status_counts[["final"]],
        superfund_proposed_count = status_counts[["proposed"]],
        superfund_deleted_count = status_counts[["deleted"]],
        superfund_other_unknown_count = status_counts[["other_or_unknown"]],
        inside_superfund_boundary = length(inside_indices) > 0,
        inside_superfund_site_count = length(inside_indices),
        nearest_superfund_distance_m = distances[[nearest_index]],
        nearest_superfund_boundary_distance_m = boundary_distances[[nearest_boundary_index]],
        nearest_superfund_epa_id = nearest$epa_id[[1]],
        nearest_superfund_site_name = nearest$site_name[[1]],
        nearest_superfund_npl_status = nearest$npl_status_group[[1]],
        superfund_snapshot_access_date = access_date,
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL
  cat("TRI-Superfund proximity features calculated.\n")

  # Build detailed TRI-Superfund pairs within the largest buffer
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
        superfund_epa_id = superfund$epa_id[[j]],
        superfund_site_name = superfund$site_name[[j]],
        superfund_npl_status = superfund$npl_status_group[[j]],
        footprint_distance_m = footprint_distance[i, j],
        boundary_distance_m = boundary_distance[i, j],
        inside_superfund_boundary = j %in% inside_list[[i]],
        within_300m = footprint_distance[i, j] <= 300,
        within_500m = footprint_distance[i, j] <= 500,
        within_1000m = footprint_distance[i, j] <= 1000,
        stringsAsFactors = FALSE
      )

      pair_index <- pair_index + 1L
    }
  }

  pairs <- if (length(pair_rows)) {
    do.call(rbind, pair_rows)
  } else {
    data.frame(
      tri_facility_id = character(),
      facility_name = character(),
      superfund_epa_id = character(),
      superfund_site_name = character(),
      superfund_npl_status = character(),
      footprint_distance_m = numeric(),
      boundary_distance_m = numeric(),
      inside_superfund_boundary = logical(),
      within_300m = logical(),
      within_500m = logical(),
      within_1000m = logical(),
      stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL
  cat("Detailed TRI-Superfund proximity pairs built.\n")

  # Validate Superfund proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)
  duplicate_rows <- sum(duplicated(features[, c("tri_facility_id", "buffer_m")]))
  buffers_per_facility <- table(features$tri_facility_id)

  status_columns <- c(
    "superfund_final_count",
    "superfund_proposed_count",
    "superfund_deleted_count",
    "superfund_other_unknown_count"
  )

  status_sum <- rowSums(features[, status_columns, drop = FALSE])
  status_mismatches <- sum(status_sum != features$superfund_site_count)
  monotonic_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$superfund_site_count) < 0)) {
      monotonic_violations <- monotonic_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$superfund_site_count > 0, na.rm = TRUE)
  }

  facility_once <- features[!duplicated(features$tri_facility_id), ]

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities", "Statewide NC Superfund sites",
      "Superfund sites intersecting Mecklenburg", "Buffer distances",
      "Expected facility-buffer rows", "Actual facility-buffer rows",
      "Duplicate facility-buffer rows", "Facilities with all buffer distances",
      "Superfund status-count mismatches", "Facilities with nonmonotonic Superfund counts",
      "TRI facilities inside a Superfund boundary", "TRI facilities with Superfund within 300m",
      "TRI facilities with Superfund within 500m", "TRI facilities with Superfund within 1000m",
      "Unique Superfund sites within 1000m of any TRI facility",
      "Minimum TRI-Superfund footprint distance meters",
      "Minimum TRI-Superfund boundary distance meters",
      "Maximum nearest-Superfund footprint distance meters"
    ),
    value = c(
      nrow(facilities), nrow(superfund),
      sum(as.logical(superfund$intersects_mecklenburg), na.rm = TRUE),
      length(buffer_m), expected_rows, nrow(features), duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)), status_mismatches,
      monotonic_violations, sum(facility_once$inside_superfund_boundary, na.rm = TRUE),
      count_at_buffer(300), count_at_buffer(500), count_at_buffer(1000),
      length(unique(pairs$superfund_epa_id)),
      round(min(features$nearest_superfund_distance_m, na.rm = TRUE), 2),
      round(min(features$nearest_superfund_boundary_distance_m, na.rm = TRUE), 2),
      round(max(features$nearest_superfund_distance_m, na.rm = TRUE), 2)
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
  cat("TRI-Superfund proximity QA/QC completed.\n")

  # Write Superfund proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_superfund_proximity_features_",
      year,
      "_superfund_",
      access_date,
      ".csv"
    )
  )

  pair_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_superfund_pairs_",
      max_buffer,
      "m_",
      year,
      "_superfund_",
      access_date,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_superfund_proximity_features_",
      year,
      "_superfund_",
      access_date,
      ".csv"
    )
  )

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-Superfund proximity outputs written.\n")
  c(feature_file, pair_file, qa_file)
}


upload_superfund_proximity_features <- function(files, processed_folder, validation_folder) {
  # Validate Superfund proximity output files
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three Superfund proximity output files.")
  }

  cat("Superfund proximity output files validated.\n")

  # Upload Superfund proximity feature tables
  invisible(lapply(
    files[1:2],
    googledrive::drive_upload,
    path = processed_folder,
    overwrite = TRUE
  ))

  cat("Superfund proximity feature tables uploaded.\n")

  # Upload Superfund proximity QA report
  googledrive::drive_upload(files[[3]], path = validation_folder, overwrite = TRUE)
  cat("Superfund proximity QA report uploaded.\n")

  TRUE
}