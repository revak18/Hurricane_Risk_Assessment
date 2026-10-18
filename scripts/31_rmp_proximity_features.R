build_rmp_proximity_features <- function(tri_fips_files, rmp_files, year, target_county_fips, buffer_m, target_crs, output_dir, validation_dir, colocated_m = 10) {
  # Load Mecklenburg TRI facilities and statewide RMP sites
  tri_file <- tri_fips_files[grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)][1]

  rmp_file <- rmp_files[
    grepl("^nc_epa_rmp_facilities_access_", basename(rmp_files), ignore.case = TRUE) &
      grepl("\\.gpkg$", basename(rmp_files), ignore.case = TRUE)
  ][1]

  if (is.na(tri_file) || is.na(rmp_file)) {
    stop("Could not locate Mecklenburg TRI or statewide RMP input.")
  }

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  rmp <- sf::st_read(rmp_file, quiet = TRUE)

  if (!nrow(facilities) || !nrow(rmp)) {
    stop("TRI or RMP input is empty.")
  }

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(rmp_file))

  if (!grepl("^[0-9]{8}$", access_date)) {
    stop("Could not determine RMP snapshot access date.")
  }

  cat("Mecklenburg TRI and statewide RMP data loaded.\n")

  # Validate TRI and RMP fields
  required_tri <- c("tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid")

  required_rmp <- c(
    "rmp_site_id",
    "frs_registry_id",
    "facility_name",
    "rmp",
    "tri",
    "rcra_lqg",
    "rcra_tsd",
    "intersects_mecklenburg"
  )

  missing_tri <- setdiff(required_tri, names(facilities))
  missing_rmp <- setdiff(required_rmp, names(rmp))

  if (length(missing_tri)) {
    stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  }

  if (length(missing_rmp)) {
    stop(paste("Missing RMP fields:", paste(missing_rmp, collapse = ", ")))
  }

  facilities <- facilities[as.character(facilities$county_geoid) == target_county_fips, ]

  if (!nrow(facilities)) {
    stop("No Mecklenburg TRI facilities remain after FIPS filtering.")
  }

  if (anyDuplicated(rmp$rmp_site_id)) {
    stop("RMP layer contains duplicate RMP site IDs.")
  }

  cat("TRI and RMP input fields validated.\n")

  # Standardize RMP program flags
  rmp$rmp <- as.logical(rmp$rmp)
  rmp$tri <- as.logical(rmp$tri)
  rmp$rcra_lqg <- as.logical(rmp$rcra_lqg)
  rmp$rcra_tsd <- as.logical(rmp$rcra_tsd)
  rmp$intersects_mecklenburg <- as.logical(rmp$intersects_mecklenburg)

  if (any(is.na(rmp$rmp))) {
    stop("RMP layer contains missing RMP classifications.")
  }

  if (any(!rmp$rmp)) {
    stop("RMP layer contains facilities without a positive RMP flag.")
  }

  cat("RMP program flags standardized.\n")

  # Detect an optional TRI FRS registry identifier
  tri_registry_candidates <- c("frs_registry_id", "frs_id", "registry_id")
  tri_registry_field <- intersect(tri_registry_candidates, names(facilities))

  if (length(tri_registry_field)) {
    tri_registry_id <- trimws(as.character(facilities[[tri_registry_field[[1]]]]))
    tri_registry_id[is.na(tri_registry_id) | tri_registry_id == ""] <- NA_character_
  } else {
    tri_registry_id <- rep(NA_character_, nrow(facilities))
  }

  rmp_registry_id <- trimws(as.character(rmp$frs_registry_id))
  rmp_registry_id[is.na(rmp_registry_id) | rmp_registry_id == ""] <- NA_character_

  cat("TRI and RMP registry identifiers prepared.\n")

  # Project TRI facilities and RMP sites
  tri_sf <- sf::st_as_sf(
    facilities,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  tri_sf <- sf::st_transform(tri_sf, target_crs)
  rmp <- sf::st_transform(rmp, target_crs)

  cat("TRI facilities and RMP sites projected.\n")

  # Calculate TRI-to-RMP distances
  distance_matrix <- sf::st_distance(tri_sf, rmp)

  distance_matrix <- matrix(
    as.numeric(distance_matrix),
    nrow = nrow(tri_sf),
    ncol = nrow(rmp)
  )

  if (any(!is.finite(distance_matrix))) {
    stop("Invalid TRI-to-RMP distances detected.")
  }

  cat("TRI-to-RMP distances calculated.\n")

  # Build RMP proximity and external-context features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- distance_matrix[i, ]
    same_registry <- rep(FALSE, nrow(rmp))

    if (!is.na(tri_registry_id[[i]])) {
      same_registry <- !is.na(rmp_registry_id) & rmp_registry_id == tri_registry_id[[i]]
    }

    colocated <- distances <= colocated_m | same_registry
    external <- !colocated
    nearest_index <- which.min(distances)
    external_index <- which(external)

    nearest_external_index <- if (length(external_index)) {
      external_index[which.min(distances[external_index])]
    } else {
      NA_integer_
    }

    nearest <- rmp[nearest_index, , drop = FALSE]

    nearest_external <- if (!is.na(nearest_external_index)) {
      rmp[nearest_external_index, , drop = FALSE]
    } else {
      NULL
    }

    nearest_external_distance <- if (!is.na(nearest_external_index)) {
      distances[[nearest_external_index]]
    } else {
      NA_real_
    }

    for (radius in sort(buffer_m)) {
      nearby <- which(distances <= radius)
      nearby_external <- which(distances <= radius & external)
      nearby_tri <- nearby[rmp$tri[nearby] %in% TRUE]
      nearby_lqg <- nearby[rmp$rcra_lqg[nearby] %in% TRUE]
      nearby_tsd <- nearby[rmp$rcra_tsd[nearby] %in% TRUE]
      external_tri <- nearby_external[rmp$tri[nearby_external] %in% TRUE]
      external_lqg <- nearby_external[rmp$rcra_lqg[nearby_external] %in% TRUE]
      external_tsd <- nearby_external[rmp$rcra_tsd[nearby_external] %in% TRUE]

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        rmp_site_count = length(nearby),
        rmp_any_within_buffer = length(nearby) > 0,
        rmp_tri_count = length(nearby_tri),
        rmp_rcra_lqg_count = length(nearby_lqg),
        rmp_rcra_tsd_count = length(nearby_tsd),
        colocated_rmp_count = sum(colocated),
        colocated_rmp_any = any(colocated),
        tri_is_rmp_by_registry = if (is.na(tri_registry_id[[i]])) NA else any(same_registry),
        external_rmp_site_count = length(nearby_external),
        external_rmp_any_within_buffer = length(nearby_external) > 0,
        external_rmp_tri_count = length(external_tri),
        external_rmp_rcra_lqg_count = length(external_lqg),
        external_rmp_rcra_tsd_count = length(external_tsd),
        nearest_rmp_distance_m = distances[[nearest_index]],
        nearest_rmp_site_id = nearest$rmp_site_id[[1]],
        nearest_rmp_registry_id = nearest$frs_registry_id[[1]],
        nearest_rmp_name = nearest$facility_name[[1]],
        nearest_rmp_tri = nearest$tri[[1]],
        nearest_rmp_rcra_lqg = nearest$rcra_lqg[[1]],
        nearest_rmp_rcra_tsd = nearest$rcra_tsd[[1]],
        nearest_external_rmp_distance_m = nearest_external_distance,
        nearest_external_rmp_site_id = if (is.null(nearest_external)) NA_character_ else nearest_external$rmp_site_id[[1]],
        nearest_external_rmp_name = if (is.null(nearest_external)) NA_character_ else nearest_external$facility_name[[1]],
        rmp_colocation_threshold_m = colocated_m,
        rmp_snapshot_access_date = access_date,
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL

  cat("TRI-RMP proximity and external-context features calculated.\n")

  # Build detailed TRI-RMP pairs within the largest buffer
  max_buffer <- max(buffer_m)
  pair_rows <- list()
  pair_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- distance_matrix[i, ]
    same_registry <- rep(FALSE, nrow(rmp))

    if (!is.na(tri_registry_id[[i]])) {
      same_registry <- !is.na(rmp_registry_id) & rmp_registry_id == tri_registry_id[[i]]
    }

    colocated <- distances <= colocated_m | same_registry
    nearby <- which(distances <= max_buffer)

    if (!length(nearby)) {
      next
    }

    for (j in nearby) {
      pair_rows[[pair_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        tri_facility_name = facilities$facility_name[[i]],
        tri_frs_registry_id = tri_registry_id[[i]],
        rmp_site_id = rmp$rmp_site_id[[j]],
        rmp_frs_registry_id = rmp$frs_registry_id[[j]],
        rmp_facility_name = rmp$facility_name[[j]],
        rmp_tri = rmp$tri[[j]],
        rmp_rcra_lqg = rmp$rcra_lqg[[j]],
        rmp_rcra_tsd = rmp$rcra_tsd[[j]],
        distance_m = distances[[j]],
        same_frs_registry = same_registry[[j]],
        colocated_rmp = colocated[[j]],
        external_rmp = !colocated[[j]],
        within_300m = distances[[j]] <= 300,
        within_500m = distances[[j]] <= 500,
        within_1000m = distances[[j]] <= 1000,
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
      tri_frs_registry_id = character(),
      rmp_site_id = character(),
      rmp_frs_registry_id = character(),
      rmp_facility_name = character(),
      rmp_tri = logical(),
      rmp_rcra_lqg = logical(),
      rmp_rcra_tsd = logical(),
      distance_m = numeric(),
      same_frs_registry = logical(),
      colocated_rmp = logical(),
      external_rmp = logical(),
      within_300m = logical(),
      within_500m = logical(),
      within_1000m = logical(),
      stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL

  cat("Detailed TRI-RMP proximity pairs built.\n")

  # Validate RMP proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)

  duplicate_rows <- sum(
    duplicated(
      features[
        ,
        c(
          "tri_facility_id",
          "buffer_m"
        )
      ]
    )
  )

  buffers_per_facility <- table(features$tri_facility_id)

  invalid_external_counts <- sum(
    features$external_rmp_site_count >
      features$rmp_site_count
  )

  invalid_tri_counts <- sum(
    features$rmp_tri_count >
      features$rmp_site_count
  )

  invalid_lqg_counts <- sum(
    features$rmp_rcra_lqg_count >
      features$rmp_site_count
  )

  invalid_tsd_counts <- sum(
    features$rmp_rcra_tsd_count >
      features$rmp_site_count
  )

  monotonic_count_violations <- 0L
  monotonic_external_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$rmp_site_count) < 0)) {
      monotonic_count_violations <- monotonic_count_violations + 1L
    }

    if (any(diff(rows$external_rmp_site_count) < 0)) {
      monotonic_external_violations <- monotonic_external_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$rmp_site_count > 0, na.rm = TRUE)
  }

  external_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
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

  facility_once <- features[!duplicated(features$tri_facility_id), ]

  cat("RMP proximity feature structure validated.\n")

  # Build RMP proximity QA table
  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Statewide NC RMP facilities",
      "RMP facilities intersecting Mecklenburg",
      "Buffer distances",
      "RMP colocation threshold meters",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
      "Invalid external RMP counts",
      "Invalid TRI-flagged RMP counts",
      "Invalid RCRA LQG RMP counts",
      "Invalid RCRA TSD RMP counts",
      "Facilities with nonmonotonic RMP counts",
      "Facilities with nonmonotonic external RMP counts",
      "TRI facilities with RMP within 300m",
      "TRI facilities with RMP within 500m",
      "TRI facilities with RMP within 1000m",
      "TRI facilities with external RMP within 300m",
      "TRI facilities with external RMP within 500m",
      "TRI facilities with external RMP within 1000m",
      "TRI facilities with colocated RMP",
      "TRI facilities matched to RMP by FRS registry ID",
      "Unique RMP sites within 1000m of any TRI",
      "Unique external RMP sites within 1000m of any TRI",
      "Minimum TRI-RMP distance meters",
      "Minimum TRI-external-RMP distance meters",
      "Maximum nearest-RMP distance meters",
      "Maximum nearest-external-RMP distance meters"
    ),
    value = c(
      nrow(facilities),
      nrow(rmp),
      sum(rmp$intersects_mecklenburg, na.rm = TRUE),
      length(buffer_m),
      colocated_m,
      expected_rows,
      nrow(features),
      duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
      invalid_external_counts,
      invalid_tri_counts,
      invalid_lqg_counts,
      invalid_tsd_counts,
      monotonic_count_violations,
      monotonic_external_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      external_at_buffer(300),
      external_at_buffer(500),
      external_at_buffer(1000),
      sum(facility_once$colocated_rmp_any, na.rm = TRUE),
      sum(facility_once$tri_is_rmp_by_registry %in% TRUE, na.rm = TRUE),
      length(unique(pairs$rmp_site_id)),
      length(unique(pairs$rmp_site_id[pairs$external_rmp %in% TRUE])),
      round(safe_min(facility_once$nearest_rmp_distance_m), 2),
      round(safe_min(facility_once$nearest_external_rmp_distance_m), 2),
      round(safe_max(facility_once$nearest_rmp_distance_m), 2),
      round(safe_max(facility_once$nearest_external_rmp_distance_m), 2)
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(nrow(features) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(all(buffers_per_facility == length(buffer_m)), "PASS", "FAIL"),
      ifelse(invalid_external_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_tri_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_lqg_counts == 0, "PASS", "FAIL"),
      ifelse(invalid_tsd_counts == 0, "PASS", "FAIL"),
      ifelse(monotonic_count_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_external_violations == 0, "PASS", "FAIL"),
      rep("INFO", 14)
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("TRI-RMP proximity QA/QC completed.\n")

  # Write RMP proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_rmp_proximity_features_",
      year,
      "_rmp_",
      access_date,
      ".csv"
    )
  )

  pair_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_rmp_pairs_",
      max_buffer,
      "m_",
      year,
      "_rmp_",
      access_date,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_rmp_proximity_features_",
      year,
      "_rmp_",
      access_date,
      ".csv"
    )
  )

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-RMP proximity outputs written.\n")

  c(feature_file, pair_file, qa_file)
}

upload_rmp_proximity_features <- function(files, processed_folder, validation_folder) {
  # Validate RMP proximity output files
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three RMP proximity output files.")
  }

  cat("RMP proximity output files validated.\n")

  # Upload RMP proximity feature tables
  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("RMP proximity feature tables uploaded.\n")

  # Upload RMP proximity QA report
  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("RMP proximity QA report uploaded.\n")

  TRUE
}