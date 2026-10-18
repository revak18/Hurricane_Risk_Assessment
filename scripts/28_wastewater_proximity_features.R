build_wastewater_proximity_features <- function(
  tri_fips_files, wastewater_files, year, target_county_fips, buffer_m, target_crs,
  output_dir, validation_dir
) {
  # Load Mecklenburg TRI facilities and statewide wastewater sites
  tri_file <- tri_fips_files[grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)][1]

  wastewater_file <- wastewater_files[
    grepl("^nc_npdes_wastewater_permits_access_", basename(wastewater_files), ignore.case = TRUE) &
      grepl("\\.gpkg$", basename(wastewater_files), ignore.case = TRUE)
  ][1]

  if (is.na(tri_file) || is.na(wastewater_file)) {
    stop("Could not locate Mecklenburg TRI or statewide wastewater input.")
  }

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  wastewater <- sf::st_read(wastewater_file, quiet = TRUE)

  if (!nrow(facilities) || !nrow(wastewater)) {
    stop("TRI or wastewater input is empty.")
  }

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(wastewater_file))
  cat("Mecklenburg TRI and statewide wastewater data loaded.\n")

  # Validate TRI and wastewater fields
  required_tri <- c("tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid")
  required_wastewater <- c(
    "wastewater_site_id", "npdes_permit_number", "facility_name", "major",
    "asbuilt_flow_gpd", "asbuilt_flow_mgd", "intersects_mecklenburg"
  )

  missing_tri <- setdiff(required_tri, names(facilities))
  missing_wastewater <- setdiff(required_wastewater, names(wastewater))

  if (length(missing_tri)) {
    stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  }

  if (length(missing_wastewater)) {
    stop(paste("Missing wastewater fields:", paste(missing_wastewater, collapse = ", ")))
  }

  facilities <- facilities[as.character(facilities$county_geoid) == target_county_fips, ]

  if (!nrow(facilities)) {
    stop("No Mecklenburg TRI facilities remain after FIPS filtering.")
  }

  if (anyDuplicated(wastewater$wastewater_site_id)) {
    stop("Wastewater layer contains duplicate wastewater site IDs.")
  }

  cat("TRI and wastewater input fields validated.\n")

  # Standardize wastewater classifications and capacity
  wastewater$major <- as.logical(wastewater$major)

  if (any(is.na(wastewater$major))) {
    stop("Wastewater layer contains missing Major/Minor classifications.")
  }

  wastewater$asbuilt_flow_gpd <- suppressWarnings(as.numeric(wastewater$asbuilt_flow_gpd))
  wastewater$asbuilt_flow_mgd <- suppressWarnings(as.numeric(wastewater$asbuilt_flow_mgd))

  if (any(wastewater$asbuilt_flow_gpd < 0, na.rm = TRUE)) {
    stop("Wastewater layer contains negative as-built flow.")
  }

  cat("Wastewater classifications and capacity standardized.\n")

  # Project TRI facilities and wastewater sites
  tri_sf <- sf::st_as_sf(
    facilities,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  tri_sf <- sf::st_transform(tri_sf, target_crs)
  wastewater <- sf::st_transform(wastewater, target_crs)

  cat("TRI facilities and wastewater sites projected.\n")

  # Calculate TRI-to-wastewater distances
  distance_matrix <- sf::st_distance(tri_sf, wastewater)

  distance_matrix <- matrix(
    as.numeric(distance_matrix),
    nrow = nrow(tri_sf),
    ncol = nrow(wastewater)
  )

  if (any(!is.finite(distance_matrix))) {
    stop("Invalid TRI-to-wastewater distances detected.")
  }

  cat("TRI-to-wastewater distances calculated.\n")

  # Build wastewater proximity and capacity features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- distance_matrix[i, ]
    nearest_index <- which.min(distances)
    nearest_major_index <- which(wastewater$major)

    nearest_major_distance <- if (length(nearest_major_index)) {
      min(distances[nearest_major_index])
    } else {
      NA_real_
    }

    nearest <- wastewater[nearest_index, , drop = FALSE]

    for (radius in sort(buffer_m)) {
      nearby <- which(distances <= radius)
      nearby_major <- nearby[wastewater$major[nearby]]
      nearby_minor <- nearby[!wastewater$major[nearby]]

      flow_available <- nearby[!is.na(wastewater$asbuilt_flow_mgd[nearby])]
      major_flow_available <- nearby_major[!is.na(wastewater$asbuilt_flow_mgd[nearby_major])]

      total_flow <- if (length(flow_available)) {
        sum(wastewater$asbuilt_flow_mgd[flow_available], na.rm = TRUE)
      } else {
        0
      }

      major_flow <- if (length(major_flow_available)) {
        sum(wastewater$asbuilt_flow_mgd[major_flow_available], na.rm = TRUE)
      } else {
        0
      }

      maximum_flow <- if (length(flow_available)) {
        max(wastewater$asbuilt_flow_mgd[flow_available], na.rm = TRUE)
      } else {
        0
      }

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        wastewater_site_count = length(nearby),
        wastewater_any_within_buffer = length(nearby) > 0,
        wastewater_major_count = length(nearby_major),
        wastewater_minor_count = length(nearby_minor),
        wastewater_flow_available_count = length(flow_available),
        wastewater_total_asbuilt_flow_mgd = total_flow,
        wastewater_major_asbuilt_flow_mgd = major_flow,
        wastewater_max_asbuilt_flow_mgd = maximum_flow,
        nearest_wastewater_distance_m = distances[[nearest_index]],
        nearest_wastewater_site_id = nearest$wastewater_site_id[[1]],
        nearest_wastewater_permit_number = nearest$npdes_permit_number[[1]],
        nearest_wastewater_name = nearest$facility_name[[1]],
        nearest_wastewater_major = nearest$major[[1]],
        nearest_wastewater_asbuilt_flow_mgd = nearest$asbuilt_flow_mgd[[1]],
        nearest_major_wastewater_distance_m = nearest_major_distance,
        wastewater_snapshot_access_date = access_date,
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL

  cat("TRI-wastewater proximity and capacity features calculated.\n")

  # Build detailed TRI-wastewater pairs within the largest buffer
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
        wastewater_site_id = wastewater$wastewater_site_id[[j]],
        npdes_permit_number = wastewater$npdes_permit_number[[j]],
        wastewater_facility_name = wastewater$facility_name[[j]],
        wastewater_major = wastewater$major[[j]],
        wastewater_asbuilt_flow_mgd = wastewater$asbuilt_flow_mgd[[j]],
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
      wastewater_site_id = character(),
      npdes_permit_number = character(),
      wastewater_facility_name = character(),
      wastewater_major = logical(),
      wastewater_asbuilt_flow_mgd = numeric(),
      distance_m = numeric(),
      within_300m = logical(),
      within_500m = logical(),
      within_1000m = logical(),
      stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL
  cat("Detailed TRI-wastewater proximity pairs built.\n")

  # Validate wastewater proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)

  duplicate_rows <- sum(
    duplicated(features[, c("tri_facility_id", "buffer_m")])
  )

  buffers_per_facility <- table(features$tri_facility_id)

  class_mismatches <- sum(
    features$wastewater_major_count +
      features$wastewater_minor_count !=
      features$wastewater_site_count
  )

  invalid_flow_counts <- sum(
    features$wastewater_flow_available_count >
      features$wastewater_site_count
  )

  negative_flow_features <- sum(
    features$wastewater_total_asbuilt_flow_mgd < 0 |
      features$wastewater_major_asbuilt_flow_mgd < 0 |
      features$wastewater_max_asbuilt_flow_mgd < 0
  )

  monotonic_count_violations <- 0L
  monotonic_flow_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$wastewater_site_count) < 0)) {
      monotonic_count_violations <- monotonic_count_violations + 1L
    }

    if (any(diff(rows$wastewater_total_asbuilt_flow_mgd) < -1e-10)) {
      monotonic_flow_violations <- monotonic_flow_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$wastewater_site_count > 0, na.rm = TRUE)
  }

  major_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$wastewater_major_count > 0, na.rm = TRUE)
  }

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Statewide NC wastewater sites",
      "Wastewater sites intersecting Mecklenburg",
      "Statewide major wastewater sites",
      "Buffer distances",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
      "Wastewater Major-Minor count mismatches",
      "Invalid wastewater flow-availability counts",
      "Negative wastewater capacity features",
      "Facilities with nonmonotonic wastewater counts",
      "Facilities with nonmonotonic wastewater capacity",
      "TRI facilities with wastewater within 300m",
      "TRI facilities with wastewater within 500m",
      "TRI facilities with wastewater within 1000m",
      "TRI facilities with major wastewater within 300m",
      "TRI facilities with major wastewater within 500m",
      "TRI facilities with major wastewater within 1000m",
      "Unique wastewater sites within 1000m of any TRI",
      "Unique major wastewater sites within 1000m of any TRI",
      "Minimum TRI-wastewater distance meters",
      "Minimum TRI-major-wastewater distance meters",
      "Maximum nearest-wastewater distance meters"
    ),
    value = c(
      nrow(facilities),
      nrow(wastewater),
      sum(as.logical(wastewater$intersects_mecklenburg), na.rm = TRUE),
      sum(wastewater$major, na.rm = TRUE),
      length(buffer_m),
      expected_rows,
      nrow(features),
      duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
      class_mismatches,
      invalid_flow_counts,
      negative_flow_features,
      monotonic_count_violations,
      monotonic_flow_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      major_at_buffer(300),
      major_at_buffer(500),
      major_at_buffer(1000),
      length(unique(pairs$wastewater_site_id)),
      length(unique(pairs$wastewater_site_id[pairs$wastewater_major])),
      round(min(features$nearest_wastewater_distance_m, na.rm = TRUE), 2),
      round(min(features$nearest_major_wastewater_distance_m, na.rm = TRUE), 2),
      round(max(features$nearest_wastewater_distance_m, na.rm = TRUE), 2)
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
      ifelse(class_mismatches == 0, "PASS", "FAIL"),
      ifelse(invalid_flow_counts == 0, "PASS", "FAIL"),
      ifelse(negative_flow_features == 0, "PASS", "FAIL"),
      ifelse(monotonic_count_violations == 0, "PASS", "FAIL"),
      ifelse(monotonic_flow_violations == 0, "PASS", "FAIL"),
      rep("INFO", 11)
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("TRI-wastewater proximity QA/QC completed.\n")

  # Write wastewater proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_wastewater_proximity_features_",
      year,
      "_wastewater_",
      access_date,
      ".csv"
    )
  )

  pair_file <- file.path(
    output_dir,
    paste0(
      "mecklenburg_tri_wastewater_pairs_",
      max_buffer,
      "m_",
      year,
      "_wastewater_",
      access_date,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_wastewater_proximity_features_",
      year,
      "_wastewater_",
      access_date,
      ".csv"
    )
  )

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-wastewater proximity outputs written.\n")

  c(feature_file, pair_file, qa_file)
}

upload_wastewater_proximity_features <- function(
  files, processed_folder, validation_folder
) {
  # Validate wastewater proximity output files
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three wastewater proximity output files.")
  }

  cat("Wastewater proximity output files validated.\n")

  # Upload wastewater proximity feature tables
  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("Wastewater proximity feature tables uploaded.\n")

  # Upload wastewater proximity QA report
  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("Wastewater proximity QA report uploaded.\n")

  TRUE
}