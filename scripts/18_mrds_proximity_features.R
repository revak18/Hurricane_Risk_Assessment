build_mrds_proximity_features <- function(
  tri_fips_files, mrds_classified_files, year, target_county_fips, buffer_m,
  target_crs, output_dir, validation_dir
) {
  # Load Mecklenburg TRI facilities and classified MRDS sites
  tri_file <- tri_fips_files[
    grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)
  ][1]
  mrds_file <- mrds_classified_files[
    grepl("mecklenburg", basename(mrds_classified_files), ignore.case = TRUE)
  ][1]

  if (is.na(tri_file) || is.na(mrds_file)) {
    stop("Could not locate Mecklenburg TRI or MRDS input file.")
  }

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  mrds <- read.csv(mrds_file, stringsAsFactors = FALSE)

  if (!nrow(facilities) || !nrow(mrds)) {
    stop("TRI facility or MRDS input table is empty.")
  }

  cat("Mecklenburg TRI and MRDS data loaded.\n")

  # Validate required TRI and MRDS fields
  required_tri <- c(
    "tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid"
  )
  required_mrds <- c(
    "dep_id", "site_name", "latitude", "longitude", "county_geoid", "resource_category"
  )

  missing_tri <- setdiff(required_tri, names(facilities))
  missing_mrds <- setdiff(required_mrds, names(mrds))

  if (length(missing_tri)) {
    stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  }

  if (length(missing_mrds)) {
    stop(paste("Missing MRDS fields:", paste(missing_mrds, collapse = ", ")))
  }

  facilities <- facilities[as.character(facilities$county_geoid) == target_county_fips, ]
  mrds <- mrds[as.character(mrds$county_geoid) == target_county_fips, ]

  if (!nrow(facilities) || !nrow(mrds)) {
    stop("No Mecklenburg TRI facilities or MRDS sites remain after FIPS filtering.")
  }

  cat("TRI and MRDS Mecklenburg FIPS validated.\n")

  # Project TRI and MRDS coordinates for metric distance calculations
  tri_sf <- sf::st_as_sf(
    facilities, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  )
  mrds_sf <- sf::st_as_sf(
    mrds, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  )

  tri_sf <- sf::st_transform(tri_sf, target_crs)
  mrds_sf <- sf::st_transform(mrds_sf, target_crs)

  cat("TRI and MRDS sites projected to metric coordinates.\n")

  # Calculate all TRI-to-MRDS distances
  distance_matrix <- sf::st_distance(tri_sf, mrds_sf)
  distance_matrix <- matrix(
    as.numeric(distance_matrix), nrow = nrow(tri_sf), ncol = nrow(mrds_sf)
  )

  if (any(!is.finite(distance_matrix))) {
    stop("Invalid TRI-to-MRDS distances detected.")
  }

  cat("TRI-to-MRDS distance matrix calculated.\n")

  # Define MRDS resource categories
  resource_categories <- c(
    "precious_metal", "base_metal", "ferrous_metal", "construction_aggregate",
    "industrial_mineral", "mixed_metal", "mixed_resource", "other_or_unknown"
  )

  mrds$resource_category[
    is.na(mrds$resource_category) | trimws(mrds$resource_category) == ""
  ] <- "other_or_unknown"

  unknown_categories <- setdiff(unique(mrds$resource_category), resource_categories)

  if (length(unknown_categories)) {
    mrds$resource_category[
      mrds$resource_category %in% unknown_categories
    ] <- "other_or_unknown"
  }

  cat("MRDS resource categories standardized.\n")

  # Build facility-buffer MRDS proximity features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- distance_matrix[i, ]
    nearest_index <- which.min(distances)
    nearest_distance <- distances[nearest_index]
    nearest_site <- mrds[nearest_index, ]

    for (radius in sort(buffer_m)) {
      inside <- which(distances <= radius)
      counts <- setNames(rep(0L, length(resource_categories)), resource_categories)

      if (length(inside)) {
        category_table <- table(mrds$resource_category[inside])
        counts[names(category_table)] <- as.integer(category_table)
      }

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        mrds_site_count = length(inside),
        mrds_any_within_buffer = length(inside) > 0,
        mrds_precious_metal_count = counts[["precious_metal"]],
        mrds_base_metal_count = counts[["base_metal"]],
        mrds_ferrous_metal_count = counts[["ferrous_metal"]],
        mrds_construction_aggregate_count = counts[["construction_aggregate"]],
        mrds_industrial_mineral_count = counts[["industrial_mineral"]],
        mrds_mixed_metal_count = counts[["mixed_metal"]],
        mrds_mixed_resource_count = counts[["mixed_resource"]],
        mrds_other_unknown_count = counts[["other_or_unknown"]],
        nearest_mrds_distance_m = nearest_distance,
        nearest_mrds_dep_id = nearest_site$dep_id[[1]],
        nearest_mrds_site_name = nearest_site$site_name[[1]],
        nearest_mrds_resource_category = nearest_site$resource_category[[1]],
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL

  cat("TRI-MRDS proximity features calculated.\n")

  # Build detailed TRI-MRDS pairs within the largest analysis buffer
  max_buffer <- max(buffer_m)
  pair_rows <- list()
  pair_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    inside <- which(distance_matrix[i, ] <= max_buffer)

    if (!length(inside)) {
      next
    }

    for (j in inside) {
      pair_rows[[pair_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        mrds_dep_id = mrds$dep_id[[j]],
        mrds_site_name = mrds$site_name[[j]],
        mrds_resource_category = mrds$resource_category[[j]],
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
      tri_facility_id = character(), facility_name = character(),
      mrds_dep_id = character(), mrds_site_name = character(),
      mrds_resource_category = character(), distance_m = numeric(),
      within_300m = logical(), within_500m = logical(),
      within_1000m = logical(), stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL

  cat("Detailed TRI-MRDS proximity pairs built.\n")

  # Validate MRDS proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)
  duplicate_rows <- sum(duplicated(features[, c("tri_facility_id", "buffer_m")]))
  buffers_per_facility <- table(features$tri_facility_id)

  count_columns <- c(
    "mrds_precious_metal_count", "mrds_base_metal_count",
    "mrds_ferrous_metal_count", "mrds_construction_aggregate_count",
    "mrds_industrial_mineral_count", "mrds_mixed_metal_count",
    "mrds_mixed_resource_count", "mrds_other_unknown_count"
  )

  category_sum <- rowSums(features[, count_columns, drop = FALSE])
  category_mismatches <- sum(category_sum != features$mrds_site_count)
  monotonic_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$mrds_site_count) < 0)) {
      monotonic_violations <- monotonic_violations + 1L
    }
  }

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Mecklenburg MRDS sites",
      "Buffer distances",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
      "MRDS category-count mismatches",
      "Facilities with nonmonotonic buffer counts",
      "TRI facilities with MRDS within 300m",
      "TRI facilities with MRDS within 500m",
      "TRI facilities with MRDS within 1000m",
      "Unique MRDS sites within 1000m of any TRI facility",
      "Minimum TRI-MRDS distance meters",
      "Maximum nearest-MRDS distance meters"
    ),
    value = c(
      nrow(facilities), nrow(mrds), length(buffer_m), expected_rows,
      nrow(features), duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
      category_mismatches, monotonic_violations,
      length(unique(
        features$tri_facility_id[
          features$buffer_m == 300 & features$mrds_site_count > 0
        ]
      )),
      length(unique(
        features$tri_facility_id[
          features$buffer_m == 500 & features$mrds_site_count > 0
        ]
      )),
      length(unique(
        features$tri_facility_id[
          features$buffer_m == 1000 & features$mrds_site_count > 0
        ]
      )),
      length(unique(pairs$mrds_dep_id)),
      round(min(features$nearest_mrds_distance_m), 2),
      round(max(features$nearest_mrds_distance_m), 2)
    ),
    status = c(
      "INFO", "INFO", "INFO", "INFO",
      ifelse(nrow(features) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(all(buffers_per_facility == length(buffer_m)), "PASS", "FAIL"),
      ifelse(category_mismatches == 0, "PASS", "FAIL"),
      ifelse(monotonic_violations == 0, "PASS", "FAIL"),
      "INFO", "INFO", "INFO", "INFO", "INFO", "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("TRI-MRDS proximity QA/QC completed.\n")

  # Write MRDS proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(
    output_dir,
    paste0("mecklenburg_tri_mrds_proximity_features_", year, ".csv")
  )

  pair_file <- file.path(
    output_dir,
    paste0("mecklenburg_tri_mrds_pairs_", max_buffer, "m_", year, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_mrds_proximity_features_", year, ".csv")
  )

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-MRDS proximity outputs written.\n")

  c(feature_file, pair_file, qa_file)
}

upload_mrds_proximity_features <- function(
  files, processed_folder, validation_folder
) {
  # Validate MRDS proximity outputs
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three MRDS proximity output files.")
  }

  cat("MRDS proximity output files validated.\n")

  # Upload MRDS proximity feature tables
  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("MRDS proximity feature tables uploaded.\n")

  # Upload MRDS proximity QA report
  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("MRDS proximity QA report uploaded.\n")

  TRUE
}