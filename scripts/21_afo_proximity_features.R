build_afo_proximity_features <- function(tri_fips_files, afo_files, year, target_county_fips, buffer_m, target_crs, output_dir, validation_dir) {
  # Load Mecklenburg TRI facilities and statewide NC AFO records
  tri_file <- tri_fips_files[grepl("mecklenburg_tri_facility", basename(tri_fips_files), ignore.case = TRUE)][1]
  afo_file <- afo_files[grepl("^nc_afo_permits_access_", basename(afo_files), ignore.case = TRUE) & grepl("\\.csv$", basename(afo_files), ignore.case = TRUE)][1]

  if (is.na(tri_file) || is.na(afo_file)) stop("Could not locate Mecklenburg TRI or statewide AFO input file.")

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE)
  afo <- read.csv(afo_file, stringsAsFactors = FALSE)

  if (!nrow(facilities) || !nrow(afo)) stop("TRI facility or AFO input table is empty.")

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(afo_file))

  cat("Mecklenburg TRI and statewide AFO data loaded.\n")

  # Validate TRI and AFO fields
  required_tri <- c("tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid")
  required_afo <- c("afo_record_id", "facility_name", "animal_group", "permit_program", "npdes_flag", "digester_flag", "reported_county", "county_geoid", "census_county", "latitude", "longitude")
  missing_tri <- setdiff(required_tri, names(facilities))
  missing_afo <- setdiff(required_afo, names(afo))

  if (length(missing_tri)) stop(paste("Missing TRI fields:", paste(missing_tri, collapse = ", ")))
  if (length(missing_afo)) stop(paste("Missing AFO fields:", paste(missing_afo, collapse = ", ")))

  facilities <- facilities[as.character(facilities$county_geoid) == target_county_fips, ]

  if (!nrow(facilities)) stop("No Mecklenburg TRI facilities remain after FIPS filtering.")

  cat("TRI and AFO input fields validated.\n")

  # Audit reported and spatial Mecklenburg AFO geography
  normalize_county <- function(x) toupper(gsub("[^A-Z0-9]", "", ifelse(is.na(x), "", x)))
  reported_norm <- normalize_county(afo$reported_county)
  spatial_norm <- normalize_county(afo$census_county)
  reported_mecklenburg <- reported_norm == "MECKLENBURG"
  spatial_mecklenburg <- as.character(afo$county_geoid) == target_county_fips
  county_comparable <- reported_norm != ""
  county_mismatch <- county_comparable & reported_norm != spatial_norm
  discrepancies <- afo[county_mismatch, , drop = FALSE]

  cat("AFO Mecklenburg geography audit completed.\n")

  # Standardize AFO categories and logical fields
  valid_animal_groups <- c("swine", "cattle", "poultry", "horse", "sheep_goat", "mixed_animals", "other_or_unknown")
  afo$animal_group[is.na(afo$animal_group) | trimws(afo$animal_group) == ""] <- "other_or_unknown"
  afo$animal_group[!afo$animal_group %in% valid_animal_groups] <- "other_or_unknown"

  to_logical <- function(x) {
    if (is.logical(x)) return(x)
    toupper(trimws(as.character(x))) %in% c("TRUE", "T", "YES", "Y", "1")
  }

  afo$npdes_flag <- to_logical(afo$npdes_flag)
  afo$digester_flag <- to_logical(afo$digester_flag)

  if ("state_permit_flag" %in% names(afo)) afo$state_permit_flag <- to_logical(afo$state_permit_flag)
  if ("individual_permit_flag" %in% names(afo)) afo$individual_permit_flag <- to_logical(afo$individual_permit_flag)

  cat("AFO categories and permit flags standardized.\n")

  # Project TRI and statewide AFO points for metric distances
  tri_sf <- sf::st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  afo_sf <- sf::st_as_sf(afo, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  tri_sf <- sf::st_transform(tri_sf, target_crs)
  afo_sf <- sf::st_transform(afo_sf, target_crs)

  cat("TRI and statewide AFO sites projected.\n")

  # Calculate all TRI-to-AFO distances
  distance_matrix <- sf::st_distance(tri_sf, afo_sf)
  distance_matrix <- matrix(as.numeric(distance_matrix), nrow = nrow(tri_sf), ncol = nrow(afo_sf))

  if (any(!is.finite(distance_matrix))) stop("Invalid TRI-to-AFO distances detected.")

  cat("TRI-to-AFO distance matrix calculated.\n")

  # Build facility-buffer AFO proximity features
  feature_rows <- list()
  feature_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    distances <- distance_matrix[i, ]
    nearest_index <- which.min(distances)
    nearest <- afo[nearest_index, , drop = FALSE]

    for (radius in sort(buffer_m)) {
      inside <- which(distances <= radius)
      counts <- setNames(rep(0L, length(valid_animal_groups)), valid_animal_groups)

      if (length(inside)) {
        animal_table <- table(afo$animal_group[inside])
        counts[names(animal_table)] <- as.integer(animal_table)
      }

      npdes_count <- if (length(inside)) sum(afo$npdes_flag[inside], na.rm = TRUE) else 0L
      digester_count <- if (length(inside)) sum(afo$digester_flag[inside], na.rm = TRUE) else 0L

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        buffer_m = radius,
        afo_site_count = length(inside),
        afo_any_within_buffer = length(inside) > 0,
        afo_swine_count = counts[["swine"]],
        afo_cattle_count = counts[["cattle"]],
        afo_poultry_count = counts[["poultry"]],
        afo_horse_count = counts[["horse"]],
        afo_sheep_goat_count = counts[["sheep_goat"]],
        afo_mixed_animals_count = counts[["mixed_animals"]],
        afo_other_unknown_count = counts[["other_or_unknown"]],
        afo_npdes_count = npdes_count,
        afo_digester_count = digester_count,
        nearest_afo_distance_m = distances[[nearest_index]],
        nearest_afo_record_id = nearest$afo_record_id[[1]],
        nearest_afo_facility_name = nearest$facility_name[[1]],
        nearest_afo_animal_group = nearest$animal_group[[1]],
        nearest_afo_permit_program = nearest$permit_program[[1]],
        nearest_afo_npdes = nearest$npdes_flag[[1]],
        nearest_afo_digester = nearest$digester_flag[[1]],
        afo_snapshot_access_date = access_date,
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  rownames(features) <- NULL

  cat("TRI-AFO proximity features calculated.\n")

  # Build detailed TRI-AFO pairs within the largest buffer
  max_buffer <- max(buffer_m)
  pair_rows <- list()
  pair_index <- 1L

  for (i in seq_len(nrow(facilities))) {
    inside <- which(distance_matrix[i, ] <= max_buffer)

    if (!length(inside)) next

    for (j in inside) {
      pair_rows[[pair_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        afo_record_id = afo$afo_record_id[[j]],
        afo_facility_name = afo$facility_name[[j]],
        afo_animal_group = afo$animal_group[[j]],
        afo_permit_program = afo$permit_program[[j]],
        afo_npdes = afo$npdes_flag[[j]],
        afo_digester = afo$digester_flag[[j]],
        afo_county_geoid = afo$county_geoid[[j]],
        afo_census_county = afo$census_county[[j]],
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
      facility_name = character(),
      afo_record_id = character(),
      afo_facility_name = character(),
      afo_animal_group = character(),
      afo_permit_program = character(),
      afo_npdes = logical(),
      afo_digester = logical(),
      afo_county_geoid = character(),
      afo_census_county = character(),
      distance_m = numeric(),
      within_300m = logical(),
      within_500m = logical(),
      within_1000m = logical(),
      stringsAsFactors = FALSE
    )
  }

  rownames(pairs) <- NULL

  cat("Detailed TRI-AFO proximity pairs built.\n")

  # Validate AFO proximity features
  expected_rows <- nrow(facilities) * length(buffer_m)
  duplicate_rows <- sum(duplicated(features[, c("tri_facility_id", "buffer_m")]))
  buffers_per_facility <- table(features$tri_facility_id)

  animal_count_columns <- c(
    "afo_swine_count",
    "afo_cattle_count",
    "afo_poultry_count",
    "afo_horse_count",
    "afo_sheep_goat_count",
    "afo_mixed_animals_count",
    "afo_other_unknown_count"
  )

  animal_count_sum <- rowSums(features[, animal_count_columns, drop = FALSE])
  animal_count_mismatches <- sum(animal_count_sum != features$afo_site_count)
  monotonic_violations <- 0L

  for (id in unique(features$tri_facility_id)) {
    rows <- features[features$tri_facility_id == id, ]
    rows <- rows[order(rows$buffer_m), ]

    if (any(diff(rows$afo_site_count) < 0)) {
      monotonic_violations <- monotonic_violations + 1L
    }
  }

  count_at_buffer <- function(radius) {
    rows <- features[features$buffer_m == radius, ]
    sum(rows$afo_site_count > 0, na.rm = TRUE)
  }

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Statewide NC AFO records",
      "Reported Mecklenburg AFO records",
      "Spatial Mecklenburg AFO records",
      "Reported versus spatial county mismatches",
      "Mismatch records involving Mecklenburg",
      "Buffer distances",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
      "AFO animal-count mismatches",
      "Facilities with nonmonotonic AFO counts",
      "TRI facilities with AFO within 300m",
      "TRI facilities with AFO within 500m",
      "TRI facilities with AFO within 1000m",
      "Unique AFO sites within 1000m of any TRI facility",
      "Minimum TRI-AFO distance meters",
      "Maximum nearest-AFO distance meters"
    ),
    value = c(
      nrow(facilities),
      nrow(afo),
      sum(reported_mecklenburg, na.rm = TRUE),
      sum(spatial_mecklenburg, na.rm = TRUE),
      sum(county_mismatch, na.rm = TRUE),
      sum(county_mismatch & (reported_mecklenburg | spatial_mecklenburg), na.rm = TRUE),
      length(buffer_m),
      expected_rows,
      nrow(features),
      duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
      animal_count_mismatches,
      monotonic_violations,
      count_at_buffer(300),
      count_at_buffer(500),
      count_at_buffer(1000),
      length(unique(pairs$afo_record_id)),
      round(min(features$nearest_afo_distance_m, na.rm = TRUE), 2),
      round(max(features$nearest_afo_distance_m, na.rm = TRUE), 2)
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(sum(county_mismatch) == 0, "PASS", "CHECK"),
      ifelse(sum(county_mismatch & (reported_mecklenburg | spatial_mecklenburg)) == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      ifelse(nrow(features) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(all(buffers_per_facility == length(buffer_m)), "PASS", "FAIL"),
      ifelse(animal_count_mismatches == 0, "PASS", "FAIL"),
      ifelse(monotonic_violations == 0, "PASS", "FAIL"),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("TRI-AFO proximity QA/QC completed.\n")

  # Write AFO proximity outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(output_dir, paste0("mecklenburg_tri_afo_proximity_features_", year, "_afo_", access_date, ".csv"))
  pair_file <- file.path(output_dir, paste0("mecklenburg_tri_afo_pairs_", max_buffer, "m_", year, "_afo_", access_date, ".csv"))
  discrepancy_file <- file.path(validation_dir, paste0("afo_geography_discrepancies_", access_date, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_afo_proximity_features_", year, "_afo_", access_date, ".csv"))

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(pairs, pair_file, row.names = FALSE)
  write.csv(discrepancies, discrepancy_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI-AFO proximity outputs written.\n")
  c(feature_file, pair_file, discrepancy_file, qa_file)
}

upload_afo_proximity_features <- function(files, processed_folder, validation_folder) {
  # Validate AFO proximity output files
  if (length(files) != 4 || !all(file.exists(files))) {
    stop("Expected four AFO proximity output files.")
  }

  cat("AFO proximity output files validated.\n")

  # Upload AFO proximity feature tables
  invisible(lapply(files[1:2], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))

  cat("AFO proximity feature tables uploaded.\n")

  # Upload AFO geography and QA reports
  invisible(lapply(files[3:4], googledrive::drive_upload, path = validation_folder, overwrite = TRUE))

  cat("AFO geography and QA reports uploaded.\n")
  TRUE
}