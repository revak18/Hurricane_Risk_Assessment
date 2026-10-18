build_tri_cdl_buffers <- function(tri_fips_files, cdl_file, year, target_county_fips, buffer_m, output_dir, validation_dir) {
  # Load Mecklenburg TRI facilities and the buffered CDL raster
  facilities <- read.csv(tri_fips_files[[3]], stringsAsFactors = FALSE)
  cdl <- terra::rast(cdl_file)
  required <- c("tri_facility_id", "facility_name", "latitude", "longitude", "county_geoid")
  missing <- setdiff(required, names(facilities))

  if (length(missing)) stop(paste("Missing TRI fields:", paste(missing, collapse = ", ")))
  if (!all(as.character(facilities$county_geoid) == target_county_fips)) stop("TRI facility table contains records outside Mecklenburg County.")

  cat("Mecklenburg TRI and CDL data loaded.\n")

  # Define broad CDL land-cover groups
  classify_cdl <- function(code) {
    group <- rep("other_or_unknown", length(code))
    group[code %in% c(1:61, 66:77, 200:254)] <- "cropland"
    group[code %in% c(62, 176)] <- "pasture_hay"
    group[code %in% c(63, 141:143)] <- "forest"
    group[code %in% c(64, 152, 171)] <- "shrub_grass"
    group[code %in% c(65, 131)] <- "barren"
    group[code %in% c(82, 121:124)] <- "developed"
    group[code %in% c(83, 111, 112)] <- "water"
    group[code %in% c(87, 190, 195)] <- "wetlands"
    group[code %in% 92] <- "aquaculture"
    group
  }

  broad_groups <- c("cropland", "pasture_hay", "forest", "shrub_grass", "barren", "developed", "water", "wetlands", "aquaculture", "other_or_unknown")
  cat("CDL land-cover groups defined.\n")

  # Convert TRI facilities to the CDL coordinate system
  points <- terra::vect(facilities, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  points <- terra::project(points, terra::crs(cdl))
  cell_area_m2 <- abs(prod(terra::res(cdl)))

  cat("TRI facility points projected to CDL coordinates.\n")

  # Extract CDL composition for each facility buffer
  feature_rows <- list()
  class_rows <- list()
  feature_index <- 1L
  class_index <- 1L

  for (radius in buffer_m) {
    buffers <- terra::buffer(points, width = radius)
    extracted <- terra::extract(cdl, buffers, exact = TRUE)
    ignored <- c("ID", "fraction", "weight", "cell", "x", "y")
    value_columns <- setdiff(names(extracted), ignored)

    if (length(value_columns) != 1) stop("Expected exactly one CDL raster value column.")

    value_column <- value_columns[[1]]
    weight_column <- intersect(c("fraction", "weight"), names(extracted))

    if (length(weight_column)) {
      extracted$pixel_weight <- extracted[[weight_column[[1]]]]
    } else {
      extracted$pixel_weight <- 1
    }

    extracted$cdl_code <- suppressWarnings(as.integer(extracted[[value_column]]))
    extracted$broad_class <- classify_cdl(extracted$cdl_code)

    for (i in seq_len(nrow(facilities))) {
      rows <- extracted[extracted$ID == i, ]
      valid <- rows[!is.na(rows$cdl_code), ]
      valid_weight <- sum(valid$pixel_weight, na.rm = TRUE)
      valid_area_m2 <- valid_weight * cell_area_m2
      buffer_area_m2 <- pi * radius^2
      coverage_percent <- min(100, 100 * valid_area_m2 / buffer_area_m2)
      shares <- setNames(rep(0, length(broad_groups)), broad_groups)
      dominant_code <- NA_integer_
      dominant_class <- NA_character_

      if (nrow(valid) && valid_weight > 0) {
        code_summary <- stats::aggregate(pixel_weight ~ cdl_code + broad_class, data = valid, FUN = sum)
        code_summary$share <- code_summary$pixel_weight / valid_weight

        group_summary <- stats::aggregate(pixel_weight ~ broad_class, data = valid, FUN = sum)
        group_summary$share <- group_summary$pixel_weight / valid_weight
        shares[group_summary$broad_class] <- group_summary$share

        dominant_code <- code_summary$cdl_code[which.max(code_summary$pixel_weight)]
        dominant_class <- group_summary$broad_class[which.max(group_summary$pixel_weight)]

        code_summary$tri_facility_id <- facilities$tri_facility_id[[i]]
        code_summary$facility_name <- facilities$facility_name[[i]]
        code_summary$buffer_m <- radius

        class_rows[[class_index]] <- code_summary[, c("tri_facility_id", "facility_name", "buffer_m", "cdl_code", "broad_class", "pixel_weight", "share")]
        class_index <- class_index + 1L
      }

      feature_rows[[feature_index]] <- data.frame(
        tri_facility_id = facilities$tri_facility_id[[i]],
        facility_name = facilities$facility_name[[i]],
        county_geoid = facilities$county_geoid[[i]],
        latitude = facilities$latitude[[i]],
        longitude = facilities$longitude[[i]],
        buffer_m = radius,
        cdl_coverage_percent = round(coverage_percent, 4),
        dominant_cdl_code = dominant_code,
        dominant_broad_class = dominant_class,
        cropland_share = shares[["cropland"]],
        pasture_hay_share = shares[["pasture_hay"]],
        agriculture_share = shares[["cropland"]] + shares[["pasture_hay"]],
        developed_share = shares[["developed"]],
        forest_share = shares[["forest"]],
        shrub_grass_share = shares[["shrub_grass"]],
        wetlands_share = shares[["wetlands"]],
        water_share = shares[["water"]],
        barren_share = shares[["barren"]],
        aquaculture_share = shares[["aquaculture"]],
        other_unknown_share = shares[["other_or_unknown"]],
        stringsAsFactors = FALSE
      )

      feature_index <- feature_index + 1L
    }
  }

  features <- do.call(rbind, feature_rows)
  classes <- do.call(rbind, class_rows)

  rownames(features) <- NULL
  rownames(classes) <- NULL

  cat("TRI CDL buffer compositions calculated.\n")

  # Validate the facility-buffer feature table
  expected_rows <- nrow(facilities) * length(buffer_m)
  duplicate_rows <- sum(duplicated(features[, c("tri_facility_id", "buffer_m")]))
  buffers_per_facility <- table(features$tri_facility_id)

  share_columns <- c(
    "cropland_share",
    "pasture_hay_share",
    "developed_share",
    "forest_share",
    "shrub_grass_share",
    "wetlands_share",
    "water_share",
    "barren_share",
    "aquaculture_share",
    "other_unknown_share"
  )

  features$broad_share_sum <- rowSums(features[, share_columns], na.rm = TRUE)
  invalid_shares <- sum(features[, share_columns] < 0 | features[, share_columns] > 1, na.rm = TRUE)
  low_coverage <- sum(features$cdl_coverage_percent < 99)

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Buffer distances",
      "Expected facility-buffer rows",
      "Actual facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Facilities with all buffer distances",
      "Minimum CDL coverage percent",
      "Feature rows below 99 percent CDL coverage",
      "Invalid land-cover shares",
      "Maximum broad-share sum deviation"
    ),
    value = c(
      nrow(facilities),
      length(buffer_m),
      expected_rows,
      nrow(features),
      duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)),
      round(min(features$cdl_coverage_percent), 4),
      low_coverage,
      invalid_shares,
      round(max(abs(features$broad_share_sum - 1)), 8)
    ),
    status = c(
      "INFO",
      ifelse(identical(sort(unique(features$buffer_m)), sort(buffer_m)), "PASS", "FAIL"),
      "INFO",
      ifelse(nrow(features) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(all(buffers_per_facility == length(buffer_m)), "PASS", "FAIL"),
      ifelse(min(features$cdl_coverage_percent) >= 99, "PASS", "CHECK"),
      ifelse(low_coverage == 0, "PASS", "CHECK"),
      ifelse(invalid_shares == 0, "PASS", "FAIL"),
      ifelse(max(abs(features$broad_share_sum - 1)) < 0.001, "PASS", "CHECK")
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("TRI CDL buffer QA/QC completed.\n")

  # Write TRI CDL exposure feature outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  feature_file <- file.path(output_dir, paste0("mecklenburg_tri_cdl_buffer_features_", year, ".csv"))
  class_file <- file.path(output_dir, paste0("mecklenburg_tri_cdl_buffer_classes_", year, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_tri_cdl_buffers_", year, ".csv"))

  write.csv(features, feature_file, row.names = FALSE)
  write.csv(classes, class_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("TRI CDL exposure feature outputs written.\n")
  c(feature_file, class_file, qa_file)
}

upload_tri_cdl_buffers <- function(files, processed_folder, validation_folder) {
  # Validate TRI CDL buffer outputs
  if (length(files) != 3 || !all(file.exists(files))) stop("Expected three TRI CDL buffer output files.")
  cat("TRI CDL buffer output files validated.\n")

  # Upload TRI CDL exposure feature tables
  invisible(lapply(files[1:2], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))
  cat("TRI CDL exposure feature tables uploaded.\n")

  # Upload TRI CDL buffer QA report
  googledrive::drive_upload(files[[3]], path = validation_folder, overwrite = TRUE)
  cat("TRI CDL buffer QA report uploaded.\n")
  TRUE
}