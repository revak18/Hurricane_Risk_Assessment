run_qaqc <- function(tri_files, tri_fips_files, cdl_file, year, expected_county_fips, output_dir) {
  # Load statewide TRI, Mecklenburg TRI, and CDL data
  if (length(tri_files) < 2) stop("Statewide TRI files are missing.")
  if (length(tri_fips_files) < 4) stop("TRI FIPS files are missing.")

  facilities <- read.csv(tri_files[[1]], stringsAsFactors = FALSE)
  chemicals <- read.csv(tri_files[[2]], stringsAsFactors = FALSE)
  mecklenburg <- read.csv(tri_fips_files[[3]], stringsAsFactors = FALSE)
  cdl <- terra::rast(cdl_file)
  cat("Statewide TRI, Mecklenburg TRI, and CDL data loaded.\n")

  # Validate statewide TRI structure
  facility_required <- c("tri_facility_id", "latitude", "longitude")
  chemical_required <- c("tri_facility_id", "tri_chem_id")
  facility_missing <- setdiff(facility_required, names(facilities))
  chemical_missing <- setdiff(chemical_required, names(chemicals))

  if (length(facility_missing)) {
    stop(paste("Missing statewide facility fields:", paste(facility_missing, collapse = ", ")))
  }

  if (length(chemical_missing)) {
    stop(paste("Missing statewide chemical fields:", paste(chemical_missing, collapse = ", ")))
  }
  cat("Statewide TRI structure validated.\n")

  # Check statewide TRI facility identifiers
  facility_ids <- trimws(as.character(facilities$tri_facility_id))
  missing_ids <- sum(is.na(facility_ids) | facility_ids == "")
  valid_ids <- facility_ids[!is.na(facility_ids) & facility_ids != ""]
  duplicate_ids <- sum(duplicated(valid_ids))
  cat("Statewide TRI facility IDs checked.\n")

  # Check statewide TRI coordinates
  latitude <- suppressWarnings(as.numeric(facilities$latitude))
  longitude <- suppressWarnings(as.numeric(facilities$longitude))
  valid_coordinates <- !is.na(latitude) & !is.na(longitude) &
    latitude >= 24 & latitude <= 50 &
    longitude >= -125 & longitude <= -66

  invalid_coordinates <- sum(!valid_coordinates)
  cat("Statewide TRI coordinates checked.\n")

  # Check statewide TRI chemical records
  chemical_facility_ids <- trimws(as.character(chemicals$tri_facility_id))
  unmatched_chemical_records <- sum(
    is.na(chemical_facility_ids) |
      chemical_facility_ids == "" |
      !chemical_facility_ids %in% valid_ids
  )

  chemical_ids <- trimws(as.character(chemicals$tri_chem_id))
  chemical_ids <- chemical_ids[!is.na(chemical_ids) & chemical_ids != ""]
  unique_chemicals <- length(unique(chemical_ids))
  cat("Statewide TRI chemical records checked.\n")

  # Check statewide TRI release values
  release_columns <- intersect(
    c(
      "fugitive_air", "stack_air", "total_air", "water", "land",
      "onsite_release", "offsite_release", "transfer_total",
      "recycling_transfer", "treatment_transfer", "recovery_transfer",
      "production_waste"
    ),
    names(chemicals)
  )

  negative_release_values <- 0L
  for (column in release_columns) {
    values <- suppressWarnings(as.numeric(chemicals[[column]]))
    negative_release_values <- negative_release_values + sum(values < 0, na.rm = TRUE)
  }
  cat("Statewide TRI release values checked.\n")

  # Validate the FIPS-derived Mecklenburg subset
  mecklenburg_required <- c("tri_facility_id", "latitude", "longitude", "county_geoid")
  missing_mecklenburg <- setdiff(mecklenburg_required, names(mecklenburg))

  if (length(missing_mecklenburg)) {
    stop(paste("Missing Mecklenburg TRI fields:", paste(missing_mecklenburg, collapse = ", ")))
  }

  mecklenburg_geoid <- trimws(as.character(mecklenburg$county_geoid))
  expected_fips_count <- sum(mecklenburg_geoid == expected_county_fips, na.rm = TRUE)
  unexpected_fips_count <- sum(is.na(mecklenburg_geoid) | mecklenburg_geoid != expected_county_fips)
  cat("Mecklenburg TRI FIPS subset validated.\n")

  # Validate the Mecklenburg CDL raster
  cdl_cells <- terra::ncell(cdl)
  cdl_crs <- terra::crs(cdl)
  cdl_crs_present <- !is.na(cdl_crs) && nzchar(cdl_crs)
  missing_cells <- terra::global(is.na(cdl), "sum", na.rm = TRUE)[1, 1]
  cdl_missing_percent <- 100 * missing_cells / cdl_cells
  cat("Mecklenburg CDL raster validated.\n")

  # Check Mecklenburg TRI facilities against the CDL raster
  meck_lat <- suppressWarnings(as.numeric(mecklenburg$latitude))
  meck_lon <- suppressWarnings(as.numeric(mecklenburg$longitude))
  meck_valid <- !is.na(meck_lat) & !is.na(meck_lon) &
    meck_lat >= 24 & meck_lat <= 50 &
    meck_lon >= -125 & meck_lon <= -66

  valid_mecklenburg <- sum(meck_valid)
  inside_cdl <- 0L

  if (valid_mecklenburg > 0) {
    points <- terra::vect(
      mecklenburg[meck_valid, ],
      geom = c("longitude", "latitude"),
      crs = "EPSG:4326"
    )

    points <- terra::project(points, terra::crs(cdl))
    extracted <- terra::extract(cdl, points)
    raster_values <- extracted[, setdiff(names(extracted), "ID"), drop = FALSE]
    inside <- rowSums(!is.na(raster_values)) > 0
    inside_cdl <- sum(inside)
  }

  outside_cdl <- valid_mecklenburg - inside_cdl
  cat("Mecklenburg TRI facilities checked against CDL coverage.\n")

  # Build the combined QA/QC report
  report <- data.frame(
    check = c(
      "Statewide TRI facilities",
      "Statewide TRI chemical records",
      "Unique TRI chemicals",
      "Missing facility IDs",
      "Duplicate facility IDs",
      "Invalid statewide coordinates",
      "Chemical records without facility match",
      "Negative release values",
      "Mecklenburg FIPS facilities",
      "Facilities with expected Mecklenburg FIPS",
      "Unexpected Mecklenburg FIPS",
      "CDL cells",
      "CDL CRS present",
      "CDL missing cells percent",
      "Valid Mecklenburg facility coordinates",
      "Mecklenburg facilities inside CDL",
      "Mecklenburg facilities outside CDL"
    ),
    value = c(
      nrow(facilities),
      nrow(chemicals),
      unique_chemicals,
      missing_ids,
      duplicate_ids,
      invalid_coordinates,
      unmatched_chemical_records,
      negative_release_values,
      nrow(mecklenburg),
      expected_fips_count,
      unexpected_fips_count,
      cdl_cells,
      cdl_crs_present,
      round(cdl_missing_percent, 4),
      valid_mecklenburg,
      inside_cdl,
      outside_cdl
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      ifelse(missing_ids == 0, "PASS", "FAIL"),
      ifelse(duplicate_ids == 0, "PASS", "FAIL"),
      ifelse(invalid_coordinates == 0, "PASS", "CHECK"),
      ifelse(unmatched_chemical_records == 0, "PASS", "FAIL"),
      ifelse(negative_release_values == 0, "PASS", "FAIL"),
      "INFO",
      ifelse(nrow(mecklenburg) > 0 && expected_fips_count == nrow(mecklenburg), "PASS", "FAIL"),
      ifelse(unexpected_fips_count == 0, "PASS", "FAIL"),
      ifelse(cdl_cells > 0, "PASS", "FAIL"),
      ifelse(cdl_crs_present, "PASS", "FAIL"),
      "INFO",
      "INFO",
      ifelse(inside_cdl == valid_mecklenburg, "PASS", "CHECK"),
      ifelse(outside_cdl == 0, "PASS", "CHECK")
    ),
    stringsAsFactors = FALSE
  )

  print(report, row.names = FALSE)
  cat("HazardGraph statewide TRI and Mecklenburg CDL QA/QC complete.\n")

  # Write the combined QA/QC report
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0("qaqc_tri_cdl_", year, ".csv"))

  write.csv(report, output_file, row.names = FALSE)
  cat("HazardGraph QA/QC report written.\n")
  output_file
}

upload_qaqc <- function(qaqc_file, validation_folder) {
  # Upload the combined QA/QC report
  if (!file.exists(qaqc_file)) stop("QA/QC report is missing.")

  googledrive::drive_upload(qaqc_file, path = validation_folder, overwrite = TRUE)
  cat("HazardGraph QA/QC report uploaded.\n")
  TRUE
}