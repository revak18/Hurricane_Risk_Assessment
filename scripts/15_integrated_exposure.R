build_integrated_exposure <- function(
  tri_fips_files,
  tri_cdl_buffer_files,
  ian_exposure_files,
  year,
  target_county_fips,
  output_dir,
  validation_dir
) {
  # Load Mecklenburg TRI, CDL, and Ian exposure data
  facilities <- read.csv(tri_fips_files[[3]], stringsAsFactors = FALSE)
  chemicals <- read.csv(tri_fips_files[[4]], stringsAsFactors = FALSE)
  cdl_features <- read.csv(tri_cdl_buffer_files[[1]], stringsAsFactors = FALSE)
  ian_tri <- read.csv(ian_exposure_files[[4]], stringsAsFactors = FALSE)

  if (!nrow(facilities) || !nrow(chemicals) || !nrow(cdl_features)) {
    stop("Integrated exposure input tables are empty.")
  }

  cat("Integrated exposure inputs loaded.\n")

  # Validate facility IDs and Mecklenburg geography
  required_facility <- c(
    "tri_facility_id",
    "facility_name",
    "latitude",
    "longitude",
    "county_geoid"
  )
  required_cdl <- c("tri_facility_id", "buffer_m")

  if (length(setdiff(required_facility, names(facilities)))) {
    stop("Required TRI facility fields are missing.")
  }

  if (!"tri_facility_id" %in% names(chemicals)) {
    stop("TRI chemical facility ID is missing.")
  }

  if (length(setdiff(required_cdl, names(cdl_features)))) {
    stop("Required CDL feature fields are missing.")
  }

  if (!all(as.character(facilities$county_geoid) == target_county_fips)) {
    stop("Facility table contains records outside Mecklenburg County.")
  }

  cat("Integrated exposure keys validated.\n")

  # Normalize TRI release quantities to pounds
  if (!"unit_of_measure" %in% names(chemicals)) {
    chemicals$unit_of_measure <- NA_character_
  }

  unit <- toupper(trimws(as.character(chemicals$unit_of_measure)))
  conversion_factor <- rep(NA_real_, nrow(chemicals))

  conversion_factor[unit %in% c("POUND", "POUNDS", "LB", "LBS")] <- 1
  conversion_factor[unit %in% c("GRAM", "GRAMS", "G")] <- 0.00220462262185
  conversion_factor[unit %in% c("KILOGRAM", "KILOGRAMS", "KG")] <- 2.20462262185

  chemicals$release_conversion_to_lb <- conversion_factor

  cat("TRI release units normalized to pounds.\n")

  # Convert TRI release fields to pounds
  release_fields <- intersect(
    c(
      "fugitive_air",
      "stack_air",
      "total_air",
      "water",
      "underground",
      "land",
      "onsite_release",
      "offsite_release",
      "transfer_total",
      "recycling_transfer",
      "treatment_transfer",
      "recovery_transfer",
      "production_waste"
    ),
    names(chemicals)
  )

  unknown_unit_rows <- 0L

  for (field in release_fields) {
    values <- suppressWarnings(as.numeric(chemicals[[field]]))

    unknown_unit_rows <- unknown_unit_rows +
      sum(!is.na(values) & is.na(conversion_factor))

    chemicals[[paste0(field, "_lb")]] <- values * conversion_factor
  }

  cat("TRI release fields converted to pounds.\n")

  # Build facility-level TRI chemical and release features
  ids <- facilities$tri_facility_id

  summarize_sum <- function(data, field) {
    values <- suppressWarnings(as.numeric(data[[field]]))

    if (all(is.na(values))) {
      return(NA_real_)
    }

    sum(values, na.rm = TRUE)
  }

  flag_count <- function(data, field) {
    if (!field %in% names(data)) {
      return(NA_integer_)
    }

    values <- toupper(trimws(as.character(data[[field]])))

    sum(values %in% c("YES", "Y", "TRUE", "1"), na.rm = TRUE)
  }

  facility_features <- lapply(
    ids,
    function(id) {
      rows <- chemicals[chemicals$tri_facility_id == id, ]

      chemical_ids <- trimws(as.character(rows$tri_chem_id))
      chemical_ids <- chemical_ids[!is.na(chemical_ids) & chemical_ids != ""]

      data.frame(
        tri_facility_id = id,
        chemical_records = nrow(rows),
        unique_chemicals = length(unique(chemical_ids)),
        carcinogen_chemicals = flag_count(rows, "carcinogen"),
        pbt_chemicals = flag_count(rows, "pbt"),
        pfas_chemicals = flag_count(rows, "pfas"),
        total_air_release_lb = summarize_sum(rows, "total_air_lb"),
        total_water_release_lb = summarize_sum(rows, "water_lb"),
        total_land_release_lb = summarize_sum(rows, "land_lb"),
        total_underground_release_lb = summarize_sum(rows, "underground_lb"),
        total_onsite_release_lb = summarize_sum(rows, "onsite_release_lb"),
        total_offsite_release_lb = summarize_sum(rows, "offsite_release_lb"),
        total_transfer_lb = summarize_sum(rows, "transfer_total_lb"),
        total_recycling_transfer_lb = summarize_sum(rows, "recycling_transfer_lb"),
        total_treatment_transfer_lb = summarize_sum(rows, "treatment_transfer_lb"),
        total_recovery_transfer_lb = summarize_sum(rows, "recovery_transfer_lb"),
        total_production_waste_lb = summarize_sum(rows, "production_waste_lb"),
        stringsAsFactors = FALSE
      )
    }
  )

  facility_features <- do.call(rbind, facility_features)
  rownames(facility_features) <- NULL

  cat("Facility-level TRI exposure features built.\n")

  # Attach facility metadata to the TRI feature table
  facility_columns <- intersect(
    c(
      "tri_facility_id",
      "facility_name",
      "street_address",
      "city",
      "county",
      "county_geoid",
      "census_county",
      "latitude",
      "longitude",
      "parent_company"
    ),
    names(facilities)
  )

  facility_summary <- merge(
    facilities[, facility_columns, drop = FALSE],
    facility_features,
    by = "tri_facility_id",
    all.x = TRUE,
    sort = FALSE
  )

  facility_summary <- facility_summary[
    match(facilities$tri_facility_id, facility_summary$tri_facility_id),
  ]

  rownames(facility_summary) <- NULL

  cat("TRI facility metadata attached.\n")

  # Attach Hurricane Ian proximity features
  ian_required <- c(
    "tri_facility_id",
    "distance_to_ian_miles",
    "within_ian_100mi"
  )

  missing_ian <- setdiff(ian_required, names(ian_tri))

  if (length(missing_ian)) {
    stop(
      paste(
        "Missing Ian TRI fields:",
        paste(missing_ian, collapse = ", ")
      )
    )
  }

  ian_features <- ian_tri[, ian_required, drop = FALSE]

  facility_summary <- merge(
    facility_summary,
    ian_features,
    by = "tri_facility_id",
    all.x = TRUE,
    sort = FALSE
  )

  facility_summary <- facility_summary[
    match(facilities$tri_facility_id, facility_summary$tri_facility_id),
  ]

  missing_ian_facilities <- sum(is.na(facility_summary$distance_to_ian_miles))

  cat("Hurricane Ian proximity features attached.\n")

  # Combine facility-level exposure with CDL buffer features
  integrated <- merge(
    cdl_features,
    facility_summary,
    by = "tri_facility_id",
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("_cdl", "")
  )

  integrated <- integrated[
    order(
      match(integrated$tri_facility_id, facilities$tri_facility_id),
      integrated$buffer_m
    ),
  ]

  rownames(integrated) <- NULL

  cat("Integrated TRI-CDL-Ian feature table built.\n")

  # Validate the integrated exposure table
  expected_rows <- nrow(facilities) * length(unique(cdl_features$buffer_m))

  duplicate_rows <- sum(
    duplicated(
      integrated[, c("tri_facility_id", "buffer_m")]
    )
  )

  integrated_ids <- unique(integrated$tri_facility_id)

  missing_facilities <- length(
    setdiff(facilities$tri_facility_id, integrated_ids)
  )

  missing_release_features <- sum(is.na(integrated$unique_chemicals))
  missing_ian_features <- sum(is.na(integrated$distance_to_ian_miles))

  qa <- data.frame(
    check = c(
      "Mecklenburg TRI facilities",
      "Facility-level TRI feature rows",
      "CDL buffer distances",
      "Expected integrated rows",
      "Actual integrated rows",
      "Duplicate facility-buffer rows",
      "Facilities missing from integrated table",
      "Rows missing TRI chemical features",
      "Facilities missing Ian distance",
      "Rows missing Ian distance",
      "TRI release values with unsupported units",
      "Minimum Ian distance miles",
      "Maximum Ian distance miles",
      "Facilities within Ian 100-mile zone"
    ),
    value = c(
      nrow(facilities),
      nrow(facility_summary),
      length(unique(cdl_features$buffer_m)),
      expected_rows,
      nrow(integrated),
      duplicate_rows,
      missing_facilities,
      missing_release_features,
      missing_ian_facilities,
      missing_ian_features,
      unknown_unit_rows,
      round(min(facility_summary$distance_to_ian_miles, na.rm = TRUE), 2),
      round(max(facility_summary$distance_to_ian_miles, na.rm = TRUE), 2),
      sum(facility_summary$within_ian_100mi, na.rm = TRUE)
    ),
    status = c(
      "INFO",
      ifelse(nrow(facility_summary) == nrow(facilities), "PASS", "FAIL"),
      "INFO",
      "INFO",
      ifelse(nrow(integrated) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_rows == 0, "PASS", "FAIL"),
      ifelse(missing_facilities == 0, "PASS", "FAIL"),
      ifelse(missing_release_features == 0, "PASS", "FAIL"),
      ifelse(missing_ian_facilities == 0, "PASS", "FAIL"),
      ifelse(missing_ian_features == 0, "PASS", "FAIL"),
      ifelse(unknown_unit_rows == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("Integrated exposure QA/QC completed.\n")

  # Write integrated exposure outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  facility_file <- file.path(
    output_dir,
    paste0("mecklenburg_tri_facility_exposure_", year, ".csv")
  )

  integrated_file <- file.path(
    output_dir,
    paste0("mecklenburg_tri_integrated_exposure_", year, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_integrated_exposure_", year, ".csv")
  )

  write.csv(facility_summary, facility_file, row.names = FALSE)
  write.csv(integrated, integrated_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("Integrated exposure outputs written.\n")

  c(facility_file, integrated_file, qa_file)
}

upload_integrated_exposure <- function(
  files,
  processed_folder,
  validation_folder
) {
  # Validate integrated exposure outputs
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("Expected three integrated exposure output files.")
  }

  cat("Integrated exposure output files validated.\n")

  # Upload integrated exposure feature tables
  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("Integrated exposure feature tables uploaded.\n")

  # Upload integrated exposure QA report
  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("Integrated exposure QA report uploaded.\n")

  TRUE
}