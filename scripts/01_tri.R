download_tri_raw <- function(year, state, raw_dir, drive_folder) {
  # Restore the requested statewide TRI source from Google Drive
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  expected_name <- paste0("tri_raw_", year, "_", state, ".csv")
  drive_files <- googledrive::drive_ls(drive_folder)
  exact_file <- drive_files[drive_files$name == expected_name, ]

  if (nrow(exact_file) == 1) {
    local_file <- file.path(raw_dir, expected_name)
    googledrive::drive_download(exact_file, path = local_file, overwrite = TRUE)
    cat("Persisted statewide TRI source restored from Google Drive.\n")
    return(local_file)
  }

  csv_files <- drive_files[
    grepl("\\.csv$", drive_files$name, ignore.case = TRUE) &
      grepl(as.character(year), drive_files$name, fixed = TRUE),
  ]

  if (nrow(csv_files) == 1) {
    local_file <- file.path(raw_dir, csv_files$name[[1]])
    googledrive::drive_download(csv_files, path = local_file, overwrite = TRUE)
    cat("Persisted statewide TRI source restored from Google Drive.\n")
    return(local_file)
  }

  if (nrow(csv_files) > 1) stop(paste0("Multiple raw TRI CSV files found for ", year, "."))
  cat("No persisted statewide TRI source found for requested year.\n")

  # Download statewide TRI when no persisted source exists
  run_dir <- file.path(raw_dir, paste0("tri_", year, "_", state))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  amadeus::download_tri(
    year = year, jurisdiction = state, directory_to_save = run_dir,
    acknowledgement = TRUE, show_progress = TRUE, hash = FALSE, max_tries = 5, rate_limit = 2
  )
  cat("Statewide TRI download complete.\n")

  # Locate the newly downloaded TRI CSV
  files <- list.files(run_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) stop("No TRI CSV was downloaded.")

  info <- file.info(files)
  raw_file <- files[which.max(info$mtime)]
  cat("Statewide TRI source file located.\n")
  raw_file
}

build_tri_tables <- function(raw_file, year, state, output_dir) {
  # Load TRI as character data and map numbered EPA fields
  tri <- readr::read_csv(
    raw_file,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )

  tri <- as.data.frame(tri, stringsAsFactors = FALSE)
  original_names <- sub("^\ufeff", "", names(tri))
  names(tri) <- original_names

  field_numbers <- suppressWarnings(as.integer(sub("^\\s*([0-9]+)\\..*$", "\\1", original_names)))

  pull_field <- function(number, required = TRUE) {
    index <- which(field_numbers == number)
    if (length(index) != 1) {
      if (required) stop(paste0("TRI field ", number, " was not found."))
      return(rep(NA_character_, nrow(tri)))
    }
    tri[[index]]
  }

  to_number <- function(x) {
    x <- as.character(x)
    x <- gsub(",", "", x)
    x <- gsub("[^0-9eE+.-]", "", x)
    x[x == ""] <- NA_character_
    suppressWarnings(as.numeric(x))
  }

  pull_number <- function(number, required = TRUE) to_number(pull_field(number, required = required))

  sum_if_any <- function(...) {
    values <- cbind(...)
    out <- rowSums(values, na.rm = TRUE)
    out[rowSums(!is.na(values)) == 0] <- NA_real_
    out
  }

  dominant_pathway <- function(air, water, land, offsite) {
    values <- cbind(air = air, water = water, land = land, offsite = offsite)
    apply(values, 1, function(x) {
      if (all(is.na(x))) return(NA_character_)
      x[is.na(x)] <- -Inf
      names(x)[which.max(x)]
    })
  }
  cat("TRI numbered field schema loaded.\n")

  # Validate year and state in the raw TRI source
  source_year <- suppressWarnings(as.integer(pull_field(1)))

  if (any(!is.na(source_year) & source_year != year)) {
    stop("TRI source contains records from the wrong reporting year.")
  }

  source_state <- toupper(trimws(pull_field(8)))
  source_state <- unique(source_state[!is.na(source_state) & source_state != ""])

  if (length(source_state) && any(source_state != toupper(state))) {
    stop("TRI source contains records outside the requested state.")
  }
  cat("TRI reporting year and state validated.\n")

  # Extract statewide facility fields
  tri_facility_id <- trimws(pull_field(2))
  facility_name <- pull_field(4)
  street_address <- pull_field(5)
  city <- pull_field(6)
  county <- pull_field(7)
  facility_state <- pull_field(8)
  zip <- pull_field(9)
  latitude <- pull_number(12)
  longitude <- pull_number(13)
  parent_company <- pull_field(15, required = FALSE)

  longitude[!is.na(longitude) & longitude > 0] <- -longitude[!is.na(longitude) & longitude > 0]
  valid_coordinates <- !is.na(latitude) & !is.na(longitude) &
    latitude >= 24 & latitude <= 50 &
    longitude >= -125 & longitude <= -66

  if (all(is.na(tri_facility_id) | tri_facility_id == "")) stop("TRI facility IDs are missing.")

  facility_rows <- data.frame(
    tri_facility_id = tri_facility_id, facility_name = facility_name,
    street_address = street_address, city = city, county = county,
    state = facility_state, zip = zip, latitude = latitude, longitude = longitude,
    parent_company = parent_company, amadeus_source_file = basename(raw_file),
    valid_coordinates = valid_coordinates, stringsAsFactors = FALSE
  )

  facilities <- facility_rows[!duplicated(facility_rows$tri_facility_id), ]
  rownames(facilities) <- NULL
  cat("Statewide TRI facility table built.\n")

  # Extract TRI chemical identifiers and classifications
  doc_ctrl_num <- pull_field(36, required = FALSE)
  chemical_name <- pull_field(37, required = FALSE)
  tri_chem_id <- pull_field(39, required = FALSE)
  cas_number <- pull_field(40, required = FALSE)
  classification <- pull_field(43, required = FALSE)
  carcinogen <- pull_field(46, required = FALSE)
  pbt <- pull_field(47, required = FALSE)
  pfas <- pull_field(48, required = FALSE)
  form_type <- pull_field(49, required = FALSE)
  unit_of_measure <- pull_field(50, required = FALSE)
  sic_code <- pull_field(24, required = FALSE)
  naics_code <- pull_field(30, required = FALSE)
  industry_sector <- pull_field(23, required = FALSE)

  if (all(is.na(tri_chem_id) | trimws(tri_chem_id) == "")) {
    stop("TRI chemical ID field is empty.")
  }
  cat("TRI chemical identifiers extracted.\n")

  # Extract TRI on-site release quantities
  fugitive_air <- pull_number(51, required = FALSE)
  stack_air <- pull_number(52, required = FALSE)
  water <- pull_number(53, required = FALSE)
  underground_class_i <- pull_number(55, required = FALSE)
  underground_class_ii_v <- pull_number(56, required = FALSE)
  underground <- sum_if_any(underground_class_i, underground_class_ii_v)

  land <- sum_if_any(
    pull_number(58, required = FALSE),
    pull_number(59, required = FALSE),
    pull_number(60, required = FALSE),
    pull_number(62, required = FALSE),
    pull_number(63, required = FALSE),
    pull_number(64, required = FALSE)
  )

  total_air <- sum_if_any(fugitive_air, stack_air)
  onsite_release <- pull_number(65, required = FALSE)
  calculated_onsite <- sum_if_any(total_air, water, underground, land)
  onsite_release[is.na(onsite_release)] <- calculated_onsite[is.na(onsite_release)]
  cat("TRI on-site release quantities extracted.\n")

  # Extract TRI off-site transfer quantities
  offsite_release <- pull_number(88, required = FALSE)
  recycling_transfer <- pull_number(94, required = FALSE)
  recovery_transfer <- pull_number(97, required = FALSE)
  treatment_transfer <- pull_number(104, required = FALSE)
  transfer_total <- pull_number(106, required = FALSE)
  production_waste <- pull_number(119, required = FALSE)
  cat("TRI off-site transfer quantities extracted.\n")

  # Build the statewide chemical-level table
  chemicals <- data.frame(
    reporting_year = year, doc_ctrl_num = doc_ctrl_num, facility_name = facility_name,
    tri_facility_id = tri_facility_id, street_address = street_address, city = city,
    county = county, state = facility_state, zip = zip, latitude = latitude,
    longitude = longitude, parent_company = parent_company, industry_sector = industry_sector,
    chemical_name = chemical_name, tri_chem_id = tri_chem_id, cas_number = cas_number,
    classification = classification, carcinogen = carcinogen, pbt = pbt, pfas = pfas,
    form_type = form_type, unit_of_measure = unit_of_measure, naics_code = naics_code,
    sic_code = sic_code, fugitive_air = fugitive_air, stack_air = stack_air,
    total_air = total_air, water = water, underground = underground, land = land,
    onsite_release = onsite_release, offsite_release = offsite_release,
    transfer_total = transfer_total, recycling_transfer = recycling_transfer,
    treatment_transfer = treatment_transfer, recovery_transfer = recovery_transfer,
    production_waste = production_waste,
    dominant_pathway = dominant_pathway(total_air, water, land, offsite_release),
    stringsAsFactors = FALSE
  )
  cat("Statewide TRI chemical table built.\n")

  # Build statewide TRI metadata and coordinate QA
  bad_coordinates <- facilities[!facilities$valid_coordinates, ]
  valid_chemical_ids <- tri_chem_id[!is.na(tri_chem_id) & trimws(tri_chem_id) != ""]

  metadata <- data.frame(
    reporting_year = year, state = state, geographic_scope = "statewide",
    raw_source_file = basename(raw_file), raw_columns = ncol(tri),
    rows_raw_state = nrow(tri), unique_facilities = nrow(facilities),
    chemical_records = nrow(chemicals), unique_chemicals = length(unique(valid_chemical_ids)),
    valid_facilities = sum(facilities$valid_coordinates, na.rm = TRUE),
    bad_coordinates = nrow(bad_coordinates), run_time = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  cat("Statewide TRI metadata built.\n")

  # Write statewide TRI outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  prefix <- tolower(state)
  facility_file <- file.path(output_dir, paste0(prefix, "_tri_facility_master_", year, ".csv"))
  chemical_file <- file.path(output_dir, paste0(prefix, "_tri_chemicals_", year, ".csv"))
  metadata_file <- file.path(output_dir, paste0(prefix, "_tri_metadata_", year, ".csv"))
  bad_coordinates_file <- file.path(output_dir, paste0(prefix, "_tri_bad_coordinates_", year, ".csv"))

  write.csv(facilities, facility_file, row.names = FALSE)
  write.csv(chemicals, chemical_file, row.names = FALSE)
  write.csv(metadata, metadata_file, row.names = FALSE)
  write.csv(bad_coordinates, bad_coordinates_file, row.names = FALSE)
  cat("Statewide TRI outputs written.\n")

  c(facility_file, chemical_file, metadata_file, bad_coordinates_file)
}

upload_tri_outputs <- function(raw_file, files, raw_folder, processed_folder, metadata_folder) {
  # Validate statewide TRI outputs
  if (length(files) != 4 || !all(file.exists(files))) {
    stop("Expected four statewide TRI output files.")
  }
  cat("Statewide TRI output files validated.\n")

  # Upload the statewide TRI source
  googledrive::drive_upload(raw_file, path = raw_folder, overwrite = TRUE)
  cat("Raw statewide TRI source uploaded.\n")

  # Upload statewide TRI processed tables
  invisible(lapply(files[1:2], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))
  cat("Processed statewide TRI tables uploaded.\n")

  # Upload statewide TRI metadata
  invisible(lapply(files[3:4], googledrive::drive_upload, path = metadata_folder, overwrite = TRUE))
  cat("Statewide TRI metadata uploaded.\n")
  TRUE
}