download_mrds_raw <- function(raw_dir, drive_folder) {
  # Restore or download the USGS MRDS archive
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  file_name <- "mrds-csv.zip"
  local_file <- file.path(raw_dir, file_name)
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_file <- drive_files[drive_files$name == file_name, ]

  if (nrow(drive_file) > 1) {
    stop("Multiple MRDS archives found on Google Drive.")
  }

  if (nrow(drive_file) == 1) {
    googledrive::drive_download(drive_file, path = local_file, overwrite = TRUE)
    cat("Persisted MRDS archive restored from Google Drive.\n")
  } else {
    url <- "https://mrdata.usgs.gov/mrds/mrds-csv.zip"
    utils::download.file(url, local_file, mode = "wb", quiet = FALSE)
    googledrive::drive_upload(local_file, path = drive_folder, overwrite = TRUE)
    cat("USGS MRDS archive downloaded and persisted.\n")
  }

  # Validate the MRDS ZIP archive
  contents <- utils::unzip(local_file, list = TRUE)
  csv_files <- contents$Name[grepl("\\.csv$", contents$Name, ignore.case = TRUE)]

  if (!length(csv_files)) {
    stop("MRDS archive contains no CSV files.")
  }

  cat("MRDS archive validated.\n")
  local_file
}

inspect_mrds_schema <- function(raw_file, output_dir) {
  # Extract and locate the primary MRDS CSV
  extract_dir <- file.path(dirname(raw_file), "mrds_extracted")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(raw_file, exdir = extract_dir)

  csv_files <- list.files(
    extract_dir,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (!length(csv_files)) {
    stop("No extracted MRDS CSV was found.")
  }

  sizes <- file.info(csv_files)$size
  source_file <- csv_files[which.max(sizes)]
  cat("Primary MRDS CSV located.\n")

  # Load the MRDS schema and records
  mrds <- readr::read_csv(
    source_file,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  mrds <- as.data.frame(mrds, stringsAsFactors = FALSE)
  original_names <- names(mrds)

  clean_names <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }

  names(mrds) <- clean_names(names(mrds))

  if (!nrow(mrds)) {
    stop("MRDS CSV contains no records.")
  }

  cat("MRDS records loaded.\n")

  # Identify important MRDS fields
  find_col <- function(candidates) {
    hit <- intersect(candidates, names(mrds))

    if (!length(hit)) {
      return(NA_character_)
    }

    hit[[1]]
  }

  id_col <- find_col(c("dep_id", "deposit_id", "record_id", "mrds_id"))
  name_col <- find_col(c("site_name", "name", "deposit_name"))
  lat_col <- find_col(c("latitude", "lat", "decimal_latitude", "lat_dec"))
  lon_col <- find_col(c("longitude", "lon", "long", "lng", "decimal_longitude", "long_dec"))
  state_col <- find_col(c("state", "state_name", "state_code"))
  county_col <- find_col(c("county", "county_name"))
  dep_type_col <- find_col(c("dep_type", "deposit_type", "site_type"))
  commodities_col <- find_col(c("commodities", "commodity", "commod1"))

  cat("MRDS core fields identified.\n")

  # Validate and summarize MRDS coordinates
  to_number <- function(x) {
    x <- as.character(x)
    x <- gsub(",", "", x)
    x[x == ""] <- NA_character_
    suppressWarnings(as.numeric(x))
  }

  if (is.na(lat_col) || is.na(lon_col)) {
    stop("MRDS latitude or longitude field could not be identified.")
  }

  latitude <- to_number(mrds[[lat_col]])
  longitude <- to_number(mrds[[lon_col]])

  valid_coordinates <-
    !is.na(latitude) &
    !is.na(longitude) &
    latitude >= -90 &
    latitude <= 90 &
    longitude >= -180 &
    longitude <= 180

  plausible_nc_coordinates <-
    valid_coordinates &
    latitude >= 33 &
    latitude <= 37.5 &
    longitude >= -85 &
    longitude <= -75

  cat("MRDS coordinates validated.\n")

  # Summarize reported North Carolina records for comparison
  reported_nc <- rep(FALSE, nrow(mrds))

  if (!is.na(state_col)) {
    state_values <- toupper(trimws(as.character(mrds[[state_col]])))
    reported_nc <- state_values %in% c("NC", "NORTH CAROLINA")
  }

  cat("MRDS reported state field summarized.\n")

  # Build the MRDS schema report
  schema <- data.frame(
    original_name = original_names,
    cleaned_name = names(mrds),
    stringsAsFactors = FALSE
  )

  field_map <- data.frame(
    field = c(
      "record_id",
      "site_name",
      "latitude",
      "longitude",
      "reported_state",
      "reported_county",
      "deposit_type",
      "commodities"
    ),
    source_column = c(
      id_col,
      name_col,
      lat_col,
      lon_col,
      state_col,
      county_col,
      dep_type_col,
      commodities_col
    ),
    stringsAsFactors = FALSE
  )

  qa <- data.frame(
    check = c(
      "National MRDS records",
      "MRDS columns",
      "Valid coordinates",
      "Invalid coordinates",
      "Reported NC records",
      "Reported NC records with valid coordinates",
      "Records with plausible NC coordinates"
    ),
    value = c(
      nrow(mrds),
      ncol(mrds),
      sum(valid_coordinates),
      sum(!valid_coordinates),
      sum(reported_nc),
      sum(reported_nc & valid_coordinates),
      sum(plausible_nc_coordinates)
    ),
    status = c(
      "INFO",
      "INFO",
      ifelse(sum(valid_coordinates) > 0, "PASS", "FAIL"),
      "INFO",
      "INFO",
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(field_map, row.names = FALSE)
  print(qa, row.names = FALSE)

  cat("MRDS schema and coordinate QA completed.\n")

  # Write MRDS inspection outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  schema_file <- file.path(output_dir, "mrds_schema.csv")
  field_map_file <- file.path(output_dir, "mrds_field_map.csv")
  qa_file <- file.path(output_dir, "qaqc_mrds_schema.csv")

  write.csv(schema, schema_file, row.names = FALSE)
  write.csv(field_map, field_map_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("MRDS schema inspection outputs written.\n")
  c(schema_file, field_map_file, qa_file)
}

upload_mrds_schema <- function(files, validation_folder) {
  # Validate MRDS schema inspection outputs
  if (length(files) != 3 || !all(file.exists(files))) {
    stop("MRDS schema inspection files are missing.")
  }

  cat("MRDS schema inspection files validated.\n")

  # Upload MRDS schema inspection outputs
  invisible(
    lapply(
      files,
      googledrive::drive_upload,
      path = validation_folder,
      overwrite = TRUE
    )
  )

  cat("MRDS schema inspection outputs uploaded.\n")
  TRUE
}