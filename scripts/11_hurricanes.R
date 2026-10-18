download_hurdat2_raw <- function(raw_dir, drive_folder) {
  # Restore the Atlantic HURDAT2 source from Google Drive
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  file_name <- "hurdat2_atlantic.txt"
  local_file <- file.path(raw_dir, file_name)
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_file <- drive_files[drive_files$name == file_name, ]

  if (nrow(drive_file) > 1) stop("Multiple Atlantic HURDAT2 files found on Google Drive.")

  if (nrow(drive_file) == 1) {
    googledrive::drive_download(drive_file, path = local_file, overwrite = TRUE)
    cat("Persisted Atlantic HURDAT2 source restored from Google Drive.\n")
    return(local_file)
  }

  cat("No persisted Atlantic HURDAT2 source found.\n")

  # Discover the current NOAA Atlantic HURDAT2 file
  index_url <- "https://www.nhc.noaa.gov/data/"
  html <- paste(readLines(index_url, warn = FALSE), collapse = "\n")
  matches <- regmatches(html, gregexpr('href="[^"]*hurdat2-1851-[^"]*\\.txt"', html, perl = TRUE))[[1]]

  if (!length(matches)) stop("NOAA Atlantic HURDAT2 file could not be located.")

  href <- gsub('^href="|"$', "", matches[[1]])

  if (grepl("^https?://", href)) {
    source_url <- href
  } else if (grepl("^/", href)) {
    source_url <- paste0("https://www.nhc.noaa.gov", href)
  } else {
    source_url <- paste0("https://www.nhc.noaa.gov/", href)
  }

  cat("NOAA Atlantic HURDAT2 source located.\n")

  # Download the Atlantic HURDAT2 source
  utils::download.file(source_url, local_file, mode = "wb", method = "libcurl", quiet = FALSE)

  if (!file.exists(local_file) || is.na(file.info(local_file)$size) || file.info(local_file)$size < 1000) {
    stop("Downloaded Atlantic HURDAT2 file is invalid.")
  }

  cat("Atlantic HURDAT2 source downloaded.\n")

  # Persist the Atlantic HURDAT2 source
  googledrive::drive_upload(local_file, path = drive_folder, overwrite = TRUE)
  cat("Atlantic HURDAT2 source persisted to Google Drive.\n")
  local_file
}

build_ian_track <- function(raw_file, year, storm_id, output_dir, validation_dir) {
  # Locate Hurricane Ian in HURDAT2
  lines <- trimws(readLines(raw_file, warn = FALSE))
  header_index <- grep(paste0("^", storm_id, ","), lines)

  if (length(header_index) != 1) {
    stop(paste0("Expected exactly one HURDAT2 header for ", storm_id, "."))
  }

  header <- trimws(strsplit(lines[[header_index]], ",", fixed = TRUE)[[1]])
  storm_name <- header[[2]]
  observation_count <- suppressWarnings(as.integer(header[[3]]))

  if (is.na(observation_count) || observation_count <= 0) {
    stop("Invalid HURDAT2 observation count.")
  }

  cat("Hurricane Ian HURDAT2 record located.\n")

  # Parse Hurricane Ian observation rows
  observation_lines <- lines[seq.int(header_index + 1, length.out = observation_count)]
  split_rows <- lapply(strsplit(observation_lines, ",", fixed = TRUE), trimws)
  max_fields <- max(lengths(split_rows))

  split_rows <- lapply(split_rows, function(x) {
    length(x) <- max_fields
    x
  })

  observations <- as.data.frame(do.call(rbind, split_rows), stringsAsFactors = FALSE)
  cat("Hurricane Ian observation records parsed.\n")

  # Define HURDAT2 conversion helpers
  parse_latitude <- function(x) {
    value <- suppressWarnings(as.numeric(sub("[NS]$", "", x)))
    south <- grepl("S$", x)
    value[south] <- -value[south]
    value
  }

  parse_longitude <- function(x) {
    value <- suppressWarnings(as.numeric(sub("[EW]$", "", x)))
    west <- grepl("W$", x)
    value[west] <- -value[west]
    value
  }

  to_number <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "-999")] <- NA_character_
    suppressWarnings(as.numeric(x))
  }

  cat("HURDAT2 conversion helpers ready.\n")

  # Build the cleaned Hurricane Ian track
  track <- data.frame(
    storm_id = storm_id,
    storm_name = storm_name,
    date = observations[[1]],
    time_utc = observations[[2]],
    record_identifier = observations[[3]],
    status = observations[[4]],
    latitude = parse_latitude(observations[[5]]),
    longitude = parse_longitude(observations[[6]]),
    max_wind_kt = to_number(observations[[7]]),
    min_pressure_mb = to_number(observations[[8]]),
    radius_34kt_ne = to_number(observations[[9]]),
    radius_34kt_se = to_number(observations[[10]]),
    radius_34kt_sw = to_number(observations[[11]]),
    radius_34kt_nw = to_number(observations[[12]]),
    radius_50kt_ne = to_number(observations[[13]]),
    radius_50kt_se = to_number(observations[[14]]),
    radius_50kt_sw = to_number(observations[[15]]),
    radius_50kt_nw = to_number(observations[[16]]),
    radius_64kt_ne = to_number(observations[[17]]),
    radius_64kt_se = to_number(observations[[18]]),
    radius_64kt_sw = to_number(observations[[19]]),
    radius_64kt_nw = to_number(observations[[20]]),
    stringsAsFactors = FALSE
  )

  if (ncol(observations) >= 21) {
    track$radius_max_wind_nm <- to_number(observations[[21]])
  } else {
    track$radius_max_wind_nm <- NA_real_
  }

  time_text <- sprintf("%04d", as.integer(track$time_utc))

  track$datetime_utc <- as.POSIXct(
    paste(track$date, time_text),
    format = "%Y%m%d %H%M",
    tz = "UTC"
  )

  track <- track[order(track$datetime_utc), ]
  rownames(track) <- NULL
  cat("Hurricane Ian track table built.\n")

  # Validate the Hurricane Ian track
  valid_coordinates <- !is.na(track$latitude) & !is.na(track$longitude) & track$latitude >= -90 & track$latitude <= 90 & track$longitude >= -180 & track$longitude <= 180
  track_year <- suppressWarnings(as.integer(format(track$datetime_utc, "%Y", tz = "UTC")))

  if (nrow(track) != observation_count) stop("Parsed Ian observation count does not match HURDAT2 header.")
  if (!all(valid_coordinates)) stop("Invalid Hurricane Ian coordinates detected.")

  if (any(is.na(track_year)) || !all(track_year == year)) {
    stop("Hurricane Ian track contains records outside the requested year.")
  }

  cat("Hurricane Ian track validated.\n")

  # Build Hurricane Ian spatial layers
  points <- sf::st_as_sf(track, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  line_geometry <- sf::st_sfc(
    sf::st_linestring(as.matrix(track[, c("longitude", "latitude")])),
    crs = 4326
  )

  line <- sf::st_sf(storm_id = storm_id, storm_name = storm_name, geometry = line_geometry)
  cat("Hurricane Ian spatial track built.\n")

  # Build Hurricane Ian QA report
  qa <- data.frame(
    check = c("Storm ID", "Storm name", "HURDAT2 observations", "Parsed observations", "Valid coordinates", "Missing wind speeds", "Missing pressure values", "Hurricane observations", "Maximum sustained wind knots", "Minimum pressure mb", "Track start UTC", "Track end UTC"),
    value = c(storm_id, storm_name, observation_count, nrow(track), sum(valid_coordinates), sum(is.na(track$max_wind_kt)), sum(is.na(track$min_pressure_mb)), sum(track$status == "HU"), max(track$max_wind_kt, na.rm = TRUE), min(track$min_pressure_mb, na.rm = TRUE), format(min(track$datetime_utc), tz = "UTC"), format(max(track$datetime_utc), tz = "UTC")),
    status = c("INFO", ifelse(toupper(storm_name) == "IAN", "PASS", "FAIL"), "INFO", ifelse(nrow(track) == observation_count, "PASS", "FAIL"), ifelse(sum(valid_coordinates) == nrow(track), "PASS", "FAIL"), "INFO", "INFO", "INFO", "INFO", "INFO", "INFO", "INFO"),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("Hurricane Ian QA/QC completed.\n")

  # Write Hurricane Ian outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  track_file <- file.path(output_dir, paste0("ian_track_", year, ".csv"))
  points_file <- file.path(output_dir, paste0("ian_track_points_", year, ".gpkg"))
  line_file <- file.path(output_dir, paste0("ian_track_line_", year, ".gpkg"))
  qa_file <- file.path(validation_dir, paste0("qaqc_ian_track_", year, ".csv"))

  write.csv(track, track_file, row.names = FALSE)
  sf::st_write(points, points_file, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(line, line_file, delete_dsn = TRUE, quiet = TRUE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("Hurricane Ian track outputs written.\n")
  c(track_file, points_file, line_file, qa_file)
}

upload_ian_track <- function(files, processed_folder, validation_folder) {
  # Validate Hurricane Ian output files
  if (length(files) != 4 || !all(file.exists(files))) stop("Expected four Hurricane Ian output files.")
  cat("Hurricane Ian output files validated.\n")

  # Upload Hurricane Ian processed files
  invisible(lapply(files[1:3], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))
  cat("Hurricane Ian processed files uploaded.\n")

  # Upload Hurricane Ian QA report
  googledrive::drive_upload(files[[4]], path = validation_folder, overwrite = TRUE)
  cat("Hurricane Ian QA report uploaded.\n")
  TRUE
}