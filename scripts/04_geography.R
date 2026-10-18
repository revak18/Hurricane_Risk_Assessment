download_nc_counties <- function(year, state_fips, raw_dir) {
  # Download full-resolution Census TIGER/Line county boundaries
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  zip_name <- paste0("tl_", year, "_us_county.zip")
  zip_file <- file.path(raw_dir, zip_name)
  url <- paste0("https://www2.census.gov/geo/tiger/TIGER", year, "/COUNTY/", zip_name)

  if (!file.exists(zip_file)) {
    utils::download.file(url, zip_file, mode = "wb", quiet = FALSE)
  }

  cat("Census TIGER/Line county boundaries downloaded.\n")

  # Extract the national TIGER county shapefile
  extract_dir <- file.path(raw_dir, paste0("tiger_counties_", year))
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_file, exdir = extract_dir)
  shp_file <- file.path(extract_dir, paste0("tl_", year, "_us_county.shp"))

  if (!file.exists(shp_file)) {
    stop("TIGER/Line county shapefile was not found after extraction.")
  }

  cat("Census TIGER/Line county shapefile extracted.\n")

  # Filter the TIGER county layer to North Carolina
  counties <- sf::st_read(shp_file, quiet = TRUE)
  nc <- counties[as.character(counties$STATEFP) == state_fips, ]

  if (nrow(nc) != 100) {
    stop(paste("Expected 100 North Carolina counties but found", nrow(nc)))
  }

  if (length(unique(nc$GEOID)) != 100) {
    stop("North Carolina county GEOIDs are not unique.")
  }

  if (!"37119" %in% as.character(nc$GEOID)) {
    stop("Mecklenburg County FIPS 37119 was not found.")
  }

  cat("North Carolina TIGER counties validated.\n")

  # Save the raw North Carolina TIGER geography
  raw_file <- file.path(raw_dir, paste0("nc_counties_tiger_", year, ".gpkg"))
  sf::st_write(nc, raw_file, delete_dsn = TRUE, quiet = TRUE)

  cat("North Carolina TIGER geography written.\n")
  raw_file
}

prepare_nc_counties <- function(raw_file, output_dir, target_crs) {
  # Load and standardize the TIGER county fields
  counties <- sf::st_read(raw_file, quiet = TRUE)
  required <- c("STATEFP", "COUNTYFP", "GEOID", "NAME")
  missing <- setdiff(required, names(counties))

  if (length(missing)) {
    stop(paste("Missing TIGER county fields:", paste(missing, collapse = ", ")))
  }

  counties <- counties[, required]
  cat("TIGER county geography fields validated.\n")

  # Repair and project the county geometries
  counties <- sf::st_make_valid(counties)
  counties <- sf::st_transform(counties, target_crs)

  if (any(sf::st_is_empty(counties))) {
    stop("Empty county geometries found.")
  }

  if (any(!sf::st_is_valid(counties))) {
    stop("Invalid county geometries remain after repair.")
  }

  cat("TIGER county geometries projected and validated.\n")

  # Write the processed FIPS geography
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, "nc_counties_fips.gpkg")
  sf::st_write(counties, output_file, delete_dsn = TRUE, quiet = TRUE)

  cat("Processed TIGER county geography written.\n")
  output_file
}

upload_nc_counties <- function(raw_file, processed_file, raw_folder, processed_folder) {
  # Validate the TIGER geography output files
  if (!file.exists(raw_file) || !file.exists(processed_file)) {
    stop("TIGER county geography files are missing.")
  }

  cat("TIGER geography output files validated.\n")

  # Upload the raw North Carolina TIGER geography
  googledrive::drive_upload(raw_file, path = raw_folder, overwrite = TRUE)
  cat("Raw TIGER county geography uploaded.\n")

  # Upload the processed county FIPS geography
  googledrive::drive_upload(processed_file, path = processed_folder, overwrite = TRUE)
  cat("Processed TIGER county geography uploaded.\n")

  TRUE
}