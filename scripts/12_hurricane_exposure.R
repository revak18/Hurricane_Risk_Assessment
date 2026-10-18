build_ian_exposure <- function(ian_track_files, tri_fips_files, mrds_classified_files, county_file, year, target_county_fips, target_crs, output_dir, validation_dir, exposure_miles = 100) {
  # Load Ian, TRI, MRDS, and county data
  ian_line <- sf::st_read(ian_track_files[[3]], quiet = TRUE)
  tri <- read.csv(tri_fips_files[[1]], stringsAsFactors = FALSE)
  mrds <- read.csv(mrds_classified_files[[1]], stringsAsFactors = FALSE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(tri) || !nrow(mrds) || !nrow(counties)) stop("Ian exposure input data are incomplete.")
  cat("Ian exposure inputs loaded.\n")

  # Validate exposure coordinates and FIPS fields
  tri_required <- c("tri_facility_id", "latitude", "longitude", "county_geoid")
  mrds_required <- c("dep_id", "latitude", "longitude", "county_geoid")

  if (length(setdiff(tri_required, names(tri)))) stop("Required TRI exposure fields are missing.")
  if (length(setdiff(mrds_required, names(mrds)))) stop("Required MRDS exposure fields are missing.")
  if (!all(c("GEOID", "NAME") %in% names(counties))) stop("Required county geography fields are missing.")

  cat("Ian exposure input fields validated.\n")

  # Build Ian's 100-mile influence zone
  exposure_m <- exposure_miles * 1609.344
  ian_projected <- sf::st_transform(ian_line, target_crs)
  ian_zone <- sf::st_buffer(ian_projected, dist = exposure_m)
  ian_zone <- sf::st_union(ian_zone)

  ian_zone <- sf::st_sf(
    storm_id = "AL092022",
    storm_name = "IAN",
    exposure_miles = exposure_miles,
    geometry = ian_zone
  )

  cat("Ian 100-mile influence zone built.\n")

  # Identify North Carolina counties intersecting the Ian zone
  counties_projected <- sf::st_transform(counties, target_crs)
  county_hits <- sf::st_intersects(counties_projected, ian_zone, sparse = FALSE)[, 1]
  exposed_counties <- counties_projected[county_hits, ]
  exposed_counties_table <- sf::st_drop_geometry(exposed_counties)

  mecklenburg_county_exposed <- any(exposed_counties_table$GEOID == target_county_fips)
  cat("Ian-exposed North Carolina counties identified.\n")

  # Calculate TRI facility distance to Ian's track
  tri_points <- sf::st_as_sf(tri, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  tri_points <- sf::st_transform(tri_points, target_crs)
  tri_distance_m <- as.numeric(sf::st_distance(tri_points, ian_projected))

  tri$distance_to_ian_miles <- tri_distance_m / 1609.344
  tri$within_ian_100mi <- tri$distance_to_ian_miles <= exposure_miles
  exposed_tri <- tri[tri$within_ian_100mi, ]
  rownames(exposed_tri) <- NULL

  cat("TRI facility exposure to Ian calculated.\n")

  # Calculate MRDS site distance to Ian's track
  mrds_points <- sf::st_as_sf(mrds, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  mrds_points <- sf::st_transform(mrds_points, target_crs)
  mrds_distance_m <- as.numeric(sf::st_distance(mrds_points, ian_projected))

  mrds$distance_to_ian_miles <- mrds_distance_m / 1609.344
  mrds$within_ian_100mi <- mrds$distance_to_ian_miles <= exposure_miles
  exposed_mrds <- mrds[mrds$within_ian_100mi, ]
  rownames(exposed_mrds) <- NULL

  cat("MRDS site exposure to Ian calculated.\n")

  # Summarize Mecklenburg exposure
  mecklenburg_tri <- tri[tri$county_geoid == target_county_fips, ]
  mecklenburg_mrds <- mrds[mrds$county_geoid == target_county_fips, ]

  mecklenburg_tri_exposed <- sum(mecklenburg_tri$within_ian_100mi, na.rm = TRUE)
  mecklenburg_mrds_exposed <- sum(mecklenburg_mrds$within_ian_100mi, na.rm = TRUE)

  min_mecklenburg_tri_distance <- min(mecklenburg_tri$distance_to_ian_miles, na.rm = TRUE)
  min_mecklenburg_mrds_distance <- min(mecklenburg_mrds$distance_to_ian_miles, na.rm = TRUE)

  cat("Mecklenburg Ian exposure summarized.\n")

  # Build Ian exposure QA report
  qa <- data.frame(
    check = c(
      "Ian exposure radius miles",
      "NC counties intersecting Ian zone",
      "Mecklenburg County intersects Ian zone",
      "Statewide TRI facilities",
      "TRI facilities within 100 miles",
      "Mecklenburg TRI facilities",
      "Mecklenburg TRI facilities within 100 miles",
      "Minimum Mecklenburg TRI distance miles",
      "NC MRDS sites",
      "MRDS sites within 100 miles",
      "Mecklenburg MRDS sites",
      "Mecklenburg MRDS sites within 100 miles",
      "Minimum Mecklenburg MRDS distance miles"
    ),
    value = c(
      exposure_miles,
      nrow(exposed_counties),
      mecklenburg_county_exposed,
      nrow(tri),
      nrow(exposed_tri),
      nrow(mecklenburg_tri),
      mecklenburg_tri_exposed,
      round(min_mecklenburg_tri_distance, 2),
      nrow(mrds),
      nrow(exposed_mrds),
      nrow(mecklenburg_mrds),
      mecklenburg_mrds_exposed,
      round(min_mecklenburg_mrds_distance, 2)
    ),
    status = c(
      "INFO",
      "INFO",
      ifelse(mecklenburg_county_exposed, "PASS", "CHECK"),
      "INFO",
      "INFO",
      "INFO",
      ifelse(mecklenburg_tri_exposed > 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(mecklenburg_mrds_exposed > 0, "PASS", "CHECK"),
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("Ian exposure QA/QC completed.\n")

  # Write Ian exposure spatial outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  zone_file <- file.path(output_dir, paste0("ian_", exposure_miles, "mile_zone_", year, ".gpkg"))
  counties_file <- file.path(output_dir, paste0("ian_exposed_nc_counties_", year, ".gpkg"))

  sf::st_write(ian_zone, zone_file, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(exposed_counties, counties_file, delete_dsn = TRUE, quiet = TRUE)

  cat("Ian exposure spatial layers written.\n")

  # Write Ian exposure data tables
  counties_table_file <- file.path(output_dir, paste0("ian_exposed_nc_counties_", year, ".csv"))
  tri_file <- file.path(output_dir, paste0("ian_exposed_tri_facilities_", year, ".csv"))
  mrds_file <- file.path(output_dir, paste0("ian_exposed_mrds_sites_", year, ".csv"))
  qa_file <- file.path(validation_dir, paste0("qaqc_ian_exposure_", year, ".csv"))

  write.csv(exposed_counties_table, counties_table_file, row.names = FALSE)
  write.csv(exposed_tri, tri_file, row.names = FALSE)
  write.csv(exposed_mrds, mrds_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("Ian exposure data tables written.\n")
  c(zone_file, counties_file, counties_table_file, tri_file, mrds_file, qa_file)
}

upload_ian_exposure <- function(files, processed_folder, validation_folder) {
  # Validate Ian exposure output files
  if (length(files) != 6 || !all(file.exists(files))) stop("Expected six Ian exposure output files.")
  cat("Ian exposure output files validated.\n")

  # Upload Ian exposure data and spatial layers
  invisible(lapply(files[1:5], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))
  cat("Ian exposure outputs uploaded.\n")

  # Upload Ian exposure QA report
  googledrive::drive_upload(files[[6]], path = validation_folder, overwrite = TRUE)
  cat("Ian exposure QA report uploaded.\n")
  TRUE
}