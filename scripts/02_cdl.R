download_cdl_crop <- function(year, county_fips, county_file, drive_folder, output_dir, target_crs, analysis_buffers, safety_margin_m = 500) {
  # Define the TIGER-aware buffered CDL output
  crop_margin_m <- max(analysis_buffers) + safety_margin_m
  file_name <- paste0("CDL_", year, "_", county_fips, "_tiger_buffer", crop_margin_m, "m.tif")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  local_file <- file.path(output_dir, file_name)

  cat("TIGER-aware buffered CDL output defined.\n")

  # Build the current TIGER analysis area
  counties <- sf::st_read(county_file, quiet = TRUE)
  county <- counties[as.character(counties$GEOID) == county_fips, ]

  if (nrow(county) != 1) stop("Expected exactly one county for the requested FIPS.")

  county <- sf::st_transform(county, target_crs)
  buffered_county <- sf::st_buffer(county, dist = crop_margin_m)
  bbox <- sf::st_bbox(buffered_county)

  bbox_text <- paste(
    round(c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])),
    collapse = ","
  )

  cat("Current TIGER buffered analysis area built.\n")

  # Define the CDL coverage validator
  validate_cdl <- function(path) {
    if (!file.exists(path)) return(FALSE)

    cdl <- try(terra::rast(path), silent = TRUE)

    if (inherits(cdl, "try-error")) return(FALSE)
    if (terra::ncell(cdl) == 0) return(FALSE)

    cdl_crs <- terra::crs(cdl)

    if (is.na(cdl_crs) || !nzchar(cdl_crs)) return(FALSE)

    if (abs(terra::res(cdl)[1] - 30) > 1 || abs(terra::res(cdl)[2] - 30) > 1) {
      return(FALSE)
    }

    buffered_vector <- terra::vect(buffered_county)
    buffered_vector <- terra::project(buffered_vector, terra::crs(cdl))
    tolerance <- max(terra::res(cdl)) * 2

    terra::xmin(cdl) <= terra::xmin(buffered_vector) + tolerance &&
      terra::xmax(cdl) >= terra::xmax(buffered_vector) - tolerance &&
      terra::ymin(cdl) <= terra::ymin(buffered_vector) + tolerance &&
      terra::ymax(cdl) >= terra::ymax(buffered_vector) - tolerance
  }

  cat("Buffered CDL validator ready.\n")

  # Restore and validate the persisted TIGER-aware CDL raster
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_file <- drive_files[drive_files$name == file_name, ]

  if (nrow(drive_file) > 1) stop("Multiple TIGER-aware CDL files found on Google Drive.")

  cache_valid <- FALSE

  if (nrow(drive_file) == 1) {
    googledrive::drive_download(drive_file, path = local_file, overwrite = TRUE)
    cache_valid <- validate_cdl(local_file)

    if (cache_valid) {
      cat("Valid TIGER-aware CDL raster restored from Google Drive.\n")
    } else {
      file.remove(local_file)
      cat("Persisted CDL raster is stale and will be rebuilt.\n")
    }
  } else if (file.exists(local_file)) {
    cache_valid <- validate_cdl(local_file)

    if (cache_valid) {
      cat("Valid local TIGER-aware CDL raster found.\n")
    } else {
      file.remove(local_file)
      cat("Local CDL raster is stale and will be rebuilt.\n")
    }
  } else {
    cat("No TIGER-aware buffered CDL raster found.\n")
  }

  # Download a fresh CDL subset when the cache is missing or stale
  rebuilt <- FALSE

  if (!cache_valid) {
    request_url <- paste0(
      "https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi",
      "?SERVICE=wcs",
      "&VERSION=1.0.0",
      "&REQUEST=GetCoverage",
      "&COVERAGE=cdl_",
      year,
      "&CRS=epsg:5070",
      "&BBOX=",
      bbox_text,
      "&RESX=30",
      "&RESY=30",
      "&FORMAT=gtiff"
    )

    download_ok <- FALSE

    for (attempt in seq_len(4)) {
      if (file.exists(local_file)) file.remove(local_file)

      result <- try(
        utils::download.file(
          request_url,
          destfile = local_file,
          mode = "wb",
          method = "libcurl",
          quiet = FALSE
        ),
        silent = TRUE
      )

      file_ok <- !inherits(result, "try-error") &&
        file.exists(local_file) &&
        !is.na(file.info(local_file)$size) &&
        file.info(local_file)$size > 1000

      if (file_ok && validate_cdl(local_file)) {
        download_ok <- TRUE
        break
      }

      if (file.exists(local_file)) file.remove(local_file)
      Sys.sleep(5 * attempt)
    }

    if (!download_ok) stop("CropScape WCS failed to create a valid TIGER-aware CDL raster.")

    rebuilt <- TRUE
    cat("Fresh TIGER-aware CDL raster downloaded.\n")
  }

  # Validate the final CDL raster
  if (!validate_cdl(local_file)) stop("Final CDL raster does not cover the TIGER analysis area.")

  cdl <- terra::rast(local_file)
  cat("Final TIGER-aware CDL raster validated.\n")

  # Persist the current CDL raster to Google Drive
  if (rebuilt || nrow(drive_file) == 0) {
    googledrive::drive_upload(local_file, path = drive_folder, overwrite = TRUE)
    cat("TIGER-aware CDL raster persisted to Google Drive.\n")
  } else {
    cat("TIGER-aware CDL raster already persisted on Google Drive.\n")
  }

  local_file
}