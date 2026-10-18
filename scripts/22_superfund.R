download_superfund_raw <- function(raw_dir, drive_folder) {

  # Define source, validation, and Drive restore helpers
  service <- paste0(
    "https://geopub.epa.gov/ArcGIS/rest/services/",
    "NEPAssist/NEPAVELayersPublic_fgdb/MapServer/14/query"
  )
  today <- format(Sys.Date(), "%Y%m%d")
  fname <- paste0("epa_superfund_npl_nc_boundaries_access_", today, ".geojson")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  required <- c("EPA_ID", "Site_Name", "County", "State", "NPL_Status")

  valid <- function(file) {
    x <- try(sf::st_read(file, quiet = TRUE), silent = TRUE)
    if (inherits(x, "try-error") || !nrow(x) || !all(required %in% names(x)))
      return(FALSE)

    state <- toupper(trimws(as.character(x$State)))
    ids <- trimws(as.character(x$EPA_ID))
    ids <- ids[!is.na(ids) & ids != ""]

    all(!is.na(state) & state == "NC") && !anyDuplicated(ids)
  }

  restore <- function(row) {
    path <- file.path(raw_dir, row$name[[1]])
    ok <- try(
      googledrive::drive_download(row, path = path, overwrite = TRUE),
      silent = TRUE
    )

    if (!inherits(ok, "try-error") && valid(path)) return(path)
    if (file.exists(path)) unlink(path)
    NA_character_
  }

  drive_files <- googledrive::drive_ls(drive_folder, n_max = Inf)
  same_day <- drive_files[drive_files$name == fname, ]

  if (nrow(same_day) > 1)
    stop("Multiple copies of today's Superfund snapshot found.")

  if (nrow(same_day) == 1) {
    path <- restore(same_day)
    if (!is.na(path)) {
      cat("Today's Superfund snapshot restored.\n")
      return(path)
    }
  }

  cat("Superfund source and Drive restore helpers ready.\n")


  # Query EPA with retries
  get_json <- function(url) {
    for (i in 1:3) {
      f <- tempfile(fileext = ".json")
      ok <- suppressWarnings(try(
        utils::download.file(url, f, mode = "wb", method = "libcurl", quiet = TRUE),
        silent = TRUE
      ))

      if (!inherits(ok, "try-error") && file.exists(f) && file.info(f)$size > 0) {
        x <- try(jsonlite::fromJSON(f), silent = TRUE)
        unlink(f)
        if (!inherits(x, "try-error")) return(x)
      } else unlink(f)

      Sys.sleep(2 * i)
    }

    stop("EPA Superfund JSON request failed.")
  }

  get_geojson <- function(url) {
    for (i in 1:3) {
      f <- tempfile(fileext = ".geojson")
      ok <- suppressWarnings(try(
        utils::download.file(url, f, mode = "wb", method = "libcurl", quiet = TRUE),
        silent = TRUE
      ))

      if (!inherits(ok, "try-error") && file.exists(f) && file.info(f)$size > 1000) {
        x <- try(sf::st_read(f, quiet = TRUE), silent = TRUE)
        unlink(f)
        if (!inherits(x, "try-error") && nrow(x)) return(x)
      } else unlink(f)

      Sys.sleep(2 * i)
    }

    stop("EPA Superfund GeoJSON request failed.")
  }

  where <- utils::URLencode("State='NC'", reserved = TRUE)

  live <- try({
    count <- get_json(paste0(
      service, "?where=", where, "&returnCountOnly=true&f=json"
    ))

    if (!is.null(count$error) || is.null(count$count) || count$count < 1)
      stop("EPA Superfund count request failed.")

    x <- get_geojson(paste0(
      service, "?where=", where,
      "&outFields=%2A&returnGeometry=true&outSR=4326&f=geojson"
    ))

    if (nrow(x) != as.integer(count$count))
      stop("Superfund feature count mismatch.")

    if (length(setdiff(required, names(x))))
      stop("Required Superfund fields are missing.")

    state <- toupper(trimws(as.character(x$State)))
    if (any(is.na(state) | state != "NC"))
      stop("Superfund snapshot contains non-NC records.")

    ids <- trimws(as.character(x$EPA_ID))
    ids <- ids[!is.na(ids) & ids != ""]
    if (anyDuplicated(ids))
      stop("Duplicate EPA IDs found.")

    x
  }, silent = TRUE)

  if (!inherits(live, "try-error")) {
    path <- file.path(raw_dir, fname)
    if (file.exists(path)) unlink(path)

    sf::st_write(live, path, driver = "GeoJSON", quiet = TRUE)
    googledrive::drive_upload(path, path = drive_folder, overwrite = TRUE)

    cat("Current Superfund snapshot downloaded and persisted.\n")
    return(path)
  }

  cat("EPA unavailable; prior Superfund snapshot required.\n")


  # Restore newest valid prior snapshot
  prior <- drive_files[
    grepl(
      "^epa_superfund_npl_nc_boundaries_access_[0-9]{8}\\.geojson$",
      drive_files$name
    ) &
      drive_files$name != fname,
  ]

  if (nrow(prior)) {
    prior$date <- sub(
      "^epa_superfund_npl_nc_boundaries_access_([0-9]{8})\\.geojson$",
      "\\1",
      prior$name
    )
    prior <- prior[order(prior$date, decreasing = TRUE), ]

    for (i in seq_len(nrow(prior))) {
      path <- restore(prior[i, ])
      if (!is.na(path)) {
        cat("Restored Superfund snapshot from", prior$date[i], "\n")
        return(path)
      }
    }
  }

  stop("EPA unavailable and no valid persisted Superfund snapshot exists.")
}


build_superfund_tables <- function(
  raw_file, county_file, target_county_fips,
  target_crs, output_dir, validation_dir
) {

  # Load, repair, and spatially assign Superfund sites
  sites <- sf::st_read(raw_file, quiet = TRUE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(sites) || !nrow(counties))
    stop("Superfund or county input is empty.")

  access_date <- sub(".*access_([0-9]{8}).*", "\\1", basename(raw_file))
  if (!grepl("^[0-9]{8}$", access_date))
    stop("Could not determine Superfund snapshot date.")

  sites <- sites[toupper(trimws(as.character(sites$State))) == "NC", ]
  invalid_before <- sum(!sf::st_is_valid(sites))
  sites <- sf::st_make_valid(sites)

  empty <- sf::st_is_empty(sites)
  empty_count <- sum(empty)
  sites <- sites[!empty, ]

  invalid_after <- sum(!sf::st_is_valid(sites))
  if (!nrow(sites) || invalid_after)
    stop("No valid Superfund geometries remain.")

  sites_p <- sf::st_transform(sites, target_crs)
  counties_p <- sf::st_transform(counties, target_crs)
  hits <- sf::st_intersects(sites_p, counties_p)

  collapse_field <- function(i, field) {
    if (!length(i)) return(NA_character_)
    paste(sort(unique(as.character(counties_p[[field]][i]))), collapse = ";")
  }

  county_geoids <- vapply(hits, collapse_field, character(1), field = "GEOID")
  census_counties <- vapply(hits, collapse_field, character(1), field = "NAME")
  county_count <- lengths(hits)

  intersects_mecklenburg <- vapply(
    hits,
    function(i) target_county_fips %in% as.character(counties_p$GEOID[i]),
    logical(1)
  )

  cat("Superfund geometries repaired and spatially assigned.\n")


  # Standardize source attributes and audit counties
  clean <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | x == ""] <- NA_character_
    x
  }

  get <- function(field) {
    if (field %in% names(sites)) clean(sites[[field]])
    else rep(NA_character_, nrow(sites))
  }

  status_raw <- get("NPL_Status")
  status <- toupper(ifelse(is.na(status_raw), "", status_raw))

  status_group <- ifelse(
    grepl("DELET", status), "deleted",
    ifelse(
      grepl("PROPOS", status), "proposed",
      ifelse(grepl("FINAL|CURRENT", status), "final", "other_or_unknown")
    )
  )

  standardized <- data.frame(
    epa_id = get("EPA_ID"),
    site_name = get("Site_Name"),
    reported_address = get("Address"),
    reported_city = get("City"),
    reported_county = get("County"),
    reported_state = get("State"),
    reported_zip = get("Zip_Code"),
    epa_region = get("Region"),
    npl_status_raw = status_raw,
    npl_status_group = status_group,
    facility_url = get("FACILITY_URL"),
    county_geoids = county_geoids,
    census_counties = census_counties,
    county_count = county_count,
    intersects_mecklenburg = intersects_mecklenburg,
    boundary_area_km2 = as.numeric(sf::st_area(sites_p)) / 1e6,
    snapshot_access_date = access_date,
    stringsAsFactors = FALSE
  )

  normalize_county <- function(x) {
    x <- toupper(ifelse(is.na(x), "", x))
    x <- gsub("\\bCOUNTY\\b|\\bCO\\.?\\b", "", x)
    gsub("[^A-Z0-9]", "", x)
  }

  reported <- normalize_county(standardized$reported_county)
  available <- reported != ""

  county_match <- vapply(seq_len(nrow(standardized)), function(i) {
    if (!available[i] || is.na(standardized$census_counties[i])) return(NA)
    spatial <- strsplit(standardized$census_counties[i], ";", fixed = TRUE)[[1]]
    reported[i] %in% normalize_county(spatial)
  }, logical(1))

  mismatch <- available & !is.na(county_match) & !county_match
  discrepancies <- standardized[mismatch, , drop = FALSE]

  cat("Superfund attributes standardized and county geography audited.\n")


  # Build statewide layers and QA
  clean_sf <- sf::st_sf(
    standardized,
    geometry = sf::st_geometry(sites_p),
    crs = sf::st_crs(sites_p)
  )

  meck_table <- standardized[standardized$intersects_mecklenburg, , drop = FALSE]
  meck_sf <- clean_sf[clean_sf$intersects_mecklenburg, ]

  duplicate_ids <- sum(duplicated(standardized$epa_id[!is.na(standardized$epa_id)]))
  missing_ids <- sum(is.na(standardized$epa_id))
  missing_county <- sum(standardized$county_count == 0)
  multicounty <- sum(standardized$county_count > 1)
  missing_status <- sum(is.na(standardized$npl_status_raw))

  qa <- data.frame(
    check = c(
      "NC Superfund NPL sites",
      "Invalid geometries before repair",
      "Invalid geometries after repair",
      "Empty geometries removed",
      "Duplicate EPA IDs",
      "Missing EPA IDs",
      "Sites without TIGER county intersection",
      "Sites spanning multiple counties",
      "Sites with reported county available",
      "Reported versus spatial county mismatches",
      "Mecklenburg-intersecting Superfund sites",
      "Final NPL sites",
      "Proposed NPL sites",
      "Deleted NPL sites",
      "Other or unknown NPL status",
      "Sites missing NPL status"
    ),
    value = c(
      nrow(standardized),
      invalid_before,
      invalid_after,
      empty_count,
      duplicate_ids,
      missing_ids,
      missing_county,
      multicounty,
      sum(available),
      sum(mismatch),
      nrow(meck_table),
      sum(status_group == "final"),
      sum(status_group == "proposed"),
      sum(status_group == "deleted"),
      sum(status_group == "other_or_unknown"),
      missing_status
    ),
    status = c(
      "INFO",
      ifelse(invalid_before == 0, "PASS", "CHECK"),
      ifelse(invalid_after == 0, "PASS", "FAIL"),
      ifelse(empty_count == 0, "PASS", "CHECK"),
      ifelse(duplicate_ids == 0, "PASS", "FAIL"),
      ifelse(missing_ids == 0, "PASS", "CHECK"),
      ifelse(missing_county == 0, "PASS", "CHECK"),
      "INFO",
      "INFO",
      ifelse(sum(mismatch) == 0, "PASS", "CHECK"),
      rep("INFO", 5),
      ifelse(missing_status == 0, "PASS", "CHECK")
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("Superfund statewide and Mecklenburg QA/QC completed.\n")


  # Build metadata and write seven outputs
  metadata <- data.frame(
    field = c(
      "source", "publisher", "dataset_type", "snapshot_access_date",
      "temporal_scope", "historical_2022_inventory", "geometry",
      "spatial_assignment", "scope_note"
    ),
    value = c(
      "Superfund NPL Site Boundaries",
      "U.S. EPA Office of Land and Emergency Management",
      "National Priorities List site boundaries",
      access_date,
      paste0("EPA geospatial snapshot accessed ", access_date),
      "FALSE",
      "EPA Superfund site boundary polygons",
      "2022 TIGER county intersection",
      paste(
        "This layer represents EPA Superfund NPL sites.",
        "It does not represent every contaminated or potentially",
        "contaminated site in North Carolina."
      )
    ),
    stringsAsFactors = FALSE
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  files <- c(
    file.path(output_dir, paste0("nc_superfund_npl_sites_access_", access_date, ".csv")),
    file.path(output_dir, paste0("mecklenburg_superfund_npl_sites_access_", access_date, ".csv")),
    file.path(output_dir, paste0("nc_superfund_npl_boundaries_access_", access_date, ".gpkg")),
    file.path(output_dir, paste0("mecklenburg_superfund_npl_boundaries_access_", access_date, ".gpkg")),
    file.path(output_dir, paste0("nc_superfund_npl_metadata_access_", access_date, ".csv")),
    file.path(validation_dir, paste0("superfund_geography_discrepancies_access_", access_date, ".csv")),
    file.path(validation_dir, paste0("qaqc_superfund_npl_access_", access_date, ".csv"))
  )

  write.csv(standardized, files[1], row.names = FALSE)
  write.csv(meck_table, files[2], row.names = FALSE)
  sf::st_write(clean_sf, files[3], delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(meck_sf, files[4], delete_dsn = TRUE, quiet = TRUE)
  write.csv(metadata, files[5], row.names = FALSE)
  write.csv(discrepancies, files[6], row.names = FALSE)
  write.csv(qa, files[7], row.names = FALSE)

  cat("Superfund processed outputs written.\n")
  files
}


upload_superfund_outputs <- function(
  files, processed_folder, metadata_folder, validation_folder
) {

  # Validate and upload Superfund outputs
  if (length(files) != 7 || !all(file.exists(files)))
    stop("Expected seven Superfund output files.")

  invisible(lapply(
    files[1:4],
    googledrive::drive_upload,
    path = processed_folder,
    overwrite = TRUE
  ))

  googledrive::drive_upload(
    files[5],
    path = metadata_folder,
    overwrite = TRUE
  )

  invisible(lapply(
    files[6:7],
    googledrive::drive_upload,
    path = validation_folder,
    overwrite = TRUE
  ))

  cat("Superfund processed, metadata, and QA outputs uploaded.\n")
  TRUE
}