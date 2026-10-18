download_rmp_raw <- function(raw_dir, drive_folder, state_abbr) {

  # Define RMP source and snapshot helpers
  service_url <- paste0(
    "https://geopub.epa.gov/ArcGIS/rest/services/",
    "ER_R4/facilitiesR4_all/MapServer/11/query"
  )
  access_date <- format(Sys.Date(), "%Y%m%d")
  file_name <- paste0("epa_frs_rmp_access_", access_date, ".geojson")
  local_file <- file.path(raw_dir, file_name)
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  required <- c(
    "OBJECTID_1", "REG_ID", "FAC_NAME", "LOC_ADD", "LOC_CITY",
    "LOC_STATE", "LOC_ZIP", "LATITUDE", "LONGITUDE", "RMP",
    "STATE_FIPS", "CNTY_FIPS", "STCOFIPS"
  )

  valid_snapshot <- function(file) {
    x <- try(sf::st_read(file, quiet = TRUE), silent = TRUE)
    if (inherits(x, "try-error") || !nrow(x) || !all(required %in% names(x)))
      return(FALSE)

    state <- toupper(trimws(as.character(x$LOC_STATE)))
    all(is.na(state) | state == toupper(state_abbr)) &&
      !anyDuplicated(x$OBJECTID_1)
  }

  restore_snapshot <- function(row) {
    path <- file.path(raw_dir, row$name[[1]])
    result <- try(
      googledrive::drive_download(row, path = path, overwrite = TRUE),
      silent = TRUE
    )

    if (!inherits(result, "try-error") && valid_snapshot(path))
      return(path)

    if (file.exists(path)) unlink(path)
    NA_character_
  }

  drive_files <- googledrive::drive_ls(drive_folder, n_max = Inf)
  cat("EPA FRS RMP source and snapshot helpers ready.\n")


  # Restore today's persisted snapshot
  today <- drive_files[drive_files$name == file_name, ]

  if (nrow(today) > 1)
    stop("Multiple copies of today's RMP snapshot found on Google Drive.")

  if (nrow(today) == 1) {
    restored <- restore_snapshot(today)

    if (!is.na(restored)) {
      cat("Today's EPA FRS RMP snapshot restored from Google Drive.\n")
      return(restored)
    }
  }

  cat("No valid same-day RMP snapshot found.\n")


  # Define retrying EPA ArcGIS readers
  read_json <- function(url) {
    for (attempt in 1:3) {
      tmp <- tempfile(fileext = ".json")

      result <- suppressWarnings(try(
        utils::download.file(
          url, tmp, mode = "wb",
          method = "libcurl", quiet = TRUE
        ),
        silent = TRUE
      ))

      if (
        !inherits(result, "try-error") &&
        file.exists(tmp) &&
        file.info(tmp)$size > 0
      ) {
        response <- try(jsonlite::fromJSON(tmp), silent = TRUE)
        unlink(tmp)

        if (!inherits(response, "try-error"))
          return(response)
      } else {
        unlink(tmp)
      }

      Sys.sleep(2 * attempt)
    }

    stop("EPA ArcGIS JSON request failed.")
  }

  read_geojson <- function(url) {
    for (attempt in 1:3) {
      tmp <- tempfile(fileext = ".geojson")

      result <- suppressWarnings(try(
        utils::download.file(
          url, tmp, mode = "wb",
          method = "libcurl", quiet = TRUE
        ),
        silent = TRUE
      ))

      if (
        !inherits(result, "try-error") &&
        file.exists(tmp) &&
        file.info(tmp)$size > 100
      ) {
        x <- try(sf::st_read(tmp, quiet = TRUE), silent = TRUE)
        unlink(tmp)

        if (!inherits(x, "try-error") && nrow(x))
          return(x)
      } else {
        unlink(tmp)
      }

      Sys.sleep(2 * attempt)
    }

    stop("EPA ArcGIS GeoJSON request failed.")
  }

  cat("RMP ArcGIS readers ready.\n")


  # Attempt current EPA RMP download
  live <- try({
    where <- utils::URLencode(
      paste0("LOC_STATE='", state_abbr, "'"),
      reserved = TRUE
    )

    ids_response <- read_json(
      paste0(
        service_url,
        "?where=", where,
        "&returnIdsOnly=true&f=json"
      )
    )

    if (!is.null(ids_response$error) || !length(ids_response$objectIds))
      stop("EPA FRS RMP feature-ID request failed.")

    object_ids <- sort(unique(ids_response$objectIds))
    batches <- split(object_ids, ceiling(seq_along(object_ids) / 100))

    rows <- lapply(batches, function(ids) {
      read_geojson(
        paste0(
          service_url,
          "?objectIds=", paste(ids, collapse = "%2C"),
          "&outFields=%2A",
          "&returnGeometry=true",
          "&outSR=4326",
          "&f=geojson"
        )
      )
    })

    raw <- do.call(rbind, rows)
    rownames(raw) <- NULL

    if (!nrow(raw) || nrow(raw) != length(object_ids))
      stop("RMP feature count mismatch.")

    missing <- setdiff(required, names(raw))
    if (length(missing))
      stop("Missing RMP fields: ", paste(missing, collapse = ", "))

    if (anyDuplicated(raw$OBJECTID_1))
      stop("Duplicate RMP ArcGIS object IDs found.")

    state <- toupper(trimws(as.character(raw$LOC_STATE)))
    if (any(!is.na(state) & state != toupper(state_abbr)))
      stop("RMP extract contains records outside requested state.")

    raw
  }, silent = TRUE)

  if (!inherits(live, "try-error")) {
    if (file.exists(local_file)) unlink(local_file)

    sf::st_write(
      live, local_file,
      driver = "GeoJSON",
      quiet = TRUE
    )

    googledrive::drive_upload(
      local_file,
      path = drive_folder,
      overwrite = TRUE
    )

    cat("Current EPA FRS RMP snapshot downloaded and persisted.\n")
    return(local_file)
  }

  cat("EPA RMP service unavailable; prior snapshot fallback required.\n")


  # Restore newest valid prior snapshot
  prior <- drive_files[
    grepl(
      "^epa_frs_rmp_access_[0-9]{8}\\.geojson$",
      drive_files$name
    ),
  ]

  if (nrow(prior)) {
    prior$snapshot_date <- sub(
      "^epa_frs_rmp_access_([0-9]{8})\\.geojson$",
      "\\1",
      prior$name
    )

    prior <- prior[
      order(prior$snapshot_date, decreasing = TRUE),
    ]

    for (i in seq_len(nrow(prior))) {
      restored <- restore_snapshot(prior[i, ])

      if (!is.na(restored)) {
        cat(
          "EPA unavailable; restored RMP snapshot from",
          prior$snapshot_date[i], "\n"
        )
        return(restored)
      }
    }
  }

  stop("EPA RMP service unavailable and no valid persisted snapshot exists.")
}


build_rmp_tables <- function(
  raw_file, county_file, target_county_fips,
  target_crs, output_dir, validation_dir
) {

  # Load and validate RMP inputs
  sites <- sf::st_read(raw_file, quiet = TRUE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(sites) || !nrow(counties))
    stop("RMP or county input is empty.")

  access_date <- sub(
    ".*access_([0-9]{8}).*",
    "\\1",
    basename(raw_file)
  )

  if (!grepl("^[0-9]{8}$", access_date))
    stop("Could not determine RMP snapshot access date.")

  source_record_count <- nrow(sites)
  cat("RMP snapshot and TIGER counties loaded.\n")


  # Validate RMP geometries
  empty_geometry <- sf::st_is_empty(sites)
  empty_count <- sum(empty_geometry)
  sites <- sites[!empty_geometry, ]

  if (!nrow(sites))
    stop("No RMP geometries remain.")

  invalid_before <- sum(!sf::st_is_valid(sites))
  sites <- sf::st_make_valid(sites)
  invalid_after <- sum(!sf::st_is_valid(sites))

  if (invalid_after)
    stop("RMP geometries remain invalid.")

  geometry_types <- as.character(sf::st_geometry_type(sites))
  if (any(!geometry_types %in% c("POINT", "MULTIPOINT")))
    stop("RMP source contains non-point geometries.")

  cat("RMP point geometries validated.\n")


  # Define RMP field helpers
  clean_text <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | x == ""] <- NA_character_
    x
  }

  get_field <- function(data, field) {
    if (!field %in% names(data))
      return(rep(NA_character_, nrow(data)))
    clean_text(data[[field]])
  }

  get_numeric <- function(data, field) {
    if (!field %in% names(data))
      return(rep(NA_real_, nrow(data)))
    suppressWarnings(as.numeric(data[[field]]))
  }

  get_flag <- function(data, field) {
    x <- get_numeric(data, field)
    ifelse(is.na(x), NA, x == 1)
  }

  cat("RMP field helpers ready.\n")


  # Standardize RMP facilities
  registry_id <- get_field(sites, "REG_ID")
  object_id <- get_field(sites, "OBJECTID_1")

  base_id <- ifelse(
    !is.na(registry_id),
    paste0("FRS_", registry_id),
    paste0("OID_", object_id)
  )

  duplicate_base <-
    duplicated(base_id) |
    duplicated(base_id, fromLast = TRUE)

  rmp_site_id <- ifelse(
    duplicate_base,
    paste0(base_id, "_OID_", object_id),
    base_id
  )

  state_fips <- get_field(sites, "STATE_FIPS")
  county_fips <- get_field(sites, "CNTY_FIPS")
  state_county_fips <- get_field(sites, "STCOFIPS")

  fallback_fips <- ifelse(
    !is.na(state_fips) & !is.na(county_fips),
    paste0(state_fips, county_fips),
    NA_character_
  )

  state_county_fips <- ifelse(
    is.na(state_county_fips),
    fallback_fips,
    state_county_fips
  )

  sites_wgs <- sf::st_transform(sites, 4326)
  coordinates <- sf::st_coordinates(sites_wgs)

  if (nrow(coordinates) != nrow(sites_wgs))
    stop("RMP geometry does not resolve to one point per facility.")

  longitude <- coordinates[, 1]
  latitude <- coordinates[, 2]

  valid_coordinates <-
    is.finite(longitude) &
    is.finite(latitude) &
    longitude >= -180 & longitude <= 180 &
    latitude >= -90 & latitude <= 90

  invalid_coordinate_count <- sum(!valid_coordinates)

  if (invalid_coordinate_count)
    stop("RMP source contains invalid coordinates.")

  standardized <- data.frame(
    rmp_site_id = rmp_site_id,
    frs_registry_id = registry_id,
    facility_name = get_field(sites, "FAC_NAME"),
    address = get_field(sites, "LOC_ADD"),
    city = get_field(sites, "LOC_CITY"),
    state = get_field(sites, "LOC_STATE"),
    zip = get_field(sites, "LOC_ZIP"),
    reported_state_fips = state_fips,
    reported_county_fips = county_fips,
    reported_state_county_fips = state_county_fips,
    source_latitude = get_numeric(sites, "LATITUDE"),
    source_longitude = get_numeric(sites, "LONGITUDE"),
    longitude = longitude,
    latitude = latitude,
    rmp = get_flag(sites, "RMP"),
    tri = get_flag(sites, "TRI"),
    air_major = get_flag(sites, "AIR_MAJOR"),
    rcra_tsd = get_flag(sites, "RCRA_TSD"),
    rcra_lqg = get_flag(sites, "RCRA_LQG"),
    npdes_major = get_flag(sites, "NPDES_MAJ"),
    cer_npl = get_flag(sites, "CER_NPL"),
    brownfield = get_flag(sites, "BROWNFIELD"),
    snapshot_access_date = access_date,
    stringsAsFactors = FALSE
  )

  sites_sf <- sf::st_sf(
    standardized,
    geometry = sf::st_geometry(sites_wgs),
    crs = 4326
  )

  sites_sf <- sf::st_transform(sites_sf, target_crs)
  cat("RMP facility attributes standardized.\n")


  # Assign facilities to TIGER counties
  counties_projected <- sf::st_transform(counties, target_crs)
  intersections <- sf::st_intersects(sites_sf, counties_projected)

  collapse_county <- function(index, field) {
    if (!length(index)) return(NA_character_)
    paste(
      sort(unique(as.character(counties_projected[[field]][index]))),
      collapse = ";"
    )
  }

  sites_sf$county_geoids <- vapply(
    intersections,
    collapse_county,
    character(1),
    field = "GEOID"
  )

  sites_sf$census_counties <- vapply(
    intersections,
    collapse_county,
    character(1),
    field = "NAME"
  )

  sites_sf$county_count <- lengths(intersections)
  spatial_nc <- sites_sf$county_count > 0

  cat("RMP facilities spatially assigned to TIGER counties.\n")


  # Audit source and spatial county assignments
  reported_available <-
    !is.na(sites_sf$reported_state_county_fips) &
    sites_sf$reported_state_county_fips != ""

  county_match <- vapply(
    seq_len(nrow(sites_sf)),
    function(i) {
      if (!reported_available[i] || is.na(sites_sf$county_geoids[i]))
        return(NA)

      sites_sf$reported_state_county_fips[i] %in%
        strsplit(sites_sf$county_geoids[i], ";", fixed = TRUE)[[1]]
    },
    logical(1)
  )

  county_mismatch <-
    reported_available &
    !is.na(county_match) &
    !county_match

  cat("RMP county geography audited.\n")


  # Build statewide and Mecklenburg layers
  nc_sf <- sites_sf[spatial_nc, ]

  nc_sf$intersects_mecklenburg <- vapply(
    strsplit(
      ifelse(is.na(nc_sf$county_geoids), "", nc_sf$county_geoids),
      ";",
      fixed = TRUE
    ),
    function(x) target_county_fips %in% x,
    logical(1)
  )

  mecklenburg_sf <- nc_sf[nc_sf$intersects_mecklenburg, ]
  nc_table <- sf::st_drop_geometry(nc_sf)
  mecklenburg_table <- sf::st_drop_geometry(mecklenburg_sf)

  rownames(nc_table) <- NULL
  rownames(mecklenburg_table) <- NULL

  cat("Statewide and Mecklenburg RMP layers built.\n")


  # Build geography discrepancies
  discrepancy_type <- rep(NA_character_, nrow(sites_sf))
  discrepancy_type[!spatial_nc] <- "outside_spatial_nc"

  discrepancy_type[county_mismatch] <- ifelse(
    is.na(discrepancy_type[county_mismatch]),
    "reported_spatial_fips_mismatch",
    paste0(
      discrepancy_type[county_mismatch],
      ";reported_spatial_fips_mismatch"
    )
  )

  multiple_counties <- sites_sf$county_count > 1

  discrepancy_type[multiple_counties] <- ifelse(
    is.na(discrepancy_type[multiple_counties]),
    "multiple_county_intersections",
    paste0(
      discrepancy_type[multiple_counties],
      ";multiple_county_intersections"
    )
  )

  discrepancies <- sf::st_drop_geometry(
    sites_sf[!is.na(discrepancy_type), ]
  )

  discrepancies$discrepancy_type <-
    discrepancy_type[!is.na(discrepancy_type)]

  cat("RMP discrepancy table built.\n")


  # Validate statewide RMP records
  registry_values <- nc_table$frs_registry_id[
    !is.na(nc_table$frs_registry_id)
  ]

  duplicate_registry_ids <- sum(duplicated(registry_values))
  duplicate_site_ids <- sum(duplicated(nc_table$rmp_site_id))
  missing_registry_ids <- sum(is.na(nc_table$frs_registry_id))
  missing_rmp_flag <- sum(is.na(nc_table$rmp))
  non_rmp_flag <- sum(nc_table$rmp %in% FALSE)

  qa <- data.frame(
    check = c(
      "Source NC-reported RMP records",
      "Valid RMP point records",
      "Empty geometries removed",
      "Invalid geometries before repair",
      "Invalid geometries after repair",
      "Invalid RMP coordinates",
      "Spatial NC RMP facilities",
      "RMP facilities outside spatial NC",
      "Duplicate RMP site IDs",
      "Duplicate FRS registry IDs",
      "Missing FRS registry IDs",
      "Reported county FIPS available",
      "Reported versus spatial county mismatches",
      "Facilities intersecting multiple counties",
      "RMP facilities missing RMP flag",
      "Facilities without positive RMP flag",
      "Mecklenburg spatial RMP facilities",
      "NC RMP facilities also flagged TRI",
      "NC RMP facilities also flagged RCRA LQG",
      "NC RMP facilities also flagged RCRA TSD"
    ),
    value = c(
      source_record_count,
      nrow(sites),
      empty_count,
      invalid_before,
      invalid_after,
      invalid_coordinate_count,
      nrow(nc_table),
      sum(!spatial_nc),
      duplicate_site_ids,
      duplicate_registry_ids,
      missing_registry_ids,
      sum(reported_available[spatial_nc]),
      sum(county_mismatch[spatial_nc]),
      sum(sites_sf$county_count[spatial_nc] > 1),
      missing_rmp_flag,
      non_rmp_flag,
      nrow(mecklenburg_table),
      sum(nc_table$tri %in% TRUE),
      sum(nc_table$rcra_lqg %in% TRUE),
      sum(nc_table$rcra_tsd %in% TRUE)
    ),
    status = c(
      "INFO",
      "INFO",
      ifelse(empty_count == 0, "PASS", "CHECK"),
      ifelse(invalid_before == 0, "PASS", "CHECK"),
      ifelse(invalid_after == 0, "PASS", "FAIL"),
      ifelse(invalid_coordinate_count == 0, "PASS", "FAIL"),
      "INFO",
      ifelse(sum(!spatial_nc) == 0, "PASS", "CHECK"),
      ifelse(duplicate_site_ids == 0, "PASS", "FAIL"),
      ifelse(duplicate_registry_ids == 0, "PASS", "CHECK"),
      ifelse(missing_registry_ids == 0, "PASS", "CHECK"),
      "INFO",
      ifelse(sum(county_mismatch[spatial_nc]) == 0, "PASS", "CHECK"),
      ifelse(sum(sites_sf$county_count[spatial_nc] > 1) == 0, "PASS", "CHECK"),
      ifelse(missing_rmp_flag == 0, "PASS", "CHECK"),
      ifelse(non_rmp_flag == 0, "PASS", "FAIL"),
      "INFO", "INFO", "INFO", "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("RMP geography and attribute QA/QC completed.\n")


  # Build RMP source metadata
  metadata <- data.frame(
    field = c(
      "source", "publisher", "program", "service", "layer",
      "dataset_type", "snapshot_access_date", "temporal_scope",
      "historical_2022_inventory", "geometry", "state_filter",
      "program_flag", "spatial_assignment", "scope_note"
    ),
    value = c(
      "FRSrmp",
      "US Environmental Protection Agency",
      "Risk Management Program",
      "ER_R4/facilitiesR4_all",
      "11",
      "EPA FRS facilities flagged for Risk Management Program",
      access_date,
      paste0("EPA FRS geospatial snapshot accessed ", access_date),
      "FALSE",
      "Facility point locations",
      "LOC_STATE = NC",
      "RMP",
      "2022 TIGER county intersection",
      paste(
        "RMP facilities are not a complete inventory of every",
        "hazardous-material or chemical-storage facility.",
        "The EPA geospatial layer omits facilities without",
        "latitude/longitude coordinates and does not indicate",
        "that a chemical release occurred."
      )
    ),
    stringsAsFactors = FALSE
  )

  cat("RMP source metadata built.\n")


  # Write RMP processed outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  nc_csv <- file.path(
    output_dir,
    paste0("nc_epa_rmp_facilities_access_", access_date, ".csv")
  )

  mecklenburg_csv <- file.path(
    output_dir,
    paste0("mecklenburg_epa_rmp_facilities_access_", access_date, ".csv")
  )

  nc_gpkg <- file.path(
    output_dir,
    paste0("nc_epa_rmp_facilities_access_", access_date, ".gpkg")
  )

  mecklenburg_gpkg <- file.path(
    output_dir,
    paste0("mecklenburg_epa_rmp_facilities_access_", access_date, ".gpkg")
  )

  metadata_file <- file.path(
    output_dir,
    paste0("epa_rmp_metadata_access_", access_date, ".csv")
  )

  discrepancy_file <- file.path(
    validation_dir,
    paste0("rmp_geography_discrepancies_access_", access_date, ".csv")
  )

  qa_file <- file.path(
    validation_dir,
    paste0("qaqc_rmp_access_", access_date, ".csv")
  )

  write.csv(nc_table, nc_csv, row.names = FALSE)
  write.csv(mecklenburg_table, mecklenburg_csv, row.names = FALSE)

  sf::st_write(
    nc_sf, nc_gpkg,
    delete_dsn = TRUE,
    quiet = TRUE
  )

  sf::st_write(
    mecklenburg_sf, mecklenburg_gpkg,
    delete_dsn = TRUE,
    quiet = TRUE
  )

  write.csv(metadata, metadata_file, row.names = FALSE)
  write.csv(discrepancies, discrepancy_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("RMP processed outputs written.\n")

  c(
    nc_csv,
    mecklenburg_csv,
    nc_gpkg,
    mecklenburg_gpkg,
    metadata_file,
    discrepancy_file,
    qa_file
  )
}


upload_rmp_outputs <- function(
  files, processed_folder,
  metadata_folder, validation_folder
) {

  # Validate RMP outputs
  if (length(files) != 7 || !all(file.exists(files)))
    stop("Expected seven RMP output files.")

  cat("RMP output files validated.\n")


  # Upload processed RMP datasets
  invisible(lapply(
    files[1:4],
    googledrive::drive_upload,
    path = processed_folder,
    overwrite = TRUE
  ))

  cat("Processed RMP datasets uploaded.\n")


  # Upload RMP metadata
  googledrive::drive_upload(
    files[[5]],
    path = metadata_folder,
    overwrite = TRUE
  )

  cat("RMP metadata uploaded.\n")


  # Upload RMP validation outputs
  invisible(lapply(
    files[6:7],
    googledrive::drive_upload,
    path = validation_folder,
    overwrite = TRUE
  ))

  cat("RMP QA outputs uploaded.\n")

  TRUE
}