build_ian_wind_exposure <- function(
  ian_track_files,
  tri_fips_files,
  year,
  target_county_fips,
  output_dir,
  validation_dir
) {

  # Load statewide TRI facilities and Hurricane Ian observations
  track_candidates <- ian_track_files[
    file.exists(ian_track_files) &
      grepl("\\.csv$", ian_track_files, ignore.case = TRUE) &
      !grepl("qaqc|metadata", basename(ian_track_files), ignore.case = TRUE)
  ]

  preferred_track <- track_candidates[
    grepl("track", basename(track_candidates), ignore.case = TRUE)
  ]

  track_file <- if (length(preferred_track)) {
    preferred_track[[1]]
  } else {
    track_candidates[[1]]
  }

  tri_file <- tri_fips_files[
    basename(tri_fips_files) ==
      paste0("nc_tri_facility_fips_", year, ".csv")
  ][1]

  if (
    is.na(track_file) ||
    !file.exists(track_file) ||
    is.na(tri_file) ||
    !file.exists(tri_file)
  ) stop("Ian track or statewide TRI facility input is missing.")

  track <- read.csv(
    track_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  facilities <- read.csv(
    tri_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!nrow(track) || !nrow(facilities))
    stop("Ian track or statewide TRI facility input is empty.")

  if (anyDuplicated(facilities$tri_facility_id))
    stop("Duplicate statewide TRI facility IDs.")

  facilities$county_geoid <- sprintf(
    "%05d",
    as.integer(facilities$county_geoid)
  )

  mecklenburg_count <- sum(
    facilities$county_geoid == target_county_fips
  )

  cat(
    "Ian track and statewide TRI facilities loaded:",
    nrow(facilities),
    "facilities.\n"
  )


  # Resolve and standardize HURDAT2 wind-radius fields
  normalize <- function(x) {
    gsub("[^a-z0-9]", "", tolower(x))
  }

  resolve_column <- function(candidates, label) {
    names_normalized <- normalize(names(track))
    hits <- which(
      names_normalized %in% normalize(candidates)
    )

    if (length(hits) != 1)
      stop("Could not uniquely resolve ", label, ".")

    names(track)[hits]
  }

  resolve_radius <- function(speed, quadrant) {
    names_normalized <- normalize(names(track))

    candidates <- normalize(c(
      paste0("r", speed, "_", quadrant),
      paste0("radius_", speed, "_", quadrant),
      paste0("wind_", speed, "_", quadrant),
      paste0(quadrant, "_", speed)
    ))

    hits <- which(names_normalized %in% candidates)

    if (length(hits) == 1)
      return(names(track)[hits])

    hits <- which(
      grepl(as.character(speed), names_normalized, fixed = TRUE) &
        grepl(tolower(quadrant), names_normalized, fixed = TRUE)
    )

    if (length(hits) != 1)
      stop(
        "Could not uniquely resolve ",
        speed,
        "kt ",
        quadrant,
        " radius."
      )

    names(track)[hits]
  }

  latitude_field <- resolve_column(
    c("latitude", "lat"),
    "Ian latitude"
  )

  longitude_field <- resolve_column(
    c("longitude", "lon", "long"),
    "Ian longitude"
  )

  datetime_field <- resolve_column(
    c(
      "datetime_utc",
      "datetime",
      "timestamp_utc",
      "timestamp"
    ),
    "Ian datetime"
  )

  for (speed in c(34, 50, 64)) {
    for (quadrant in c("NE", "SE", "SW", "NW")) {
      source <- resolve_radius(speed, quadrant)
      target <- paste0(
        "r",
        speed,
        "_",
        tolower(quadrant)
      )

      track[[target]] <- suppressWarnings(
        as.numeric(track[[source]])
      )

      track[[target]][track[[target]] < 0] <- NA_real_
    }
  }

  cat("HURDAT2 wind-radius fields standardized.\n")


  # Validate Ian observations and statewide TRI coordinates
  track$latitude <- suppressWarnings(
    as.numeric(track[[latitude_field]])
  )

  track$longitude <- suppressWarnings(
    as.numeric(track[[longitude_field]])
  )

  track$datetime_utc <- as.POSIXct(
    track[[datetime_field]],
    tz = "UTC"
  )

  if (
    any(!is.finite(track$latitude)) ||
    any(!is.finite(track$longitude)) ||
    any(is.na(track$datetime_utc))
  ) stop("Ian observations contain invalid spatial or temporal values.")

  track <- track[
    order(track$datetime_utc),
    ,
    drop = FALSE
  ]

  facilities$latitude <- suppressWarnings(
    as.numeric(facilities$latitude)
  )

  facilities$longitude <- suppressWarnings(
    as.numeric(facilities$longitude)
  )

  if (
    any(!is.finite(facilities$latitude)) ||
    any(!is.finite(facilities$longitude))
  ) stop("TRI facilities contain invalid coordinates.")

  if (!"facility_name" %in% names(facilities))
    facilities$facility_name <- facilities$tri_facility_id

  cat("Ian observations and statewide TRI coordinates validated.\n")


  # Define geodesic distance and storm-quadrant calculations
  geodesic <- function(
    storm_lat,
    storm_lon,
    facility_lat,
    facility_lon
  ) {
    lat1 <- storm_lat * pi / 180
    lon1 <- storm_lon * pi / 180
    lat2 <- facility_lat * pi / 180
    lon2 <- facility_lon * pi / 180

    dlat <- lat2 - lat1
    dlon <- lon2 - lon1

    a <- sin(dlat / 2)^2 +
      cos(lat1) *
      cos(lat2) *
      sin(dlon / 2)^2

    a <- pmin(1, pmax(0, a))

    angle <- 2 * atan2(
      sqrt(a),
      sqrt(1 - a)
    )

    distance_nm <- 3440.065 * angle

    y <- sin(dlon) * cos(lat2)

    x <- cos(lat1) * sin(lat2) -
      sin(lat1) *
      cos(lat2) *
      cos(dlon)

    bearing <- (
      atan2(y, x) * 180 / pi + 360
    ) %% 360

    list(
      distance_nm = distance_nm,
      bearing = bearing
    )
  }

  quadrant <- function(x) {
    ifelse(
      x < 90,
      "NE",
      ifelse(
        x < 180,
        "SE",
        ifelse(x < 270, "SW", "NW")
      )
    )
  }

  cat("Ian wind-radius geometry helpers ready.\n")


  # Calculate statewide facility exposure at every HURDAT2 observation
  observation_rows <- vector(
    "list",
    nrow(track)
  )

  for (j in seq_len(nrow(track))) {
    metrics <- geodesic(
      track$latitude[j],
      track$longitude[j],
      facilities$latitude,
      facilities$longitude
    )

    q <- quadrant(metrics$bearing)

    radius_for <- function(speed) {
      radii <- c(
        NE = track[[paste0("r", speed, "_ne")]][j],
        SE = track[[paste0("r", speed, "_se")]][j],
        SW = track[[paste0("r", speed, "_sw")]][j],
        NW = track[[paste0("r", speed, "_nw")]][j]
      )

      unname(radii[q])
    }

    r34 <- radius_for(34)
    r50 <- radius_for(50)
    r64 <- radius_for(64)

    observation_rows[[j]] <- data.frame(
      tri_facility_id = facilities$tri_facility_id,
      facility_name = facilities$facility_name,
      county_geoid = facilities$county_geoid,
      observation_index = j,
      datetime_utc = format(
        track$datetime_utc[j],
        "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      storm_latitude = track$latitude[j],
      storm_longitude = track$longitude[j],
      facility_latitude = facilities$latitude,
      facility_longitude = facilities$longitude,
      distance_nm = metrics$distance_nm,
      distance_miles =
        metrics$distance_nm * 1.150779448,
      bearing_deg = metrics$bearing,
      quadrant = q,
      radius_34kt_nm = r34,
      radius_50kt_nm = r50,
      radius_64kt_nm = r64,
      inside_34kt = ifelse(
        is.na(r34),
        NA,
        metrics$distance_nm <= r34
      ),
      inside_50kt = ifelse(
        is.na(r50),
        NA,
        metrics$distance_nm <= r50
      ),
      inside_64kt = ifelse(
        is.na(r64),
        NA,
        metrics$distance_nm <= r64
      ),
      stringsAsFactors = FALSE
    )
  }

  observation_exposure <- do.call(
    rbind,
    observation_rows
  )

  rownames(observation_exposure) <- NULL

  cat(
    "Statewide TRI wind-radius observations calculated:",
    nrow(observation_exposure),
    "rows.\n"
  )


  # Summarize facility-level wind-radius exposure and duration
  summarize_any <- function(x) {
    if (any(x %in% TRUE, na.rm = TRUE))
      return(TRUE)

    if (any(!is.na(x)))
      return(FALSE)

    NA
  }

  estimate_hours <- function(times, flags) {
    times <- as.POSIXct(times, tz = "UTC")

    if (length(times) < 2)
      return(c(hours = NA_real_, coverage_pct = NA_real_))

    interval <- as.numeric(
      difftime(
        times[-1],
        times[-length(times)],
        units = "hours"
      )
    )

    valid <- is.finite(interval) & interval >= 0

    known <- valid &
      !is.na(flags[-length(flags)]) &
      !is.na(flags[-1])

    total_hours <- sum(interval[valid])
    known_hours <- sum(interval[known])

    exposure_hours <- if (known_hours > 0) {
      sum(
        interval[known] *
          (
            as.numeric(flags[-length(flags)][known]) +
              as.numeric(flags[-1][known])
          ) / 2
      )
    } else {
      NA_real_
    }

    coverage <- if (total_hours > 0) {
      100 * known_hours / total_hours
    } else {
      NA_real_
    }

    c(
      hours = exposure_hours,
      coverage_pct = coverage
    )
  }

  facility_rows <- lapply(
    seq_len(nrow(facilities)),
    function(i) {
      id <- facilities$tri_facility_id[i]

      rows <- observation_exposure[
        observation_exposure$tri_facility_id == id,
        ,
        drop = FALSE
      ]

      times <- as.POSIXct(
        rows$datetime_utc,
        tz = "UTC"
      )

      h34 <- estimate_hours(
        times,
        rows$inside_34kt
      )

      h50 <- estimate_hours(
        times,
        rows$inside_50kt
      )

      h64 <- estimate_hours(
        times,
        rows$inside_64kt
      )

      e34 <- summarize_any(rows$inside_34kt)
      e50 <- summarize_any(rows$inside_50kt)
      e64 <- summarize_any(rows$inside_64kt)

      max_threshold <- if (isTRUE(e64)) {
        64L
      } else if (isTRUE(e50)) {
        50L
      } else if (isTRUE(e34)) {
        34L
      } else if (identical(e34, FALSE)) {
        0L
      } else {
        NA_integer_
      }

      category <- if (is.na(max_threshold)) {
        "insufficient_34kt_radius_data"
      } else if (max_threshold == 64) {
        "hurricane_force_64kt"
      } else if (max_threshold == 50) {
        "strong_tropical_storm_50kt"
      } else if (max_threshold == 34) {
        "tropical_storm_34kt"
      } else {
        "outside_reported_wind_radii"
      }

      data.frame(
        tri_facility_id = id,
        facility_name = facilities$facility_name[i],
        county_geoid = facilities$county_geoid[i],
        latitude = facilities$latitude[i],
        longitude = facilities$longitude[i],
        ian_min_center_distance_nm =
          min(rows$distance_nm, na.rm = TRUE),
        ian_min_center_distance_miles =
          min(rows$distance_miles, na.rm = TRUE),
        ian_inside_34kt = e34,
        ian_inside_50kt = e50,
        ian_inside_64kt = e64,
        ian_max_wind_threshold_kt = max_threshold,
        ian_max_wind_category = category,
        ian_hours_34kt_est = unname(h34["hours"]),
        ian_hours_50kt_est = unname(h50["hours"]),
        ian_hours_64kt_est = unname(h64["hours"]),
        ian_34kt_temporal_coverage_pct =
          unname(h34["coverage_pct"]),
        ian_50kt_temporal_coverage_pct =
          unname(h50["coverage_pct"]),
        ian_64kt_temporal_coverage_pct =
          unname(h64["coverage_pct"]),
        stringsAsFactors = FALSE
      )
    }
  )

  facility_exposure <- do.call(
    rbind,
    facility_rows
  )

  rownames(facility_exposure) <- NULL

  cat("Statewide TRI wind-radius exposure summaries built.\n")


  # Validate statewide wind-radius results
  expected_rows <-
    nrow(facilities) * nrow(track)

  duplicate_rows <- sum(
    duplicated(
      observation_exposure[
        ,
        c(
          "tri_facility_id",
          "observation_index"
        )
      ]
    )
  )

  nesting_64_50 <- sum(
    observation_exposure$inside_64kt %in% TRUE &
      observation_exposure$inside_50kt %in% FALSE,
    na.rm = TRUE
  )

  nesting_50_34 <- sum(
    observation_exposure$inside_50kt %in% TRUE &
      observation_exposure$inside_34kt %in% FALSE,
    na.rm = TRUE
  )

  duration_violation <- with(
    facility_exposure,
    is.finite(ian_hours_64kt_est) &
      is.finite(ian_hours_50kt_est) &
      ian_hours_64kt_est >
        ian_hours_50kt_est + 1e-8 |
      is.finite(ian_hours_50kt_est) &
      is.finite(ian_hours_34kt_est) &
      ian_hours_50kt_est >
        ian_hours_34kt_est + 1e-8
  )

  duration_violations <- sum(
    duration_violation,
    na.rm = TRUE
  )

  safe_min <- function(x) {
    x <- x[is.finite(x)]
    if (length(x)) min(x) else NA_real_
  }

  qa <- data.frame(
    check = c(
      "Statewide NC TRI facilities",
      "Mecklenburg TRI facilities",
      "Ian HURDAT2 observations",
      "Expected facility-observation rows",
      "Actual facility-observation rows",
      "Duplicate facility-observation rows",
      "Facility wind-exposure summary rows",
      "Missing center-distance summaries",
      "Facilities inside reported 34kt radii",
      "Facilities inside reported 50kt radii",
      "Facilities inside reported 64kt radii",
      "Facilities with insufficient 34kt radius classification",
      "64kt outside 50kt nesting violations",
      "50kt outside 34kt nesting violations",
      "Exposure-hour nesting violations",
      "Minimum center distance miles",
      "Minimum 34kt temporal coverage percent",
      "Minimum 50kt temporal coverage percent",
      "Minimum 64kt temporal coverage percent"
    ),
    value = c(
      nrow(facilities),
      mecklenburg_count,
      nrow(track),
      expected_rows,
      nrow(observation_exposure),
      duplicate_rows,
      nrow(facility_exposure),
      sum(
        !is.finite(
          facility_exposure$ian_min_center_distance_miles
        )
      ),
      sum(
        facility_exposure$ian_inside_34kt %in% TRUE
      ),
      sum(
        facility_exposure$ian_inside_50kt %in% TRUE
      ),
      sum(
        facility_exposure$ian_inside_64kt %in% TRUE
      ),
      sum(
        is.na(
          facility_exposure$ian_max_wind_threshold_kt
        )
      ),
      nesting_64_50,
      nesting_50_34,
      duration_violations,
      round(
        safe_min(
          facility_exposure$ian_min_center_distance_miles
        ),
        2
      ),
      round(
        safe_min(
          facility_exposure$ian_34kt_temporal_coverage_pct
        ),
        2
      ),
      round(
        safe_min(
          facility_exposure$ian_50kt_temporal_coverage_pct
        ),
        2
      ),
      round(
        safe_min(
          facility_exposure$ian_64kt_temporal_coverage_pct
        ),
        2
      )
    ),
    status = c(
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(
        nrow(observation_exposure) == expected_rows,
        "PASS",
        "FAIL"
      ),
      ifelse(
        duplicate_rows == 0,
        "PASS",
        "FAIL"
      ),
      ifelse(
        nrow(facility_exposure) == nrow(facilities),
        "PASS",
        "FAIL"
      ),
      ifelse(
        all(
          is.finite(
            facility_exposure$ian_min_center_distance_miles
          )
        ),
        "PASS",
        "FAIL"
      ),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(
        nesting_64_50 == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        nesting_50_34 == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        duration_violations == 0,
        "PASS",
        "CHECK"
      ),
      "INFO",
      "INFO",
      "INFO",
      "INFO"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("Statewide Ian wind-radius QA/QC completed.\n")


  # Write statewide Ian wind-radius outputs
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    validation_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  facility_file <- file.path(
    output_dir,
    paste0(
      "nc_tri_ian_wind_exposure_",
      year,
      ".csv"
    )
  )

  observation_file <- file.path(
    output_dir,
    paste0(
      "nc_tri_ian_wind_observations_",
      year,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_nc_ian_wind_exposure_",
      year,
      ".csv"
    )
  )

  write.csv(
    facility_exposure,
    facility_file,
    row.names = FALSE
  )

  write.csv(
    observation_exposure,
    observation_file,
    row.names = FALSE
  )

  write.csv(
    qa,
    qa_file,
    row.names = FALSE
  )

  cat("Statewide Ian wind-radius outputs written.\n")

  c(
    facility_file,
    observation_file,
    qa_file
  )
}


upload_ian_wind_exposure <- function(
  files,
  processed_folder,
  validation_folder
) {

  # Validate and upload statewide Ian wind-radius outputs
  if (
    length(files) != 3 ||
    !all(file.exists(files))
  ) stop("Expected three Ian wind-exposure output files.")

  invisible(
    lapply(
      files[1:2],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  googledrive::drive_upload(
    files[[3]],
    path = validation_folder,
    overwrite = TRUE
  )

  cat("Statewide Ian wind-radius outputs uploaded.\n")

  TRUE
}