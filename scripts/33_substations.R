download_substations_raw <- function(raw_dir, drive_folder, state_abbr) {
  # Define the October 2022 electric-substation source
  layer_url <- paste0(
    "https://services.arcgis.com/2DbqGRRQS9wbBetw/",
    "ArcGIS/rest/services/Electric_Substations_OCT_2022/FeatureServer/0"
  )

  query_url <- paste0(layer_url, "/query")
  access_date <- format(Sys.Date(), "%Y%m%d")

  file_name <- paste0(
    "hifld_substations_oct2022_access_",
    access_date,
    ".geojson"
  )

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  local_file <- file.path(raw_dir, file_name)

  cat("October 2022 electric-substation source defined.\n")


  # Restore today's persisted substation snapshot
  drive_files <- googledrive::drive_ls(drive_folder)
  drive_match <- drive_files[drive_files$name == file_name, ]

  if (nrow(drive_match) > 1) {
    stop("Multiple copies of today's substation snapshot found on Google Drive.")
  }

  if (nrow(drive_match) == 1) {
    googledrive::drive_download(
      drive_match,
      path = local_file,
      overwrite = TRUE
    )

    restored <- try(
      sf::st_read(local_file, quiet = TRUE),
      silent = TRUE
    )

    if (!inherits(restored, "try-error") && nrow(restored) > 0) {
      cat("Electric-substation snapshot restored from Google Drive.\n")
      return(local_file)
    }
  }

  cat("No valid persisted substation snapshot found.\n")


  # Define a robust ArcGIS JSON reader
  read_arcgis_json <- function(url) {
    temp_file <- tempfile(fileext = ".json")
    response <- NULL
    success <- FALSE

    for (attempt in seq_len(3)) {
      result <- try(
        utils::download.file(
          url,
          temp_file,
          mode = "wb",
          method = "libcurl",
          quiet = TRUE
        ),
        silent = TRUE
      )

      if (
        !inherits(result, "try-error") &&
        file.exists(temp_file) &&
        file.info(temp_file)$size > 20
      ) {
        response <- try(
          jsonlite::fromJSON(
            temp_file,
            simplifyVector = FALSE
          ),
          silent = TRUE
        )

        if (!inherits(response, "try-error")) {
          success <- TRUE
          break
        }
      }

      Sys.sleep(2 * attempt)
    }

    unlink(temp_file)

    if (!success) {
      stop("ArcGIS JSON request failed.")
    }

    if (!is.null(response$error)) {
      message_text <- if (!is.null(response$error$message)) {
        response$error$message
      } else {
        "Unknown ArcGIS service error"
      }

      stop(paste("ArcGIS service error:", message_text))
    }

    response
  }

  cat("Substation ArcGIS JSON reader ready.\n")


  # Read layer schema and resolve required fields
  metadata <- read_arcgis_json(
    paste0(layer_url, "?f=json")
  )

  if (is.null(metadata$fields) || !length(metadata$fields)) {
    stop("Substation layer metadata contains no fields.")
  }

  field_names <- vapply(
    metadata$fields,
    function(x) {
      if (is.null(x$name)) "" else as.character(x$name)
    },
    character(1)
  )

  oid_field <- if (!is.null(metadata$objectIdField)) {
    as.character(metadata$objectIdField)
  } else {
    oid_candidates <- vapply(
      metadata$fields,
      function(x) {
        if (
          !is.null(x$type) &&
          identical(x$type, "esriFieldTypeOID")
        ) {
          as.character(x$name)
        } else {
          ""
        }
      },
      character(1)
    )

    oid_candidates <- oid_candidates[oid_candidates != ""]

    if (!length(oid_candidates)) {
      stop("Could not determine substation object-ID field.")
    }

    oid_candidates[[1]]
  }

  state_candidates <- c(
    "STATE",
    "State",
    "ST_ABBREV",
    "STATE_ABBR"
  )

  state_field <- state_candidates[
    state_candidates %in% field_names
  ][1]

  if (is.na(state_field)) {
    stop("Could not determine substation state field.")
  }

  cat("Substation ArcGIS schema resolved.\n")


  # Define ArcGIS feature-page conversion
  feature_page_to_sf <- function(response, oid_field) {
    features <- response$features

    if (is.null(features) || !length(features)) {
      return(NULL)
    }

    attribute_rows <- lapply(
      features,
      function(feature) {
        attributes <- feature$attributes

        if (is.null(attributes)) {
          attributes <- list()
        }

        as.data.frame(
          attributes,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    )

    all_names <- unique(
      unlist(
        lapply(attribute_rows, names),
        use.names = FALSE
      )
    )

    attribute_rows <- lapply(
      attribute_rows,
      function(row) {
        missing_names <- setdiff(
          all_names,
          names(row)
        )

        for (name in missing_names) {
          row[[name]] <- NA
        }

        row[, all_names, drop = FALSE]
      }
    )

    attributes <- do.call(
      rbind,
      attribute_rows
    )

    get_geometry_value <- function(feature, name) {
      if (
        is.null(feature$geometry) ||
        is.null(feature$geometry[[name]])
      ) {
        return(NA_real_)
      }

      suppressWarnings(
        as.numeric(feature$geometry[[name]])
      )
    }

    geometry_x <- vapply(
      features,
      get_geometry_value,
      numeric(1),
      name = "x"
    )

    geometry_y <- vapply(
      features,
      get_geometry_value,
      numeric(1),
      name = "y"
    )

    if (
      any(!is.finite(geometry_x)) ||
      any(!is.finite(geometry_y))
    ) {
      stop("Substation page contains missing or invalid point geometry.")
    }

    if (!oid_field %in% names(attributes)) {
      stop("Substation page is missing its object-ID field.")
    }

    attributes$source_object_id <- as.character(
      attributes[[oid_field]]
    )

    attributes$geometry_x <- geometry_x
    attributes$geometry_y <- geometry_y

    sf::st_as_sf(
    attributes,
    coords = c("geometry_x", "geometry_y"),
    crs = 4326,
    remove = TRUE
    )

  }

  cat("Substation ArcGIS feature converter ready.\n")


  # Find the valid North Carolina state query
  state_full <- state.name[
    match(
      toupper(state_abbr),
      state.abb
    )
  ]

  state_values <- unique(
    c(
      state_abbr,
      if (!is.na(state_full)) state_full else NULL
    )
  )

  first_page <- NULL
  selected_state_value <- NULL

  for (state_value in state_values) {
    where <- utils::URLencode(
      paste0(
        state_field,
        "='",
        state_value,
        "'"
      ),
      reserved = TRUE
    )

    test_url <- paste0(
      query_url,
      "?where=",
      where,
      "&outFields=%2A",
      "&returnGeometry=true",
      "&outSR=4326",
      "&orderByFields=",
      utils::URLencode(oid_field, reserved = TRUE),
      "&resultOffset=0",
      "&resultRecordCount=2000",
      "&f=json"
    )

    test_response <- read_arcgis_json(
      test_url
    )

    candidate <- feature_page_to_sf(
      test_response,
      oid_field
    )

    if (!is.null(candidate) && nrow(candidate) > 0) {
      first_page <- candidate
      selected_state_value <- state_value
      break
    }
  }

  if (is.null(first_page)) {
    stop("Substation service returned no North Carolina records.")
  }

  cat("North Carolina substation query validated.\n")


  # Download all North Carolina substations using pagination
  page_size <- 2000L
  page_rows <- list(first_page)
  page_index <- 1L

  if (nrow(first_page) == page_size) {
    offset <- page_size

    repeat {
      where <- utils::URLencode(
        paste0(
          state_field,
          "='",
          selected_state_value,
          "'"
        ),
        reserved = TRUE
      )

      page_url <- paste0(
        query_url,
        "?where=",
        where,
        "&outFields=%2A",
        "&returnGeometry=true",
        "&outSR=4326",
        "&orderByFields=",
        utils::URLencode(oid_field, reserved = TRUE),
        "&resultOffset=",
        offset,
        "&resultRecordCount=",
        page_size,
        "&f=json"
      )

      response <- read_arcgis_json(
        page_url
      )

      page <- feature_page_to_sf(
        response,
        oid_field
      )

      if (is.null(page) || !nrow(page)) {
        break
      }

      page_index <- page_index + 1L
      page_rows[[page_index]] <- page

      if (nrow(page) < page_size) {
        break
      }

      offset <- offset + page_size
    }
  }

  raw <- do.call(
    rbind,
    page_rows
  )

  rownames(raw) <- NULL

  cat(
    "North Carolina substation records downloaded:",
    nrow(raw),
    "\n"
  )


  # Validate the downloaded North Carolina substation layer
  if (!nrow(raw)) {
    stop("Combined substation extract is empty.")
  }

  if (anyDuplicated(raw$source_object_id)) {
    stop("Duplicate substation source object IDs found.")
  }

  if (state_field %in% names(raw)) {
    downloaded_state <- toupper(
      trimws(
        as.character(
          raw[[state_field]]
        )
      )
    )

    valid_state_values <- toupper(
      c(
        state_abbr,
        if (!is.na(state_full)) state_full else NULL
      )
    )

    outside_state <- sum(
      !is.na(downloaded_state) &
        !downloaded_state %in% valid_state_values
    )

    if (outside_state > 0) {
      stop("Substation extract contains records outside North Carolina.")
    }
  }

  geometry_types <- as.character(
    sf::st_geometry_type(raw)
  )

  if (
    any(
      !geometry_types %in%
        c("POINT", "MULTIPOINT")
    )
  ) {
    stop("Substation raw extract contains non-point geometry.")
  }

  cat("North Carolina raw substation snapshot validated.\n")


  # Persist the raw substation snapshot
  if (file.exists(local_file)) {
    file.remove(local_file)
  }

  sf::st_write(
    raw,
    local_file,
    driver = "GeoJSON",
    quiet = TRUE
  )

  googledrive::drive_upload(
    local_file,
    path = drive_folder,
    overwrite = TRUE
  )

  cat("North Carolina raw substation snapshot persisted.\n")

  local_file
}


build_substation_tables <- function(
  raw_file,
  county_file,
  target_county_fips,
  target_crs,
  output_dir,
  validation_dir
) {
  # Load substations and TIGER counties
  sites <- sf::st_read(
    raw_file,
    quiet = TRUE
  )

  counties <- sf::st_read(
    county_file,
    quiet = TRUE
  )

  if (!nrow(sites) || !nrow(counties)) {
    stop("Substation or county input is empty.")
  }

  access_date <- sub(
    ".*access_([0-9]{8}).*",
    "\\1",
    basename(raw_file)
  )

  if (!grepl("^[0-9]{8}$", access_date)) {
    stop("Could not determine substation snapshot access date.")
  }

  source_record_count <- nrow(sites)

  cat("Substation snapshot and TIGER counties loaded.\n")


  # Validate substation point geometries
  empty_geometry <- sf::st_is_empty(
    sites
  )

  empty_count <- sum(
    empty_geometry
  )

  sites <- sites[
    !empty_geometry,
  ]

  if (!nrow(sites)) {
    stop("No substation geometries remain.")
  }

  invalid_before <- sum(
    !sf::st_is_valid(sites)
  )

  sites <- sf::st_make_valid(
    sites
  )

  invalid_after <- sum(
    !sf::st_is_valid(sites)
  )

  geometry_types <- as.character(
    sf::st_geometry_type(sites)
  )

  nonpoint_count <- sum(
    !geometry_types %in%
      c("POINT", "MULTIPOINT")
  )

  if (invalid_after > 0) {
    stop("Substation geometries remain invalid.")
  }

  if (nonpoint_count > 0) {
    stop("Substation source contains non-point geometries.")
  }

  cat("Substation point geometries validated.\n")


  # Define flexible substation field helpers
  clean_text <- function(x) {
    value <- trimws(
      as.character(x)
    )

    value[
      is.na(value) |
        value == ""
    ] <- NA_character_

    value
  }

  first_field <- function(
    data,
    candidates
  ) {
    found <- candidates[
      candidates %in% names(data)
    ]

    if (!length(found)) {
      return(NULL)
    }

    found[[1]]
  }

  get_field <- function(
    data,
    candidates
  ) {
    field <- first_field(
      data,
      candidates
    )

    if (is.null(field)) {
      return(
        rep(
          NA_character_,
          nrow(data)
        )
      )
    }

    clean_text(
      data[[field]]
    )
  }

  get_numeric <- function(
    data,
    candidates
  ) {
    field <- first_field(
      data,
      candidates
    )

    if (is.null(field)) {
      return(
        rep(
          NA_real_,
          nrow(data)
        )
      )
    }

    suppressWarnings(
      as.numeric(
        data[[field]]
      )
    )
  }

  normalize_fips <- function(x) {
    value <- clean_text(x)

    value <- gsub(
      "[^0-9]",
      "",
      value
    )

    value[
      is.na(value) |
        nchar(value) == 0
    ] <- NA_character_

    needs_pad <-
      !is.na(value) &
      nchar(value) < 5

    value[needs_pad] <- sprintf(
      "%05d",
      as.integer(
        value[needs_pad]
      )
    )

    value
  }

  cat("Substation field helpers ready.\n")


  # Standardize substation identifiers and attributes
  hifld_id <- get_field(
    sites,
    c(
      "ID",
      "SUB_ID",
      "SUBST_ID",
      "FACILITYID"
    )
  )

  source_object_id <- get_field(
    sites,
    c(
      "source_object_id",
      "OBJECTID_1",
      "OBJECTID"
    )
  )

  base_id <- ifelse(
    !is.na(hifld_id),
    paste0(
      "HIFLD_",
      hifld_id
    ),
    paste0(
      "OID_",
      source_object_id
    )
  )

  duplicate_base <-
    duplicated(base_id) |
    duplicated(
      base_id,
      fromLast = TRUE
    )

  substation_site_id <- ifelse(
    duplicate_base,
    paste0(
      base_id,
      "_OID_",
      source_object_id
    ),
    base_id
  )

  reported_county_fips <- normalize_fips(
    get_field(
      sites,
      c(
        "COUNTYFIPS",
        "COUNTY_FIPS",
        "CNTY_FIPS"
      )
    )
  )

  max_voltage_kv <- get_numeric(
    sites,
    c(
      "MAX_VOLT",
      "MAX_VOLTAGE",
      "MAX_KV"
    )
  )

  min_voltage_kv <- get_numeric(
    sites,
    c(
      "MIN_VOLT",
      "MIN_VOLTAGE",
      "MIN_KV"
    )
  )

  line_count <- get_numeric(
    sites,
    c(
      "LINES",
      "LINE_COUNT",
      "NUMLINES"
    )
  )


  max_voltage_sentinel <- sum(max_voltage_kv == -999999, na.rm = TRUE)
  min_voltage_sentinel <- sum(min_voltage_kv == -999999, na.rm = TRUE)
  line_count_sentinel <- sum(line_count == -999999, na.rm = TRUE)

  max_voltage_kv[max_voltage_kv == -999999] <- NA_real_
  min_voltage_kv[min_voltage_kv == -999999] <- NA_real_
  line_count[line_count == -999999] <- NA_real_



  cat("Substation identifiers and attributes standardized.\n")


  # Extract and validate substation coordinates
  sites_wgs <- sf::st_transform(
    sites,
    4326
  )

  coordinates <- sf::st_coordinates(
    sites_wgs
  )

  if (
    nrow(coordinates) !=
      nrow(sites_wgs)
  ) {
    stop("Substation geometry does not resolve to one point per facility.")
  }

  longitude <- coordinates[, 1]
  latitude <- coordinates[, 2]

  valid_coordinates <-
    is.finite(longitude) &
    is.finite(latitude) &
    longitude >= -180 &
    longitude <= 180 &
    latitude >= -90 &
    latitude <= 90

  invalid_coordinate_count <- sum(
    !valid_coordinates
  )

  if (invalid_coordinate_count > 0) {
    stop("Substation source contains invalid coordinates.")
  }

  cat("Substation coordinates validated.\n")


  # Build standardized substation point layer
  standardized <- data.frame(
    substation_site_id =
      substation_site_id,

    hifld_id =
      hifld_id,

    facility_name =
      get_field(
        sites,
        c(
          "NAME",
          "SUB_NAME",
          "FACILITY_NAME"
        )
      ),

    city =
      get_field(
        sites,
        c(
          "CITY"
        )
      ),

    state =
      get_field(
        sites,
        c(
          "STATE",
          "State",
          "ST_ABBREV",
          "STATE_ABBR"
        )
      ),

    zip =
      get_field(
        sites,
        c(
          "ZIP",
          "ZIPCODE"
        )
      ),

    substation_type =
      get_field(
        sites,
        c(
          "TYPE",
          "SUB_TYPE"
        )
      ),

    status =
      get_field(
        sites,
        c(
          "STATUS"
        )
      ),

    reported_county =
      get_field(
        sites,
        c(
          "COUNTY",
          "County"
        )
      ),

    reported_county_fips =
      reported_county_fips,

    max_voltage_kv =
      max_voltage_kv,

    min_voltage_kv =
      min_voltage_kv,

    line_count =
      line_count,

    max_voltage_inferred =
      get_field(
        sites,
        c(
          "MAX_INFER"
        )
      ),

    min_voltage_inferred =
      get_field(
        sites,
        c(
          "MIN_INFER"
        )
      ),

    source =
      get_field(
        sites,
        c(
          "SOURCE"
        )
      ),

    source_date =
      get_field(
        sites,
        c(
          "SOURCEDATE",
          "SOURCE_DATE"
        )
      ),

    validation_method =
      get_field(
        sites,
        c(
          "VAL_METHOD",
          "VALIDATION_METHOD"
        )
      ),

    validation_date =
      get_field(
        sites,
        c(
          "VAL_DATE",
          "VALIDATION_DATE"
        )
      ),

    longitude =
      longitude,

    latitude =
      latitude,

    snapshot_access_date =
      access_date,

    stringsAsFactors = FALSE
  )

  sites_sf <- sf::st_sf(
    standardized,
    geometry =
      sf::st_geometry(sites_wgs),
    crs = 4326
  )

  sites_sf <- sf::st_transform(
    sites_sf,
    target_crs
  )

  cat("Standardized substation point layer built.\n")


  # Spatially assign substations to TIGER counties
  counties_projected <- sf::st_transform(
    counties,
    target_crs
  )

  intersections <- sf::st_intersects(
    sites_sf,
    counties_projected
  )

  county_geoids <- vapply(
    intersections,
    function(index) {
      if (!length(index)) {
        return(NA_character_)
      }

      paste(
        sort(
          unique(
            as.character(
              counties_projected$GEOID[index]
            )
          )
        ),
        collapse = ";"
      )
    },
    character(1)
  )

  census_counties <- vapply(
    intersections,
    function(index) {
      if (!length(index)) {
        return(NA_character_)
      }

      paste(
        sort(
          unique(
            as.character(
              counties_projected$NAME[index]
            )
          )
        ),
        collapse = ";"
      )
    },
    character(1)
  )

  county_count <- lengths(
    intersections
  )

  sites_sf$county_geoids <-
    county_geoids

  sites_sf$census_counties <-
    census_counties

  sites_sf$county_count <-
    county_count

  spatial_nc <-
    county_count > 0

  cat("Substations spatially assigned to TIGER counties.\n")


  # Audit reported versus TIGER county FIPS
  reported_available <-
    !is.na(
      sites_sf$reported_county_fips
    )

  county_match <- vapply(
    seq_len(
      nrow(sites_sf)
    ),
    function(i) {
      if (
        !reported_available[[i]] ||
        is.na(
          sites_sf$county_geoids[[i]]
        )
      ) {
        return(NA)
      }

      spatial_fips <- strsplit(
        sites_sf$county_geoids[[i]],
        ";",
        fixed = TRUE
      )[[1]]

      sites_sf$reported_county_fips[[i]] %in%
        spatial_fips
    },
    logical(1)
  )

  county_mismatch <-
    reported_available &
    !is.na(county_match) &
    !county_match

  cat("Substation county geography audited.\n")


  # Build statewide and Mecklenburg substation layers
  nc_sf <- sites_sf[
    spatial_nc,
  ]

  intersects_mecklenburg <- vapply(
    strsplit(
      ifelse(
        is.na(
          nc_sf$county_geoids
        ),
        "",
        nc_sf$county_geoids
      ),
      ";",
      fixed = TRUE
    ),
    function(x) {
      target_county_fips %in% x
    },
    logical(1)
  )

  nc_sf$intersects_mecklenburg <-
    intersects_mecklenburg

  mecklenburg_sf <- nc_sf[
    nc_sf$intersects_mecklenburg,
  ]

  nc_table <- sf::st_drop_geometry(
    nc_sf
  )

  mecklenburg_table <- sf::st_drop_geometry(
    mecklenburg_sf
  )

  rownames(nc_table) <- NULL
  rownames(mecklenburg_table) <- NULL

  cat("Statewide and Mecklenburg substation layers built.\n")


  # Build substation discrepancy table
  discrepancy_type <- rep(
    NA_character_,
    nrow(sites_sf)
  )

  discrepancy_type[
    !spatial_nc
  ] <- "outside_spatial_nc"

  discrepancy_type[
    county_mismatch
  ] <- ifelse(
    is.na(
      discrepancy_type[
        county_mismatch
      ]
    ),
    "reported_spatial_fips_mismatch",
    paste0(
      discrepancy_type[
        county_mismatch
      ],
      ";reported_spatial_fips_mismatch"
    )
  )

  multiple_counties <-
    sites_sf$county_count > 1

  discrepancy_type[
    multiple_counties
  ] <- ifelse(
    is.na(
      discrepancy_type[
        multiple_counties
      ]
    ),
    "multiple_county_intersections",
    paste0(
      discrepancy_type[
        multiple_counties
      ],
      ";multiple_county_intersections"
    )
  )

  discrepancies <- sf::st_drop_geometry(
    sites_sf[
      !is.na(discrepancy_type),
    ]
  )

  discrepancies$discrepancy_type <-
    discrepancy_type[
      !is.na(discrepancy_type)
    ]

  cat("Substation discrepancy table built.\n")


  # Validate statewide substation records
  hifld_values <- nc_table$hifld_id[
    !is.na(
      nc_table$hifld_id
    )
  ]

  duplicate_hifld_ids <- sum(
    duplicated(
      hifld_values
    )
  )

  duplicate_site_ids <- sum(
    duplicated(
      nc_table$substation_site_id
    )
  )

  missing_hifld_ids <- sum(
    is.na(
      nc_table$hifld_id
    )
  )

  negative_voltage <- sum(
    nc_table$max_voltage_kv < 0 |
      nc_table$min_voltage_kv < 0,
    na.rm = TRUE
  )

  invalid_voltage_order <- sum(
    nc_table$min_voltage_kv >
      nc_table$max_voltage_kv,
    na.rm = TRUE
  )

  negative_lines <- sum(
    nc_table$line_count < 0,
    na.rm = TRUE
  )

  qa <- data.frame(
    check = c(
      "Source NC substation records",
      "Valid substation point records",
      "Empty geometries removed",
      "Invalid geometries before repair",
      "Invalid geometries after repair",
      "Invalid substation coordinates",
      "Spatial NC substations",
      "Substations outside spatial NC",
      "Duplicate substation site IDs",
      "Duplicate HIFLD IDs",
      "Missing HIFLD IDs",
      "Reported county FIPS available",
      "Reported versus spatial county mismatches",
      "Facilities intersecting multiple counties",
      "Mecklenburg spatial substations",
      "NC substations with maximum voltage available",
      "NC substations with minimum voltage available",
      "NC substations with line count available",
      "NC substations with negative voltage",
      "NC substations with minimum voltage above maximum",
      "NC substations with negative line count"
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
      duplicate_hifld_ids,
      missing_hifld_ids,
      sum(
        reported_available[
          spatial_nc
        ]
      ),
      sum(
        county_mismatch[
          spatial_nc
        ]
      ),
      sum(
        sites_sf$county_count[
          spatial_nc
        ] > 1
      ),
      nrow(mecklenburg_table),
      sum(
        !is.na(
          nc_table$max_voltage_kv
        )
      ),
      sum(
        !is.na(
          nc_table$min_voltage_kv
        )
      ),
      sum(
        !is.na(
          nc_table$line_count
        )
      ),
      negative_voltage,
      invalid_voltage_order,
      negative_lines
    ),

    status = c(
      "INFO",
      "INFO",
      ifelse(
        empty_count == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        invalid_before == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        invalid_after == 0,
        "PASS",
        "FAIL"
      ),
      ifelse(
        invalid_coordinate_count == 0,
        "PASS",
        "FAIL"
      ),
      "INFO",
      ifelse(
        sum(!spatial_nc) == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        duplicate_site_ids == 0,
        "PASS",
        "FAIL"
      ),
      ifelse(
        duplicate_hifld_ids == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        missing_hifld_ids == 0,
        "PASS",
        "CHECK"
      ),
      "INFO",
      ifelse(
        sum(
          county_mismatch[
            spatial_nc
          ]
        ) == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        sum(
          sites_sf$county_count[
            spatial_nc
          ] > 1
        ) == 0,
        "PASS",
        "CHECK"
      ),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      ifelse(
        negative_voltage == 0,
        "PASS",
        "FAIL"
      ),
      ifelse(
        invalid_voltage_order == 0,
        "PASS",
        "CHECK"
      ),
      ifelse(
        negative_lines == 0,
        "PASS",
        "FAIL"
      )
    ),

    stringsAsFactors = FALSE
  )

  print(
    qa,
    row.names = FALSE
  )

  cat("Substation geography and attribute QA/QC completed.\n")


  # Build substation source metadata
  metadata <- data.frame(
    field = c(
      "source",
      "publisher",
      "dataset_type",
      "snapshot_access_date",
      "source_temporal_scope",
      "historical_2022_source",
      "geometry",
      "state_filter",
      "spatial_assignment",
      "voltage_scope",
      "scope_note"
    ),

    value = c(
      "Electric Substations OCT 2022",
      "HIFLD-derived ArcGIS feature layer",
      "Electric power substation point locations",
      access_date,
      "October 2022 source layer",
      "TRUE",
      "Facility point locations",
      "North Carolina source attribute filter",
      "2022 TIGER county intersection",
      paste(
        "Primarily transmission-scale substations;",
        "lower-voltage coverage may be incomplete"
      ),
      paste(
        "Substations represent electric infrastructure context.",
        "Presence does not indicate a hazardous-material release."
      )
    ),

    stringsAsFactors = FALSE
  )

  cat("Substation source metadata built.\n")


  # Write substation processed outputs
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

  nc_csv <- file.path(
    output_dir,
    paste0(
      "nc_hifld_substations_oct2022_access_",
      access_date,
      ".csv"
    )
  )

  mecklenburg_csv <- file.path(
    output_dir,
    paste0(
      "mecklenburg_hifld_substations_oct2022_access_",
      access_date,
      ".csv"
    )
  )

  nc_gpkg <- file.path(
    output_dir,
    paste0(
      "nc_hifld_substations_oct2022_access_",
      access_date,
      ".gpkg"
    )
  )

  mecklenburg_gpkg <- file.path(
    output_dir,
    paste0(
      "mecklenburg_hifld_substations_oct2022_access_",
      access_date,
      ".gpkg"
    )
  )

  metadata_file <- file.path(
    output_dir,
    paste0(
      "hifld_substations_oct2022_metadata_access_",
      access_date,
      ".csv"
    )
  )

  discrepancy_file <- file.path(
    validation_dir,
    paste0(
      "substation_geography_discrepancies_access_",
      access_date,
      ".csv"
    )
  )

  qa_file <- file.path(
    validation_dir,
    paste0(
      "qaqc_substations_access_",
      access_date,
      ".csv"
    )
  )

  write.csv(
    nc_table,
    nc_csv,
    row.names = FALSE
  )

  write.csv(
    mecklenburg_table,
    mecklenburg_csv,
    row.names = FALSE
  )

  sf::st_write(
    nc_sf,
    nc_gpkg,
    delete_dsn = TRUE,
    quiet = TRUE
  )

  sf::st_write(
    mecklenburg_sf,
    mecklenburg_gpkg,
    delete_dsn = TRUE,
    quiet = TRUE
  )

  write.csv(
    metadata,
    metadata_file,
    row.names = FALSE
  )

  write.csv(
    discrepancies,
    discrepancy_file,
    row.names = FALSE
  )

  write.csv(
    qa,
    qa_file,
    row.names = FALSE
  )

  cat("Substation processed outputs written.\n")

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


upload_substation_outputs <- function(
  files,
  processed_folder,
  metadata_folder,
  validation_folder
) {
  # Validate substation output files
  if (
    length(files) != 7 ||
    !all(file.exists(files))
  ) {
    stop("Expected seven substation output files.")
  }

  cat("Substation output files validated.\n")


  # Upload processed substation datasets
  invisible(
    lapply(
      files[1:4],
      googledrive::drive_upload,
      path = processed_folder,
      overwrite = TRUE
    )
  )

  cat("Processed substation datasets uploaded.\n")


  # Upload substation metadata
  googledrive::drive_upload(
    files[[5]],
    path = metadata_folder,
    overwrite = TRUE
  )

  cat("Substation metadata uploaded.\n")


  # Upload substation QA reports
  invisible(
    lapply(
      files[6:7],
      googledrive::drive_upload,
      path = validation_folder,
      overwrite = TRUE
    )
  )

  cat("Substation QA reports uploaded.\n")

  TRUE
}