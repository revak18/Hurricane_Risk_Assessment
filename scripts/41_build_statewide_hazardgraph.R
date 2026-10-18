build_statewide_hazardgraph <- function(
  tri_fips_files, ian_exposure_files, mrds_classified_files,
  brownfields_files, wastewater_files, rmp_files,
  substation_files, hospital_files, year, state_fips,
  buffer_m, target_crs, output_dir, validation_dir,
  afo_files = character(), superfund_files = character()
) {

  # Define reusable helpers
  col1 <- function(x, n) {
    i <- match(tolower(n), tolower(names(x))); i <- i[!is.na(i)]
    if (length(i)) names(x)[i[1]] else NA_character_
  }
  num <- function(x, n) {
    f <- col1(x, n); if (is.na(f)) rep(NA_real_, nrow(x)) else suppressWarnings(as.numeric(x[[f]]))
  }
  chr <- function(x, n) {
    f <- col1(x, n); if (is.na(f)) return(rep(NA_character_, nrow(x)))
    y <- trimws(as.character(x[[f]])); y[is.na(y) | y == ""] <- NA_character_; y
  }
  flag <- function(x) {
    if (is.logical(x)) return(replace(x, is.na(x), FALSE))
    toupper(trimws(as.character(x))) %in% c("TRUE", "T", "YES", "Y", "1")
  }
  pick <- function(files, patterns) {
    files <- files[!is.na(files) & file.exists(files)]
    b <- tolower(basename(files)); keep <- rep(TRUE, length(files))
    for (p in patterns) keep <- keep & grepl(p, b, ignore.case = TRUE)
    keep <- keep & !grepl("mecklenburg|qaqc|metadata|discrep|summary|validation|pair", b)
    files <- files[keep]
    if (!length(files)) stop("No statewide source file found for: ", paste(patterns, collapse = ", "))
    b <- tolower(basename(files))
    score <- 100 * grepl("\\.gpkg$", b) + 90 * grepl("\\.geojson$", b) + 10 * grepl("\\.csv$", b)
    files[order(-score, b)][1]
  }
  read_layer <- function(file) {
    ext <- tolower(tools::file_ext(file))
    if (ext %in% c("gpkg", "geojson", "json", "shp")) {
      x <- sf::st_read(file, quiet = TRUE)
    } else {
      x <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
      lo <- col1(x, c("longitude", "lon", "long")); la <- col1(x, c("latitude", "lat"))
      if (is.na(lo) || is.na(la)) stop("No coordinates in ", basename(file))
      x[[lo]] <- suppressWarnings(as.numeric(x[[lo]]))
      x[[la]] <- suppressWarnings(as.numeric(x[[la]]))
      x <- x[is.finite(x[[lo]]) & is.finite(x[[la]]), ]
      x <- sf::st_as_sf(x, coords = c(lo, la), crs = 4326, remove = FALSE)
    }
    x <- sf::st_make_valid(x); x <- x[!sf::st_is_empty(x), ]
    if (!nrow(x) || is.na(sf::st_crs(x))) stop("Invalid spatial source: ", basename(file))
    sf::st_transform(x, target_crs)
  }
  attach <- function(x, y) {
    if (!identical(paste(x$tri_facility_id, x$buffer_m), paste(y$tri_facility_id, y$buffer_m)))
      stop("Feature key order mismatch.")
    cbind(x, y[, setdiff(names(y), c("tri_facility_id", "buffer_m")), drop = FALSE])
  }
  cat("Statewide HazardGraph helpers ready.\n")


  # Build facility-level TRI release features
  tri_file <- tri_fips_files[basename(tri_fips_files) == paste0("nc_tri_facility_fips_", year, ".csv")][1]
  chem_file <- tri_fips_files[basename(tri_fips_files) == paste0("nc_tri_chemicals_fips_", year, ".csv")][1]
  if (is.na(tri_file) || is.na(chem_file)) stop("Statewide TRI inputs not found.")

  facilities <- read.csv(tri_file, stringsAsFactors = FALSE, check.names = FALSE)
  chemicals <- read.csv(chem_file, stringsAsFactors = FALSE, check.names = FALSE)
  if (anyDuplicated(facilities$tri_facility_id)) stop("Duplicate TRI facility IDs.")
  if (any(as.character(facilities$state_fips) != as.character(state_fips))) stop("TRI facilities outside configured state.")
  if (any(!chemicals$tri_facility_id %in% facilities$tri_facility_id)) stop("Unmatched TRI chemical facility IDs.")

  fields <- c(
    "fugitive_air", "stack_air", "total_air", "water", "underground", "land",
    "onsite_release", "offsite_release", "transfer_total", "recycling_transfer",
    "treatment_transfer", "recovery_transfer", "production_waste"
  )
  for (f in fields) chemicals[[f]] <- suppressWarnings(as.numeric(chemicals[[f]]))
  if (any(vapply(chemicals[fields], function(x) any(x < 0, na.rm = TRUE), logical(1))))
    stop("Negative TRI quantities detected.")

  u <- tolower(trimws(chemicals$unit_of_measure))
  conv <- ifelse(u %in% c("pound", "pounds", "lb", "lbs"), 1,
                 ifelse(u %in% c("gram", "grams", "g"), 0.00220462262185, NA_real_))
  unsupported_units <- sum(is.na(conv))
  if (unsupported_units) stop("Unsupported TRI reporting units detected.")
  gram_records <- sum(conv != 1)

  for (f in fields) chemicals[[paste0(f, "_lbs")]] <- chemicals[[f]] * conv
  chemicals$total_release_lbs <- chemicals$onsite_release_lbs + chemicals$offsite_release_lbs
  chemicals$carcinogen_flag <- flag(chemicals$carcinogen)
  chemicals$pbt_flag <- flag(chemicals$pbt)
  chemicals$pfas_flag <- flag(chemicals$pfas)
  chemicals$released_flag <- chemicals$total_release_lbs > 0
  chemicals$chemical_key <- trimws(as.character(chemicals$tri_chem_id))
  z <- is.na(chemicals$chemical_key) | chemicals$chemical_key == ""
  chemicals$chemical_key[z] <- chemicals$chemical_name[z]

  air_mismatch <- sum(abs(chemicals$total_air_lbs - chemicals$fugitive_air_lbs -
                            chemicals$stack_air_lbs) > 1e-8, na.rm = TRUE)
  onsite_mismatch <- sum(abs(chemicals$onsite_release_lbs - chemicals$total_air_lbs -
                               chemicals$water_lbs - chemicals$underground_lbs -
                               chemicals$land_lbs) > 1e-8, na.rm = TRUE)
  if (air_mismatch) stop("TRI total-air arithmetic validation failed.")

  by_facility <- split(chemicals, chemicals$tri_facility_id)
  sum_fields <- c(fields, "total_release")
  nuniq <- function(x) length(unique(x[!is.na(x) & x != ""]))

  tri_release <- do.call(rbind, lapply(facilities$tri_facility_id, function(id) {
    x <- by_facility[[id]]
    sums <- vapply(sum_fields, function(f) sum(x[[paste0(f, "_lbs")]], na.rm = TRUE), numeric(1))
    names(sums) <- paste0("tri_", sum_fields, "_lbs")
    cbind(data.frame(
      tri_facility_id = id,
      tri_chemical_count = nuniq(x$chemical_key),
      tri_released_chemical_count = nuniq(x$chemical_key[x$released_flag]),
      tri_carcinogen_chemical_count = nuniq(x$chemical_key[x$carcinogen_flag]),
      tri_pbt_chemical_count = nuniq(x$chemical_key[x$pbt_flag]),
      tri_pfas_chemical_count = nuniq(x$chemical_key[x$pfas_flag])
    ), as.data.frame(as.list(sums)))
  }))

  order_id <- facilities$tri_facility_id
  facilities <- merge(facilities, tri_release, by = "tri_facility_id", all.x = TRUE, sort = FALSE)
  facilities <- facilities[match(order_id, facilities$tri_facility_id), , drop = FALSE]
  missing_release <- sum(!complete.cases(facilities[, setdiff(names(tri_release), "tri_facility_id")]))
  if (missing_release) stop("TRI release features missing for facilities.")
  cat("Statewide TRI release features integrated.\n")


  # Build facility-buffer backbone
  buffer_m <- sort(unique(as.numeric(buffer_m)))
  backbone <- facilities[rep(seq_len(nrow(facilities)), each = length(buffer_m)), , drop = FALSE]
  backbone$buffer_m <- rep(buffer_m, times = nrow(facilities))
  rownames(backbone) <- NULL
  tri_sf <- sf::st_transform(
    sf::st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE),
    target_crs
  )
  cat("Statewide TRI-buffer backbone built.\n")


  # Load statewide spatial source layers
  source_files <- list(
    mrds = pick(mrds_classified_files, c("^nc", "mrds")),
    brownfields = pick(brownfields_files, c("^nc_brownfields_boundaries_access_", "\\.gpkg$")),
    wastewater = pick(wastewater_files, c("^nc_npdes_wastewater_permits_access_")),
    rmp = pick(rmp_files, c("^nc", "rmp")),
    substation = pick(substation_files, c("^nc", "substation")),
    hospital = pick(hospital_files, c("^nc", "hospital")),
    afo = pick(afo_files, c("^nc", "afo")),
    superfund = pick(superfund_files, c("^nc", "superfund"))
  )
  layers <- lapply(source_files, read_layer)
  cat("Statewide processed source layers loaded.\n")


  # Build generic source proximity features
  summarize_buffers <- function(dm, fun) {
    do.call(rbind, lapply(seq_len(nrow(facilities)), function(i) {
      do.call(rbind, lapply(buffer_m, function(r) {
        cbind(
          data.frame(tri_facility_id = facilities$tri_facility_id[i], buffer_m = r),
          as.data.frame(fun(i, r, dm[i, ]), stringsAsFactors = FALSE)
        )
      }))
    }))
  }

  proximity <- lapply(names(layers), function(nm) {
    dm <- matrix(as.numeric(sf::st_distance(tri_sf, layers[[nm]])),
                 nrow = nrow(facilities), ncol = nrow(layers[[nm]]))
    f <- summarize_buffers(dm, function(i, r, d) {
      n <- sum(d <= r)
      setNames(list(n, n > 0, min(d)),
               c(paste0(nm, "_site_count"),
                 paste0(nm, "_any_within_buffer"),
                 paste0("nearest_", nm, "_distance_m")))
    })
    list(features = f, distance = dm)
  })
  names(proximity) <- names(layers)

  features <- backbone
  for (nm in names(proximity)) features <- attach(features, proximity[[nm]]$features)
  cat("Statewide proximity features calculated.\n")


  # Add source-specific context features
  categories <- c(
    "industrial_mineral", "construction_aggregate", "precious_metal",
    "ferrous_metal", "mixed_resource", "other_or_unknown", "mixed_metal", "base_metal"
  )
  resource <- gsub("[^a-z0-9]+", "_", tolower(chr(layers$mrds, "resource_category")))
  resource[is.na(resource)] <- "other_or_unknown"

  extra <- summarize_buffers(proximity$mrds$distance, function(i, r, d) {
    v <- as.integer(table(factor(resource[d <= r], levels = categories)))
    setNames(as.list(v), paste0("mrds_", categories, "_count"))
  })
  features <- attach(features, extra)

  major_field <- col1(layers$wastewater, "major")
  if (is.na(major_field)) stop("Wastewater major field missing.")
  major <- flag(layers$wastewater[[major_field]])
  flow <- num(layers$wastewater, "asbuilt_flow_mgd")
  major_idx <- which(major)

  extra <- summarize_buffers(proximity$wastewater$distance, function(i, r, d) {
    near <- which(d <= r); maj <- near[major[near]]
    avail <- near[!is.na(flow[near])]; maj_avail <- maj[!is.na(flow[maj])]
    list(
      wastewater_major_count = length(maj),
      wastewater_minor_count = length(near) - length(maj),
      wastewater_flow_available_count = length(avail),
      wastewater_total_asbuilt_flow_mgd = sum(flow[avail], na.rm = TRUE),
      wastewater_major_asbuilt_flow_mgd = sum(flow[maj_avail], na.rm = TRUE),
      wastewater_max_asbuilt_flow_mgd = if (length(avail)) max(flow[avail]) else 0,
      nearest_major_wastewater_distance_m = if (length(major_idx)) min(d[major_idx]) else NA_real_
    )
  })
  features <- attach(features, extra)

  voltage <- num(layers$substation,
                 c("maximum_voltage", "max_voltage", "maximum_voltage_kv", "max_voltage_kv", "max_volt"))
  line_count <- num(layers$substation,
                    c("line_count", "num_lines", "number_of_lines", "lines"))

  extra <- summarize_buffers(proximity$substation$distance, function(i, r, d) {
    near <- which(d <= r); v <- voltage[near]; l <- line_count[near]
    list(
      substation_voltage_available_count = sum(!is.na(v)),
      substation_max_voltage_kv = if (any(!is.na(v))) max(v, na.rm = TRUE) else NA_real_,
      substation_total_line_count = sum(l, na.rm = TRUE)
    )
  })
  features <- attach(features, extra)

  beds <- num(layers$hospital, c("beds", "bed_count", "total_beds", "num_beds"))
  trauma <- toupper(ifelse(
    is.na(chr(layers$hospital, c("trauma", "trauma_level", "trauma_center", "trauma_status"))), "",
    chr(layers$hospital, c("trauma", "trauma_level", "trauma_center", "trauma_status"))
  ))
  trauma_flag <- trauma != "" &
    !trauma %in% c("0", "NO", "NONE", "NA", "N/A", "UNKNOWN", "NOT AVAILABLE", "NOT APPLICABLE")
  hf <- col1(layers$hospital, c("helipad", "has_helipad", "helipad_flag"))
  helipad_flag <- if (is.na(hf)) rep(FALSE, nrow(layers$hospital)) else flag(layers$hospital[[hf]])

  extra <- summarize_buffers(proximity$hospital$distance, function(i, r, d) {
    near <- which(d <= r); b <- beds[near]; available <- !is.na(b)
    list(
      hospital_bed_available_count = sum(available),
      hospital_total_beds = if (any(available)) sum(b, na.rm = TRUE) else NA_real_,
      hospital_max_beds = if (any(available)) max(b, na.rm = TRUE) else NA_real_,
      trauma_hospital_count = sum(trauma_flag[near]),
      helipad_hospital_count = sum(helipad_flag[near])
    )
  })
  features <- attach(features, extra)
  cat("Source-specific statewide context features calculated.\n")


  # Attach Hurricane Ian screening features
  ian_file <- ian_exposure_files[
    grepl(paste0("^ian_exposed_tri_facilities_", year, "\\.csv$"),
          basename(ian_exposure_files), ignore.case = TRUE)
  ][1]
  if (is.na(ian_file)) stop("Ian exposed TRI table not found.")

  ian <- read.csv(ian_file, stringsAsFactors = FALSE, check.names = FALSE)
  if (anyDuplicated(ian$tri_facility_id)) stop("Duplicate Ian TRI facility IDs.")

  m <- match(facilities$tri_facility_id, ian$tri_facility_id)
  ian_summary <- data.frame(
    tri_facility_id = facilities$tri_facility_id,
    distance_to_ian_miles = NA_real_,
    within_ian_100mi = !is.na(m)
  )
  ian_summary$distance_to_ian_miles[!is.na(m)] <-
    as.numeric(ian$distance_to_ian_miles[m[!is.na(m)]])

  m <- match(features$tri_facility_id, ian_summary$tri_facility_id)
  features$distance_to_ian_miles <- ian_summary$distance_to_ian_miles[m]
  features$within_ian_100mi <- ian_summary$within_ian_100mi[m]
  ian_count <- sum(ian_summary$within_ian_100mi)
  cat("Statewide Hurricane Ian screening features attached.\n")


  # Validate statewide integration
  expected_rows <- nrow(facilities) * length(buffer_m)
  duplicate_rows <- sum(duplicated(features[, c("tri_facility_id", "buffer_m")]))
  buffers_per_facility <- table(features$tri_facility_id)
  unexpected_buffers <- sum(!features$buffer_m %in% buffer_m)
  missing_keys <- expected_rows - length(unique(paste(features$tri_facility_id, features$buffer_m)))

  count_cols <- names(features)[grepl("_count$", names(features)) & !grepl("^tri_", names(features))]
  nonmonotonic <- 0L
  for (id in facilities$tri_facility_id) {
    z <- features[features$tri_facility_id == id, ]
    z <- z[order(z$buffer_m), ]
    nonmonotonic <- nonmonotonic +
      sum(vapply(count_cols, function(f) any(diff(as.numeric(z[[f]])) < 0, na.rm = TRUE), logical(1)))
  }

  if (nrow(features) != expected_rows || duplicate_rows || missing_keys || nonmonotonic)
    stop("Statewide HazardGraph validation failed.")
  cat("Statewide HazardGraph structural validation completed.\n")


  # Build QA, missingness, and provenance
  missingness <- data.frame(
    field = names(features),
    missing = vapply(features, function(x) sum(is.na(x)), integer(1)),
    total_rows = nrow(features),
    missing_percent = round(100 * vapply(features, function(x) mean(is.na(x)), numeric(1)), 4)
  )

  manifest <- data.frame(
    source = c("tri_facilities", "tri_chemical_releases", "hurricane_ian_screening", names(source_files)),
    source_file = basename(c(tri_file, chem_file, ian_file, unlist(source_files, use.names = FALSE))),
    temporal_scope = c(
      paste0(year, " TRI inventory"), paste0(year, " TRI release reporting"),
      paste0(year, " Ian 100-mile screening"), rep("See source-specific metadata", length(source_files))
    ),
    stringsAsFactors = FALSE
  )

  qa <- data.frame(
    check = c(
      "Statewide NC TRI facilities", "Statewide TRI chemical records",
      "TRI facilities with release summaries", "TRI release features missing facilities",
      "TRI gram-reported records converted", "Unsupported TRI reporting units",
      "TRI total-air arithmetic mismatches", "TRI reported onsite-component differences",
      "Statewide TRI release integration", "Configured buffer distances",
      "Expected facility-buffer rows", "Actual facility-buffer rows",
      "Duplicate facility-buffer rows", "Facilities with all buffer distances",
      "Unexpected buffer values", "Missing facility-buffer keys",
      "Nonmonotonic source-count checks", "Integrated source families",
      "TRI facilities within Ian 100-mile screening zone",
      "TRI facilities outside Ian 100-mile screening zone", "CDL statewide integration"
    ),
    value = c(
      nrow(facilities), nrow(chemicals), nrow(tri_release), missing_release,
      gram_records, unsupported_units, air_mismatch, onsite_mismatch, 1,
      length(buffer_m), expected_rows, nrow(features), duplicate_rows,
      sum(buffers_per_facility == length(buffer_m)), unexpected_buffers,
      missing_keys, nonmonotonic, length(layers), ian_count,
      nrow(facilities) - ian_count, 0
    ),
    status = c(
      "PASS", "INFO", "PASS", "PASS", "INFO", "PASS", "PASS", "CHECK", "PASS",
      "INFO", "INFO", "PASS", "PASS", "PASS", "PASS", "PASS", "PASS", "INFO",
      "PASS", "INFO", "PENDING"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("Statewide HazardGraph QA/QC completed.\n")


  # Write statewide outputs
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  out <- c(
    file.path(output_dir, paste0("nc_hazardgraph_features_", year, ".csv")),
    file.path(output_dir, paste0("nc_hazardgraph_source_manifest_", year, ".csv")),
    file.path(output_dir, paste0("nc_hazardgraph_missingness_", year, ".csv")),
    file.path(validation_dir, paste0("qaqc_nc_hazardgraph_", year, ".csv"))
  )

  write.csv(features, out[1], row.names = FALSE)
  write.csv(manifest, out[2], row.names = FALSE)
  write.csv(missingness, out[3], row.names = FALSE)
  write.csv(qa, out[4], row.names = FALSE)
  cat("Statewide HazardGraph outputs written.\n")

  out
}


upload_statewide_hazardgraph <- function(files, processed_folder, validation_folder) {
  # Upload statewide outputs
  if (length(files) != 4 || !all(file.exists(files)))
    stop("Expected four statewide HazardGraph output files.")

  qa <- files[grepl("^qaqc_nc_hazardgraph_", basename(files), ignore.case = TRUE)]
  for (f in setdiff(files, qa))
    googledrive::drive_upload(f, path = processed_folder, overwrite = TRUE)

  googledrive::drive_upload(qa, path = validation_folder, overwrite = TRUE)
  cat("Statewide HazardGraph outputs uploaded.\n")
  invisible(TRUE)
}