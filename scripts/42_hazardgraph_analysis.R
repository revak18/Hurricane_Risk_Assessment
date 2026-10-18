analyze_statewide_hazardgraph <- function(
  statewide_hazardgraph_files, year, buffer_m,
  output_dir, validation_dir, figure_dir
) {

  # Define analysis helpers
  logical_value <- function(x) {
    if (is.logical(x)) return(x)
    x <- toupper(trimws(as.character(x)))
    x %in% c("TRUE", "T", "YES", "Y", "1")
  }

  percentile <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x)
    out[ok] <- 0
    pos <- ok & x > 0

    if (!any(pos)) return(out)
    if (length(unique(x[pos])) == 1) {
      out[pos] <- 1
      return(out)
    }

    r <- rank(x[pos], ties.method = "average")
    out[pos] <- (r - 1) / (length(r) - 1)
    out
  }

  row_mean <- function(data, fields) {
    fields <- intersect(fields, names(data))
    x <- as.matrix(data[, fields, drop = FALSE])
    n <- rowSums(!is.na(x))
    out <- rowMeans(x, na.rm = TRUE)
    out[n == 0] <- NA_real_
    out
  }

  spearman <- function(x, y) {
    keep <- is.finite(x) & is.finite(y)
    if (
      sum(keep) < 3 ||
      length(unique(x[keep])) < 2 ||
      length(unique(y[keep])) < 2
    ) return(NA_real_)

    suppressWarnings(cor(x[keep], y[keep], method = "spearman"))
  }

  safe_min <- function(x) {
    x <- x[is.finite(x)]
    if (length(x)) min(x) else NA_real_
  }

  cat("Statewide HazardGraph analysis helpers ready.\n")


  # Load and validate Step 41 features
  feature_file <- statewide_hazardgraph_files[
    grepl(
      paste0("^nc_hazardgraph_features_", year, "\\.csv$"),
      basename(statewide_hazardgraph_files),
      ignore.case = TRUE
    )
  ][1]

  if (is.na(feature_file) || !file.exists(feature_file))
    stop("Could not locate statewide HazardGraph feature table.")

  hazard <- read.csv(
    feature_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required <- c(
    "tri_facility_id", "county_geoid", "buffer_m",
    "within_ian_100mi", "distance_to_ian_miles",
    "tri_total_release_lbs"
  )

  missing <- setdiff(required, names(hazard))
  if (length(missing))
    stop("Missing Step 41 fields: ", paste(missing, collapse = ", "))

  hazard$county_geoid <- sprintf(
    "%05d",
    as.integer(hazard$county_geoid)
  )
  hazard$buffer_m <- as.numeric(hazard$buffer_m)
  hazard$within_ian_100mi <- logical_value(hazard$within_ian_100mi)
  hazard$distance_to_ian_miles <- as.numeric(hazard$distance_to_ian_miles)
  hazard$tri_total_release_lbs <- as.numeric(hazard$tri_total_release_lbs)

  key <- paste(hazard$tri_facility_id, hazard$buffer_m)
  duplicate_keys <- sum(duplicated(key))
  facilities <- unique(hazard$tri_facility_id)
  expected_rows <- length(facilities) * length(buffer_m)

  if (duplicate_keys)
    stop("Duplicate facility-buffer keys.")

  if (
    nrow(hazard) != expected_rows ||
    !setequal(sort(unique(hazard$buffer_m)), sort(buffer_m))
  ) stop("Invalid statewide facility-buffer structure.")

  if (
    any(!is.finite(hazard$tri_total_release_lbs)) ||
    any(hazard$tri_total_release_lbs < 0)
  ) stop("Invalid TRI total-release values.")

  exposed_facilities <- length(unique(
    hazard$tri_facility_id[
      hazard$within_ian_100mi
    ]
  ))

  cat("Step 41 statewide HazardGraph loaded and validated.\n")


  # Build environmental and context scores
  environmental_fields <- c(
    "mrds_site_count",
    "brownfields_site_count",
    "wastewater_site_count",
    "rmp_site_count",
    "afo_site_count",
    "superfund_site_count"
  )

  missing <- setdiff(environmental_fields, names(hazard))
  if (length(missing))
    stop("Missing environmental fields: ", paste(missing, collapse = ", "))

  if (!"substation_site_count" %in% names(hazard))
    stop("Substation context field is missing.")

  hospital_fields <- intersect(
    c(
      "hospital_site_count",
      "hospital_total_beds",
      "trauma_hospital_count",
      "helipad_hospital_count"
    ),
    names(hazard)
  )

  if (!"hospital_site_count" %in% hospital_fields)
    stop("Hospital context field is missing.")

  environmental_scores <- character()

  for (field in environmental_fields) {
    score <- sub("_site_count$", "_density_score", field)
    hazard[[score]] <- NA_real_

    for (distance in buffer_m) {
      i <- hazard$buffer_m == distance
      hazard[[score]][i] <- percentile(
        log1p(hazard[[field]][i])
      )
    }

    environmental_scores <- c(environmental_scores, score)
  }

  hazard$environmental_inventory_score <-
    row_mean(hazard, environmental_scores)

  hazard$substation_context_score <- NA_real_

  for (distance in buffer_m) {
    i <- hazard$buffer_m == distance
    hazard$substation_context_score[i] <-
      percentile(log1p(hazard$substation_site_count[i]))
  }

  hospital_scores <- character()

  for (field in hospital_fields) {
    score <- paste0(field, "_score")
    hazard[[score]] <- NA_real_

    for (distance in buffer_m) {
      i <- hazard$buffer_m == distance
      hazard[[score]][i] <-
        percentile(log1p(hazard[[field]][i]))
    }

    hospital_scores <- c(hospital_scores, score)
  }

  hazard$emergency_response_capacity_score <-
    row_mean(hazard, hospital_scores)

  exposed <- hazard$within_ian_100mi
  hazard$ian_screening_score <- 0

  valid <- exposed & is.finite(hazard$distance_to_ian_miles)

  if (any(exposed & !valid))
    stop("Ian-screened facilities are missing track distance.")

  hazard$ian_screening_score[valid] <- pmax(
    0,
    pmin(
      1,
      1 - hazard$distance_to_ian_miles[valid] / 100
    )
  )

  hazard$ian_context_score_equal <- ifelse(
    exposed,
    0.50 * hazard$ian_screening_score +
      0.50 * hazard$environmental_inventory_score,
    0
  )

  hazard$ian_context_score_storm_heavy <- ifelse(
    exposed,
    0.70 * hazard$ian_screening_score +
      0.30 * hazard$environmental_inventory_score,
    0
  )

  hazard$ian_context_score_inventory_heavy <- ifelse(
    exposed,
    0.30 * hazard$ian_screening_score +
      0.70 * hazard$environmental_inventory_score,
    0
  )

  cat("Environmental, infrastructure, hospital and Ian scores calculated.\n")


  # Score TRI release separately from the composite
  primary_buffer <- max(buffer_m)

  release <- hazard[
    hazard$buffer_m == primary_buffer,
    c("tri_facility_id", "tri_total_release_lbs")
  ]

  if (anyDuplicated(release$tri_facility_id))
    stop("Duplicate facility release rows.")

  release$tri_release_magnitude_score <-
    percentile(release$tri_total_release_lbs)

  hazard$tri_release_magnitude_score <-
    release$tri_release_magnitude_score[
      match(
        hazard$tri_facility_id,
        release$tri_facility_id
      )
    ]

  missing_release_scores <-
    sum(!is.finite(hazard$tri_release_magnitude_score))

  release_buffer_inconsistency <- sum(vapply(
    split(
      hazard$tri_release_magnitude_score,
      hazard$tri_facility_id
    ),
    function(x) length(unique(x[is.finite(x)])) != 1,
    logical(1)
  ))

  cat("TRI release magnitude analyzed separately from composite scores.\n")


  # Build facility table, summaries, and correlations
  facility <- hazard[
    hazard$buffer_m == primary_buffer,
    ,
    drop = FALSE
  ]

  facility <- facility[
    order(
      facility$ian_context_score_equal,
      decreasing = TRUE
    ),
  ]

  facility$screening_order <- seq_len(nrow(facility))
  rownames(facility) <- NULL

  summary_fields <- unique(c(
    environmental_fields,
    "substation_site_count",
    hospital_fields,
    "tri_total_release_lbs",
    "tri_release_magnitude_score",
    "distance_to_ian_miles",
    "ian_screening_score",
    environmental_scores,
    "environmental_inventory_score",
    "substation_context_score",
    "emergency_response_capacity_score",
    "ian_context_score_equal",
    "ian_context_score_storm_heavy",
    "ian_context_score_inventory_heavy"
  ))

  summarize_field <- function(field) {
    x <- suppressWarnings(as.numeric(facility[[field]]))
    x <- x[is.finite(x)]

    data.frame(
      feature = field,
      rows = nrow(facility),
      available = length(x),
      missing = nrow(facility) - length(x),
      zero_count = sum(x == 0),
      zero_percent = round(100 * mean(x == 0), 2),
      minimum = min(x),
      median = median(x),
      p90 = as.numeric(quantile(x, 0.90)),
      p95 = as.numeric(quantile(x, 0.95)),
      maximum = max(x),
      mean = mean(x),
      sd = if (length(x) > 1) sd(x) else NA_real_
    )
  }

  feature_summary <- do.call(
    rbind,
    lapply(summary_fields, summarize_field)
  )

  correlation_fields <- unique(c(
    environmental_fields,
    "substation_site_count",
    "hospital_site_count",
    "tri_total_release_lbs",
    "environmental_inventory_score",
    "ian_screening_score",
    "ian_context_score_equal"
  ))

  correlation_matrix <- suppressWarnings(
    cor(
      facility[, correlation_fields, drop = FALSE],
      use = "pairwise.complete.obs",
      method = "spearman"
    )
  )

  correlation_pairs <- expand.grid(
    feature_1 = rownames(correlation_matrix),
    feature_2 = colnames(correlation_matrix),
    stringsAsFactors = FALSE
  )

  correlation_pairs$spearman_rho <- mapply(
    function(a, b) correlation_matrix[a, b],
    correlation_pairs$feature_1,
    correlation_pairs$feature_2
  )

  correlation_pairs <- correlation_pairs[
    correlation_pairs$feature_1 < correlation_pairs$feature_2,
  ]

  correlation_pairs <- correlation_pairs[
    order(
      abs(correlation_pairs$spearman_rho),
      decreasing = TRUE,
      na.last = TRUE
    ),
  ]

  rownames(correlation_pairs) <- NULL

  cat("Facility summaries and TRI-release correlation audit built.\n")


  # Run weight, buffer, and source sensitivity analyses
  exposed_primary <- facility[
    facility$within_ian_100mi,
  ]

  sensitivity <- data.frame(
    analysis_type = character(),
    comparison = character(),
    spearman_rho = numeric(),
    n = integer()
  )

  sensitivity <- rbind(
    sensitivity,
    data.frame(
      analysis_type = "weight_scenario",
      comparison = "equal_vs_storm_heavy",
      spearman_rho = spearman(
        exposed_primary$ian_context_score_equal,
        exposed_primary$ian_context_score_storm_heavy
      ),
      n = nrow(exposed_primary)
    ),
    data.frame(
      analysis_type = "weight_scenario",
      comparison = "equal_vs_inventory_heavy",
      spearman_rho = spearman(
        exposed_primary$ian_context_score_equal,
        exposed_primary$ian_context_score_inventory_heavy
      ),
      n = nrow(exposed_primary)
    )
  )

  for (pair in combn(sort(buffer_m), 2, simplify = FALSE)) {
    a <- hazard[
      hazard$buffer_m == pair[1],
      c(
        "tri_facility_id",
        "within_ian_100mi",
        "ian_context_score_equal"
      )
    ]

    b <- hazard[
      hazard$buffer_m == pair[2],
      c(
        "tri_facility_id",
        "ian_context_score_equal"
      )
    ]

    names(a)[3] <- "score_a"
    names(b)[2] <- "score_b"

    joined <- merge(a, b, by = "tri_facility_id")
    joined <- joined[joined$within_ian_100mi, ]

    sensitivity <- rbind(
      sensitivity,
      data.frame(
        analysis_type = "buffer_sensitivity",
        comparison = paste0(pair[1], "m_vs_", pair[2], "m"),
        spearman_rho = spearman(
          joined$score_a,
          joined$score_b
        ),
        n = nrow(joined)
      )
    )
  }

  for (omitted in environmental_scores) {
    remaining <- setdiff(environmental_scores, omitted)
    environment <- row_mean(exposed_primary, remaining)

    score <- 0.50 * exposed_primary$ian_screening_score +
      0.50 * environment

    sensitivity <- rbind(
      sensitivity,
      data.frame(
        analysis_type = "leave_one_source_out",
        comparison = paste0(
          "omit_",
          sub("_density_score$", "", omitted)
        ),
        spearman_rho = spearman(
          exposed_primary$ian_context_score_equal,
          score
        ),
        n = nrow(exposed_primary)
      )
    )
  }

  rownames(sensitivity) <- NULL

  cat("Weight, buffer and leave-one-source-out sensitivity completed.\n")


  # Build county summaries
  county_field <- intersect(
    c("census_county", "county"),
    names(facility)
  )[1]

  county_summary <- do.call(
    rbind,
    lapply(
      split(facility, facility$county_geoid),
      function(data) {
        county_name <- NA_character_

        if (!is.na(county_field)) {
          value <- data[[county_field]]
          value <- value[
            !is.na(value) &
              trimws(as.character(value)) != ""
          ]
          if (length(value))
            county_name <- as.character(value[1])
        }

        data.frame(
          county_geoid = data$county_geoid[1],
          county_name = county_name,
          tri_facilities = nrow(data),
          ian_screened_facilities =
            sum(data$within_ian_100mi),
          ian_screened_percent =
            round(100 * mean(data$within_ian_100mi), 2),
          mean_environmental_inventory_score =
            mean(data$environmental_inventory_score),
          mean_tri_release_magnitude_score =
            mean(data$tri_release_magnitude_score),
          mean_ian_context_score =
            mean(data$ian_context_score_equal),
          maximum_ian_context_score =
            max(data$ian_context_score_equal)
        )
      }
    )
  )

  county_summary <- county_summary[
    order(
      county_summary$maximum_ian_context_score,
      decreasing = TRUE
    ),
  ]

  rownames(county_summary) <- NULL

  cat("County-level statewide summary built.\n")


  # Document score interpretation
  score_definition <- data.frame(
    component = c(
      "ian_screening_score",
      "environmental_inventory_score",
      "tri_release_magnitude_score",
      "substation_context_score",
      "emergency_response_capacity_score",
      "ian_context_score_equal",
      "statewide_CDL",
      "statewide_HURDAT_wind_radii",
      "ToxPi_or_final_composite"
    ),
    status = c(
      "ACTIVE",
      "ACTIVE",
      "ACTIVE_SEPARATE_ANALYSIS",
      "ACTIVE_CONTEXT_ONLY",
      "ACTIVE_RESILIENCE_CONTEXT",
      "ACTIVE_SCREENING_ONLY",
      "PENDING",
      "PENDING",
      "DEFERRED"
    ),
    interpretation = c(
      "0-1 proximity screening based on Ian-track distance inside 100-mile corridor; not wind intensity",
      "Equal-weight percentile summary of MRDS, Brownfields, wastewater, RMP, AFO and Superfund counts",
      "Percentile of TRI total release mass in pounds; onsite plus offsite; mass-based and not toxicity-weighted; excluded from current composite",
      "Relative nearby electric-substation density; descriptive infrastructure context",
      "Relative nearby hospital presence and capacity; higher values indicate more nearby response capacity",
      "Equal-weight Ian proximity plus environmental inventory context; release magnitude is not included",
      "Validated statewide CDL has not yet been integrated",
      "Actual statewide 34/50/64 kt HURDAT radius exposure has not yet been integrated",
      "Current screening outputs must not be interpreted as a final risk index"
    ),
    stringsAsFactors = FALSE
  )

  cat("Score definitions documented.\n")


  # Build prevalence data and figures
  prevalence_fields <- c(
    environmental_fields,
    "substation_site_count",
    "hospital_site_count"
  )

  prevalence <- do.call(
    rbind,
    lapply(
      prevalence_fields,
      function(field) {
        data.frame(
          source = sub("_site_count$", "", field),
          facilities_within_buffer =
            sum(facility[[field]] > 0),
          percent_facilities =
            round(100 * mean(facility[[field]] > 0), 2)
        )
      }
    )
  )

  prevalence <- prevalence[
    order(prevalence$percent_facilities),
  ]

  prevalence$source <- factor(
    prevalence$source,
    levels = prevalence$source
  )

  dir.create(
    figure_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  prevalence_plot <- ggplot2::ggplot(
    prevalence,
    ggplot2::aes(source, percent_facilities)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0(
        "NC TRI Facilities With Nearby HazardGraph Sources, ",
        primary_buffer,
        " m"
      ),
      x = NULL,
      y = "TRI facilities with source nearby (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  priority_plot <- ggplot2::ggplot(
    exposed_primary,
    ggplot2::aes(ian_context_score_equal)
  ) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::labs(
      title = "Hurricane Ian Context Screening Scores",
      subtitle = paste0(
        exposed_facilities,
        " NC TRI facilities inside the 100-mile screening corridor; not a risk score"
      ),
      x = "Ian context screening score",
      y = "TRI facilities"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  top <- exposed_primary[
    order(
      exposed_primary$ian_context_score_equal,
      decreasing = TRUE
    ),
  ][seq_len(min(25, nrow(exposed_primary))), ]

  if ("facility_name" %in% names(top))
    top$plot_label <- substr(top$facility_name, 1, 45)
  else
    top$plot_label <- top$tri_facility_id

  top$plot_label <- make.unique(as.character(top$plot_label))
  top$plot_label <- factor(
    top$plot_label,
    levels = rev(top$plot_label)
  )

  top_plot <- ggplot2::ggplot(
    top,
    ggplot2::aes(plot_label, ian_context_score_equal)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0(
        "Highest Ian Context Screening Scores at ",
        primary_buffer,
        " m"
      ),
      subtitle = "TRI release magnitude is analyzed separately and is not included in this score",
      x = NULL,
      y = "Ian context screening score"
    ) +
    ggplot2::theme_minimal(base_size = 10)

  prevalence_figure <- file.path(
    figure_dir,
    paste0(
      "nc_hazardgraph_source_prevalence_",
      primary_buffer,
      "m_",
      year,
      ".png"
    )
  )

  priority_figure <- file.path(
    figure_dir,
    paste0(
      "nc_hazardgraph_ian_score_distribution_",
      primary_buffer,
      "m_",
      year,
      ".png"
    )
  )

  top_figure <- file.path(
    figure_dir,
    paste0(
      "nc_hazardgraph_top25_ian_context_",
      primary_buffer,
      "m_",
      year,
      ".png"
    )
  )

  ggplot2::ggsave(
    prevalence_figure,
    prevalence_plot,
    width = 10,
    height = 6,
    dpi = 300
  )

  ggplot2::ggsave(
    priority_figure,
    priority_plot,
    width = 9,
    height = 6,
    dpi = 300
  )

  ggplot2::ggsave(
    top_figure,
    top_plot,
    width = 11,
    height = 8,
    dpi = 300
  )

  cat("Statewide HazardGraph analysis figures created.\n")


  # Build Step 42 QA report
  score_fields <- c(
    "ian_screening_score",
    "environmental_inventory_score",
    "tri_release_magnitude_score",
    "substation_context_score",
    "emergency_response_capacity_score",
    "ian_context_score_equal",
    "ian_context_score_storm_heavy",
    "ian_context_score_inventory_heavy"
  )

  score_out_of_range <- sum(vapply(
    score_fields,
    function(field)
      sum(
        hazard[[field]] < 0 |
          hazard[[field]] > 1,
        na.rm = TRUE
      ),
    integer(1)
  ))

  missing_environment_scores <-
    sum(is.na(hazard$environmental_inventory_score))

  outside_nonzero <- sum(
    !hazard$within_ian_100mi &
      hazard$ian_context_score_equal > 0
  )

  missing_exposed_scores <- sum(
    hazard$within_ian_100mi &
      is.na(hazard$ian_context_score_equal)
  )

  release_facilities <- sum(
    is.finite(release$tri_total_release_lbs)
  )

  min_weight <- safe_min(
    sensitivity$spearman_rho[
      sensitivity$analysis_type == "weight_scenario"
    ]
  )

  min_buffer <- safe_min(
    sensitivity$spearman_rho[
      sensitivity$analysis_type == "buffer_sensitivity"
    ]
  )

  min_leave_one_out <- safe_min(
    sensitivity$spearman_rho[
      sensitivity$analysis_type == "leave_one_source_out"
    ]
  )

  qa <- data.frame(
    check = c(
      "Statewide TRI facilities",
      "Analysis facility-buffer rows",
      "Duplicate facility-buffer rows",
      "Ian-screened TRI facilities",
      "Environmental inventory source families",
      "Missing environmental inventory scores",
      "Facilities with TRI release values",
      "Missing TRI release magnitude scores",
      "TRI release score buffer inconsistencies",
      "Score values outside 0-1",
      "Outside-Ian rows with nonzero Ian context score",
      "Ian-screened rows missing Ian context score",
      "Primary analysis buffer meters",
      "Minimum weight-scenario Spearman rho",
      "Minimum buffer-sensitivity Spearman rho",
      "Minimum leave-one-source-out Spearman rho",
      "Statewide TRI release integration",
      "Statewide CDL integration",
      "Statewide HURDAT wind-radius integration",
      "Final ToxPi/composite interpretation"
    ),
    value = c(
      length(facilities),
      nrow(hazard),
      duplicate_keys,
      exposed_facilities,
      length(environmental_fields),
      missing_environment_scores,
      release_facilities,
      missing_release_scores,
      release_buffer_inconsistency,
      score_out_of_range,
      outside_nonzero,
      missing_exposed_scores,
      primary_buffer,
      round(min_weight, 4),
      round(min_buffer, 4),
      round(min_leave_one_out, 4),
      1,
      0,
      0,
      0
    ),
    status = c(
      ifelse(length(facilities) == 776, "PASS", "CHECK"),
      ifelse(nrow(hazard) == expected_rows, "PASS", "FAIL"),
      ifelse(duplicate_keys == 0, "PASS", "FAIL"),
      ifelse(exposed_facilities == 602, "PASS", "CHECK"),
      ifelse(length(environmental_fields) == 6, "PASS", "CHECK"),
      ifelse(missing_environment_scores == 0, "PASS", "FAIL"),
      ifelse(release_facilities == length(facilities), "PASS", "FAIL"),
      ifelse(missing_release_scores == 0, "PASS", "FAIL"),
      ifelse(release_buffer_inconsistency == 0, "PASS", "FAIL"),
      ifelse(score_out_of_range == 0, "PASS", "FAIL"),
      ifelse(outside_nonzero == 0, "PASS", "FAIL"),
      ifelse(missing_exposed_scores == 0, "PASS", "FAIL"),
      "INFO",
      "INFO",
      "INFO",
      "INFO",
      "PASS",
      "PENDING",
      "PENDING",
      "DEFERRED"
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("Statewide HazardGraph Step 42 QA/QC completed.\n")


  # Write twelve Step 42 outputs
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

  files <- c(
    file.path(
      output_dir,
      paste0("nc_hazardgraph_scored_features_", year, ".csv")
    ),
    file.path(
      output_dir,
      paste0(
        "nc_hazardgraph_facility_scores_",
        primary_buffer,
        "m_",
        year,
        ".csv"
      )
    ),
    file.path(
      output_dir,
      paste0(
        "nc_hazardgraph_feature_summary_",
        primary_buffer,
        "m_",
        year,
        ".csv"
      )
    ),
    file.path(
      output_dir,
      paste0(
        "nc_hazardgraph_feature_correlations_",
        primary_buffer,
        "m_",
        year,
        ".csv"
      )
    ),
    file.path(
      output_dir,
      paste0(
        "nc_hazardgraph_county_summary_",
        primary_buffer,
        "m_",
        year,
        ".csv"
      )
    ),
    file.path(
      output_dir,
      paste0("nc_hazardgraph_sensitivity_", year, ".csv")
    ),
    file.path(
      output_dir,
      paste0("nc_hazardgraph_score_definition_", year, ".csv")
    ),
    file.path(
      output_dir,
      paste0(
        "nc_hazardgraph_source_prevalence_",
        primary_buffer,
        "m_",
        year,
        ".csv"
      )
    ),
    file.path(
      validation_dir,
      paste0("qaqc_nc_hazardgraph_analysis_", year, ".csv")
    ),
    prevalence_figure,
    priority_figure,
    top_figure
  )

  write.csv(hazard, files[1], row.names = FALSE)
  write.csv(facility, files[2], row.names = FALSE)
  write.csv(feature_summary, files[3], row.names = FALSE)
  write.csv(correlation_pairs, files[4], row.names = FALSE)
  write.csv(county_summary, files[5], row.names = FALSE)
  write.csv(sensitivity, files[6], row.names = FALSE)
  write.csv(score_definition, files[7], row.names = FALSE)
  write.csv(prevalence, files[8], row.names = FALSE)
  write.csv(qa, files[9], row.names = FALSE)

  cat("Statewide HazardGraph Step 42 outputs written.\n")

  files
}


upload_statewide_hazardgraph_analysis <- function(
  files, processed_folder, validation_folder, figures_folder
) {

  # Validate and upload Step 42 outputs
  if (length(files) != 12 || !all(file.exists(files)))
    stop("Expected twelve Step 42 analysis output files.")

  csv_files <- files[grepl("\\.csv$", files)]
  qa_files <- csv_files[
    grepl("^qaqc_", basename(csv_files))
  ]

  processed_files <- setdiff(csv_files, qa_files)
  figure_files <- files[grepl("\\.png$", files)]

  invisible(lapply(
    processed_files,
    googledrive::drive_upload,
    path = processed_folder,
    overwrite = TRUE
  ))

  invisible(lapply(
    qa_files,
    googledrive::drive_upload,
    path = validation_folder,
    overwrite = TRUE
  ))

  invisible(lapply(
    figure_files,
    googledrive::drive_upload,
    path = figures_folder,
    overwrite = TRUE
  ))

  cat("Statewide HazardGraph Step 42 outputs uploaded.\n")

  TRUE
}