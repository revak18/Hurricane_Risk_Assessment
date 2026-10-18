build_mrds_visualizations <- function(mrds_files, county_file, target_county_fips, target_crs, figures_dir, validation_dir) {
  # Locate and spatialize the statewide MRDS FIPS table
  mrds_file <- mrds_files[basename(mrds_files) == "nc_mrds_sites_fips.csv"][1]

  if (is.na(mrds_file) || !file.exists(mrds_file)) {
    stop("Could not locate nc_mrds_sites_fips.csv.")
  }

  mrds_table <- read.csv(mrds_file, stringsAsFactors = FALSE)
  required_fields <- c("latitude", "longitude")
  missing_fields <- setdiff(required_fields, names(mrds_table))

  if (length(missing_fields)) {
    stop(paste("Missing MRDS coordinate fields:", paste(missing_fields, collapse = ", ")))
  }

  mrds_table$latitude <- suppressWarnings(as.numeric(mrds_table$latitude))
  mrds_table$longitude <- suppressWarnings(as.numeric(mrds_table$longitude))

  valid_coordinates <- is.finite(mrds_table$latitude) &
    is.finite(mrds_table$longitude) &
    mrds_table$latitude >= -90 &
    mrds_table$latitude <= 90 &
    mrds_table$longitude >= -180 &
    mrds_table$longitude <= 180

  if (!all(valid_coordinates)) {
    stop("Statewide MRDS FIPS table contains invalid coordinates.")
  }

  mrds <- sf::st_as_sf(mrds_table, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  mrds <- sf::st_transform(mrds, target_crs)

  cat("Statewide MRDS spatial table built:", nrow(mrds), "records.\n")

  # Load and prepare North Carolina counties
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(counties) || !"GEOID" %in% names(counties)) {
    stop("Valid TIGER county input with GEOID is required.")
  }

  counties <- sf::st_transform(counties, target_crs)
  mecklenburg <- counties[as.character(counties$GEOID) == target_county_fips, ]

  if (nrow(mecklenburg) != 1) {
    stop("Could not resolve exactly one Mecklenburg County polygon.")
  }

  cat("North Carolina and Mecklenburg boundaries prepared.\n")

  # Identify Mecklenburg MRDS records spatially
  mecklenburg_index <- lengths(sf::st_intersects(mrds, mecklenburg)) > 0
  mecklenburg_mrds <- mrds[mecklenburg_index, ]
  statewide_count <- nrow(mrds)
  mecklenburg_count <- nrow(mecklenburg_mrds)

  cat("Statewide and Mecklenburg MRDS counts calculated.\n")

  # Build the statewide MRDS point map
  statewide_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = counties, fill = "grey97", color = "grey75", linewidth = 0.25) +
    ggplot2::geom_sf(data = mrds, size = 0.65, alpha = 0.45) +
    ggplot2::geom_sf(data = mecklenburg, fill = NA, color = "red", linewidth = 1.1) +
    ggplot2::labs(
      title = paste0("North Carolina MRDS Mineral-Resource Sites (n = ", format(statewide_count, big.mark = ","), ")"),
      subtitle = paste0("Every QA-clean spatial MRDS record is plotted; ", "Mecklenburg County is outlined in red"),
      caption = paste0("MRDS records represent mineral-resource/deposit sites; ", "they should not automatically be interpreted as active mines.")
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  cat("Statewide MRDS point map built.\n")

  # Build the Mecklenburg MRDS zoom map
  mecklenburg_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = mecklenburg, fill = "grey97", color = "grey40", linewidth = 0.8) +
    ggplot2::geom_sf(data = mecklenburg_mrds, size = 2, alpha = 0.7) +
    ggplot2::labs(
      title = paste0("Mecklenburg County MRDS Sites (n = ", format(mecklenburg_count, big.mark = ","), ")"),
      subtitle = "QA-clean MRDS mineral-resource/deposit locations",
      caption = paste0("Spatial county assignment uses TIGER boundaries and FIPS ", target_county_fips, ".")
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  cat("Mecklenburg MRDS zoom map built.\n")

  # Calculate spatial MRDS counts for every NC county
  county_membership <- sf::st_intersects(counties, mrds)
  counties$mrds_site_count <- lengths(county_membership)

  cat("County-level MRDS site counts calculated.\n")

  # Build the county-level MRDS count map
  county_plot <- ggplot2::ggplot(counties) +
    ggplot2::geom_sf(ggplot2::aes(fill = mrds_site_count), color = "white", linewidth = 0.25) +
    ggplot2::geom_sf(data = mecklenburg, fill = NA, color = "red", linewidth = 1) +
    ggplot2::scale_fill_viridis_c(name = "MRDS sites", option = "C") +
    ggplot2::labs(
      title = "MRDS Mineral-Resource Sites by North Carolina County",
      subtitle = paste0("Site counts calculated by spatial intersection with ", "TIGER county boundaries"),
      caption = "Mecklenburg County is outlined in red."
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  cat("County-level MRDS count map built.\n")

  # Build a top-county MRDS ranking table
  county_names <- if ("NAME" %in% names(counties)) as.character(counties$NAME) else as.character(counties$GEOID)

  county_counts <- data.frame(
    county_geoid = as.character(counties$GEOID),
    county_name = county_names,
    mrds_site_count = counties$mrds_site_count,
    stringsAsFactors = FALSE
  )

  county_counts <- county_counts[order(county_counts$mrds_site_count, decreasing = TRUE), ]
  rownames(county_counts) <- NULL

  cat("County-level MRDS ranking table built.\n")

  # Validate plotted MRDS records
  empty_geometry <- sum(sf::st_is_empty(mrds))
  invalid_geometry <- sum(!sf::st_is_valid(mrds))
  county_total <- sum(counties$mrds_site_count)
  spatial_count_difference <- county_total - statewide_count

  qa <- data.frame(
    check = c(
      "Statewide MRDS records plotted",
      "Mecklenburg MRDS records plotted",
      "Empty MRDS geometries",
      "Invalid MRDS geometries",
      "NC counties",
      "Counties with at least one MRDS record",
      "Sum of county MRDS counts",
      "Difference between county count sum and statewide records"
    ),
    value = c(
      statewide_count,
      mecklenburg_count,
      empty_geometry,
      invalid_geometry,
      nrow(counties),
      sum(counties$mrds_site_count > 0),
      county_total,
      spatial_count_difference
    ),
    status = c(
      "INFO",
      "INFO",
      ifelse(empty_geometry == 0, "PASS", "FAIL"),
      ifelse(invalid_geometry == 0, "PASS", "FAIL"),
      "INFO",
      "INFO",
      "INFO",
      ifelse(spatial_count_difference == 0, "PASS", "CHECK")
    ),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)

  cat("MRDS visualization QA/QC completed.\n")

  # Write MRDS visualization outputs
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  statewide_png <- file.path(figures_dir, "nc_mrds_sites_statewide.png")
  statewide_pdf <- file.path(figures_dir, "nc_mrds_sites_statewide.pdf")
  mecklenburg_png <- file.path(figures_dir, "mecklenburg_mrds_sites.png")
  county_png <- file.path(figures_dir, "nc_mrds_sites_by_county.png")
  county_counts_file <- file.path(figures_dir, "nc_mrds_site_counts_by_county.csv")
  qa_file <- file.path(validation_dir, "qaqc_mrds_visualization.csv")

  ggplot2::ggsave(statewide_png, statewide_plot, width = 12, height = 8, dpi = 300)
  ggplot2::ggsave(statewide_pdf, statewide_plot, width = 12, height = 8)
  ggplot2::ggsave(mecklenburg_png, mecklenburg_plot, width = 10, height = 8, dpi = 300)
  ggplot2::ggsave(county_png, county_plot, width = 12, height = 8, dpi = 300)

  write.csv(county_counts, county_counts_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("MRDS visualization outputs written.\n")

  c(statewide_png, statewide_pdf, mecklenburg_png, county_png, county_counts_file, qa_file)
}

upload_mrds_visualizations <- function(files, figures_folder, validation_folder) {
  # Validate MRDS visualization files
  if (length(files) != 6 || !all(file.exists(files))) {
    stop("Expected six MRDS visualization output files.")
  }

  cat("MRDS visualization files validated.\n")

  # Upload MRDS visualization outputs
  invisible(lapply(files[1:5], googledrive::drive_upload, path = figures_folder, overwrite = TRUE))

  cat("MRDS visualization figures and county counts uploaded.\n")

  # Upload MRDS visualization QA
  googledrive::drive_upload(files[[6]], path = validation_folder, overwrite = TRUE)

  cat("MRDS visualization QA uploaded.\n")

  TRUE
}