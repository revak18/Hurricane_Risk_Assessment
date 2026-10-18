create_mrds_maps <- function(mrds_classified_files, county_file, target_county_fips, figures_dir) {
  # Load classified MRDS sites and county geography
  nc <- read.csv(mrds_classified_files[[1]], stringsAsFactors = FALSE)
  mecklenburg <- read.csv(mrds_classified_files[[2]], stringsAsFactors = FALSE)
  counties <- sf::st_read(county_file, quiet = TRUE)

  if (!nrow(nc) || !nrow(mecklenburg)) stop("MRDS classified tables are empty.")
  cat("MRDS map data loaded.\n")

  # Convert MRDS records to spatial points
  nc_sf <- sf::st_as_sf(nc, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  mecklenburg_sf <- sf::st_as_sf(mecklenburg, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  nc_sf <- sf::st_transform(nc_sf, sf::st_crs(counties))
  mecklenburg_sf <- sf::st_transform(mecklenburg_sf, sf::st_crs(counties))

  mecklenburg_county <- counties[as.character(counties$GEOID) == target_county_fips, ]

  if (nrow(mecklenburg_county) != 1) stop("Expected one Mecklenburg County polygon.")
  cat("MRDS spatial layers prepared.\n")

  # Standardize resource category labels
  category_levels <- c("precious_metal", "mixed_metal", "construction_aggregate", "industrial_mineral", "ferrous_metal", "base_metal", "mixed_resource", "other_or_unknown")

  mecklenburg_sf$resource_category <- factor(
    mecklenburg_sf$resource_category,
    levels = category_levels
  )

  category_labels <- c(
    precious_metal = "Precious metal",
    mixed_metal = "Mixed metal",
    construction_aggregate = "Construction aggregate",
    industrial_mineral = "Industrial mineral",
    ferrous_metal = "Ferrous metal",
    base_metal = "Base metal",
    mixed_resource = "Mixed resource",
    other_or_unknown = "Other / unknown"
  )

  cat("MRDS resource categories prepared.\n")

  # Create the statewide North Carolina MRDS map
  overview <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = counties, fill = "grey97", color = "grey75", linewidth = 0.25) +
    ggplot2::geom_sf(data = mecklenburg_county, fill = "gold", color = "black", linewidth = 0.8, alpha = 0.35) +
    ggplot2::geom_sf(data = nc_sf, color = "grey30", size = 0.7, alpha = 0.45) +
    ggplot2::geom_sf(data = mecklenburg_sf, color = "red3", size = 1.7, alpha = 0.9) +
    ggplot2::labs(
      title = "USGS MRDS Mineral-Resource Sites in North Carolina",
      subtitle = paste0(nrow(nc), " spatially assigned NC sites; ", nrow(mecklenburg), " in Mecklenburg County"),
      caption = "USGS MRDS sites are mineral-resource records and do not imply active mining in 2022."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold")
    )

  cat("North Carolina MRDS overview map built.\n")

  # Create the Mecklenburg resource-category map
  detail <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = mecklenburg_county, fill = "grey97", color = "black", linewidth = 0.9) +
    ggplot2::geom_sf(data = mecklenburg_sf, ggplot2::aes(color = resource_category), size = 2.4, alpha = 0.85) +
    ggplot2::scale_color_brewer(palette = "Dark2", drop = TRUE, labels = category_labels, name = "Resource category") +
    ggplot2::labs(
      title = "USGS MRDS Mineral-Resource Sites in Mecklenburg County",
      subtitle = paste0(nrow(mecklenburg), " sites classified by reported commodity type"),
      caption = "Classification reflects MRDS commodity metadata, not operational mine status."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold")
    )

  cat("Mecklenburg MRDS detail map built.\n")

  # Write the MRDS map figures
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  overview_file <- file.path(figures_dir, "nc_mrds_sites_overview.png")
  detail_file <- file.path(figures_dir, "mecklenburg_mrds_resource_sites.png")

  ggplot2::ggsave(overview_file, overview, width = 11, height = 8.5, dpi = 220)
  ggplot2::ggsave(detail_file, detail, width = 10, height = 8.5, dpi = 220)

  cat("MRDS map figures written.\n")
  c(overview_file, detail_file)
}

upload_mrds_maps <- function(files, figures_folder) {
  # Validate MRDS map figures
  if (length(files) != 2 || !all(file.exists(files))) stop("Expected two MRDS map figures.")
  cat("MRDS map figures validated.\n")

  # Upload MRDS map figures
  invisible(lapply(files, googledrive::drive_upload, path = figures_folder, overwrite = TRUE))

  cat("MRDS map figures uploaded.\n")
  TRUE
}