create_ian_maps <- function(ian_track_files, ian_exposure_files, county_file, target_county_fips, target_crs, figures_dir, exposure_miles = 100) {
  # Load Ian, exposure, and county spatial data
  counties <- sf::st_read(county_file, quiet = TRUE)
  ian_line <- sf::st_read(ian_track_files[[3]], quiet = TRUE)
  ian_zone <- sf::st_read(ian_exposure_files[[1]], quiet = TRUE)
  exposed_counties <- sf::st_read(ian_exposure_files[[2]], quiet = TRUE)
  exposed_tri <- read.csv(ian_exposure_files[[4]], stringsAsFactors = FALSE)
  exposed_mrds <- read.csv(ian_exposure_files[[5]], stringsAsFactors = FALSE)

  cat("Ian mapping inputs loaded.\n")

  # Prepare all spatial layers in the project CRS
  counties <- sf::st_transform(counties, target_crs)
  ian_line <- sf::st_transform(ian_line, target_crs)
  ian_zone <- sf::st_transform(ian_zone, target_crs)
  exposed_counties <- sf::st_transform(exposed_counties, target_crs)
  mecklenburg <- counties[as.character(counties$GEOID) == target_county_fips, ]

  if (nrow(mecklenburg) != 1) stop("Expected one Mecklenburg County polygon.")
  cat("Ian mapping spatial layers prepared.\n")

  # Convert exposed TRI and MRDS records to spatial points
  tri_sf <- sf::st_as_sf(exposed_tri, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  mrds_sf <- sf::st_as_sf(exposed_mrds, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  tri_sf <- sf::st_transform(tri_sf, target_crs)
  mrds_sf <- sf::st_transform(mrds_sf, target_crs)

  tri_mecklenburg <- tri_sf[as.character(tri_sf$county_geoid) == target_county_fips, ]
  mrds_mecklenburg <- mrds_sf[as.character(mrds_sf$county_geoid) == target_county_fips, ]

  cat("Ian-exposed TRI and MRDS points prepared.\n")

  # Define the North Carolina map extent with storm-path context
  margin_m <- exposure_miles * 1609.344
  nc_extent <- sf::st_bbox(sf::st_buffer(sf::st_union(counties), dist = margin_m))
  ian_line_map <- suppressWarnings(sf::st_crop(ian_line, nc_extent))
  ian_zone_map <- suppressWarnings(sf::st_crop(ian_zone, nc_extent))

  cat("Ian map extent prepared.\n")

  # Build the statewide Ian exposure overview
  overview <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = counties, fill = "grey96", color = "grey75", linewidth = 0.25) +
    ggplot2::geom_sf(data = exposed_counties, fill = "lightskyblue2", color = "grey55", linewidth = 0.25, alpha = 0.55) +
    ggplot2::geom_sf(data = ian_zone_map, fill = "gold", color = "orange3", linewidth = 0.8, alpha = 0.20) +
    ggplot2::geom_sf(data = ian_line_map, color = "red3", linewidth = 1.2) +
    ggplot2::geom_sf(data = mecklenburg, fill = NA, color = "black", linewidth = 1.2) +
    ggplot2::coord_sf(xlim = c(nc_extent["xmin"], nc_extent["xmax"]), ylim = c(nc_extent["ymin"], nc_extent["ymax"]), expand = FALSE) +
    ggplot2::labs(
      title = "Hurricane Ian and the 100-Mile North Carolina Screening Zone",
      subtitle = paste0(nrow(exposed_counties), " NC counties intersect the ", exposure_miles, "-mile track buffer"),
      caption = paste0("Ian track: NOAA HURDAT2. ", exposure_miles, "-mile zone is a proximity-screening layer, not an observed wind-field boundary.")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  cat("Ian statewide exposure overview built.\n")

  # Build the Ian environmental-source exposure map
  sources <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = counties, fill = "grey97", color = "grey80", linewidth = 0.25) +
    ggplot2::geom_sf(data = ian_zone_map, fill = "gold", color = "orange3", linewidth = 0.7, alpha = 0.12) +
    ggplot2::geom_sf(data = ian_line_map, color = "red3", linewidth = 1.1) +
    ggplot2::geom_sf(data = mrds_sf, ggplot2::aes(color = "MRDS mineral-resource sites"), size = 0.9, alpha = 0.45) +
    ggplot2::geom_sf(data = tri_sf, ggplot2::aes(color = "TRI facilities"), size = 1.5, alpha = 0.75) +
    ggplot2::geom_sf(data = mecklenburg, fill = NA, color = "black", linewidth = 1.2) +
    ggplot2::geom_sf(data = mrds_mecklenburg, shape = 21, fill = "white", color = "black", size = 2.0, stroke = 0.5) +
    ggplot2::geom_sf(data = tri_mecklenburg, shape = 24, fill = "red", color = "black", size = 2.3, stroke = 0.5) +
    ggplot2::scale_color_manual(
      values = c("TRI facilities" = "red3", "MRDS mineral-resource sites" = "steelblue4"),
      name = "Exposure source"
    ) +
    ggplot2::coord_sf(xlim = c(nc_extent["xmin"], nc_extent["xmax"]), ylim = c(nc_extent["ymin"], nc_extent["ymax"]), expand = FALSE) +
    ggplot2::labs(
      title = "Environmental Sources Within 100 Miles of Hurricane Ian",
      subtitle = paste0(format(nrow(exposed_tri), big.mark = ","), " TRI facilities and ", format(nrow(exposed_mrds), big.mark = ","), " MRDS sites within the screening zone"),
      caption = paste0("Mecklenburg outlined in black; ", nrow(tri_mecklenburg), " TRI facilities and ", nrow(mrds_mecklenburg), " MRDS sites are located there. MRDS records do not imply active mining in 2022.")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  cat("Ian environmental-source exposure map built.\n")

  # Write the Ian map figures
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  overview_file <- file.path(figures_dir, "ian_nc_exposure_overview_2022.png")
  sources_file <- file.path(figures_dir, "ian_environmental_sources_2022.png")

  ggplot2::ggsave(overview_file, overview, width = 11, height = 8.5, dpi = 220)
  ggplot2::ggsave(sources_file, sources, width = 11, height = 8.5, dpi = 220)

  cat("Ian map figures written.\n")
  c(overview_file, sources_file)
}

upload_ian_maps <- function(files, figures_folder) {
  # Validate Ian map figures
  if (length(files) != 2 || !all(file.exists(files))) stop("Expected two Hurricane Ian map figures.")
  cat("Ian map figures validated.\n")

  # Upload Ian map figures
  invisible(lapply(files, googledrive::drive_upload, path = figures_folder, overwrite = TRUE))

  cat("Ian map figures uploaded.\n")
  TRUE
}