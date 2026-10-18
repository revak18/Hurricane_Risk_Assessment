# Define shared project settings
options(timeout = 900, scipen = 999)

year <- 2022L
state_abbr <- "NC"
state_fips <- "37"
county_fips <- "37119"
target_city <- "CHARLOTTE"
target_crs <- 5070
buffer_m <- c(300, 500, 1000)

storm_id <- "AL092022"
cat("Hurricane Ian settings ready.\n")

# Define Hurricane Ian exposure settings
ian_exposure_miles <- 100
cat("Hurricane Ian exposure settings ready.\n")

project_dir <- "/content/hazardgraph_pipeline"
local_base <- "/content/hazardgraph"
cat("Project settings ready.\n")

# Create local pipeline folders
local_dirs <- c(
  file.path(local_base, "raw", c("tri", "cdl", "mines", "hurricanes", "geography")),
  file.path(local_base, "processed", c("tri", "cdl", "mines", "hurricanes", "geography", "exposure_features")),
  file.path(local_base, c("metadata", "validation", "figures"))
)

invisible(lapply(
  local_dirs,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))
cat("Local project folders ready.\n")

# Load Google Drive folder IDs created by bootstrap
folder_file <- file.path(project_dir, "drive_folders.rds")

if (!file.exists(folder_file)) {
  stop("Run 00_bootstrap.R before starting the pipeline.")
}

drive_folders <- readRDS(folder_file)
cat("Google Drive folder IDs loaded.\n")

# Convert Google Drive IDs into pipeline folder objects
raw_folders <- list(
  tri = googledrive::as_id(drive_folders$raw_tri),
  cdl = googledrive::as_id(drive_folders$raw_cdl),
  geography = googledrive::as_id(drive_folders$raw_geography),
  mines = googledrive::as_id(drive_folders$raw_mines),
  hurricanes = googledrive::as_id(drive_folders$raw_hurricanes),
  livestock = googledrive::as_id(drive_folders$raw_livestock),
  superfund = googledrive::as_id(drive_folders$raw_superfund),
  brownfields = googledrive::as_id(drive_folders$raw_brownfields),
  wastewater = googledrive::as_id(drive_folders$raw_wastewater),
  rmp = googledrive::as_id(drive_folders$raw_rmp),
  substations = googledrive::as_id(drive_folders$raw_substations),
  hospitals = googledrive::as_id(drive_folders$raw_hospitals)
)

processed_folders <- list(
  tri = googledrive::as_id(drive_folders$processed_tri),
  cdl = googledrive::as_id(drive_folders$processed_cdl),
  geography = googledrive::as_id(drive_folders$processed_geography),
  mines = googledrive::as_id(drive_folders$processed_mines),
  hurricanes = googledrive::as_id(drive_folders$processed_hurricanes),
  exposure_features = googledrive::as_id(drive_folders$processed_exposure_features),
  livestock = googledrive::as_id(drive_folders$processed_livestock),
  superfund = googledrive::as_id(drive_folders$processed_superfund),
  brownfields = googledrive::as_id(drive_folders$processed_brownfields),
  wastewater = googledrive::as_id(drive_folders$processed_wastewater),
  rmp = googledrive::as_id(drive_folders$processed_rmp),
  substations = googledrive::as_id(drive_folders$processed_substations),
  hospitals = googledrive::as_id(drive_folders$processed_hospitals)
)

metadata_folder <- googledrive::as_id(drive_folders$metadata)
validation_folder <- googledrive::as_id(drive_folders$validation)
figures_folder <- googledrive::as_id(drive_folders$figures)
cat("Google Drive pipeline folders ready.\n")