# Load configuration and pipeline functions
source("00_config.R")
source("01_tri.R")
source("02_cdl.R")
source("03_qaqc.R")
source("04_geography.R")
source("05_tri_fips.R")
source("06_tri_geography_qaqc.R")
source("07_mines.R")
source("08_mines_fips.R")
source("09_mines_classify.R")
source("10_mines_map.R")
source("11_hurricanes.R")
source("12_hurricane_exposure.R")
source("13_hurricane_map.R")
source("14_tri_cdl_buffers.R")
source("15_integrated_exposure.R")
source("16_ian_wind_exposure.R")
source("17_hazard_features.R")
source("18_mrds_proximity_features.R")
source("19_integrate_mrds.R")
source("20_livestock.R")
source("21_afo_proximity_features.R")
source("22_superfund.R")
source("23_superfund_proximity_features.R")
source("24_brownfields.R")
source("25_brownfields_proximity_features.R")
source("26_integrate_brownfields.R")
source("27_wastewater.R")
source("28_wastewater_proximity_features.R")
source("29_integrate_wastewater.R")
source("30_rmp.R")
source("31_rmp_proximity_features.R")
source("32_integrate_rmp.R")
source("33_substations.R")
source("34_substation_proximity_features.R")
source("35_integrate_substations.R")
source("36_hospitals.R")
source("37_hospital_proximity_features.R")
source("38_integrate_hospitals.R")
source("39_mrds_visualization.R")
source("40_mrds_record_reconciliation.R")
source("41_build_statewide_hazardgraph.R")
source("42_hazardgraph_analysis.R")
cat("Pipeline files loaded.\n")

# Register packages used by pipeline targets
targets::tar_option_set(packages = c("amadeus", "googledrive", "readr", "terra", "sf", "ggplot2"))
cat("Pipeline packages registered.\n")

# Define local pipeline paths
tri_raw_dir <- file.path(local_base, "raw", "tri")
tri_output_dir <- file.path(local_base, "processed", "tri")
cdl_output_dir <- file.path(local_base, "processed", "cdl")
qaqc_output_dir <- file.path(local_base, "validation")
geography_raw_dir <- file.path(local_base, "raw", "geography")
geography_output_dir <- file.path(local_base, "processed", "geography")
mines_raw_dir <- file.path(local_base, "raw", "mines")
mines_output_dir <- file.path(local_base, "processed", "mines")
hurricanes_raw_dir <- file.path(local_base, "raw", "hurricanes")
hurricanes_output_dir <- file.path(local_base, "processed", "hurricanes")
exposure_features_output_dir <- file.path(local_base, "processed", "exposure_features")
livestock_raw_dir <- file.path(local_base, "raw", "livestock")
livestock_output_dir <- file.path(local_base, "processed", "livestock")
superfund_raw_dir <- file.path(local_base, "raw", "superfund")
superfund_output_dir <- file.path(local_base, "processed", "superfund")
brownfields_raw_dir <- file.path(local_base, "raw", "brownfields")
brownfields_output_dir <- file.path(local_base, "processed", "brownfields")
wastewater_raw_dir <- file.path(local_base, "raw", "wastewater")
wastewater_output_dir <- file.path(local_base, "processed", "wastewater")
rmp_raw_dir <- file.path(local_base, "raw", "rmp")
rmp_output_dir <- file.path(local_base, "processed", "rmp")
substations_raw_dir <- file.path(local_base, "raw", "substations")
substations_output_dir <- file.path(local_base, "processed", "substations")
hospitals_raw_dir <- file.path(local_base, "raw", "hospitals")
hospitals_output_dir <- file.path(local_base, "processed", "hospitals")
exposure_output_dir <- file.path(local_base, "processed", "exposure_features")

figures_output_dir <- file.path(local_base, "figures")
cat("Pipeline paths ready.\n")

# Define the HazardGraph target pipeline
list(
  targets::tar_target(tri_raw_file, download_tri_raw(year, state_abbr, tri_raw_dir, raw_folders$tri), format = "file"),
  targets::tar_target(tri_files, build_tri_tables(tri_raw_file, year, state_abbr, tri_output_dir), format = "file"),
  targets::tar_target(tri_upload, upload_tri_outputs(tri_raw_file, tri_files, raw_folders$tri, processed_folders$tri, metadata_folder)),
  targets::tar_target(cdl_file, download_cdl_crop(year, county_fips, nc_counties, processed_folders$cdl, cdl_output_dir, target_crs, buffer_m), format = "file"),
  targets::tar_target(nc_counties_raw, download_nc_counties(year, state_fips, geography_raw_dir), format = "file"),
  targets::tar_target(nc_counties, prepare_nc_counties(nc_counties_raw, geography_output_dir, target_crs), format = "file"),
  targets::tar_target(nc_counties_upload, upload_nc_counties(nc_counties_raw, nc_counties, raw_folders$geography, processed_folders$geography)),
  targets::tar_target(tri_fips_files, assign_tri_fips(tri_files, nc_counties, year, county_fips, tri_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(tri_fips_upload, upload_tri_fips(tri_fips_files, processed_folders$tri, validation_folder)),
  targets::tar_target(qaqc_report, run_qaqc(tri_files, tri_fips_files, cdl_file, year, county_fips, qaqc_output_dir), format = "file"),
  targets::tar_target(qaqc_upload, upload_qaqc(qaqc_report, validation_folder)),
  targets::tar_target(tri_geography_qaqc_files, audit_tri_geography(tri_fips_files, year, county_fips, qaqc_output_dir), format = "file"),
  targets::tar_target(tri_geography_qaqc_upload, upload_tri_geography_qaqc(tri_geography_qaqc_files, validation_folder)),
  targets::tar_target(mrds_raw_file, download_mrds_raw(mines_raw_dir, raw_folders$mines), format = "file"),
  targets::tar_target(mrds_schema_files, inspect_mrds_schema(mrds_raw_file, qaqc_output_dir), format = "file"),
  targets::tar_target(mrds_schema_upload, upload_mrds_schema(mrds_schema_files, validation_folder)),
  # Spatially assign MRDS sites to NC county FIPS
  targets::tar_target(mrds_fips_files, assign_mrds_fips(mrds_raw_file, nc_counties, year, county_fips, mines_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(mrds_fips_upload, upload_mrds_fips(mrds_fips_files, processed_folders$mines, validation_folder)),
  # Classify NC and Mecklenburg MRDS sites
  targets::tar_target(mrds_classified_files, classify_mrds_sites(mrds_fips_files, mines_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(mrds_classification_upload, upload_mrds_classification(mrds_classified_files, processed_folders$mines, validation_folder)),
  # Create statewide and Mecklenburg MRDS maps
  targets::tar_target(mrds_map_files, create_mrds_maps(mrds_classified_files, nc_counties, county_fips, figures_output_dir), format = "file"),
  targets::tar_target(mrds_maps_upload, upload_mrds_maps(mrds_map_files, figures_folder)),
  # Build the 2022 Hurricane Ian track
  targets::tar_target(hurdat2_raw_file, download_hurdat2_raw(hurricanes_raw_dir, raw_folders$hurricanes), format = "file"),
  targets::tar_target(ian_track_files, build_ian_track(hurdat2_raw_file, year, storm_id, hurricanes_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(ian_track_upload, upload_ian_track(ian_track_files, processed_folders$hurricanes, validation_folder)),
  # Build and upload Hurricane Ian exposure layers
  targets::tar_target(ian_exposure_files, build_ian_exposure(ian_track_files, tri_fips_files, mrds_classified_files, nc_counties, year, county_fips, target_crs, hurricanes_output_dir, qaqc_output_dir, ian_exposure_miles), format = "file"),
  targets::tar_target(ian_exposure_upload, upload_ian_exposure(ian_exposure_files, processed_folders$hurricanes, validation_folder)),
  # Create and upload Hurricane Ian maps
  targets::tar_target(ian_map_files, create_ian_maps(ian_track_files, ian_exposure_files, nc_counties, county_fips, target_crs, figures_output_dir, ian_exposure_miles), format = "file"),
  targets::tar_target(ian_maps_upload, upload_ian_maps(ian_map_files, figures_folder)),
  # Build and upload TRI-CDL buffer exposure features
  targets::tar_target(tri_cdl_buffer_files, build_tri_cdl_buffers(tri_fips_files, cdl_file, year, county_fips, buffer_m, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(tri_cdl_buffer_upload, upload_tri_cdl_buffers(tri_cdl_buffer_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload the integrated exposure feature table
  targets::tar_target(integrated_exposure_files, build_integrated_exposure(tri_fips_files, tri_cdl_buffer_files, ian_exposure_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(integrated_exposure_upload, upload_integrated_exposure(integrated_exposure_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload Ian wind-radius exposure features
  targets::tar_target(ian_wind_exposure_files, build_ian_wind_exposure(ian_track_files, tri_fips_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(ian_wind_exposure_upload, upload_ian_wind_exposure(ian_wind_exposure_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload final HazardGraph feature tables
  targets::tar_target(hazard_feature_files, build_hazard_features(integrated_exposure_files, ian_wind_exposure_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hazard_feature_upload, upload_hazard_features(hazard_feature_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload TRI-MRDS proximity features
  targets::tar_target(mrds_proximity_files, build_mrds_proximity_features(tri_fips_files, mrds_classified_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(mrds_proximity_upload, upload_mrds_proximity_features(mrds_proximity_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload HazardGraph-MRDS features
  targets::tar_target(hazardgraph_mrds_files, build_hazardgraph_mrds(hazard_feature_files, mrds_proximity_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hazardgraph_mrds_upload, upload_hazardgraph_mrds(hazardgraph_mrds_files, processed_folders$exposure_features, validation_folder)),
  # Download, process, and upload NC AFO data
  targets::tar_target(afo_raw_file, download_afo_raw(livestock_raw_dir, raw_folders$livestock), format = "file"),
  targets::tar_target(afo_files, build_afo_tables(afo_raw_file, nc_counties, county_fips, target_crs, livestock_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(afo_upload, upload_afo_outputs(afo_files, processed_folders$livestock, metadata_folder, validation_folder)),
  # Build and upload TRI-AFO proximity features
  targets::tar_target(afo_proximity_files, build_afo_proximity_features(tri_fips_files, afo_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(afo_proximity_upload, upload_afo_proximity_features(afo_proximity_files, processed_folders$exposure_features, validation_folder)),
  # Download, process, and upload EPA Superfund data
  targets::tar_target(superfund_raw_file, download_superfund_raw(superfund_raw_dir, raw_folders$superfund), format = "file"),
  targets::tar_target(superfund_files, build_superfund_tables(superfund_raw_file, nc_counties, county_fips, target_crs, superfund_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(superfund_upload, upload_superfund_outputs(superfund_files, processed_folders$superfund, metadata_folder, validation_folder)),
  # Build and upload TRI-Superfund proximity features
  targets::tar_target(superfund_proximity_files, build_superfund_proximity_features(tri_fips_files, superfund_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(superfund_proximity_upload, upload_superfund_proximity_features(superfund_proximity_files, processed_folders$exposure_features, validation_folder)),
  # Download, process, and upload NC Brownfields data
  targets::tar_target(brownfields_raw_file, download_brownfields_raw(brownfields_raw_dir, raw_folders$brownfields), format = "file"),
  targets::tar_target(brownfields_files, build_brownfields_tables(brownfields_raw_file, nc_counties, county_fips, target_crs, brownfields_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(brownfields_upload, upload_brownfields_outputs(brownfields_files, processed_folders$brownfields, metadata_folder, validation_folder)),
  # Build and upload TRI-Brownfields proximity features
  targets::tar_target(brownfields_proximity_files, build_brownfields_proximity_features(tri_fips_files, brownfields_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(brownfields_proximity_upload, upload_brownfields_proximity_features(brownfields_proximity_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload the consolidated HazardGraph table
  targets::tar_target(hazardgraph_consolidated_files, build_hazardgraph_brownfields(hazardgraph_mrds_files, brownfields_proximity_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hazardgraph_consolidated_upload, upload_hazardgraph_brownfields(hazardgraph_consolidated_files, processed_folders$exposure_features, validation_folder)),
  # Download, process, and upload NC wastewater permits
  targets::tar_target(wastewater_raw_file, download_wastewater_raw(wastewater_raw_dir, raw_folders$wastewater), format = "file"),
  targets::tar_target(wastewater_files, build_wastewater_tables(wastewater_raw_file, nc_counties, county_fips, target_crs, wastewater_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(wastewater_upload, upload_wastewater_outputs(wastewater_files, processed_folders$wastewater, metadata_folder, validation_folder)),
  # Build and upload TRI-wastewater proximity features
  targets::tar_target(wastewater_proximity_files, build_wastewater_proximity_features(tri_fips_files, wastewater_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(wastewater_proximity_upload, upload_wastewater_proximity_features(wastewater_proximity_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload HazardGraph wastewater integration
  targets::tar_target(hazardgraph_wastewater_files, build_hazardgraph_wastewater(hazardgraph_consolidated_files, wastewater_proximity_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hazardgraph_wastewater_upload, upload_hazardgraph_wastewater(hazardgraph_wastewater_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload EPA RMP facility snapshot
  targets::tar_target(rmp_raw_file, download_rmp_raw(rmp_raw_dir, raw_folders$rmp, state_abbr), format = "file"),
  targets::tar_target(rmp_files, build_rmp_tables(rmp_raw_file, nc_counties, county_fips, target_crs, rmp_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(rmp_upload, upload_rmp_outputs(rmp_files, processed_folders$rmp, metadata_folder, validation_folder)),
  # Build and upload TRI-RMP proximity features
  targets::tar_target(rmp_proximity_files, build_rmp_proximity_features(tri_fips_files, rmp_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(rmp_proximity_upload, upload_rmp_proximity_features(rmp_proximity_files, processed_folders$exposure_features, validation_folder)),
  # Build and upload HazardGraph RMP integration
  targets::tar_target(hazardgraph_rmp_files, build_hazardgraph_rmp(hazardgraph_wastewater_files, rmp_proximity_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hazardgraph_rmp_upload, upload_hazardgraph_rmp(hazardgraph_rmp_files, processed_folders$exposure_features, validation_folder)),
  # Download, process, and upload electric substations
  targets::tar_target(substations_raw_file, download_substations_raw(substations_raw_dir, raw_folders$substations, state_abbr), format = "file"),
  targets::tar_target(substations_files, build_substation_tables(substations_raw_file, nc_counties, county_fips, target_crs, substations_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(substations_upload, upload_substation_outputs(substations_files, processed_folders$substations, metadata_folder, validation_folder)),
  
  targets::tar_target(substation_proximity_files, build_substation_proximity_features(tri_fips_files, substations_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(substation_proximity_upload, upload_substation_proximity_features(substation_proximity_files, processed_folders$exposure_features, validation_folder)),
  
  targets::tar_target(hazardgraph_substation_files, build_hazardgraph_substations(hazardgraph_rmp_files, substation_proximity_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hazardgraph_substation_upload, upload_hazardgraph_substations(hazardgraph_substation_files, processed_folders$exposure_features, validation_folder)),

  targets::tar_target(hospitals_raw_file, download_hospitals_raw(hospitals_raw_dir, raw_folders$hospitals, state_abbr), format = "file"),
  targets::tar_target(hospitals_files, build_hospital_tables(hospitals_raw_file, nc_counties, county_fips, target_crs, hospitals_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hospitals_upload, upload_hospital_outputs(hospitals_files, processed_folders$hospitals, metadata_folder, validation_folder)),

  targets::tar_target(hospital_proximity_files, build_hospital_proximity_features(tri_fips_files, hospitals_files, year, county_fips, buffer_m, target_crs, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hospital_proximity_upload, upload_hospital_proximity_features(hospital_proximity_files, processed_folders$exposure_features, validation_folder)),

  targets::tar_target(hazardgraph_hospital_files, build_hazardgraph_hospitals(hazardgraph_substation_files, hospital_proximity_files, year, county_fips, exposure_features_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(hazardgraph_hospital_upload, upload_hazardgraph_hospitals(hazardgraph_hospital_files, processed_folders$exposure_features, validation_folder)),

  targets::tar_target(mrds_visualization_files, build_mrds_visualizations(mrds_fips_files, nc_counties, county_fips, target_crs, figures_output_dir, qaqc_output_dir), format = "file"),
  targets::tar_target(mrds_visualization_upload, upload_mrds_visualizations(mrds_visualization_files, figures_folder, validation_folder)),

  targets::tar_target(mrds_reconciliation_files, build_mrds_record_reconciliation(mrds_raw_file, mrds_fips_files, nc_counties, state_abbr, state_fips, county_fips, target_crs, file.path(local_base, "processed", "mines"), qaqc_output_dir), format = "file"),
  targets::tar_target(mrds_reconciliation_upload, upload_mrds_record_reconciliation(mrds_reconciliation_files, processed_folders$mines, validation_folder)),

  targets::tar_target(statewide_hazardgraph_files, build_statewide_hazardgraph(tri_fips_files = tri_fips_files, ian_exposure_files = ian_exposure_files, mrds_classified_files = mrds_classified_files, brownfields_files = brownfields_files, wastewater_files = wastewater_files, rmp_files = rmp_files, substation_files = substations_files, hospital_files = hospitals_files, year = year, state_fips = state_fips, buffer_m = buffer_m, target_crs = target_crs, output_dir = exposure_output_dir, validation_dir = qaqc_output_dir, afo_files = afo_files, superfund_files = superfund_files), format = "file"),
  targets::tar_target(statewide_hazardgraph_upload, upload_statewide_hazardgraph(statewide_hazardgraph_files, processed_folders$exposure_features, validation_folder)),

  targets::tar_target(statewide_hazardgraph_analysis_files, analyze_statewide_hazardgraph(statewide_hazardgraph_files = statewide_hazardgraph_files, year = year, buffer_m = buffer_m, output_dir = exposure_output_dir, validation_dir = qaqc_output_dir, figure_dir = figures_output_dir), format = "file"), 
  targets::tar_target(statewide_hazardgraph_analysis_upload, upload_statewide_hazardgraph_analysis(statewide_hazardgraph_analysis_files, processed_folders$exposure_features, validation_folder, figures_folder))
)