classify_mrds_sites <- function(mrds_fips_files, output_dir, validation_dir) {
  # Load North Carolina and Mecklenburg MRDS tables
  if (length(mrds_fips_files) < 2) stop("MRDS FIPS files are missing.")
  nc <- read.csv(mrds_fips_files[[1]], stringsAsFactors = FALSE)
  mecklenburg <- read.csv(mrds_fips_files[[2]], stringsAsFactors = FALSE)
  if (!nrow(nc)) stop("North Carolina MRDS table is empty.")
  cat("North Carolina and Mecklenburg MRDS tables loaded.\n")

  # Prepare MRDS resource fields
  required_fields <- c("dep_type", "commod1", "commod2", "commod3", "site_type", "dev_status")
  for (field in required_fields) {
    if (!field %in% names(nc)) nc[[field]] <- NA_character_
    if (!field %in% names(mecklenburg)) mecklenburg[[field]] <- NA_character_
  }
  cat("MRDS resource fields prepared.\n")

  # Define resource classification helpers
  clean_text <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x[is.na(x)] <- ""
    x
  }

  commodity_text <- function(data) apply(data[, c("commod1", "commod2", "commod3")], 1, function(x) {
    x <- clean_text(x)
    x <- x[x != ""]
    paste(unique(x), collapse = " | ")
  })

  has_pattern <- function(text, pattern) grepl(pattern, text, ignore.case = TRUE)
  cat("MRDS resource classification helpers ready.\n")

  # Classify MRDS records by commodity type
  classify_table <- function(data) {
    text <- commodity_text(data)
    precious <- has_pattern(text, "GOLD|SILVER|PLATINUM|PALLADIUM")
    base <- has_pattern(text, "COPPER|LEAD|ZINC|NICKEL|COBALT|TIN")
    ferrous <- has_pattern(text, "IRON|MANGANESE|CHROMIUM|CHROMITE")
    aggregate <- has_pattern(text, "STONE|CRUSHED|SAND AND GRAVEL|GRANITE|LIMESTONE|DOLOMITE|MARBLE|DIMENSION STONE")
    industrial <- has_pattern(text, "MICA|FELDSPAR|KAOLIN|CLAY|QUARTZ|TALC|BARITE|GRAPHITE|PHOSPHATE|GYPSUM|SILICA")

    metal_groups <- precious + base + ferrous
    nonmetal_groups <- aggregate + industrial
    total_groups <- metal_groups + nonmetal_groups
    category <- rep("other_or_unknown", nrow(data))

    category[precious & metal_groups == 1 & nonmetal_groups == 0] <- "precious_metal"
    category[base & metal_groups == 1 & nonmetal_groups == 0] <- "base_metal"
    category[ferrous & metal_groups == 1 & nonmetal_groups == 0] <- "ferrous_metal"
    category[aggregate & metal_groups == 0 & nonmetal_groups == 1] <- "construction_aggregate"
    category[industrial & metal_groups == 0 & nonmetal_groups == 1] <- "industrial_mineral"
    category[metal_groups > 1 & nonmetal_groups == 0] <- "mixed_metal"
    category[total_groups > 1 & nonmetal_groups > 0] <- "mixed_resource"

    data$commodity_text <- text
    data$resource_category <- category
    data$precious_metal_flag <- precious
    data$base_metal_flag <- base
    data$ferrous_metal_flag <- ferrous
    data$construction_aggregate_flag <- aggregate
    data$industrial_mineral_flag <- industrial
    data
  }

  nc <- classify_table(nc)
  mecklenburg <- classify_table(mecklenburg)
  cat("MRDS sites classified by resource type.\n")

  # Summarize statewide and Mecklenburg resource categories
  summarize_classes <- function(data, geography) {
    counts <- sort(table(data$resource_category), decreasing = TRUE)
    data.frame(geography = geography, resource_category = names(counts), records = as.integer(counts), percent = round(100 * as.integer(counts) / nrow(data), 2), stringsAsFactors = FALSE)
  }

  class_summary <- rbind(summarize_classes(nc, "North Carolina"), summarize_classes(mecklenburg, "Mecklenburg"))
  print(class_summary, row.names = FALSE)
  cat("MRDS resource classification summary built.\n")

  # Summarize source geology and commodity fields
  summarize_field <- function(data, field, geography) {
    values <- trimws(as.character(data[[field]]))
    values[is.na(values) | values == ""] <- "MISSING"
    counts <- sort(table(values), decreasing = TRUE)
    data.frame(geography = geography, field = field, value = names(counts), records = as.integer(counts), stringsAsFactors = FALSE)
  }

  source_fields <- c("site_type", "dev_status", "dep_type", "commod1", "commod2", "commod3")
  source_summary <- do.call(rbind, lapply(source_fields, function(field) rbind(summarize_field(nc, field, "North Carolina"), summarize_field(mecklenburg, field, "Mecklenburg"))))
  cat("MRDS source fields summarized.\n")

  # Build MRDS resource QA report
  populated_site_type <- sum(!is.na(nc$site_type) & trimws(nc$site_type) != "")
  populated_dev_status <- sum(!is.na(nc$dev_status) & trimws(nc$dev_status) != "")

  qa <- data.frame(
    check = c("NC MRDS sites", "NC resource-classified sites", "NC sites with known resource category", "Mecklenburg MRDS sites", "Mecklenburg resource-classified sites", "Mecklenburg sites with known resource category", "NC populated site_type records", "NC populated dev_status records", "Operational status supported by this MRDS export"),
    value = c(nrow(nc), sum(!is.na(nc$resource_category)), sum(nc$resource_category != "other_or_unknown"), nrow(mecklenburg), sum(!is.na(mecklenburg$resource_category)), sum(mecklenburg$resource_category != "other_or_unknown"), populated_site_type, populated_dev_status, FALSE),
    status = c("INFO", ifelse(all(!is.na(nc$resource_category)), "PASS", "FAIL"), "INFO", "INFO", ifelse(all(!is.na(mecklenburg$resource_category)), "PASS", "FAIL"), "INFO", "INFO", "INFO", "INFO"),
    stringsAsFactors = FALSE
  )

  print(qa, row.names = FALSE)
  cat("MRDS resource classification QA/QC completed.\n")

  # Write classified MRDS tables
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  nc_file <- file.path(output_dir, "nc_mrds_sites_classified.csv")
  mecklenburg_file <- file.path(output_dir, "mecklenburg_mrds_sites_classified.csv")
  write.csv(nc, nc_file, row.names = FALSE)
  write.csv(mecklenburg, mecklenburg_file, row.names = FALSE)
  cat("Classified MRDS resource tables written.\n")

  # Write MRDS resource validation outputs
  summary_file <- file.path(validation_dir, "mrds_resource_classification_summary.csv")
  source_summary_file <- file.path(validation_dir, "mrds_source_field_summary.csv")
  qa_file <- file.path(validation_dir, "qaqc_mrds_resource_classification.csv")

  write.csv(class_summary, summary_file, row.names = FALSE)
  write.csv(source_summary, source_summary_file, row.names = FALSE)
  write.csv(qa, qa_file, row.names = FALSE)

  cat("MRDS resource validation outputs written.\n")
  c(nc_file, mecklenburg_file, summary_file, source_summary_file, qa_file)
}

upload_mrds_classification <- function(files, processed_folder, validation_folder) {
  # Validate MRDS resource classification outputs
  if (length(files) != 5 || !all(file.exists(files))) stop("Expected five MRDS resource classification files.")
  cat("MRDS resource classification files validated.\n")

  # Upload classified MRDS resource tables
  invisible(lapply(files[1:2], googledrive::drive_upload, path = processed_folder, overwrite = TRUE))
  cat("Classified MRDS resource tables uploaded.\n")

  # Upload MRDS resource validation outputs
  invisible(lapply(files[3:5], googledrive::drive_upload, path = validation_folder, overwrite = TRUE))
  cat("MRDS resource validation outputs uploaded.\n")
  TRUE
}