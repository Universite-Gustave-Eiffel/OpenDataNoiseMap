# Script to check geometry validity in OSM engineered file
# Usage: Rscript check_geometry.R <filepath>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript check_geometry.R <filepath>")
}

filepath <- args[1]

library(sf)
library(dplyr)

cat("Checking geometry validity in:", filepath, "\n")

# First, check if file exists and get basic info without loading
if (!file.exists(filepath)) {
  stop("File does not exist:", filepath)
}

# Get layer info
layers <- st_layers(filepath)
cat("Available layers:", paste(layers$name, collapse = ", "), "\n")

# Assume the main layer is the first one or look for common names
layer_name <- layers$name[1]
if ("osm_network_france_engineered" %in% layers$name) {
  layer_name <- "osm_network_france_engineered"
}

cat("Using layer:", layer_name, "\n")

# Get basic info about the layer
layer_info <- layers$geomtype[layers$name == layer_name]
cat("Geometry type:", paste(layer_info, collapse = ", "), "\n")

# Read just the first few rows to check structure
cat("Reading first 10 rows to check structure...\n")
tryCatch({
  sample_data <- st_read(filepath, layer = layer_name, n_max = 10, quiet = TRUE)
  cat("Sample data loaded successfully\n")
  cat("Number of columns:", ncol(sample_data), "\n")
  cat("Column names:", paste(names(sample_data), collapse = ", "), "\n")

  # Check if geometry column exists
  geom_col <- attr(sample_data, "sf_column")
  cat("Geometry column:", geom_col, "\n")

  # Check CRS
  cat("CRS:", st_crs(sample_data)$epsg, "\n")

  # Check geometry validity on sample
  valid_sample <- st_is_valid(sample_data)
  cat("Sample geometries valid:", all(valid_sample), "\n")
  if (!all(valid_sample)) {
    cat("Invalid geometries in sample:", sum(!valid_sample), "\n")
  }

  # Check for empty geometries in sample
  empty_sample <- st_is_empty(sample_data)
  cat("Empty geometries in sample:", sum(empty_sample), "\n")

  cat("✅ Basic structure check passed!\n")

}, error = function(e) {
  cat("❌ Error reading file:", e$message, "\n")
  cat("This might indicate corrupted geometry data\n")
})