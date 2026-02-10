# ==============================================================================
# TEST CONFIGURATION - Small France Region for Fast Testing
# ==============================================================================
# This configuration reduces data volume for rapid testing cycles
# Suitable for: Full pipeline validation, development, CI/CD pipelines
# Coverage: ~50km x 50km region around a major city center
# Expected runtime: ~10-15% of full France pipeline

pipeline_message(text = "TEST CONFIGURATION loaded: Small France region", 
                 level = 0, progress = "start", process = "config")

# ==============================================================================
# Region Definition: Nantes Area (Ouest France)
# ==============================================================================
# Bounding box: approximately 50km x 50km around Nantes city center
# Coordinates in Lambert-93 (EPSG:2154)

TEST_REGION <- list(
  name = "Nantes_50km",
  description = "50km x 50km bbox around Nantes city center",
  
  # Bounding box in WGS84 (EPSG:4326, degrees - for cropping raw OSM data)
  bbox_wgs84 = c(
    xmin = -1.8,    # ~-30km from Nantes center
    ymin = 46.9,    # ~-30km from Nantes center
    xmax = -1.3,    # ~+30km from Nantes center
    ymax = 47.5     # ~+30km from Nantes center
  ),
  
  # Bounding box in Lambert-93 (EPSG:2154, meters - for cropping transformed data)
  # Nantes center ≈ (356074, 6689650) in EPSG:2154
  bbox = c(
    xmin = 331000,   # ~-25km from center
    ymin = 6664650,  # ~-25km from center
    xmax = 381000,   # ~+25km from center
    ymax = 6714650   # ~+25km from center
  ),
  
  # Geographic extent (reference)
  # Cities covered: Nantes, Saint-Nazaire, Saint-Herblain, Orvault, Rezé
  # Approximate area: 2,500 km²
  
  center_lon = -1.547,     # Nantes center (WGS84)
  center_lat = 47.218,
  
  # Radius for sensor buffer operations (meters)
  sensor_buffer_radius = 800
)

# ==============================================================================
# Override Output Paths for Test Region
# ==============================================================================
# All test outputs go to separate TEST subdirectory

TEST_OUTPUT_DIR <- normalizePath(path = file.path(OUTPUT_DATA_DIR, "TEST_OUTPUTS"), 
                                 mustWork = FALSE)
if (!dir.exists(TEST_OUTPUT_DIR)) {
  dir.create(TEST_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
}

# Redefine key output paths to use TEST directory
CONFIG$OSM_DEGRE_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "01_commune_density_lookup_TEST.rds")

CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "01_osm_network_augmented_TEST.gpkg")

CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "02_osm_network_france_engineered_TEST.gpkg")

CONFIG$IMPUTATION_RULES_FRANCE_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "02_imputation_rules_france_TEST.rds")

CONFIG$AVATAR_RDS_DATA_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "03_avatar_raw_traffic_TEST.rds")

CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "03_osm_network_with_avatar_ids_TEST.gpkg")

CONFIG$AVATAR_AGGREGATED_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "04_avatar_aggregated_with_ratios_TEST.rds")

CONFIG$AVATAR_AGGREGATED_CLEAN_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "04_avatar_aggregated_clean_TEST.rds")

CONFIG$TRAINING_RDS_DATA_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "05_training_dataset_TEST.rds")

CONFIG$TRAINING_GPKG_DATA_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "05_training_dataset_TEST.gpkg")

CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "06_xgboost_trained_models_TEST.rds")

CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "06_xgboost_feature_info_TEST.rds")

# Prediction outputs
CONFIG$NANTES_PREDICTION_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "07_predictions_nantes_TEST.gpkg")

CONFIG$PARIS_PREDICTION_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "07_predictions_paris_TEST.gpkg")

CONFIG$SENSORS_ALL_PREDICTION_FILEPATH <- 
  file.path(TEST_OUTPUT_DIR, "07_predictions_sensors_TEST.gpkg")

CONFIG$FORECAST_DATA_DIR <- TEST_OUTPUT_DIR

# ==============================================================================
# Crop Parameters for Test Region
# ==============================================================================

# Function to crop spatial data to test region
crop_to_test_region <- function(spatial_data) {
  
  # Determine which bbox to use based on CRS
  current_crs <- sf::st_crs(spatial_data)
  
  if (is.na(current_crs) || current_crs$epsg == 4326) {
    # WGS84 - use bbox_wgs84
    bbox_to_use <- TEST_REGION$bbox_wgs84
    target_crs <- 4326
  } else {
    # Assume Lambert-93 or transformed - use Lambert-93 bbox
    bbox_to_use <- TEST_REGION$bbox
    target_crs <- TARGET_CRS
  }
  
  # Create bbox as proper sf object with numeric values (avoid NAs)
  xmin <- as.numeric(bbox_to_use["xmin"])
  ymin <- as.numeric(bbox_to_use["ymin"])
  xmax <- as.numeric(bbox_to_use["xmax"])
  ymax <- as.numeric(bbox_to_use["ymax"])
  
  # Ensure no NAs
  if (anyNA(c(xmin, ymin, xmax, ymax))) {
    stop("Invalid bbox coordinates: ", paste(bbox_to_use, collapse=", "))
  }
  
  # Create bbox as proper sf object
  bbox_coords <- matrix(
    c(
      xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin  # close polygon
    ),
    ncol = 2, byrow = TRUE
  )
  
  bbox_geom <- sf::st_sfc(
    sf::st_polygon(list(bbox_coords)), 
    crs = target_crs
  )
  
  # Ensure input data is in correct CRS
  if (!is.na(current_crs) && current_crs$epsg != target_crs) {
    spatial_data <- sf::st_transform(spatial_data, crs = target_crs)
  }
  
  # Crop data and clean invalid geometries
  cropped <- sf::st_crop(spatial_data, bbox_geom)
  
  # Fix invalid geometries after cropping (st_crop can break geometries at edges)
  cropped <- sf::st_make_valid(cropped)
  
  # Remove empty geometries if any exist after cropping
  non_empty_idx <- !sf::st_is_empty(cropped)
  cropped <- cropped[non_empty_idx, ]
  
  pipeline_message(
    text = sprintf("Region cropped: %d → %d rows (invalid geometries fixed)", 
                   nrow(spatial_data), nrow(cropped)),
    level = 2, process = "info"
  )
  
  return(cropped)
}

# ==============================================================================
# Data Reduction Parameters
# ==============================================================================

# Avatar sampling: use only every Nth point to speed up downloads
AVATAR_SAMPLING_FACTOR <- 2  # Use ~50% of counting points

# Training data reduction for model training
TRAINING_DATA_SAMPLING <- 0.8  # Use 80% of training data for faster model training

# Model parameters - lighter for faster training
CONFIG$TRAINING_PARAMS <- list(
  max_depth = 6,          # Shallower trees
  eta = 0.1,              # Faster learning
  subsample = 0.8,        # Less data per tree (faster)
  colsample_bytree = 0.8, # Fewer features per tree
  min_child_weight = 2,
  gamma = 0.1,
  reg_alpha = 0.01,
  reg_lambda = 0.01,
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

CONFIG$TRUCK_PARAMS <- list(
  max_depth = 5,          # Shallower
  eta = 0.15,             # Faster
  subsample = 0.9,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  gamma = 0.05,
  reg_alpha = 0.005,
  reg_lambda = 0.01,
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

CONFIG$NROUNDS <- 500   # Fewer boosting rounds (vs 1500 for full pipeline)

# ==============================================================================
# Test Avatar Data Parameters
# ==============================================================================

# Use reduced time period for faster downloads
TEST_START_TIME <- "2023-06-01T00:00:00"  # June 2023 (1 month)
TEST_END_TIME <- "2023-06-30T00:00:00"

CONFIG$START_TIME <- TEST_START_TIME
CONFIG$END_TIME <- TEST_END_TIME

pipeline_message(
  text = sprintf("Test period: %s to %s", TEST_START_TIME, TEST_END_TIME),
  level = 1, process = "info"
)

# ==============================================================================
# Test Summary
# ==============================================================================

pipeline_message(
  text = sprintf(
    "TEST REGION: %s | BBOX: %.2f-%.2f x %.2f-%.2f | OUTPUTS: %s",
    TEST_REGION$name,
    TEST_REGION$bbox["xmin"]/1e6, TEST_REGION$bbox["xmax"]/1e6,
    TEST_REGION$bbox["ymin"]/1e6, TEST_REGION$bbox["ymax"]/1e6,
    TEST_OUTPUT_DIR
  ),
  level = 1, process = "info"
)

