# ==============================================================================
# Global pipeline configuration
# ==============================================================================

pipeline_message(text = "Global pipeline configuration", 
                 level = 0, progress = "start", process = "install")

# ------------------------------------------------------------------------------
# Execution context (TTY / batch detection)
# ------------------------------------------------------------------------------
# TRUE  -> interactive terminal (local)
# FALSE -> non-interactive (HPC / SLURM)
IS_TTY <- interactive() && isatty(stdout())

# ------------------------------------------------------------------------------
# Spatial processing configuration
# ------------------------------------------------------------------------------
# CRS for transforming or converting simple feature coordinates
TARGET_CRS = 2154 # Lambert-93

# Use a single global st_join (TRUE) or chunked spatial join (FALSE)
JOIN_ROADS_AND_TOWNS_IN_ONE_SHOT <- FALSE

# Radius (in meters) used to match counting points to nearest OSM roads
BUFFER_RADIUS <- 50

# Maximum path length to consider when calculating the betweenness
CUTOFF_BETWEENNESS <- 25

# Maximum path length to consider when calculating the closeness
CUTOFF_CLOSENESS <- 25

# ------------------------------------------------------------------------------
# Traffic data processing configuration
# ------------------------------------------------------------------------------
# Default values for imputation rules
DEFAULT_NUMBER_OF_LANES = 2
DEFAULT_VEHICLE_SPEED = 50
DEFAULT_NUMBER_OF_ROADS = 0
DEFAULT_DEGRE = 1

# ------------------------------------------------------------------------------
# Avatar API configuration
# ------------------------------------------------------------------------------
# API token (read from environment variable)
START_TIME = "2023-01-01T00:00:00"
END_TIME = "2023-12-31T00:00:00"

# Load environment variables from .Renviron (project-level or home-level)
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
} else if (file.exists("~/.Renviron")) {
  readRenviron("~/.Renviron")
}

# API token (read from environment variable)
AVATAR_API_TOKEN <- Sys.getenv("AVATAR_API_TOKEN", 
                               unset = "")

# Force re-download options
FORCE_REJOIN_OSM_AND_COMMUNES <- FALSE
FORCE_REDOWNLOAD_COUNT_POINTS <- FALSE
FORCE_REDOWNLOAD_CHUNKS <- FALSE
FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS <- TRUE

# ------------------------------------------------------------------------------
# Training configuration
# ------------------------------------------------------------------------------
# Training parameters
TRAINING_PARAMS <- list(
  max_depth = 10,                 # Deeper trees for complex patterns
  eta = 0.05,                     # Lower learning rate for finer optimization  
  subsample = 0.9,                # More data per tree
  colsample_bytree = 0.9,         # More features per tree
  min_child_weight = 2,           # Better regularization
  gamma = 0.1,                    # Minimum loss reduction
  reg_alpha = 0.01,               # L1 regularization
  reg_lambda = 0.01,              # L2 regularization
  objective = "reg:squarederror", 
  eval_metric = "rmse"
)
# Enhanced training parameters pour heavy vehicles
TRUCK_PARAMS <- list(
  max_depth = 6,           # Deeper than before but controlled
  eta = 0.1,               # Moderate learning rate
  subsample = 0.95,        # Use most data (small sample)
  colsample_bytree = 0.9,  # Use most features
  min_child_weight = 1,    # Less strict regularization
  gamma = 0.05,            # Small loss reduction threshold
  reg_alpha = 0.005,       # Light L1 regularization
  reg_lambda = 0.01,       # Light L2 regularization
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

NROUNDS <- 1500            # More boosting rounds for better convergence

# Quality/robustness options (low overhead)
# Split by count_point_id to avoid leakage between train/test
USE_GROUPED_SENSOR_SPLIT <- TRUE
# Use Avatar quality indicators as XGBoost sample weights
USE_AVATAR_QUALITY_WEIGHTS <- TRUE
# Lower bound for sample weights (avoid zero-weight rows)
MIN_AVATAR_SAMPLE_WEIGHT <- 0.20

# ------------------------------------------------------------------------------
# Directories
# ------------------------------------------------------------------------------
# Input data directory
INPUT_DATA_DIR <- normalizePath(path = file.path("data", "input"), 
                                mustWork = FALSE)

# Output data directory
OUTPUT_DATA_DIR <- normalizePath(path = file.path("data", "output"), 
                                 mustWork = FALSE)

# Output training data directory
TRAINING_DATA_DIR <- normalizePath(
  path = file.path(OUTPUT_DATA_DIR, "training"), 
  mustWork = FALSE)

# Output forecasting data directory
FORECAST_DATA_DIR <- normalizePath(path = file.path(OUTPUT_DATA_DIR, "forecast"), 
                                   mustWork = FALSE)

# Figures directory
FIGS_DIR <- normalizePath(path = "figures", 
                          mustWork = FALSE)

# ------------------------------------------------------------------------------
# Input data paths (depend on INPUT_DATA_DIR defined in setup)
# ------------------------------------------------------------------------------

# Output directory path to OSM data
OSM_DATA_DIR <-file.path("data", "osm")

# OSM PBG download path
OSM_PBF_DIR <- file.path(OSM_DATA_DIR, "pbf")
OSM_GPKG_DIR <- file.path(OSM_DATA_DIR, "gpkg")

# OSM road data
osm_roads_filename <- "osm_roads_france.gpkg"
OSM_ROADS_FILEPATH <- file.path(OSM_DATA_DIR, osm_roads_filename)

# OSM data for communes
osm_typologies_filename <- "COMMUNE_TYPO_DENSITE.shp"
OSM_TYPOLOGIES_FILEPATH <- file.path("data", "insee", 
                                     osm_typologies_filename)

# ------------------------------------------------------------------------------
# Output data paths (depend on OUTPUT_DATA_DIR defined in setup)
# ------------------------------------------------------------------------------

# OSM data
osm_degre_filename <- "01_commune_density_lookup.rds"
OSM_DEGRE_FILEPATH <- 
  file.path(OUTPUT_DATA_DIR, "osm", osm_degre_filename)

osm_roads_connectivity_and_towns_filename <-
  "01_osm_network_augmented.gpkg"
OSM_ROADS_CONNECTIVITY_FILEPATH <- 
  file.path(OUTPUT_DATA_DIR, "osm", osm_roads_connectivity_and_towns_filename)

# OSM France network with feature engineering (NOUVEAU)
osm_roads_france_engineered_filename <- "02_osm_network_france_engineered.gpkg"
OSM_ROADS_FRANCE_ENGINEERED_FILEPATH <-
  file.path(OUTPUT_DATA_DIR, "osm", osm_roads_france_engineered_filename)

# Imputation rules computed on all France (NOUVEAU)
imputation_rules_france_filename <- "02_imputation_rules_france.rds"
IMPUTATION_RULES_FRANCE_FILEPATH <-
  file.path(OUTPUT_DATA_DIR, "osm", imputation_rules_france_filename)

# Avatar data directory
AVATAR_DATA_DIR <- normalizePath(path = file.path(OUTPUT_DATA_DIR, "avatar"), 
                                 mustWork = FALSE)

# Avatar / traffic data
count_points_filename <- "03_avatar_count_points.json"
AVATAR_COUNT_POINTS_FILEPATH <- file.path(AVATAR_DATA_DIR, "json", 
                                          count_points_filename)

AVATAR_CSV_DATA_DIRPATH <- file.path(AVATAR_DATA_DIR, "csv")

avatar_data_filename <- "03_avatar_raw_traffic.rds"
AVATAR_RDS_DATA_FILEPATH <- file.path(AVATAR_DATA_DIR, "rds", 
                                      avatar_data_filename)

avatar_ids_full_network_filename <- "03_osm_network_with_avatar_ids.gpkg"
AVATAR_IDS_FULL_NETWORK_FILEPATH <- file.path(AVATAR_DATA_DIR, "gpkg", 
                                              avatar_ids_full_network_filename)

avatar_aggregated_data_filename <- "04_avatar_aggregated_with_ratios.rds"
AVATAR_AGGREGATED_FILEPATH <- file.path(AVATAR_DATA_DIR, "rds", 
                                        avatar_aggregated_data_filename)

avatar_aggregated_clean_data_filename <- "05_avatar_traffic_clean.rds"
AVATAR_AGGREGATED_CLEAN_FILEPATH <- 
  file.path(AVATAR_DATA_DIR, "rds", avatar_aggregated_clean_data_filename)

avatar_clean_network_filename <- "05_avatar_clean_network.rds"
AVATAR_CLEAN_NETWORK_RDS_DATA_FILEPATH <- 
  file.path(AVATAR_DATA_DIR, "rds", avatar_clean_network_filename)

# Training data
TRAINING_RDS_DATA_FILEPATH <- file.path(TRAINING_DATA_DIR, "rds", 
                                        "05_training_dataset.rds")
TRAINING_GPKG_DATA_FILEPATH <- file.path(TRAINING_DATA_DIR, "gpkg", 
                                        "05_training_dataset.gpkg")
XGB_MODELS_WITH_RATIOS_FILEPATH <- file.path(TRAINING_DATA_DIR, "rds", 
                                             "06_xgboost_trained_models.rds")
XGB_RATIO_FEATURE_INFO_FILEPATH <- file.path(TRAINING_DATA_DIR, "rds", 
                                             "06_xgboost_feature_info.rds")

# Forecasting / Prediction outputs
NANTES_PREDICTION_FILEPATH <- file.path(FORECAST_DATA_DIR, 
                                        "07_predictions_nantes.gpkg")
PARIS_PREDICTION_FILEPATH <- file.path(FORECAST_DATA_DIR, 
                                       "07_predictions_paris.gpkg")
SENSORS_ALL_PREDICTION_FILEPATH <- file.path(FORECAST_DATA_DIR, 
                                              "07_predictions_sensors_all.gpkg")
PEMB_PREDICTION_FILEPATH <- file.path(FORECAST_DATA_DIR, 
                                      "07_predictions_pemb.gpkg")

# France-wide prediction (tiled, geometry-separated)
FRANCE_OUTPUT_DIR <- file.path(FORECAST_DATA_DIR, "france")
FRANCE_GEOMETRY_FILEPATH <- file.path(FRANCE_OUTPUT_DIR,
                                       "07_france_network.gpkg")
FRANCE_TRAFFIC_DEN_FILEPATH <- file.path(FRANCE_OUTPUT_DIR,
                                          "07_france_traffic_DEN.gpkg")
FRANCE_TRAFFIC_HOURLY_FILEPATH <- file.path(FRANCE_OUTPUT_DIR,
                                             "07_france_traffic_hourly.gpkg")
FRANCE_TRAFFIC_HOURLY_WD_FILEPATH <- file.path(FRANCE_OUTPUT_DIR,
                                                "07_france_traffic_hourly_wd.gpkg")
FRANCE_TRAFFIC_HOURLY_WE_FILEPATH <- file.path(FRANCE_OUTPUT_DIR,
                                                "07_france_traffic_hourly_we.gpkg")

# Figures (avec préfixe de phase)
FIG_HOURLY_TRAFFIC_FILENAME <- "04_hourly_traffic_patterns.pdf"
SPEED_AND_TRUCK_PERCENTAGE <- "04_speed_and_truck_percentage.pdf"
TRAFFIC_PERIOD_COMPARISONS <- "04_traffic_period_comparisons.pdf"
TRAFFIC_FLOW_DISTRIBUTION_AND_DATA_QUALITY <- 
  "04_traffic_flow_distribution_and_data_quality.pdf"

# ------------------------------------------------------------------------------
# Global config
# ------------------------------------------------------------------------------
CONFIG <<- list(
  # Running environment
  IS_TTY = IS_TTY,
  RUN_CONTEXT = RUN_CONTEXT,
  # Directories
  INPUT_DATA_DIR = INPUT_DATA_DIR,
  OUTPUT_DATA_DIR = OUTPUT_DATA_DIR,
  AVATAR_RDS_DATA_FILEPATH = AVATAR_RDS_DATA_FILEPATH,
  AVATAR_CLEAN_NETWORK_RDS_DATA_FILEPATH = 
    AVATAR_CLEAN_NETWORK_RDS_DATA_FILEPATH,
  FIGS_DIR = FIGS_DIR,
  # Figure filenames
  FIG_HOURLY_TRAFFIC_FILENAME = FIG_HOURLY_TRAFFIC_FILENAME,
  SPEED_AND_TRUCK_PERCENTAGE = SPEED_AND_TRUCK_PERCENTAGE,
  TRAFFIC_PERIOD_COMPARISONS = TRAFFIC_PERIOD_COMPARISONS,
  TRAFFIC_FLOW_DISTRIBUTION_AND_DATA_QUALITY = 
    TRAFFIC_FLOW_DISTRIBUTION_AND_DATA_QUALITY,
  # OSM
  TARGET_CRS = TARGET_CRS,
  FORCE_REJOIN_OSM_AND_COMMUNES = FORCE_REJOIN_OSM_AND_COMMUNES,
  CUTOFF_BETWEENNESS = CUTOFF_BETWEENNESS,
  CUTOFF_CLOSENESS = CUTOFF_CLOSENESS,
  JOIN_ROADS_AND_TOWNS_IN_ONE_SHOT = JOIN_ROADS_AND_TOWNS_IN_ONE_SHOT,
  OSM_PBF_DIR = OSM_PBF_DIR,
  OSM_GPKG_DIR = OSM_GPKG_DIR,
  OSM_ROADS_FILEPATH = OSM_ROADS_FILEPATH,
  OSM_TYPOLOGIES_FILEPATH = OSM_TYPOLOGIES_FILEPATH,
  OSM_DEGRE_FILEPATH = OSM_DEGRE_FILEPATH,
  OSM_ROADS_CONNECTIVITY_FILEPATH = OSM_ROADS_CONNECTIVITY_FILEPATH,
  OSM_ROADS_FRANCE_ENGINEERED_FILEPATH = OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
  IMPUTATION_RULES_FRANCE_FILEPATH = IMPUTATION_RULES_FRANCE_FILEPATH,
  # Avatar
  START_TIME = START_TIME,
  END_TIME = END_TIME,
  BUFFER_RADIUS = BUFFER_RADIUS,
  AVATAR_API_TOKEN = AVATAR_API_TOKEN,
  FORCE_REDOWNLOAD_COUNT_POINTS = FORCE_REDOWNLOAD_COUNT_POINTS,
  FORCE_REDOWNLOAD_CHUNKS = FORCE_REDOWNLOAD_CHUNKS,
  FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS = 
    FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS,
  AVATAR_DATA_DIR = AVATAR_DATA_DIR,
  AVATAR_RDS_DATA_FILEPATH = AVATAR_RDS_DATA_FILEPATH,
  AVATAR_CSV_DATA_DIRPATH = AVATAR_CSV_DATA_DIRPATH,
  AVATAR_COUNT_POINTS_FILEPATH = AVATAR_COUNT_POINTS_FILEPATH,
  AVATAR_AGGREGATED_FILEPATH = AVATAR_AGGREGATED_FILEPATH,
  AVATAR_AGGREGATED_CLEAN_FILEPATH = AVATAR_AGGREGATED_CLEAN_FILEPATH,
  AVATAR_IDS_FULL_NETWORK_FILEPATH = AVATAR_IDS_FULL_NETWORK_FILEPATH,
  # Traffic data processing
  DEFAULT_NUMBER_OF_LANES = DEFAULT_NUMBER_OF_LANES,
  DEFAULT_VEHICLE_SPEED = DEFAULT_VEHICLE_SPEED,
  DEFAULT_NUMBER_OF_ROADS = DEFAULT_NUMBER_OF_ROADS,
  DEFAULT_DEGRE = DEFAULT_DEGRE,
  # XGBoost training
  TRAINING_PARAMS=TRAINING_PARAMS,
  TRUCK_PARAMS=TRUCK_PARAMS,
  NROUNDS=NROUNDS,
  USE_GROUPED_SENSOR_SPLIT = USE_GROUPED_SENSOR_SPLIT,
  USE_AVATAR_QUALITY_WEIGHTS = USE_AVATAR_QUALITY_WEIGHTS,
  MIN_AVATAR_SAMPLE_WEIGHT = MIN_AVATAR_SAMPLE_WEIGHT,
  TRAINING_DATA_DIR = TRAINING_DATA_DIR,
  TRAINING_RDS_DATA_FILEPATH = TRAINING_RDS_DATA_FILEPATH,
  TRAINING_GPKG_DATA_FILEPATH = TRAINING_GPKG_DATA_FILEPATH,
  XGB_MODELS_WITH_RATIOS_FILEPATH = XGB_MODELS_WITH_RATIOS_FILEPATH,
  XGB_RATIO_FEATURE_INFO_FILEPATH = XGB_RATIO_FEATURE_INFO_FILEPATH,
  # Forecasting / Prediction
  FORECAST_DATA_DIR = FORECAST_DATA_DIR,
  NANTES_PREDICTION_FILEPATH = NANTES_PREDICTION_FILEPATH,
  PARIS_PREDICTION_FILEPATH = PARIS_PREDICTION_FILEPATH,
  SENSORS_ALL_PREDICTION_FILEPATH = SENSORS_ALL_PREDICTION_FILEPATH,
  PEMB_PREDICTION_FILEPATH = PEMB_PREDICTION_FILEPATH,
  # France-wide tiled prediction
  FRANCE_OUTPUT_DIR = FRANCE_OUTPUT_DIR,
  FRANCE_GEOMETRY_FILEPATH = FRANCE_GEOMETRY_FILEPATH,
  FRANCE_TRAFFIC_DEN_FILEPATH = FRANCE_TRAFFIC_DEN_FILEPATH,
  FRANCE_TRAFFIC_HOURLY_FILEPATH = FRANCE_TRAFFIC_HOURLY_FILEPATH,
  FRANCE_TRAFFIC_HOURLY_WD_FILEPATH = FRANCE_TRAFFIC_HOURLY_WD_FILEPATH,
  FRANCE_TRAFFIC_HOURLY_WE_FILEPATH = FRANCE_TRAFFIC_HOURLY_WE_FILEPATH
)

pipeline_message(text = "Pipeline configured", 
                 level = 0, progress = "end", process = "valid")