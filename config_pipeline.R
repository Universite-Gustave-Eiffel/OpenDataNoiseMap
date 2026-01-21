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
CUTOFF_BETWEENNESS <- 100

# Maximum path length to consider when calculating the closeness
CUTOFF_CLOSENESS <- 20

# ------------------------------------------------------------------------------
# Traffic data processing configuration
# ------------------------------------------------------------------------------
# Default values for imputation rules
DEFAULT_NUMBER_OF_LANES = 2
DEFAULT_VEHICLE_SPEED = 2
DEFAULT_NUMBER_OF_ROADS = 0
DEFAULT_DEGRE = 1

# ------------------------------------------------------------------------------
# Avatar API configuration
# ------------------------------------------------------------------------------
# API token (read from environment variable)
START_TIME = "2023-01-01T00:00:00"
END_TIME = "2023-12-31T00:00:00"

# Load environment variables from file .env
readRenviron(".Renviron")

# API token (read from environment variable)
AVATAR_API_TOKEN <- Sys.getenv("AVATAR_API_TOKEN", 
                               unset = "")

# Force re-download options
FORCE_REJOIN_OSM_AND_COMMUNES <- FALSE
FORCE_REDOWNLOAD_COUNT_POINTS <- FALSE
FORCE_REDOWNLOAD_CHUNKS <- FALSE
FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS <- FALSE

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

# ------------------------------------------------------------------------------
# Directories
# ------------------------------------------------------------------------------
# Input data directory
INPUT_DATA_DIR <- normalizePath(path = file.path("data", "input"), 
                                mustWork = FALSE)
assign(x = "INPUT_DATA_DIR", 
       value = INPUT_DATA_DIR, 
       envir = .GlobalEnv)

# Output data directory
OUTPUT_DATA_DIR <- normalizePath(path = file.path("data", "output"), 
                                 mustWork = FALSE)
assign(x = "OUTPUT_DATA_DIR", 
       value = OUTPUT_DATA_DIR, 
       envir = .GlobalEnv)

# Output OSM data directory
OUTPUT_OSM_DATA_DIR <- normalizePath(path = file.path("data", "output"), 
                                     mustWork = FALSE)
assign(x = "OUTPUT_OSM_DATA_DIR", 
       value = OUTPUT_OSM_DATA_DIR, 
       envir = .GlobalEnv)

# Output training data directory
TRAINING_DATA_DIR <- normalizePath(
  path = file.path(OUTPUT_DATA_DIR, "training"), 
  mustWork = FALSE)
assign(x = "TRAINING_DATA_DIR", 
       value = TRAINING_DATA_DIR, 
       envir = .GlobalEnv)

# Figures directory
FIGS_DIR <- normalizePath(path = "figures", 
                          mustWork = FALSE)
assign(x = "FIGS_DIR", 
       value = FIGS_DIR, 
       envir = .GlobalEnv)

# ------------------------------------------------------------------------------
# Input data paths (depend on INPUT_DATA_DIR defined in setup)
# ------------------------------------------------------------------------------

# OSM road data
osm_roads_filename <- "osm_roads_france.gpkg"
OSM_ROADS_FILEPATH <- file.path(INPUT_DATA_DIR, osm_roads_filename)

# OSM data for communes
osm_typologies_filename <- "COMMUNE_TYPO_DENSITE.shp"
OSM_TYPOLOGIES_FILEPATH <- file.path(INPUT_DATA_DIR, 
                                     osm_typologies_filename)

# ------------------------------------------------------------------------------
# Output data paths (depend on OUTPUT_DATA_DIR defined in setup)
# ------------------------------------------------------------------------------

# OSM data
osm_degre_filename <- "degre.rds"
OSM_DEGRE_FILEPATH <- 
  file.path(OUTPUT_DATA_DIR, "osm", osm_degre_filename)

osm_roads_connectivity_and_towns_filename <-
  "osm_roads_france_including_connectivity_and_communes.gpkg"
OSM_ROADS_CONNECTIVITY_FILEPATH <- 
  file.path(OUTPUT_DATA_DIR, "osm", osm_roads_connectivity_and_towns_filename)

# Avatar data directory
AVATAR_DATA_DIR <- normalizePath(path = file.path(OUTPUT_DATA_DIR, "avatar"), 
                                 mustWork = FALSE)
assign(x = "AVATAR_DATA_DIR", 
       value = AVATAR_DATA_DIR, 
       envir = .GlobalEnv)

# Avatar / traffic data
count_points_filename <- "count_points.json"
AVATAR_COUNT_POINTS_FILEPATH <- file.path(AVATAR_DATA_DIR, "json", 
                                          count_points_filename)

AVATAR_CSV_DATA_DIRPATH <- file.path(AVATAR_DATA_DIR, "csv")

avatar_data_filename <- "avatar_data.rds"
AVATAR_RDS_DATA_FILEPATH <- file.path(AVATAR_DATA_DIR, "rds", 
                                      avatar_data_filename)

avatar_clean_network_rds_data_filename <- "avatar_clean_network.rds"
AVATAR_CLEAN_NETWORK_RDS_DATA_FILEPATH <- file.path(
  AVATAR_DATA_DIR, "rds", 
  avatar_clean_network_rds_data_filename)

avatar_aggregated_data_filename <- "avatar_aggregated_hourly_period.rds"
AVATAR_AGGREGATED_FILEPATH <- file.path(AVATAR_DATA_DIR, "rds", 
                                        avatar_aggregated_data_filename)

avatar_imputation_rules_filename <- "avatar_imputation_rules.rds"
AVATAR_IMPUTATION_RULES_FILEPATH <- file.path(AVATAR_DATA_DIR, "rds", 
                                              avatar_imputation_rules_filename)


avatar_aggregated_clean_data_filename <- "avatar_aggregated_clean.rds"
AVATAR_AGGREGATED_CLEAN_FILEPATH <- 
  file.path(AVATAR_DATA_DIR, "rds", avatar_aggregated_clean_data_filename)

avatar_ids_full_network_filename <- "avatar_ids_full_network_filename.gpkg"
AVATAR_IDS_FULL_NETWORK_FILEPATH <- file.path(AVATAR_DATA_DIR, "gpkg", 
                                              avatar_ids_full_network_filename)

# Training data
TRAINING_RDS_DATA_FILEPATH <- file.path(TRAINING_DATA_DIR, "rds", 
                                        "training_data.rds")
TRAINING_GPKG_DATA_FILEPATH <- file.path(TRAINING_DATA_DIR, "gpkg", 
                                        "training_data_with_periods_ms.gpkg")
XGB_MODELS_WITH_RATIOS_FILEPATH <- file.path(TRAINING_DATA_DIR, "rds", 
                                             "xgb_models_with_ratios.rds")
XGB_RATIO_FEATURE_INFO_FILEPATH <- file.path(TRAINING_DATA_DIR, "rds", 
                                             "xgb_ratio_feature_info.rds")

# Figures
FIG_HOURLY_TRAFFIC_FILENAME <- "hourly_traffic_patterns.pdf"
SPEED_AND_TRUCK_PERCENTAGE <- "speed_and_truck_percentage.pdf"
TRAFFIC_PERIOD_COMPARISONS <- "traffic_period_comparisons.pdf"
TRAFFIC_FLOW_DISTRIBUTION_AND_DATA_QUALITY <- 
  "traffic_flow_distribution_and_data_quality.pdf"

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
  OSM_ROADS_FILEPATH = OSM_ROADS_FILEPATH,
  OSM_TYPOLOGIES_FILEPATH = OSM_TYPOLOGIES_FILEPATH,
  OSM_DEGRE_FILEPATH = OSM_DEGRE_FILEPATH,
  OSM_ROADS_CONNECTIVITY_FILEPATH = OSM_ROADS_CONNECTIVITY_FILEPATH,
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
  AVATAR_IMPUTATION_RULES_FILEPATH = AVATAR_IMPUTATION_RULES_FILEPATH,
  AVATAR_AGGREGATED_CLEAN_FILEPATH = AVATAR_AGGREGATED_CLEAN_FILEPATH,
  AVATAR_IDS_FULL_NETWORK_FILEPATH=AVATAR_IDS_FULL_NETWORK_FILEPATH,
  # Traffic data processing
  DEFAULT_NUMBER_OF_LANES = DEFAULT_NUMBER_OF_LANES,
  DEFAULT_VEHICLE_SPEED = DEFAULT_VEHICLE_SPEED,
  DEFAULT_NUMBER_OF_ROADS = DEFAULT_NUMBER_OF_ROADS,
  DEFAULT_DEGRE = DEFAULT_DEGRE,
  # XGBoost training
  TRAINING_PARAMS=TRAINING_PARAMS,
  TRUCK_PARAMS=TRUCK_PARAMS,
  NROUNDS=NROUNDS,
  TRAINING_DATA_DIR = TRAINING_DATA_DIR,
  TRAINING_RDS_DATA_FILEPATH = TRAINING_RDS_DATA_FILEPATH,
  TRAINING_GPKG_DATA_FILEPATH = TRAINING_GPKG_DATA_FILEPATH,
  XGB_MODELS_WITH_RATIOS_FILEPATH = XGB_MODELS_WITH_RATIOS_FILEPATH,
  XGB_RATIO_FEATURE_INFO_FILEPATH = XGB_RATIO_FEATURE_INFO_FILEPATH
)

pipeline_message(text = "Pipeline configured!", 
                 level = 0, progress = "end", process = "valid")