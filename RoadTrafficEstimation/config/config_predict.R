# ==============================================================================
# Prediction pipeline configuration
# ==============================================================================

# Prediction paths and filenames
PREDICTION_DIR <- file.path("data", "prediction")

# Use the MODE variable to tag outputs (same as in training pipeline)
mode_suffix    <- if (exists("MODE") && nzchar(MODE)) { MODE } else { "all" }

# Build all prediction paths for direct CONFIG access (backward-compatible)
CONFIG_PREDICT <- list(
  
  # Directories
  PREDICTION_DIR    = PREDICTION_DIR,
  FRANCE_OUTPUT_DIR = file.path(PREDICTION_DIR, mode_suffix),

  # Force reprocessing of all French tiled output files even when existing
  FORCE_REPROCESS_ALL_TILES = FALSE,
  # Number of cores to use for France tiled prediction.
  # If NULL, the pipeline will use all physical cores minus one.
  PREDICTION_TILE_CORES = NULL,
  
  # ============================================================================
  # Regional predictions (single file per extent)
  # ============================================================================
  
  # Sensors (all noise monitoring stations)
  SENSORS_DATA_DIR                = file.path(DATA_DIR, "noise_sensors"), 
  SENSORS_ALL_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, 
                                              mode_suffix, 
                                              sprintf("07_predictions_%s.gpkg", 
                                                      mode_suffix)), 
  
  # Nantes  
  NANTES_PREDICTION_FILEPATH      = file.path(PREDICTION_DIR, 
                                              mode_suffix,
                                              sprintf("07_predictions_%s.gpkg", 
                                                      mode_suffix)),
  
  # Paris
  PARIS_PREDICTION_FILEPATH       = file.path(PREDICTION_DIR, 
                                              mode_suffix,
                                              sprintf("07_predictions_%s.gpkg", 
                                                      mode_suffix)),
  
  # PEMB (Paris Est Marne & Bois)
  PEMB_PREDICTION_FILEPATH        = file.path(PREDICTION_DIR, 
                                              mode_suffix,
                                              sprintf("07_predictions_%s.gpkg", 
                                                      mode_suffix)),
  
  # ============================================================================
  # France tiled predictions (geometry + temporal chunks)
  # ============================================================================
  
  # Geometry layer (same for all temporal chunks)
  FRANCE_GEOMETRY_FILEPATH = file.path(PREDICTION_DIR, mode_suffix,
                                       sprintf("07_predictions_%s_network.gpkg", 
                                               mode_suffix)),
  
  # Traffic attributes split by temporal chunk (den, hourly, hourly_wd, hourly_we)
  FRANCE_TRAFFIC_DEN_FILEPATH       = file.path(PREDICTION_DIR, mode_suffix, 
                                                sprintf("07_predictions_%s_traffic_DEN.csv.gz", 
                                                        mode_suffix)),
  FRANCE_TRAFFIC_HOURLY_FILEPATH    = file.path(PREDICTION_DIR, mode_suffix, 
                                                sprintf("07_predictions_%s_traffic_hourly.csv.gz",              
                                                        mode_suffix)),
  FRANCE_TRAFFIC_HOURLY_WD_FILEPATH = file.path(PREDICTION_DIR, mode_suffix, 
                                                sprintf("07_predictions_%s_traffic_hourly_wd.csv.gz",           
                                                        mode_suffix)),
  FRANCE_TRAFFIC_HOURLY_WE_FILEPATH = file.path(PREDICTION_DIR, mode_suffix,
                                                sprintf("07_predictions_%s_traffic_hourly_we.csv.gz", 
                                                        mode_suffix)),
  
  # Legacy single-file export
  FRANCE_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, "france", 
                                         sprintf("07_predictions_france_%s.gpkg", 
                                                 mode_suffix))
)