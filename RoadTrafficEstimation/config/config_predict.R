# ==============================================================================
# Prediction pipeline configuration
# ==============================================================================

# Prediction paths and filenames
PREDICTION_DIR                    <- file.path("data", "prediction")

# Use the MODE variable to tag outputs with the current pipeline mode.  This
# mirrors the behaviour already in place for the `pemb` output (i.e.
# "07_predictions_pemb.gpkg") and ensures that all prediction files are
# unambiguously associated with the mode that produced them.
mode_suffix <- if (exists("MODE") && nzchar(MODE)) MODE else "all"

sensor_predictions_filename       <- sprintf("07_predictions_sensors_%s.gpkg", mode_suffix)
nantes_predictions_filename       <- sprintf("07_predictions_nantes_%s.gpkg", mode_suffix)
paris_predictions_filename        <- sprintf("07_predictions_paris_%s.gpkg", mode_suffix)
pemb_predictions_filename         <- sprintf("07_predictions_pemb_%s.gpkg", mode_suffix)
FRANCE_OUTPUT_DIR                 <- file.path(PREDICTION_DIR, "france")
france_network_filename           <- sprintf("07_france_network_%s.gpkg", mode_suffix)
france_traffic_den_filename       <- sprintf("07_france_traffic_DEN_%s.gpkg", mode_suffix)
france_traffic_hourly_filename    <- sprintf("07_france_traffic_hourly_%s.gpkg", mode_suffix)
france_traffic_hourly_wd_filename <- sprintf("07_france_traffic_hourly_wd_%s.gpkg", mode_suffix)
france_traffic_hourly_we_filename <- sprintf("07_france_traffic_hourly_we_%s.gpkg", mode_suffix)

CONFIG_PREDICT <- list(
  
  # ----------------------------------------------------------------------------
  # Directories and files
  # ----------------------------------------------------------------------------
  
  PREDICTION_DIR                    = PREDICTION_DIR, 
  SENSORS_ALL_PREDICTION_FILEPATH   = file.path(PREDICTION_DIR, 
                                                "sensor_roads_dt.rds"), 
  PARIS_PREDICTION_FILEPATH         = file.path(PREDICTION_DIR, 
                                                "paris_roads_dt.rds"), 
  NANTES_PREDICTION_FILEPATH        = file.path(PREDICTION_DIR, 
                                                nantes_predictions_filename), 
  PEMB_PREDICTION_FILEPATH          = file.path(PREDICTION_DIR, 
                                                pemb_predictions_filename), 
  FRANCE_OUTPUT_DIR                 = FRANCE_OUTPUT_DIR, 
  # single-file export (used by predict_france.R when not using tiling)
  FRANCE_PREDICTION_FILEPATH        = file.path(FRANCE_OUTPUT_DIR, 
                                                "07_predictions_france.gpkg"),
  FRANCE_GEOMETRY_FILEPATH          = file.path(FRANCE_OUTPUT_DIR, 
                                                france_network_filename), 
  FRANCE_TRAFFIC_DEN_FILEPATH       = file.path(FRANCE_OUTPUT_DIR, 
                                                france_traffic_den_filename), 
  FRANCE_TRAFFIC_HOURLY_FILEPATH    = file.path(FRANCE_OUTPUT_DIR, 
                                                france_traffic_hourly_filename), 
  FRANCE_TRAFFIC_HOURLY_WD_FILEPATH = file.path(FRANCE_OUTPUT_DIR, 
                                                france_traffic_hourly_wd_filename), 
  FRANCE_TRAFFIC_HOURLY_WE_FILEPATH = file.path(FRANCE_OUTPUT_DIR, 
                                                france_traffic_hourly_we_filename)
)