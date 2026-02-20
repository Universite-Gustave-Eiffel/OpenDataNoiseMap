# ==============================================================================
# Prediction pipeline configuration
# ==============================================================================

# Prediction paths and filenames
PREDICTION_DIR                    <- file.path("data", "prediction")
sensor_predictions_filename       <- "07_predictions_sensors_all.gpkg"
nantes_predictions_filename       <- "07_predictions_nantes.gpkg"
paris_predictions_filename        <- "07_predictions_paris.gpkg"
pemb_predictions_filename         <- "07_predictions_pemb.gpkg"
FRANCE_OUTPUT_DIR                 <- file.path(PREDICTION_DIR, "france")
france_network_filename           <- "07_france_network.gpkg"
france_traffic_den_filename       <- "07_france_traffic_DEN.gpkg"
france_traffic_hourly_filename    <- "07_france_traffic_hourly.gpkg"
france_traffic_hourly_wd_filename <- "07_france_traffic_hourly_wd.gpkg"
france_traffic_hourly_we_filename <- "07_france_traffic_hourly_we.gpkg"

CONFIG_PREDICT <- list(
  
  # ----------------------------------------------------------------------------
  # Directories and files
  # ----------------------------------------------------------------------------
  PREDICTION_DIR                    = PREDICTION_DIR, 
  SENSORS_ALL_PREDICTION_FILEPATH   = file.path(PREDICTION_DIR, "sensor_roads_dt.rds"), 
  PARIS_PREDICTION_FILEPATH         = file.path(PREDICTION_DIR, "paris_roads_dt.rds"), 
  NANTES_PREDICTION_FILEPATH        = file.path(PREDICTION_DIR, nantes_predictions_filename), 
  PEMB_PREDICTION_FILEPATH          = file.path(PREDICTION_DIR, pemb_predictions_filename), 
  FRANCE_GEOMETRY_FILEPATH          = file.path(FRANCE_OUTPUT_DIR, france_network_filename), 
  FRANCE_TRAFFIC_DEN_FILEPATH       = file.path(FRANCE_OUTPUT_DIR, france_traffic_den_filename), 
  FRANCE_TRAFFIC_HOURLY_FILEPATH    = file.path(FRANCE_OUTPUT_DIR, france_traffic_hourly_filename), 
  FRANCE_TRAFFIC_HOURLY_WD_FILEPATH = file.path(FRANCE_OUTPUT_DIR, france_traffic_hourly_wd_filename), 
  FRANCE_TRAFFIC_HOURLY_WE_FILEPATH = file.path(FRANCE_OUTPUT_DIR, france_traffic_hourly_we_filename)
)
