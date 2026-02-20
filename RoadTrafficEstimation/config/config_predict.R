# ==============================================================================
# Forecast pipeline configuration
# ==============================================================================

# Forecast paths
PREDICTION_DIR <- file.path("data", "forecast")

CONFIG_PREDICT <- list(
  
  # ----------------------------------------------------------------------------
  # Directories and files
  # ----------------------------------------------------------------------------
  PREDICTION_DIR = PREDICTION_DIR,
  SENSOR_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, "sensor_roads_dt.rds"), 
  PARIS_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, "paris_roads_dt.rds"), 
  NANTES_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, "nantes_roads_dt.rds")
)
