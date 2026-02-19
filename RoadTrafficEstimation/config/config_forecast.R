# ==============================================================================
# Forecast pipeline configuration
# ==============================================================================

# Forecast paths
FORECAST_DIR <- file.path("data", "forecast")

CONFIG_FORECAST <- list(
  
  # ----------------------------------------------------------------------------
  # Directories and files
  # ----------------------------------------------------------------------------
  FORECAST_DIR = FORECAST_DIR,
  SENSOR_FORECAST_FILEPATH = file.path(FORECAST_DIR, "sensor_roads_dt.rds"), 
  PARIS_FORECAST_FILEPATH = file.path(FORECAST_DIR, "paris_roads_dt.rds"), 
  NANTES_FORECAST_FILEPATH = file.path(FORECAST_DIR, "nantes_roads_dt.rds")
)
