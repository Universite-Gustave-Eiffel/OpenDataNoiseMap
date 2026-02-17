# ==============================================================================
# Pipeline mode registry
# ==============================================================================

MODE_REGISTRY <- list(
  # Data preparation (OSM + Avatar)
  data_prep = list(
    description = "OSM + Avatar data preparation", 
    log_key     = "data_prep", 
    log_file    = file.path(CFG$global$dirs$logs, "data_preparation.log"),
    script      = "pipelines/data_preparation/run_data_preparation.R"), 
  # Avatar data download
  avatar = list(
    description = "Avatar download only", 
    log_key     = "avatar", 
    log_file    = file.path(CFG$global$dirs$logs, "avatar_download.log"),
    script      = "pipelines/data_preparation/03_avatar_download.R"), 
  # Learning model training
  training = list(
    description = "Model training", 
    log_key     = "training", 
    log_file    = file.path(CFG$global$dirs$logs, "train_model.log"),
    script      = "pipelines/training/run_training.R"), 
  # Forecast for the city of Nantes
  nantes = list(
    description = "Forecast – Nantes southeast", 
    log_key     = "nantes", 
    log_file    = file.path(CFG$global$dirs$logs, "nantes_forecast.log"),
    script      = "pipelines/forecast/predict_nantes_southeast.R"), 
  # Forecast for the city of Paris
  paris = list(
    description = "Forecast – Paris area", 
    log_key     = "paris", 
    log_file    = file.path(CFG$global$dirs$logs, "paris_forecast.log"),
    script      = "pipelines/forecast/predict_paris_area.R"), 
  # Forecast on a sensor network
  sensors = list(
    description = "Forecast – sensor-based", 
    log_key     = "sensors", 
    log_file    = file.path(CFG$global$dirs$logs, "sensors_forecast.log"),
    script      = "pipelines/forecast/predict_sensors.R")
)

assign("MODE_REGISTRY", MODE_REGISTRY, envir = .GlobalEnv)
