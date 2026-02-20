# ==============================================================================
# Pipeline mode registry
# ==============================================================================

MODE_REGISTRY <- list(
  # Data preparation (OSM + Avatar)
  data_prep = list(
    description = "OSM + Avatar data preparation", 
    log_key     = "data_prep", 
    log_file    = file.path(CFG$global$dirs$logs, "data_preparation.log"),
    script      = "R/pipelines/data_preparation/run_data_preparation.R"), 
  # Avatar data download
  avatar = list(
    description = "Avatar download only", 
    log_key     = "avatar", 
    log_file    = file.path(CFG$global$dirs$logs, "avatar_download.log"),
    script      = "R/pipelines/data_preparation/03_avatar_download.R"), 
  # Learning model training
  training = list(
    description = "Model training", 
    log_key     = "training", 
    log_file    = file.path(CFG$global$dirs$logs, "train_model.log"),
    script      = "R/pipelines/training/run_training.R"), 
  # Prediction for the city of Nantes
  nantes = list(
    description = "Prediction – Nantes southeast", 
    log_key     = "nantes", 
    log_file    = file.path(CFG$global$dirs$logs, "nantes_prediction.log"),
    script      = "R/pipelines/prediction/predict_nantes_southeast.R"), 
  # Prediction for the city of Paris
  paris = list(
    description = "Prediction – Paris area", 
    log_key     = "paris", 
    log_file    = file.path(CFG$global$dirs$logs, "paris_prediction.log"),
    script      = "R/pipelines/prediction/predict_paris_area.R"), 
  # Prediction on a sensor network
  sensors = list(
    description = "Prediction – sensor-based", 
    log_key     = "sensors", 
    log_file    = file.path(CFG$global$dirs$logs, "sensors_prediction.log"),
    script      = "R/pipelines/prediction/predict_sensors.R")
)

assign("MODE_REGISTRY", MODE_REGISTRY, envir = .GlobalEnv)
