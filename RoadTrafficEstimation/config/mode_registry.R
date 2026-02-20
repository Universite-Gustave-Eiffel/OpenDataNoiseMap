# ==============================================================================
# Pipeline mode registry
# ==============================================================================

logs_dir <- CFG$global$dirs$logs
pipelines_dir <- "R/pipelines"

MODE_REGISTRY <- list(

  # Data preparation (OSM + Avatar)
  data_prep = list(
    description = "OSM + Avatar data preparation", 
    log_key     = "data_prep", 
    log_file    = file.path(logs_dir, "data_preparation.log"),
    script      = file.path(pipelines_dir, 
                            "data_preparation", "run_data_preparation.R")), 
  
  # Avatar data download
  avatar = list(
    description = "Avatar download only", 
    log_key     = "avatar", 
    log_file    = file.path(logs_dir, "avatar_download.log"),
    script      = file.path(pipelines_dir, 
                            "data_preparation", "03_avatar_download.R")), 
  
  # Learning model training
  training = list(
    description = "Model training", 
    log_key     = "training", 
    log_file    = file.path(logs_dir, "train_model.log"),
    script      = file.path(pipelines_dir, 
                            "model_training", "run_training.R")), 
  
  # Prediction for the city of Nantes
  nantes = list(
    description = "Prediction - Nantes southeast", 
    log_key     = "nantes", 
    log_file    = file.path(logs_dir, "nantes_prediction.log"),
    script      = file.path(pipelines_dir, 
                            "prediction", "predict_nantes_southeast.R")),
  
  # Prediction for the city of Paris
  paris = list(
    description = "Prediction - Paris area", 
    log_key     = "paris", 
    log_file    = file.path(logs_dir, "paris_prediction.log"),
    script      = file.path(pipelines_dir, 
                            "prediction", "predict_paris_area.R")), 
  
  # Prediction on a sensor network
  sensors = list(
    description = "Prediction - sensor-based", 
    log_key     = "sensors", 
    log_file    = file.path(logs_dir, "sensors_prediction.log"),
    script      = file.path(pipelines_dir, 
                            "prediction", "predict_sensors.R")), 
  
  # Prediction on a PEMB (PARIS EST MARNE & BOIS)
  pemb = list(
    description = "Prediction - pemb (Paris Est Marne & Bois)", 
    log_key     = "pemb", 
    log_file    = file.path(logs_dir, "pemb_prediction.log"),
    script      = file.path(pipelines_dir, 
                            "prediction", "predict_pemb.R")), 
  
  # Prediction for all of France
  france = list(
    description = "Prediction - France-wide", 
    log_key     = "france", 
    log_file    = file.path(logs_dir, "france_prediction.log"),
    script      = file.path(pipelines_dir, 
                            "prediction", "predict_france.R"))
)

assign(x = "MODE_REGISTRY", value = MODE_REGISTRY, envir = .GlobalEnv)
