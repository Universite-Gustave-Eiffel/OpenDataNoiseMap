# ==============================================================================
# MASTER PIPELINE SCRIPT
# ==============================================================================
# Orchestrateur du pipeline avec support des phases, modes et régions.
# 
# Usage:
#   Rscript run_pipeline.R --phase <preparation|training|prediction|all> \
#                          --mode <nantes|paris|pemb|sensors|all> \
#                          [--region <full|small|test>] [--test]
# ==============================================================================

pipeline_message("Run pipeline", level = 0, progress = "start", process = "calc")

# ==============================================================================
# LOAD CONFIGURATION
# ==============================================================================

pipeline_message("Parsing arguments", level = 1, progress = "start", process = "install")

# Define phases to run
phases_to_run <- if (PHASE == "all") {
  c("preparation", "training", "prediction")
} else {
  PHASE
}

# Define modes to run
modes_to_run <- if (MODE == "all") {
  c("preparation", "training", "nantes", "paris", "pemb", "sensors", "france")
} else if (MODE %in% c("nantes", "paris", "pemb", "sensors")) {
  c("preparation", "training", MODE)
} else {
  MODE
}

if (PHASE == "prediction" && MODE != "all") {
  # Only add preparation and training if models are not already available
  if (!file.exists(CFG$XGB_MODELS_WITH_RATIOS_FILEPATH) ||
      !file.exists(CFG$XGB_RATIO_FEATURE_INFO_FILEPATH)) {
    phases_to_run <- unique(c("preparation", "training", phases_to_run))
    pipeline_message("Models not found — adding preparation and training phases", 
                     process = "warning")
  }
}

pipeline_message(sprintf("Phase: %s | Mode: %s | Region: %s | Tests: %s", 
                         PHASE, MODE, REGION, if (TEST_FLAG) "ON" else "OFF"), 
                 level = 1, progress = "end", process = "install")

# ==============================================================================
# FILTER MODES AND PHASES
# ==============================================================================

# ------------------------------------------------------------------------------
# PHASE 1: DATA PREPARATION
# ------------------------------------------------------------------------------

if ("preparation" %in% phases_to_run) {
  pipeline_message("PHASE 1: DATA PREPARATION", level = 0, 
                   progress = "start", process = "calc")
  
  # source("R/data_preparation/01_setup_environment.R")
  source("R/data_preparation/02_osm_processing.R")
  source("R/data_preparation/03_osm_feature_engineering.R")
  source("R/data_preparation/04_avatar_download.R")
  source("R/data_preparation/05_avatar_aggregation.R")
  source("R/data_preparation/06_training_dataset_merge.R")
  
  if (TEST_FLAG) {
    source("R/tests/test_data_preparation.R")
  }
  
  pipeline_message("Data preparation phase completed", level = 0, 
                   progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# PHASE 2: MODEL TRAINING
# ------------------------------------------------------------------------------

if ("training" %in% phases_to_run) {
  pipeline_message("PHASE 2: MODEL TRAINING", level = 0, 
                   progress = "start", process = "calc")
  
  source("R/model_training/train_xgboost_models.R")
  
  if (TEST_FLAG) {
    source("R/tests/test_model_training.R")
  }
  
  pipeline_message("Model training phase completed", level = 0, 
                   progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# PHASE 3: PREDICTION
# ------------------------------------------------------------------------------

if ("prediction" %in% phases_to_run) {
  pipeline_message("PHASE 3: PREDICTION", level = 0, 
                   progress = "start", process = "calc")
  
  # Prediction for Nantes
  if ("nantes" %in% modes_to_run) {
    source("R/prediction/predict_nantes.R")
  }
  
  # Prediction for Paris
  if ("paris" %in% modes_to_run) {
    source("R/prediction/predict_paris.R")
  }
  
  # Prediction for PEMB (Paris Est Marne & Bois)
  if ("pemb" %in% modes_to_run) {
    source("R/prediction/predict_pemb.R")
  }
  
  # Prediction for sensors
  if ("sensors" %in% modes_to_run) {
    source("R/prediction/predict_sensors.R")
  }
  
  # Prediction for France (tiled, geometry-separated)
  if ("france" %in% modes_to_run) {
    source("R/prediction/predict_france.R")
  }
  
  if (TEST_FLAG) {
    source("R/tests/test_prediction.R")
  }
  
  pipeline_message(
    text = "Prediction phase completed",
    level = 0, progress = "end", process = "valid")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

pipeline_message(
  text = "Pipeline completed successfully",
  level = 0, progress = "end", process = "valid")