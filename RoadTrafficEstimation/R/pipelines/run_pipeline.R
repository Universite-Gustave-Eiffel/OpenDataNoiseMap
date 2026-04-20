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

pipeline_message("Run pipeline", level = 0, 
                 progress = "start", process = "calc")

# ==============================================================================
# LOAD CONFIGURATION
# ==============================================================================

pipeline_message("Parsing arguments", level = 1, 
                 progress = "start", process = "install")

# Normalize phase name
PHASE_NORMALIZED <- tolower(PHASE)
PHASE_NORMALIZED <- gsub(pattern     = "data_prep.*", 
                         replacement = "preparation", 
                         x           = PHASE_NORMALIZED)
PHASE_NORMALIZED <- gsub(pattern     = "train.*", 
                         replacement = "training", 
                         x           = PHASE_NORMALIZED)
PHASE_NORMALIZED <- gsub(pattern     = "pred.*", 
                         replacement = "prediction", 
                         x           = PHASE_NORMALIZED)

# Define phases to run
phases_to_run <- if (PHASE_NORMALIZED == "all") {
  c("preparation", "training", "prediction")
} else {
  PHASE_NORMALIZED
}

# Define modes to run
modes_to_run <- if (MODE == "all") {
  c("preparation", "training", "nantes", "paris", "pemb", "sensors", "france")
} else if (MODE %in% c("nantes", "paris", "pemb", "sensors")) {
  c("preparation", "training", MODE)
} else {
  MODE
}

# If prediction phase is selected with a specific mode, ensure preparation and 
# training are included if models are not already available
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
  
  # Check if OSM engineering steps can be skipped
  osm_reengineering_needed <- 
    isTRUE(CFG$FORCE_REENGINEER_OSM_FRANCE) ||
    isTRUE(CFG$FORCE_REJOIN_OSM_AND_COMMUNES) ||
    !file.exists(CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH) ||
    !file.exists(CFG$IMPUTATION_RULES_FRANCE_FILEPATH)
  
  # Check if Avatar aggregation can be skipped
  avatar_aggregation_needed <- !file.exists(CFG$AVATAR_AGGREGATED_FILEPATH)
  
  # Check if training dataset merge is needed
  training_merge_needed <- isTRUE(CFG$FORCE_BUILD_TRAINING_DATASET) || 
                           !file.exists(CFG$TRAINING_GPKG_DATA_FILEPATH) || 
                           !file.exists(CFG$TRAINING_RDS_DATA_FILEPATH)
  
  # Run OSM processing and feature engineering if needed
  if (osm_reengineering_needed) {
    source("R/pipelines/data_preparation/01_osm_processing.R")
    source("R/pipelines/data_preparation/02_osm_feature_engineering.R")
  } else {
    pipeline_message(
      paste("OSM engineering data already exists. ", 
            "Skipping 01_osm_processing.R and 02_osm_feature_engineering.R. ", 
            "To rebuild, set FORCE_REENGINEER_OSM_FRANCE=TRUE or ", 
            "FORCE_REJOIN_OSM_AND_COMMUNES=TRUE in config/config_data_prep.R"), 
      level = 1, progress = "end", process = "warning")
  }
  
  # Run Avatar download and aggregation if needed
  if (osm_reengineering_needed || avatar_aggregation_needed) {
    source("R/pipelines/data_preparation/03_avatar_download.R")
    source("R/pipelines/data_preparation/04_avatar_aggregation.R")
  } else {
    pipeline_message(
      paste("AVATAR aggregated data already exists. ", 
            "Skipping 03_avatar_download.R and 04_avatar_aggregation.R. ", 
            "To rebuild, set FORCE_REDOWNLOAD_CHUNKS=TRUE ", 
            "in config/config_data_prep.R"),
      level = 1, progress = "end", process = "warning")
  }
  
  # Run training dataset merge if needed
  if (training_merge_needed) {
    source("R/pipelines/data_preparation/05_training_dataset_merge.R")
  } else {
    pipeline_message(
      paste("Training dataset already exists. ",
            "Skipping 05_training_dataset_merge.R. ",
            "To rebuild, set FORCE_BUILD_TRAINING_DATASET=TRUE in ", 
            "config/config_data_prep.R"),
      level = 1, progress = "end", process = "warning")
  }
  
  if (TEST_FLAG) {
    source("R/pipelines/tests/test_data_preparation.R")
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
  
  source("R/pipelines/model_training/train_xgboost_models.R")
  
  if (TEST_FLAG) {
    source("R/pipelines/tests/test_model_training.R")
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
    source("R/pipelines/prediction/predict_nantes.R")
  }
  
  # Prediction for Paris
  if ("paris" %in% modes_to_run) {
    source("R/pipelines/prediction/predict_paris.R")
  }
  
  # Prediction for PEMB (Paris Est Marne & Bois)
  if ("pemb" %in% modes_to_run) {
    source("R/pipelines/prediction/predict_pemb.R")
  }
  
  # Prediction for sensors
  if ("sensors" %in% modes_to_run) {
    source("R/pipelines/prediction/predict_sensors.R")
  }
  
  # Prediction for France (tiled, geometry-separated)
  if ("france" %in% modes_to_run) {
    source("R/pipelines/prediction/predict_france.R")
  }
  
  if (TEST_FLAG) {
    source("R/pipelines/tests/test_prediction.R")
  }
  
  pipeline_message(
    "Prediction phase completed", level = 0, 
    progress = "end", process = "valid")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

pipeline_message("Pipeline completed successfully", level = 0, 
                 progress = "end", process = "valid")
