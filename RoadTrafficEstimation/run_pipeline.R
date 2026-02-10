# ==============================================================================
# MASTER PIPELINE SCRIPT
# ==============================================================================
# Orchestrateur du pipeline avec support des phases, modes et régions.
# 
# Usage:
#   Rscript run_pipeline.R --phase <preparation|training|prediction|all> \
#                          --mode <nantes|paris|sensors|all> \
#                          [--region <full|small|test>] [--test]
# ==============================================================================

pipeline_message(text = "Run pipeline", 
                 level = 0, progress = "start", process = "calc")

# ------------------------------------------------------------------------------
# Parse command-line arguments
# ------------------------------------------------------------------------------

pipeline_message(text = "Parsing arguments", 
                 level = 1, progress = "start", process = "install")

args <- commandArgs(trailingOnly = TRUE)

# Default values
PHASE_ARG <- "all"      # preparation | training | prediction | all
MODE_ARG <- "all"       # nantes | paris | sensors | all
REGION_ARG <- "full"    # full | small | test
RUN_TESTS <- FALSE

# Parse arguments
i <- 1
while (i <= length(args)) {
  arg <- args[i]
  
  if (arg == "--phase" && i < length(args)) {
    PHASE_ARG <- args[i + 1]
    i <- i + 2
  } else if (arg == "--mode" && i < length(args)) {
    MODE_ARG <- args[i + 1]
    i <- i + 2
  } else if (arg == "--region" && i < length(args)) {
    REGION_ARG <- args[i + 1]
    i <- i + 2
  } else if (arg == "--test") {
    RUN_TESTS <- TRUE
    i <- i + 1
  } else {
    i <- i + 1
  }
}

# Validate arguments
valid_phases <- c("preparation", "training", "prediction", "all")
valid_modes <- c("nantes", "paris", "sensors", "all")
valid_regions <- c("full", "small", "test")

if (!PHASE_ARG %in% valid_phases) {
  pipeline_message(
    text = sprintf("Invalid phase: %s. Valid: %s", 
                   PHASE_ARG, paste(valid_phases, collapse = ", ")),
    process = "stop")
}

if (!MODE_ARG %in% valid_modes) {
  pipeline_message(
    text = sprintf("Invalid mode: %s. Valid: %s", 
                   MODE_ARG, paste(valid_modes, collapse = ", ")),
    process = "stop")
}

if (!REGION_ARG %in% valid_regions) {
  pipeline_message(
    text = sprintf("Invalid region: %s. Valid: %s", 
                   REGION_ARG, paste(valid_regions, collapse = ", ")),
    process = "stop")
}

# Load TEST_CONFIG if small or test region requested
if (REGION_ARG %in% c("small", "test")) {
  if (file.exists("TEST_CONFIG.R")) {
    source("TEST_CONFIG.R")
    REGION_ARG <- "test"  # Normalize to 'test'
  } else {
    pipeline_message(
      text = "TEST_CONFIG.R not found. Falling back to full region.",
      process = "warning")
    REGION_ARG <- "full"
  }
}

pipeline_message(
  text = sprintf("Phase: %s | Mode: %s | Region: %s | Tests: %s", 
                 PHASE_ARG, MODE_ARG, REGION_ARG, if (RUN_TESTS) "ON" else "OFF"),
  level = 1, progress = "end", process = "install")

# Define modes to run
modes_to_run <- if (MODE_ARG == "all") {
  c("preparation", "training", "nantes", "paris", "sensors")
} else if (MODE_ARG %in% c("nantes", "paris", "sensors")) {
  c("preparation", "training", MODE_ARG)
} else {
  MODE_ARG
}

# Define phases to run
phases_to_run <- if (PHASE_ARG == "all") {
  c("preparation", "training", "prediction")
} else {
  PHASE_ARG
}

# Helper pour %notin%
`%notin%` <- Negate(`%in%`)

# Filter modes and phases

# ==============================================================================
# PHASE 1: DATA PREPARATION
# ==============================================================================

if ("preparation" %in% phases_to_run) {
  pipeline_message(
    text = "PHASE 1: DATA PREPARATION",
    level = 0, progress = "start", process = "calc")
  
  source("R/data_preparation/01_setup_environment.R")
  source("R/data_preparation/02_osm_processing.R")
  source("R/data_preparation/03_osm_feature_engineering.R")
  source("R/data_preparation/04_avatar_download.R")
  source("R/data_preparation/05_avatar_aggregation.R")
  source("R/data_preparation/06_training_dataset_merge.R")
  
  if (RUN_TESTS) {
    source("R/tests/test_data_preparation.R")
  }
  
  pipeline_message(
    text = "Data preparation phase completed",
    level = 0, progress = "end", process = "valid")
}

# ==============================================================================
# PHASE 2: MODEL TRAINING
# ==============================================================================

if ("training" %in% phases_to_run) {
  pipeline_message(
    text = "PHASE 2: MODEL TRAINING",
    level = 0, progress = "start", process = "calc")
  
  source("R/model_training/train_xgboost_models.R")
  
  if (RUN_TESTS) {
    source("R/tests/test_model_training.R")
  }
  
  pipeline_message(
    text = "Model training phase completed",
    level = 0, progress = "end", process = "valid")
}

# ==============================================================================
# PHASE 3: PREDICTION
# ==============================================================================

if ("prediction" %in% phases_to_run) {
  pipeline_message(
    text = "PHASE 3: PREDICTION",
    level = 0, progress = "start", process = "calc")
  
  # Prediction for Nantes
  if ("nantes" %in% modes_to_run) {
    source("R/prediction/predict_nantes.R")
  }
  
  # Prediction for Paris
  if ("paris" %in% modes_to_run) {
    source("R/prediction/predict_paris.R")
  }
  
  # Prediction for sensors
  if ("sensors" %in% modes_to_run) {
    source("R/prediction/predict_sensors.R")
  }
  
  if (RUN_TESTS) {
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