# ==============================================================================
# MASTER PIPELINE SCRIPT
# ==============================================================================

pipeline_message(text = "Run pipeline", 
                 level = 0, progress = "start", process = "calc")

# ------------------------------------------------------------------------------
# Calculation mode
# ------------------------------------------------------------------------------

pipeline_message(text = "Calculation mode recovery", 
                 level = 1, progress = "start", process = "install")

# Parsing arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1 || !args %in% c("nantes", "paris", "sensors")) {
  pipeline_message(
    text = "Usage: Rscript run_pipeline.R <nantes|paris|sensors>", 
    process = "stop")
}

# Mode selection
MODE <- args[1]
pipeline_message(text = sprintf("Running pipeline in mode: ", MODE), 
                 level = 1, progress = "end", process = "install")

# ------------------------------------------------------------------------------
# Setup environment
# ------------------------------------------------------------------------------
source("01_setup_minimal.R")

# ------------------------------------------------------------------------------
# Pipeline stages
# ------------------------------------------------------------------------------
source("02_osm_processing_minimal.R")
source("03_avatar_download_minimal.R")
source("04_data_integration_minimal.R")
source("05_feature_engineering_minimal.R")
source("06d_xgboost_training_with_ratios.R")

# ------------------------------------------------------------------------------
# Prediction stage (conditional)
# ------------------------------------------------------------------------------
if (MODE == "nantes") {
  source("07d_predict_nantes_southeast.R")
} else if (MODE == "sensors") {
  source("07d_predict_sensors.R")
} else if (MODE == "paris") {
  source("07e_predict_paris_area.R")
}

pipeline_message(text = "Pipeline ready", 
                 level = 0, progress = "end", process = "valid")