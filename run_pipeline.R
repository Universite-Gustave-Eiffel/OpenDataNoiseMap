# ==============================================================================
# MASTER PIPELINE SCRIPT
# ==============================================================================

# ------------------------------------------------------------------------------
# Calculation mode
# ------------------------------------------------------------------------------

# Parsing arguments
args <- commandArgs(trailingOnly = TRUE)

if (isTRUE(IS_TTY)){
  if (length(args) != 1 || !args %in% c("nantes", "paris", "sensors")) {
    stop("Usage: Rscript run_pipeline.R <nantes|paris|sensors>")
  }
} else {
  if (length(args) != 1 || !args %in% c("nantes", "paris", "sensors")) {
    message("Running pipeline in local without chosen mode. Setting mode to `sensors`")
    args[1] = "sensors"
  }
}

# Mode selection
MODE <- args[1]
message("Running pipeline in mode: ", MODE)

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