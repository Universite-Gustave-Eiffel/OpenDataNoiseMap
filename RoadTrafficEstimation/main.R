# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# Force working directory to project root
# ------------------------------------------------------------------------------

if (!exists("PROJECT_ROOT")) {
  # Option 1: use the path given in the script .sh as environment variable
  PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT")
  if (PROJECT_ROOT == "") {
    # Option 2: fallback -> current repository
    PROJECT_ROOT <- getwd()
  }
}
setwd(PROJECT_ROOT)

message(sprintf("📁 Current working directory: %s", PROJECT_ROOT))

# ------------------------------------------------------------------------------
# Pipeline mode (CLI argument)
# ------------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
MODE <- if (length(args) >= 1) {
  args[1]
} else {
  "data_prep"
}
assign("MODE", MODE, envir = .GlobalEnv)

message(sprintf("🚦 Pipeline mode: %s", 
                ifelse(test = MODE=="avatar", 
                       yes = paste(MODE, "download"), 
                       no = MODE)))

# ------------------------------------------------------------------------------
# Run successive steps
# ------------------------------------------------------------------------------

# Bootstrap configuration
source("bootstrap/bootstrap.R")
source("config/config_global.R")
source("config/config_data_prep.R")
source("config/config_training.R")
source("config/config_forecast.R")

# Complete configuration list
CFG <- list(
  global    = CONFIG_GLOBAL, 
  data_prep = CONFIG_DATA_PREP, 
  training  = CONFIG_TRAINING, 
  forecast  = CONFIG_FORECAST)
assign("CFG", CFG, envir = .GlobalEnv)

# --------------------------------------------------
# Register pipeline modes (depends on CFG)
# --------------------------------------------------
source("./config/mode_registry.R")
if (!MODE %in% names(MODE_REGISTRY)) {
  stop("Unknown MODE: ", MODE)
}
# Logs
LOG_FILE <- MODE_REGISTRY[[MODE]]$log_file
assign("LOG_FILE", LOG_FILE, envir = .GlobalEnv)

# Setup
source("project_setup.R")

# Run selected pipeline
source(MODE_REGISTRY[[MODE]]$script)