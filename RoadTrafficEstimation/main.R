# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# Force working directory to project root
# ------------------------------------------------------------------------------

if (!exists("PROJECT_ROOT")) {
  # Option 1: use the path given in the batch script as environment variable
  PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT")
  if (PROJECT_ROOT == "") {
    # Option 2: fallback -> use the current repository as project root
    PROJECT_ROOT <- getwd()
  }
}
setwd(PROJECT_ROOT)

message(sprintf("📁 Current working directory: %s", PROJECT_ROOT))

# ------------------------------------------------------------------------------
# Bootstrap
# ------------------------------------------------------------------------------
source("bootstrap/bootstrap.R")

# ------------------------------------------------------------------------------
# Parse command line arguments
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

# PHASE (argument 1 / default = "all")
PHASE     <- get_arg_value("--phase", default = "all", args = args)
assign(x = "PHASE", value = PHASE, envir = .GlobalEnv)

# MODE (argument 2 / default = "paris")
MODE      <- get_arg_value("--mode",   default = "paris", args = args)
assign(x = "MODE", value = MODE, envir = .GlobalEnv)

# REGION (argument 3 / default = "full")
REGION    <- get_arg_value("--region", default = "full", args = args)
assign(x = "REGION", value = REGION, envir = .GlobalEnv)

# TEST_FLAG (argument 4 / yes = "--test" | no = "" / default = "")
TEST_FLAG <- "--test" %in% args
assign(x = "TEST_FLAG", value = TEST_FLAG, envir = .GlobalEnv)

# ------------------------------------------------------------------------------
# Confirmation messages
# ------------------------------------------------------------------------------
message(sprintf("⚡ Phase          : %s", PHASE))
message(sprintf("🚦 Pipeline mode : %s", ifelse(test = MODE=="avatar", 
                                                yes = paste(MODE,"download"), 
                                                no = MODE)))
message(sprintf("🌍 Region        : %s", REGION))
message(sprintf("🧪 Test flag     : %s", ifelse(test = TEST_FLAG, 
                                                yes = "ON", no = "OFF")))

# ------------------------------------------------------------------------------
# Run successive steps
# ------------------------------------------------------------------------------

# Configuration
source("config/config_global.R")
source("config/config_data_prep.R")
source("config/config_training.R")
source("config/config_predict.R")

# Complete configuration list
CFG <- c(CONFIG_GLOBAL, CONFIG_DATA_PREP, CONFIG_TRAINING, CONFIG_PREDICT)
assign(x = "CFG", value = CFG, envir = .GlobalEnv)

# Setup
source("project_setup.R")

# Run pipeline
source("R/pipelines/run_pipeline.R")