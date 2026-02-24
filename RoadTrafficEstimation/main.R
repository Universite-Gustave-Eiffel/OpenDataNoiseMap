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

# -------------------------------------------
# Parse command line arguments
# -------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

# PHASE (argument 1 / default = "all")
PHASE <- if (length(args) >= 2) args[2] else "all"
assign(x = "PHASE", value = PHASE, envir = .GlobalEnv)

# MODE (argument 1 / default = "paris")
MODE <- if (length(args) >= 1) args[1] else "paris"
assign(x = "MODE", value = MODE, envir = .GlobalEnv)

# REGION (argument 3 / default = "full")
REGION <- if (length(args) >= 3) args[3] else "full"
assign(x = "REGION", value = REGION, envir = .GlobalEnv)

# TEST_FLAG (argument 4 / yes = "--test" | no = "" / default = "")
TEST_FLAG <- if (length(args) >= 4) args[4] else ""
assign(x = "TEST_FLAG", value = TEST_FLAG, envir = .GlobalEnv)
# -------------------------------------------
# Confirmation messages
# -------------------------------------------
message(sprintf("⚡ Phase          : %s", PHASE))
message(sprintf("🚦 Pipeline mode : %s", ifelse(MODE=="avatar", paste(MODE,"download"), MODE)))
message(sprintf("🌍 Region        : %s", REGION))
message(sprintf("🧪 Test flag     : %s", ifelse(TEST_FLAG=="--test", "ON", "OFF")))

# ------------------------------------------------------------------------------
# Run successive steps
# ------------------------------------------------------------------------------

source("bootstrap.R")
source("config_pipeline.R")
source("run_pipeline.R")