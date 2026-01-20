# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# R version
# ------------------------------------------------------------------------------

message("💻 ", R.version.string)

# ------------------------------------------------------------------------------
# Force working directory to project root (robust for HPC)
# ------------------------------------------------------------------------------
if (!exists("PROJECT_ROOT")) {
  # Option 1: use the path given in the script .sh as environment variable
  PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT")
  if (PROJECT_ROOT == "") {
    # Fallback: current repository
    PROJECT_ROOT <- getwd()
  }
}
setwd(PROJECT_ROOT)

message("💻 Current working directory: ", getwd())

# ------------------------------------------------------------------------------
# Run successive steps
# ------------------------------------------------------------------------------

source("bootstrap.R")
source("config_pipeline.R")
source("run_pipeline.R")