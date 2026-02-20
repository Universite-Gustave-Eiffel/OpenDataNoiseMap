# ==============================================================================
# Global pipeline configuration
# ==============================================================================

pipeline_message("Definition of the global configuration",
                 level = 0, progress = "start", process = "install")

ROOT_DIR <- PROJECT_ROOT
DATA_DIR <- file.path("data")
FIGS_DIR <- file.path("figures")
LOGS_DIR <- file.path("logs")

# Global configuration list
CONFIG_GLOBAL <- list(
  
  # ----------------------------------------------------------------------------
  # CRS for transforming or converting simple feature coordinates (Lambert-93)
  # ----------------------------------------------------------------------------

  TARGET_CRS = 2154,

  # ----------------------------------------------------------------------------
  # Project structure
  # ----------------------------------------------------------------------------
  
  ROOT_DIR = ROOT_DIR,
  DATA_DIR = DATA_DIR,
  FIGS_DIR = FIGS_DIR,
  LOGS_DIR = LOGS_DIR
)

pipeline_message("Global configuration successfully defined",
                 level = 0, progress = "end", process = "valid")