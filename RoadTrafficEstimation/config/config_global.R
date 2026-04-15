# ==============================================================================
# Global pipeline configuration
# ==============================================================================

pipeline_message("Definition of the global configuration", level = 0, 
                 progress = "start", process = "install")

DATA_DIR <- file.path("data")
FIGS_DIR <- file.path("figures")
LOGS_DIR <- file.path("logs")

# Global configuration list
CONFIG_GLOBAL <- list(
  
  # ----------------------------------------------------------------------------
  # Number of cores for parallel processing in tiled prediction
  # ----------------------------------------------------------------------------
  PREDICTION_TILE_CORES = 12,

  # ----------------------------------------------------------------------------
  # CRS for transforming or converting simple feature coordinates (Lambert-93)
  # ----------------------------------------------------------------------------

  TARGET_CRS = 2154,

  # ----------------------------------------------------------------------------
  # Project structure
  # ----------------------------------------------------------------------------
  
  DATA_DIR   = DATA_DIR,
  FIGS_DIR   = FIGS_DIR,
  LOGS_DIR   = LOGS_DIR
)

pipeline_message("Global configuration successfully defined", level = 0, 
                 progress = "end", process = "valid")