# ==============================================================================
# STAGE 1: SETUP
# ==============================================================================

pipeline_message(text = "Setup configuration", 
                 level = 0, progress = "start", process = "install")

# ------------------------------------------------------------------------------
# Required packages
# ------------------------------------------------------------------------------
pkgs_needed <- c(
  "dplyr", "tidyr", "sf", "httr", "jsonlite", "lubridate", "fs",
  "data.table", "stringr", "sfnetworks", "igraph", "tidygraph", "progress", 
  "ggplot2", "gridExtra", "xgboost", "Matrix")

pipeline_message(text = "Loading packages and installing missing packages", 
                 level = 1, progress = "start", process = "install")

installed <- rownames(installed.packages())
missing <- setdiff(pkgs_needed, installed)

if (length(missing) > 0) {
  install.packages(missing)
}

# Load libraries
for (p in pkgs_needed) {
  suppressPackageStartupMessages(
    library(p, character.only = TRUE)
  )
}

pipeline_message(text = "Packages loaded successfully", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Directories
# ------------------------------------------------------------------------------

pipeline_message(text = "Creating required directories", 
                 level = 1, progress = "start", process = "install")

# -------------------- #
# Input file directory #
# -------------------- #
dir.create(path = CONFIG$INPUT_DATA_DIR, 
           showWarnings = FALSE, 
           recursive = TRUE)

# --------------------- #
# Output file directory #
# --------------------- #
# Global output file directory
dir.create(path = CONFIG$OUTPUT_DATA_DIR,  
           showWarnings = FALSE, 
           recursive = TRUE)
# OSM data directory
dir.create(path = file.path(OUTPUT_DATA_DIR, "osm"), 
           showWarnings = FALSE, 
           recursive = TRUE)
# Avatar data directories
dir.create(path = file.path(CONFIG$AVATAR_DATA_DIR, "csv"), 
          showWarnings = FALSE, 
          recursive = TRUE)
dir.create(path = file.path(CONFIG$AVATAR_DATA_DIR, "json"), 
          showWarnings = FALSE, 
          recursive = TRUE)
dir.create(path = file.path(CONFIG$AVATAR_DATA_DIR, "rds"), 
          showWarnings = FALSE, 
          recursive = TRUE)
dir.create(path = file.path(CONFIG$AVATAR_DATA_DIR, "gpkg"), 
          showWarnings = FALSE, 
          recursive = TRUE)
# Training data directories
dir.create(path = file.path(CONFIG$TRAINING_DATA_DIR, "rds"), 
           showWarnings = FALSE, 
           recursive = TRUE)
dir.create(path = file.path(CONFIG$TRAINING_DATA_DIR, "gpkg"), 
           showWarnings = FALSE, 
           recursive = TRUE)
# FORECASTING data directories
dir.create(path = FORECAST_DATA_DIR, 
           showWarnings = FALSE, 
           recursive = TRUE)

# ------- #
# Figures #
# ------- #
dir.create(path = CONFIG$FIGS_DIR,  
           showWarnings = FALSE, 
           recursive = TRUE)

pipeline_message(text = "Required directories created", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Options
# ------------------------------------------------------------------------------
op <- options(digits.secs = 1, 
              digits = 2)

pipeline_message(text = "Setup stage completed", 
                 level = 0, progress = "end", process = "valid")
