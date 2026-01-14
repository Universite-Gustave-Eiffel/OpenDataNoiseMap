# ==============================================================================
# STAGE 1: SETUP
# ==============================================================================

message("\n === Setup configuration === \n")

# ------------------------------------------------------------------------------
# Required packages
# ------------------------------------------------------------------------------
pkgs_needed <- c(
  "dplyr", "tidyr", "sf", "httr", "jsonlite", "lubridate", "randomForest", 
  "data.table", "stringr", "sfnetworks", "igraph", "tidygraph", "progress", 
  "ggplot2", "gridExtra", "data.table", "xgboost", "Matrix")
message("\t 💻 Loading packages and installing missing packages \n")

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

message("\t\t ✓ packages loaded successfully \n")

# ------------------------------------------------------------------------------
# Directories
# ------------------------------------------------------------------------------

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

# ------- #
# Figures #
# ------- #
dir.create(path = CONFIG$FIGS_DIR,  
           showWarnings = FALSE, 
           recursive = TRUE)

# ------------------------------------------------------------------------------
# Load utility functions
# ------------------------------------------------------------------------------
utils_files <- list.files(path = "R", 
                          pattern = "\\.R$", 
                          full.names = TRUE)
for (uf in utils_files) {
  source(uf)
}

# ------------------------------------------------------------------------------
# Options
# ------------------------------------------------------------------------------
op <- options(digits.secs = 1)


message("\t ✓ Setup stage completed!\n")
