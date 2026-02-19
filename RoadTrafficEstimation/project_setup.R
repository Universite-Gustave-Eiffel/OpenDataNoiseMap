# ==============================================================================
# STAGE 1: SETUP
# ==============================================================================

pipeline_message("Setup configuration", level = 0, 
                 progress = "start", process = "install")

Sys.getenv("LD_LIBRARY_PATH")

# ------------------------------------------------------------------------------
# Required packages
# ------------------------------------------------------------------------------
pkgs_needed <- c(
  "Rcpp", "dplyr", "tidyr", "sf", "lwgeom", "httr", "jsonlite", "lubridate", 
  "randomForest", "data.table", "stringr", "sfnetworks", "igraph", "tidygraph", 
  "progress", "ggplot2", "gridExtra", "data.table", "xgboost", "Matrix", "tools")

installed <- rownames(installed.packages(lib.loc = .libPaths()))
missing <- setdiff(pkgs_needed, installed)

if (length(missing) > 0) {
  if (RUN_CONTEXT == "local") {
    pipeline_message("Installing missing packages", level = 1, 
                     progress = "start", process = "install")
    install.packages(missing)
  }
  if (RUN_CONTEXT == "slurm") {
      pipeline_message(sprintf("Missing packages on HPC: %s\nStop execution", 
                               paste(missing, collapse = ", ")), 
                       process = "stop")
  }
} else {
  # All required packages are already installed
  pipeline_message("Loading packages", 
                   level = 1, progress = "start", process = "load")
}

pipeline_message(sprintf("Active lib paths: %s", 
                         paste(.libPaths(), collapse = " | ")), process = "info")

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

# ------------- #
# Configuration #
# ------------- #
cfg_g <- CFG$global
cfg_data <- CFG$data_prep
cfg_train <- CFG$training
cfg_forecast <- CFG$forecast

# --------------------------- #
# Create required directories #
# --------------------------- #
setup_directories <- function(cfg) {
  # Flatten with names
  all_values <- unlist(x = cfg, recursive = TRUE, use.names = TRUE)
  # Keep only path-like entries via name
  path_idx <- grepl(pattern = "(_DIR$|_DIRPATH$|_FILEPATH$)",
                    x = names(all_values))
  paths <- all_values[path_idx]
  # Parent dirs for files
  parent_dirs <- dirname(path = paths)
  # Explicit dirs
  explicit_dirs <- paths[grepl(pattern = "_DIR$|_DIRPATH$",
                               x = names(paths))]
  dirs <- unique(x = c(parent_dirs, explicit_dirs))
  dirs <- dirs[nzchar(x = dirs)]
  created_dirs <- character(0)
  for (d in dirs) {
    if (!dir.exists(paths = d)) {
      dir.create(path = d, recursive = TRUE, showWarnings = FALSE)
      created_dirs <- c(created_dirs, d)
      pipeline_message(sprintf("Created directory: %s", d), process = "info")
    }
  }
  invisible(created_dirs)
}

setup_directories(c(cfg_g, cfg_data, cfg_train, cfg_forecast))

pipeline_message(text = "Required directories created", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Options
# ------------------------------------------------------------------------------
op <- options(digits.secs = 1, 
              digits = 2)

pipeline_message(text = "Setup stage completed", 
                 level = 0, progress = "end", process = "valid")
