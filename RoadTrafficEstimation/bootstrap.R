# ==============================================================================
# BOOTSTRAP — ENVIRONMENT ONLY
# ==============================================================================

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
# Get computing environment information
# ------------------------------------------------------------------------------

pipeline_message(text = "Bootstrapping environment", 
                 level = 0, progress = "start", process = "install")

pipeline_message(text = "Obtaining the computing environment information", 
                 level = 1, progress = "start", process = "install")

RUN_CONTEXT <- Sys.getenv(x = "RUN_CONTEXT",
                          unset = "local")

pipeline_message(text = paste("Running environment: ", RUN_CONTEXT), 
                 level = 1, progress = "end", process = "install")

# ------------------------------------------------------------------------------
# LOCAL → renv
# ------------------------------------------------------------------------------
if (RUN_CONTEXT == "local") {
  pipeline_message(text = "Running locally", 
                   level = 1, progress = "start", process = "install")
  # Activate local renv environment if available
  if (file.exists("renv/activate.R")) {
    if (!requireNamespace("renv", quietly = TRUE)) {
      install.packages("renv", repos = "https://cloud.r-project.org")
    }
    source("renv/activate.R")
    pipeline_message(text = "Virtual environment activated", 
                     level = 1, progress = "end", process = "valid")
  } else {
    pipeline_message(text = "No renv environment found, using system libraries", 
                     level = 1, progress = "end", process = "info")
  }
}

# ------------------------------------------------------------------------------
# HPC → user library
# ------------------------------------------------------------------------------
if (RUN_CONTEXT == "hpc") {
  pipeline_message(text = "Running on HPC", 
                   level = 1, progress = "start", process = "install")
  # Path to R librairies on HPC
  user_lib <- "../R/x86_64-pc-linux-gnu-library/4.3"
  Sys.setenv(R_LIBS_USER = user_lib)
  .libPaths(c(user_lib, .libPaths()))
  # Add system libraries path for sf/units
  udunits_lib <- file.path(user_lib, "units/libs")
  Sys.setenv(LD_LIBRARY_PATH = paste(udunits_lib,
                                     Sys.getenv("LD_LIBRARY_PATH"),
                                     sep = ":"))
  pipeline_message(text = "LD_LIBRARY_PATH updated for units", 
                   level = 1, progress = "end", process = "install")
  # Disabling automatic opening of a graphics device
  grDevices::pdf(NULL)
  pipeline_message(text = "Automatic opening of a graphics device disabled!", 
                   process = "info")
}

# ------------------------------------------------------------------------------
# # Project root path
# ------------------------------------------------------------------------------
PROJECT_ROOT <- normalizePath(getwd())
assign("PROJECT_ROOT", PROJECT_ROOT, envir = .GlobalEnv)

pipeline_message(text = "Bootstrap completed", 
                 level = 0, progress = "end", process = "valid")