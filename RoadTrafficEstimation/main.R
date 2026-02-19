# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# R version
# ------------------------------------------------------------------------------

message("💻 ", R.version.string)

get_script_path <- function() {
  # Rscript / bash mode
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(pattern = file_arg, replacement = "", x = args[grep(file_arg, args)])
  if (length(script_path) > 0) {
    return(normalizePath(path = script_path))
  }
  # VS Code / source() mode
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(path = sys.frames()[[1]]$ofile))
  }
  # Interactive fallback
  return(normalizePath(path = file.path(getwd(), "main.R")))
}

# ------------------------------------------------------------------------------
# Force working directory = script directory
# ------------------------------------------------------------------------------
script_path  <- get_script_path()
PROJECT_ROOT <- dirname(path = script_path)

setwd(PROJECT_ROOT)

message("💻 Current working directory: ", getwd())

# ------------------------------------------------------------------------------
# Run successive steps
# ------------------------------------------------------------------------------

source("bootstrap.R")
source("config_pipeline.R")
source("run_pipeline.R")