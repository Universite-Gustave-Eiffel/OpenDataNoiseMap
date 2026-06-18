# ==============================================================================
# BOOTSTRAP — ENVIRONMENT ONLY
# ==============================================================================

# ------------------------------------------------------------------------------
# Load utility functions
# ------------------------------------------------------------------------------

# Ensure a writable temporary directory exists on HPC, to avoid R_TempDir creation failures.
project_tmp <- file.path(PROJECT_ROOT, "tmp")
dir.create(project_tmp, recursive = TRUE, showWarnings = FALSE)
use_tmp <- FALSE
if (dir.exists(project_tmp) && file.access(project_tmp, mode = 2) == 0) {
  use_tmp <- TRUE
} else if (nzchar(Sys.getenv(x = "TMPDIR"))) {
  existing_tmp <- Sys.getenv(x = "TMPDIR")
  if (dir.exists(existing_tmp) && file.access(existing_tmp, mode = 2) == 0) {
    use_tmp <- TRUE
    project_tmp <- existing_tmp
  }
}
if (use_tmp) {
  Sys.setenv(TMPDIR = project_tmp,
             TEMP   = project_tmp,
             TMP    = project_tmp)
}

utils_dir   <- file.path(PROJECT_ROOT, "R", "utils")
utils_files <- list.files(
  path       = utils_dir,
  pattern    = "\\.R$",
  full.names = TRUE)

for (uf in utils_files) {
  source(uf)
}

# ----------------------------------------------------------------------------
# Get computing environment information
# ------------------------------------------------------------------------------

pipeline_message("Bootstrapping environment", level = 0, 
                 progress = "start", process = "install")

pipeline_message("Obtaining the computing environment information", level = 1, 
                 progress = "start", process = "install")

# Execution context (local / slurm)
RUN_CONTEXT <- Sys.getenv(x = "RUN_CONTEXT", unset = "local")
assign(x = "RUN_CONTEXT", value = RUN_CONTEXT, envir = .GlobalEnv)

pipeline_message(sprintf("Avatar token: %s", 
                         ifelse(test = Sys.getenv(x = "AVATAR_API_TOKEN") != "", 
                                yes  = "DETECTED", 
                                no   = "MISSING")), 
                 process = "info")

pipeline_message(paste("Running environment: ", RUN_CONTEXT), level = 1, 
                 progress = "end", process = "install")

# ------------------------------------------------------------------------------
# Detect library environment
# ------------------------------------------------------------------------------

user_lib <- Sys.getenv(x = "R_LIBS_USER")
is_hpc_lib <- user_lib != ""

pipeline_message(sprintf("R_LIBS_USER detected: %s", user_lib), process = "info")

# ------------------------------------------------------------------------------
# LOCAL mode → renv
# ------------------------------------------------------------------------------

if (RUN_CONTEXT == "local" && !is_hpc_lib) {
  
  pipeline_message("Running locally with renv", level = 1, 
                   progress = "start", process = "install")
  
  if (!requireNamespace(package = "renv", quietly = TRUE)) {
    install.packages(pkgs = "renv", repos = "http://cran.rstudio.com/")
  }
  
  if (file.exists("renv/activate.R")) {
    source("renv/activate.R")
    pipeline_message("Virtual environment activated", level = 1, 
                     progress = "end", process = "valid")
  } else {
    pipeline_message("Non-existent virtual environment", process = "stop")
  }
}

# ------------------------------------------------------------------------------
# HPC mode (login node OR SLURM) → user library
# ------------------------------------------------------------------------------

if (is_hpc_lib) {
  
  pipeline_message("Using HPC user library", level = 1, 
                   progress = "start", process = "install")
  
  .libPaths(c(user_lib, .libPaths()))
  
  pipeline_message(sprintf("Active R library: %s", user_lib), level = 1, 
                   progress = "end", process = "install")
  
  # Disable graphics device
  grDevices::pdf(NULL)
  
  pipeline_message("Automatic graphics device disabled", process = "info")
}

# ------------------------------------------------------------------------------
# Token for Avatar data download
# ------------------------------------------------------------------------------

AVATAR_API_TOKEN <- Sys.getenv(x = "AVATAR_API_TOKEN")
assign(x = "AVATAR_API_TOKEN", value = AVATAR_API_TOKEN, envir = .GlobalEnv)
if (AVATAR_API_TOKEN == "") {
  pipeline_message("Avatar API token not found. Avatar download mode will fail.", 
                   process = "warning")
} else {
  pipeline_message("Avatar API token detected", process = "info")
}

pipeline_message("Bootstrap completed", level = 0, 
                 progress = "end", process = "valid")
