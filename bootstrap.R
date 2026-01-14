# ==============================================================================
# BOOTSTRAP — ENVIRONMENT ONLY
# ==============================================================================

message("\n === Bootstrapping environment === \n")

message("\t 💻 Get running environment information \n")
run_context <- Sys.getenv(x = "RUN_CONTEXT",
                          unset = "local")
message("\t\t 💫  Execution context: ", run_context, "\n")

# ------------------------------------------------------------------------------
# LOCAL → renv
# ------------------------------------------------------------------------------
if (run_context == "local") {
  message("\t 💻 Running locally \n")
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  # Activate local renv environment
  if (file.exists("renv/activate.R")) {
    source("renv/activate.R")
    message("\t\t ✓ renv activated (local) \n")
  } else {
    message("\t\t ⛔ renv folder missing, skipping activation \n")
  }
}

# ------------------------------------------------------------------------------
# HPC → user library
# ------------------------------------------------------------------------------
if (run_context == "hpc") {
  message("\t 💻 Running on HPC \n")
  # Path to R librairies on HPC
  user_lib <- "../R/x86_64-pc-linux-gnu-library/4.3"
  Sys.setenv(R_LIBS_USER = user_lib)
  .libPaths(c(user_lib, .libPaths()))
  message("\t\t 📚 HPC user library:", user_lib, "\n")
  # Add system libraries path for sf/units
  udunits_lib <- file.path(user_lib, "units/libs")
  Sys.setenv(LD_LIBRARY_PATH = paste(udunits_lib,
                                     Sys.getenv("LD_LIBRARY_PATH"),
                                     sep = ":"))
  message("\t\t ✓ LD_LIBRARY_PATH updated for units \n")
}

message("\t ✓ Bootstrap completed! \n")