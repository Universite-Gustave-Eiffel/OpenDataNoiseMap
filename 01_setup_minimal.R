# =============================================================================
# STAGE 1: SETUP - MINIMAL VERSION
# =============================================================================

# Required packages
packages <- c("dplyr", "tidyr", "sf", "httr", "jsonlite", "lubridate", 
              "randomForest", "data.table", "stringr","sfnetworks","tidygraph")


# Install and load
missing <- packages[!packages %in% installed.packages()[,"Package"]]
if (length(missing) > 0) install.packages(missing)
invisible(lapply(packages, library, character.only = TRUE))

# Create directories
dir.create("rdsFiles", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("data/avatar", showWarnings = FALSE, recursive = TRUE)

# Global config
CONFIG <<- list(
  START_TIME = "2023-01-01T00:00:00",
  END_TIME = "2023-12-31T00:00:00",
  BUFFER_DISTANCE = 50,
  TARGET_CRS = 2154,
  FORCE_AVATAR_DOWNLOAD = FALSE,
  VERBOSE = FALSE
)

