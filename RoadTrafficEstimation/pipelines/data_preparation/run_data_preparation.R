# ------------------------------------------------------------------------------
# Retrieving the configuration list
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Run pipeline steps
# ------------------------------------------------------------------------------
#source("pipelines/data_preparation/01_osm_download.R")
source("pipelines/data_preparation/02_osm_processing.R")
source("pipelines/data_preparation/03_avatar_download.R")
source("pipelines/data_preparation/04_data_integration.R")
source("pipelines/data_preparation/05_feature_engineering.R")