# ------------------------------------------------------------------------------
# Retrieving the configuration list
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Run pipeline steps
# ------------------------------------------------------------------------------
#source("R/pipelines/data_preparation/01_osm_download.R")
source("R/pipelines/data_preparation/02_osm_processing.R")
source("R/pipelines/data_preparation/03_osm_feature_engineering.R")
source("R/pipelines/data_preparation/04_avatar_download.R")
source("R/pipelines/data_preparation/05_avatar_aggregation.R")
# source("R/pipelines/data_preparation/05_feature_engineering.R")
source("R/pipelines/data_preparation/06_training_dataset_merge.R")