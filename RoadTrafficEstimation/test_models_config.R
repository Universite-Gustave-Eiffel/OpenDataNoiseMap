PROJECT_ROOT <- "/home/gguillaume/Documents/Article_NM_OSM/scripts/OpenDataNoiseMap/RoadTrafficEstimation"
setwd(PROJECT_ROOT)
source('bootstrap/bootstrap.R')
source('config/config_training.R')
cat('XGB_MODELS_WITH_RATIOS_FILEPATH:', CONFIG_TRAINING$XGB_MODELS_WITH_RATIOS_FILEPATH, '\n')
cat('XGB_RATIO_FEATURE_INFO_FILEPATH:', CONFIG_TRAINING$XGB_RATIO_FEATURE_INFO_FILEPATH, '\n')
cat('Models file exists:', file.exists(CONFIG_TRAINING$XGB_MODELS_WITH_RATIOS_FILEPATH), '\n')
cat('Features file exists:', file.exists(CONFIG_TRAINING$XGB_RATIO_FEATURE_INFO_FILEPATH), '\n')