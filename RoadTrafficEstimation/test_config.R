source('bootstrap/bootstrap.R')
source('config/config_training.R')
cat('TRAINING_RDS_DATA_FILEPATH:', CONFIG_TRAINING$TRAINING_RDS_DATA_FILEPATH, '\n')
cat('File exists:', file.exists(CONFIG_TRAINING$TRAINING_RDS_DATA_FILEPATH), '\n')