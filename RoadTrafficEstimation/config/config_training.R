# ==============================================================================
# Training learning model pipeline configuration
# ==============================================================================

# Training paths and filenames
TRAINING_DATA_DIR      <- file.path("data", "training")
TRAINING_RDS_DATA_DIR  <- file.path(TRAINING_DATA_DIR, "rds")
TRAINING_GPKG_DATA_DIR <- file.path(TRAINING_DATA_DIR, "gpkg")
# Training data
training_data_rds_filename      <- "05_training_dataset.rds"
training_data_gpkg_filename     <- "05_training_dataset.gpkg"
xgb_models_with_ratios_filename <- "06_xgboost_trained_models.rds"
xgboost_feature_info_filename   <- "06_xgboost_feature_info.rds"
                       
# Training configuration list                 
CONFIG_TRAINING <- list(
  
  # ----------------------------------------------------------------------------
  # Directories and files
  # ----------------------------------------------------------------------------
  TRAINING_DATA_DIR               = TRAINING_DATA_DIR, 
  TRAINING_RDS_DATA_DIR           = TRAINING_RDS_DATA_DIR, 
  TRAINING_RDS_DATA_FILEPATH      = file.path(TRAINING_RDS_DATA_DIR, 
                                              training_data_rds_filename), 
  TRAINING_GPKG_DATA_FILEPATH     = file.path(TRAINING_GPKG_DATA_DIR, 
                                              training_data_gpkg_filename), 

  XGB_MODELS_WITH_RATIOS_FILEPATH = file.path(TRAINING_DATA_DIR, 
                                              xgb_models_with_ratios_filename), 
  XGB_RATIO_FEATURE_INFO_FILEPATH = file.path(TRAINING_RDS_DATA_DIR, 
                                              xgboost_feature_info_filename), 
  
  # ----------------------------------------------------------------------------
  # Training configuration
  # ----------------------------------------------------------------------------
  
  # Training parameters
  TRAINING_PARAMS = list(
    max_depth        = 10,     # Deeper trees for complex patterns
    eta              = 0.05,   # Lower learning rate for finer optimization  
    subsample        = 0.9,    # More data per tree
    colsample_bytree = 0.9,    # More features per tree
    min_child_weight = 2,      # Better regularization
    gamma            = 0.1,    # Minimum loss reduction
    reg_alpha        = 0.01,   # L1 regularization
    reg_lambda       = 0.01,   # L2 regularization
    objective        = "reg:squarederror", 
    eval_metric      = "rmse"), 
  
  # Enhanced training parameters pour heavy vehicles
  TRUCK_PARAMS = list(
    max_depth        = 6,      # Deeper than before but controlled
    eta              = 0.1,    # Moderate learning rate
    subsample        = 0.95,   # Use most data (small sample)
    colsample_bytree = 0.9,    # Use most features
    min_child_weight = 1,      # Less strict regularization
    gamma            = 0.05,   # Small loss reduction threshold
    reg_alpha        = 0.005,  # Light L1 regularization
    reg_lambda       = 0.01,   # Light L2 regularization
    objective        = "reg:squarederror",
    eval_metric      = "rmse"), 

  # Number of boosting iterations
  NROUNDS = 1500,
  
  # Quality/robustness options (low overhead)
  # Split by count_point_id to avoid leakage between train/test
  USE_GROUPED_SENSOR_SPLIT = TRUE,
  # Use Avatar quality indicators as XGBoost sample weights
  USE_AVATAR_QUALITY_WEIGHTS = TRUE,
  # Lower bound for sample weights (avoid zero-weight rows)
  MIN_AVATAR_SAMPLE_WEIGHT = 0.20
)
