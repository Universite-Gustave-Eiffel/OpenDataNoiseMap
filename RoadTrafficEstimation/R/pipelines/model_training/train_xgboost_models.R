# ==============================================================================
# STAGE 6: XGBOOST TRAINING LEARNING MODEL
# ==============================================================================
# This stage trains the learning model to estimate traffic flow, truck 
# percentage, and speed ratio to OSM maxspeed for each time period. The model 
# architecture is designed to leverage the stability of truck percentage and 
# speed ratios, and to capture temporal patterns through ratio models. The 
# training process includes careful data filtering, feature engineering with 
# interaction terms, and adaptive strategies for small samples. The resulting 
# models will be used in the prediction phase to estimate traffic variables 
# across all periods based on OSM features and the base day model predictions.
#
# Inputs:
#  - TRAINING_RDS_DATA_FILEPATH: RDS file containing the training dataset with 
#    OSM features and target variables
# Outputs:
#  - Trained XGBoost models for each target variable and period
#  - Summary of model performance and feature importance
# Architecture:
#   1. Base models for period D (day): 
#      - flow_D: Total traffic (vehicles/hour)
#      - truck_pct_D: Truck percentage (HGV/TV as percentage 0-100%)
#      - speed_D: Speed ratio to OSM maxspeed (aggregate_speed/speed)
#   2. Ratio models for other periods (E, N, h0-h23):
#      - ratio_flow_P: flow_P/flow_D
#      - ratio_truck_pct_P: truck_pct_P/truck_pct_D (ratio of percentages)
#      - ratio_speed_P: speed_P/speed_D
#   3. Final predictions:
#      - flow_P = flow_D x ratio_flow_P
#      - truck_pct_P = truck_pct_D x ratio_truck_pct_P
#      - HGV_P = flow_P x (truck_pct_P/100)
#      - LV_P = flow_P - HGV_P
#      - speed_D_abs = osm_speed x speed_D
#      - speed_P = speed_D_abs x ratio_speed_P
# Benefits:
#   - Truck percentage more stable than absolute truck count
#   - Ratios capture temporal patterns (more trucks at night on highways)
#   - Prevents HGV > TV inconsistencies
#   - Better performance for periods with sparse data
# ==============================================================================
# ==============================================================================

pipeline_message("Training the learning model", level = 0, 
                 progress = "start", process = "learn")



# ------------------------------------------------------------------------------
# Load training data for the learning model
# ------------------------------------------------------------------------------

pipeline_message(sprintf("Loading training data for the learning model from %s", 
                         rel_path(CFG$TRAINING_RDS_DATA_FILEPATH)), 
                 level = 1, progress = "start", process = "load")

if (!exists(x= 'training_data', inherits = FALSE) && 
    !file.exists(CFG$TRAINING_RDS_DATA_FILEPATH)){
  pipeline_message(
    sprintf(paste("File %s doesn't exists.", 
                  "Training data is required to train the model.", 
                  "Run the data preparation scripts to generate the dataset."), 
            CFG$TRAINING_RDS_DATA_FILEPATH), 
    process = "stop")
}

training_data <- readRDS(CFG$TRAINING_RDS_DATA_FILEPATH)

if (!"ratio_speed_to_osm" %in% names(x = training_data)) {
  if (all(c("aggregate_speed", "speed") %in% names(x = training_data))) {
    pipeline_message(paste("ratio_speed_to_osm missing in training dataset:", 
                           "computing fallback from aggregate_speed/speed"),
      process = "warning")
    training_data$ratio_speed_to_osm <- ifelse(
      test = !is.na(x = training_data$aggregate_speed) & 
             training_data$aggregate_speed >= 0 & 
             !is.na(x = training_data$speed) & 
             training_data$speed > 0, 
      yes  = training_data$aggregate_speed / training_data$speed,
      no   = NA_real_)
  } else {
    pipeline_message(
      paste("Missing required target ratio_speed_to_osm and cannot compute", 
            "fallback (aggregate_speed/speed unavailable).", 
            "Please ensure the training dataset contains the column", 
            "ratio_speed_to_osm or the necessary columns to compute it"), 
      process = "stop")
  }
}

pipeline_message(describe_df(training_data), process = "info")

pipeline_message("Training data successfully loaded", level = 1, 
                 progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Global configuration of features and periods
# ------------------------------------------------------------------------------

pipeline_message("Configuring training model", level = 1, 
                 progress = "start", process = "configure")

# ****************************** #
# OSM variables used as features #
# ****************************** #

# Candidate OSM/network features (use those actually present in training data)
candidate_road_features <- c("highway", "DEGRE", "ref_letter", "first_word", 
                             "oneway_osm", "lanes_osm", "lanes_directional",
                             "speed", "junction_osm", "lane_number",
                             "connectivity", "betweenness", 
                             "closeness", "pagerank",
                             "coreness", "dead_end_score", "edge_length_m")

available_road_features <- intersect(x = candidate_road_features, 
                                     y =  names(x = training_data))
missing_road_features   <- setdiff(x = candidate_road_features, 
                                   y = available_road_features)

if (length(x = missing_road_features) > 0) {
  pipeline_message(
    sprintf("Some candidate features are missing and will be skipped: %s",
            paste(missing_road_features, collapse = ", ")),
    process = "warning")
}

if (length(x = available_road_features) == 0) {
  pipeline_message(
    paste("No candidate road/network features are available in the training", 
          "dataset. The model will not be able to learn meaningful patterns", 
          "and predictions will be unreliable. Please ensure the training", 
          "dataset contains relevant OSM/network features for the model to", 
          "train on."), 
  process = "stop")}

pipeline_message(
  sprintf("Using %d road/network features: %s",
          length(x = available_road_features),
          paste(available_road_features, collapse = ", ")),
  process = "info")

# Build formula with existing features and interaction terms.  Interactions 
# capture highway-specific traffic patterns by density zone, lane count, and 
# network connectivity
interaction_terms <- character(0)
if (all(c("highway", "DEGRE") %in% available_road_features)) {
  interaction_terms <- c(interaction_terms, "highway:DEGRE")
}
if (all(c("highway", "lanes_osm") %in% available_road_features)) {
  interaction_terms <- c(interaction_terms, "highway:lanes_osm")
}
if (all(c("DEGRE", "connectivity") %in% available_road_features)) {
  interaction_terms <- c(interaction_terms, "DEGRE:connectivity")
}
if (all(c("junction_osm", "highway") %in% available_road_features)) {
  interaction_terms <- c(interaction_terms, "junction_osm:highway")
}

formula_parts <- c(available_road_features, interaction_terms)
road_feature_formula <- as.formula(
  object = paste("~", paste(formula_parts, collapse = " + ")))

if (length(x = interaction_terms) > 0) {
  pipeline_message(sprintf("Added %d interaction terms: %s", 
                           length(x = interaction_terms), 
                           paste(interaction_terms, collapse = ", ")), 
                   process = "info")
}

# Temporal periods configuration
all_periods <- c("D", "E", "N", 
                 paste0("h", 0:23),
                 paste0("h", 0:23, "_wd"), 
                 paste0("h", 0:23, "_we"))

# ***************************** #
# Training model configurations #
# ***************************** #

# Base models (period D only)
base_configs <- list(
    flow_D      = list(
      name      = "Traffic Flow (Day)",
      period    = "D",
      target    = "aggregate_flow",
      baseline  = "flow_D",
      transform = "log10",  # log transform for base models
      min_valid = 1),
    truck_pct_D = list(
      name      = "Truck Percentage (Day)",
      period    = "D",
      target    = "truck_pct",
      baseline  = "truck_pct_D",
      transform = NULL,     # Already percentage 0-100
      min_valid = 0),
    speed_D     = list(
      name      = "Speed Ratio to OSM (Day)",
      period    = "D",
      target    = "ratio_speed_to_osm",
      baseline  = "speed_D",
      transform = NULL,
      min_valid = 0.05))

# Ratio models (all periods except D)
ratio_configs <- list()
ratio_periods <- setdiff(x = all_periods, y = "D")

for (p in ratio_periods) {
  ratio_configs[[paste0("ratio_flow_", p)]] <- list(
    name      = paste0("Flow Ratio (", p, "/D)"),
    period    = p,
    target    = "ratio_flow",  
    transform = NULL,  # No log transform for ratios
    min_valid = 0.01)
  
  ratio_configs[[paste0("ratio_truck_pct_", p)]] <- list(
    name      = paste0("Truck % Ratio (", p, "/D)"),
    period    = p,
    target    = "ratio_truck_pct",
    transform = NULL,   # No log transform for ratios
    min_valid = 0.001)  # More permissive for truck ratios
  
  ratio_configs[[paste0("ratio_speed_", p)]] <- list(
    name      = paste0("Speed Ratio (", p, "/D)"),
    period    = p,
    target    = "ratio_speed",
    transform = NULL,  # No log transform for ratios
    min_valid = 0.01)
}

# Merge all configurations
all_configs <- c(base_configs, ratio_configs)

pipeline_message(sprintf("Number of variables to be estimated: %d", 
                         length(x = all_configs)), 
                 process = "info")

pipeline_message("Training model successfully configured", level = 1, 
                 progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Train models
# ------------------------------------------------------------------------------

models_list           <- list()
results_summary       <- data.frame()
base_test_predictions <- list()

# Pre-compute a shared sensor split for base models (flow_D, truck_pct_D, 
# speed_D). This ensures all 3 base models use the same test sensors, so the 
# emission dB analysis can merge their predictions without losing rows to split 
# mismatch.
shared_base_test_sensors <- NULL
if (isTRUE(CFG$USE_GROUPED_SENSOR_SPLIT)) {
  d_data_for_split <- training_data %>% filter(period == "D")
  all_d_sensors    <- unique(x = d_data_for_split$count_point_id)
  if (length(x = all_d_sensors) >= 5) {
    set.seed(42)  # Different seed from per-model splits to avoid correlation
    n_train                   <- max(1, floor(x = 0.8 * length(x = all_d_sensors)))
    shared_base_train_sensors <- sample(x    = all_d_sensors, 
                                        size = n_train)
    shared_base_test_sensors  <- setdiff(x = all_d_sensors, 
                                         y = shared_base_train_sensors)
    pipeline_message(
      sprintf("Shared base-model sensor split: %d train/%d test sensors",
              length(x = shared_base_train_sensors), 
              length(x = shared_base_test_sensors)),
      process = "info")
  }
}

# ----------------------------------------------------------------------------
# CNOSSOS-EU emission via NoiseModelling Java bridge
# ----------------------------------------------------------------------------
# Uses the real CNOSSOS-EU 2020 implementation from NoiseModelling 5.0.2 to 
# compute A-weighting acoustic emission power for all octave bands (63-8000 Hz)

for (model_name in names(x = all_configs)) {
  
  # Current configuration
  model_config <- all_configs[[model_name]]
  
  pipeline_message(
    sprintf("Training step [%d/%d] - Estimation of the variable %s", 
            which(names(x = all_configs) == model_name), 
            length(x = all_configs), model_name), 
    level = 1, progress = "start", process = "wait")
  
  pipeline_message("Time period selection and target filtering", level = 2, 
                   progress = "start", process = "configure")
  
  # Filter data for this period
  training_data_over_period <- training_data %>% 
    filter(period == model_config$period)
  
  # Extract target variable
  training_data_target <- training_data_over_period[[model_config$target]]
  
  # Filter valid target data
  valid_idx <- !is.na(x = training_data_target) & 
               training_data_target >= model_config$min_valid
  
  # More permissive threshold for truck models (less data available than for 
  # light vehicles)
  min_obs_threshold <- ifelse(test = grepl(pattern = "truck", 
                                           x       = model_config$target), 
                              yes  = 20, 
                              no  = 50)
  # Verification of the number of observations
  if (sum(valid_idx) < min_obs_threshold) {
    pipeline_message(
      sprintf("Too few valid observations for trucks (< %d). Skipping!", 
              min_obs_threshold), 
      process = "warning")
    next
  }
  
  pipeline_message(sprintf("Number of valid observations: %d", sum(valid_idx)), 
                   process = "info")
  
  # Filter data
  clean_training_data_over_period <- training_data_over_period[valid_idx, ]
  clean_training_data_target      <- training_data_target[valid_idx]
  quality_col                     <- get_quality_indicator_column(
    target_name    = model_config$target,
    available_cols = names(x = clean_training_data_over_period))
    
  quality_indicator <- if (!is.na(x = quality_col)) {
    as.numeric(x = clean_training_data_over_period[[quality_col]])
  } else {
    rep(x = NA_real_, times = nrow(x = clean_training_data_over_period))
  }
  
  pipeline_message("Time period selected and target data filtered", level = 2, 
                   progress = "end", process = "valid")
  
  pipeline_message("Construction of the sparse feature matrix", level = 2, 
                   progress = "start", process = "calc")
  
  # Create sparse feature matrix (may eliminate more rows due to NA in features)
  sparse_data_matrix <- safe_sparse_model_matrix(
    formula_obj = road_feature_formula,
    data_df     = clean_training_data_over_period)

  pipeline_message(
    sprintf("Constructed sparse feature matrix with %d rows and %d features", 
            nrow(x = sparse_data_matrix), ncol(x = sparse_data_matrix)), 
    process = "info")

  pipeline_message("Sparse feature matrix constructed successfully", level = 2, 
                   progress = "end", process = "valid")
  
  # Check if the sparse data matrix eliminated additional rows
  if (nrow(x = sparse_data_matrix) 
      != nrow(x = clean_training_data_over_period)) {
    # Find which rows were kept by sparse.model.matrix
    kept_rows <- as.integer(x = rownames(x = sparse_data_matrix))
    # Align y and data with sparse_data_matrix
    clean_training_data_target <- 
      clean_training_data_target[kept_rows]
    clean_training_data_over_period <- 
      clean_training_data_over_period[kept_rows, ]
    quality_indicator <- quality_indicator[kept_rows]
  }
  
  pipeline_message(
    "Target transformation and split of the training and testing data", 
    level = 2, progress = "start", process = "calc")
  
  # Apply log transform to aligned target
  if (!is.null(x = model_config$transform) && 
      model_config$transform == "log10") {
    transformed_training_data_target <- log10(
      x = pmax(clean_training_data_target, model_config$min_valid))
  } else {
    transformed_training_data_target <- clean_training_data_target
  }
  
  # Verify dimensions are now synchronized
  pipeline_message(
    sprintf("Final data: %d rows (Sparse matrix: %d x %d | Target data: %d)", 
            nrow(x = clean_training_data_over_period), 
            nrow(x = sparse_data_matrix), 
            ncol(x = sparse_data_matrix), 
            length(x = transformed_training_data_target)), 
    process = "info")
  
  if (nrow(x = sparse_data_matrix) 
      != length(x = transformed_training_data_target)) {
    next
  }
  
  # Train/test split on synchronized data
  set.seed(123)
  n_final           <- nrow(x = sparse_data_matrix)
  use_grouped_split <- isTRUE(CFG$USE_GROUPED_SENSOR_SPLIT)
  is_base_model     <- model_name %in% c("flow_D", "truck_pct_D", "speed_D")
  if (use_grouped_split && "count_point_id" %in% names(x = clean_training_data_over_period)) {
    sensor_ids     <- clean_training_data_over_period$count_point_id
    unique_sensors <- unique(x = sensor_ids)
    if (is_base_model && !is.null(x = shared_base_test_sensors)) {
      # Use the shared split for base models so emission dB analysis
      # can merge test predictions from all 3 models without row loss
      train_idx <- which(!sensor_ids %in% shared_base_test_sensors)
      test_idx  <- which(sensor_ids %in% shared_base_test_sensors)
      if (length(x = train_idx) == 0 || length(x = test_idx) == 0) {
        train_idx <- sample(x    = seq_len(n_final),
                            size = floor(x = 0.8 * n_final))
        test_idx  <- setdiff(x = seq_len(n_final), 
                             y = train_idx)
      }
    } else if (length(x = unique_sensors) >= 5) {
      n_train_sensors <- max(1, floor(x = 0.8 * length(x = unique_sensors)))
      train_sensors   <- sample(unique_sensors, size = n_train_sensors)
      train_idx       <- which(sensor_ids %in% train_sensors)
      test_idx        <- which(!sensor_ids %in% train_sensors)
      if (length(x = train_idx) == 0 || length(x = test_idx) == 0) {
        train_idx <- sample(x    = seq_len(n_final), 
                            size = floor(x = 0.8 * n_final))
        test_idx  <- setdiff(x = seq_len(n_final), 
                             y = train_idx)
      }
    } else {
      train_idx <- sample(x    = seq_len(n_final), 
                          size = floor(x = 0.8 * n_final))
      test_idx  <- setdiff(x = seq_len(n_final), 
                           y = train_idx)
    }
  } else {
    train_idx <- sample(x    = seq_len(n_final), 
                        size = floor(x = 0.8 * n_final))
    test_idx  <- setdiff(x = seq_len(n_final), 
                         y = train_idx)
  }
  
  X_train       <- sparse_data_matrix[train_idx, ]
  X_test        <- sparse_data_matrix[test_idx, ]
  y_train       <- transformed_training_data_target[train_idx]
  y_test        <- transformed_training_data_target[test_idx]
  quality_train <- quality_indicator[train_idx]
  quality_test  <- quality_indicator[test_idx]
  test_meta     <- clean_training_data_over_period[
                      test_idx, 
                      c("osm_id", "count_point_id", "period", 
                        "highway", "speed")]
  
  # Check for invalid values in labels
  invalid_train <- is.na(x = y_train) | is.infinite(x = y_train) | is.nan(x = y_train)
  invalid_test  <- is.na(x = y_test) | is.infinite(x = y_test) | is.nan(x = y_test)
  highway_test  <- as.character(x = clean_training_data_over_period$highway[test_idx])
  
  if (any(invalid_train) || any(invalid_test)) {
    pipeline_message(
      sprintf("Found %d invalid train labels and %d invalid test labels", 
              sum(invalid_train), sum(invalid_test)), 
      process = "warning")
    
    # Remove invalid observations
    if (any(invalid_train)) {
      valid_train_idx <- !invalid_train
      X_train         <- X_train[valid_train_idx, ]
      y_train         <- y_train[valid_train_idx]
      quality_train   <- quality_train[valid_train_idx]
    }
    if (any(invalid_test)) {
      valid_test_idx <- !invalid_test
      X_test         <- X_test[valid_test_idx, ]
      y_test         <- y_test[valid_test_idx]
      quality_test   <- quality_test[valid_test_idx]
      highway_test   <- highway_test[valid_test_idx]
      test_meta      <- test_meta[valid_test_idx, , drop = FALSE]
    }
  }
  
  # Final check
  if (length(x = y_train) < 10 || length(x = y_test) < 5) {
    next
  }
  
  n_train <- length(x = y_train)
  n_test  <- length(x = y_test)
  n_total <- n_train + n_test
  
  pipeline_message(sprintf("Training: %s observations (%.1f%%)", fmt(n_train), 
                           100 * n_train / n_total), 
                   process = "info")
  pipeline_message(sprintf("Test: %s observations (%.1f%%)", fmt(n_test), 
                           100 * n_test / n_total), 
                   process = "info")
  pipeline_message(sprintf("Range of learning values: [%.3f, %.3f]", 
                           min(y_train, na.rm=TRUE), max(y_train, na.rm=TRUE)), 
                   process = "info")
  
  pipeline_message("Target transformed and training/testing data splitted", 
                   level = 2, progress = "end", process = "valid")
  
  # Train model
  pipeline_message("Training of the learning model", level = 2, 
                   progress = "start", process = "learn")
  
  # Adaptive training strategy for small samples
  use_watchlist <- TRUE
  if (length(x = y_train) < 100 || length(x = y_test) < 30) {
    pipeline_message(sprintf("Small sample detected:\n\t\t", 
                             "-> train = %d\n\t\t", 
                             "-> test = %d\n\t\t", 
                             "=> Disabling early stopping and watchlist", 
                             length(x = y_train), length(x = y_test)), 
                     process = "warning")
    
    use_watchlist <- FALSE
  }
  
  use_quality_weights <- isTRUE(x = CFG$USE_AVATAR_QUALITY_WEIGHTS)
  min_weight <- if (!is.null(x = CFG$MIN_AVATAR_SAMPLE_WEIGHT)) {
    CFG$MIN_AVATAR_SAMPLE_WEIGHT
  } else {
    0.20
  }
  if (use_quality_weights && any(!is.na(x = quality_train))) {
    weight_train <- 1 - pmin(pmax(quality_train, 0), 100) / 100
    weight_train[is.na(x = weight_train)] <- 1
    weight_train <- pmax(weight_train, min_weight)
    dtrain       <- xgboost::xgb.DMatrix(data   = X_train, 
                                         label  = y_train, 
                                         weight = weight_train)
    dtest        <- xgboost::xgb.DMatrix(data  = X_test, 
                                         label = y_test)
    pipeline_message(
      sprintf("Using Avatar quality weights: min=%.2f, mean=%.2f, max=%.2f", 
              min(weight_train), mean(x = weight_train), max(weight_train)),
      process = "info")
  } else {
    dtrain <- xgboost::xgb.DMatrix(data = X_train, 
                                   label = y_train)
    dtest  <- xgboost::xgb.DMatrix(data = X_test, 
                                   label = y_test)
  }
  
  # Choose parameters and training strategy based on model type
  start_timer()
  if (grepl(pattern = "truck", x = model_config$target)) {
    params <- CFG$TRUCK_PARAMS
    # For truck models with small samples, use CV for robust estimation
    if (length(x = y_train) < 200) {
      nfold <- min(5, length(x = y_train) %/% 10)
      if (nfold >= 2) {
        cv_result <- xgboost::xgb.cv(
          params                = params,
          data                  = dtrain,
          nrounds               = CFG$NROUNDS,
          nfold                 = nfold,  # Adaptive CV folds
          early_stopping_rounds = 30,
          verbose               = 0,
          showsd                = FALSE)
        best_rounds <- cv_result$best_iteration
        pipeline_message(sprintf("CV selected %d rounds (from max %d)", 
                                 best_rounds, CFG$NROUNDS), 
                         process = "clip")
      } else {
        best_rounds <- CFG$NROUNDS
      }
    } else {
      best_rounds <- CFG$NROUNDS
    }
  } else {
    params <- CFG$TRAINING_PARAMS
    # Cross-validation to select optimal nrounds (avoids overfitting)
    nfold <- min(5, max(2, length(x = y_train) %/% 50))
    if (nfold >= 2 && length(x = y_train) >= 100) {
      cv_result <- xgboost::xgb.cv(
        params                = params,
        data                  = dtrain,
        nrounds               = CFG$NROUNDS,
        nfold                 = nfold,
        early_stopping_rounds = 50,
        verbose               = 0,
        showsd                = FALSE)
      best_rounds <- cv_result$best_iteration
      pipeline_message(sprintf("CV(%d-fold) selected %d rounds (from max %d)",
                               nfold, best_rounds, CFG$NROUNDS), 
                       process = "info")
    } else {
      best_rounds <- CFG$NROUNDS
    }
  }
  
  # Acceptable limits for training
  min_train_xgb <- 150
  min_test_xgb  <- 50
  if (model_config$period == "D" && 
      model_config$target == "ratio_speed_to_osm") {
    min_train_xgb <- 10
    min_test_xgb  <- 5
  }
  if (grepl(pattern = "truck", x = model_config$target)) {
    min_train_xgb <- 10
    min_test_xgb  <- 5
  }
  if (length(x = y_train) < min_train_xgb || 
      length(x = y_test) < min_test_xgb) {
    pipeline_message(sprintf("Sample too small for XGBoost:\n\t\t", 
                             "-> train = %d\n\t\t", 
                             "-> test = %d\n\t\t", 
                             "=> Model skipped!", 
                             length(x = y_train), length(x = y_test)), 
      process = "warning")
    next
  }
  
  # xgb.DMatrix datasets to use for evaluating model performance
  eval_list <- if (use_watchlist) {
    list(train = dtrain, 
         test  = dtest)
  } else {
    NULL
  }

  # Safety check on best_rounds
  if (is.null(x  = best_rounds) || 
      is.na(x = best_rounds) || 
      best_rounds < 1) {
    pipeline_message(sprintf("Invalid best_rounds (%s). 
                             Falling back to CFG$NROUNDS = %d", 
                             best_rounds, CFG$NROUNDS), 
                     process = "warning")
    best_rounds <- CFG$NROUNDS
  }
  # Training
  xgb_model <- xgboost::xgb.train(
    params                = params,
    data                  = dtrain,
    nrounds               = best_rounds,
    evals                 = eval_list,
    early_stopping_rounds = if (use_watchlist){50} else {NULL},
    maximize              = FALSE,
    verbose               = 0)
  elapsed <- stop_timer()
  
  pipeline_message("Learning model trained successfully", level = 2, 
                   progress = "end", process = "valid")
  
  pipeline_message("Assessment and diagnostics of the learning model", 
                   level = 2, progress = "start", process = "search")
  
  # Evaluate
  pred_test <- xgboost::predict(object  = xgb_model, 
                                newdata = X_test)
  
  # Back-transform if needed
  if (!is.null(x = model_config$transform) && 
      model_config$transform == "log10") {
    pred_original   <- 10^pred_test
    actual_original <- 10^y_test
  } else {
    pred_original   <- pred_test
    actual_original <- y_test
  }

  if (model_name == "speed_D" && 
      model_config$target == "ratio_speed_to_osm") {
    speed_osm_test <- suppressWarnings(expr = as.numeric(x = test_meta$speed))
    speed_osm_test[is.na(x = speed_osm_test) | 
                   speed_osm_test <= 0] <- CFG$DEFAULT_VEHICLE_SPEED
    pred_eval      <- pmax(0, pred_original * speed_osm_test)
    actual_eval    <- pmax(0, actual_original * speed_osm_test)
  } else {
    pred_eval   <- pred_original
    actual_eval <- actual_original
  }
  
  # Metrics (true R² = 1 - SSres/SStot)
  model_metrics <- compute_model_metrics(
    y_true = actual_eval,
    y_pred = pred_eval)
  mae <- model_metrics$mae
  rmse <- model_metrics$rmse
  r2 <- model_metrics$r2
  mape <- model_metrics$mape

  # Additional robust summary used for comparability with previous runs
  mape_values <- abs(x = (pred_eval - actual_eval) / 
                         pmax(actual_eval, 0.01)) * 100
  medape      <- median(x     = mape_values[is.finite(x = mape_values)], 
                        na.rm = TRUE)
  
  pipeline_message(sprintf("R²=%.3f | MAPE=%.1f%% | MedAPE=%.1f%%", 
                           r2, mape, medape), 
                   process = "info")

  # Per-highway diagnostics on test fold (lightweight)
  highway_metrics <- data.table(
    highway = highway_test,
    actual  = actual_eval,
    pred    = pred_eval)
  highway_metrics <- highway_metrics[!is.na(highway)]
  if (nrow(highway_metrics) > 0) {
    highway_metrics_summary <- highway_metrics[, {
      m <- compute_model_metrics(y_true = actual, 
                                 y_pred = pred)
      list(n    = .N, 
           r2   = m$r2, 
           mae  = m$mae, 
           rmse = m$rmse, 
           mape = m$mape)}, 
      by = highway][order(-n)]
    top_hw_pool <- highway_metrics_summary[n >= 30]
    top_hw      <- top_hw_pool[1:min(nrow(top_hw_pool), 5)]
    if (nrow(top_hw) > 0) {
      pipeline_message(
        paste0("Top highway diagnostics: ",
               paste(sprintf("%s(n=%d,R²=%.3f,MAPE=%.1f%%)", 
                             top_hw$highway, top_hw$n, 
                             top_hw$r2, top_hw$mape), 
                     collapse = " | ")),
        process = "info")
    }
  } else {
    highway_metrics_summary <- data.table()
  }
  
  # Feature importance analysis
  importance   <- xgboost::xgb.importance(model = xgb_model)
  top_features <- head(x = importance, 5)  # Top 5 most important features
  
  pipeline_message(paste("Top 5 most important features:", 
                         paste0(sprintf("\t\t\t%d. %-15s (%.1f%%)", 
                                        seq_len(nrow(top_features)), 
                                        top_features$Feature, 
                                        top_features$Gain * 100), 
                                collapse = "\n"), 
                         sep = "\n"), 
                   process = "info")
  
  pipeline_message("Learning model evaluated and diagnosed", level = 2, 
                   progress = "end", process = "valid")
  
  pipeline_message("Storage of the model and statistical evaluation indicators", 
                   level = 2, progress = "start", process = "save")
  
  # Store model
  models_list[[model_name]] <- list(
    model              = xgb_model,
    config             = model_config,
    metrics            = list(mae = mae, rmse = rmse, r2 = r2, mape = mape, medape = medape),
    feature_names      = colnames(x = X_train),
    feature_importance = importance,    # Full importance table
    top_features       = top_features,  # Top 5 for quick reference
    highway_metrics    = as.data.frame(highway_metrics_summary),
    n_train            = length(x = y_train),
    n_test             = length(x = y_test),
    training_time      = elapsed)

  # Keep base-model test predictions for acoustic emission diagnostics
  if (model_name %in% c("flow_D", "truck_pct_D", "speed_D")) {
    base_test_predictions[[model_name]] <- data.frame(
      osm_id           = test_meta$osm_id,
      count_point_id   = test_meta$count_point_id,
      period           = as.character(x = test_meta$period),
      highway          = as.character(x = test_meta$highway),
      pred             = as.numeric(x = pred_eval),
      actual           = as.numeric(x = actual_eval),
      stringsAsFactors = FALSE)
  }
  
  # Add to summary
  results_summary <- rbind(
    results_summary, 
    data.frame(
      Model    = model_name, 
      Target   = model_config$name, 
      N_train  = length(x = y_train), 
      N_test   = length(x = y_test), 
      R2       = round(x = r2, digits = 3), 
      MAE      = round(x = mae, digits = 2), 
      RMSE     = round(x = rmse, digits = 2), 
      MAPE     = round(x = mape, digits = 1), 
      Time_min = round(x = elapsed, digits = 2)))
  
  pipeline_message(
    "Model and statistical evaluation indicators successfully stored", 
    level = 2, progress = "end", process = "valid")
  
  pipeline_message(sprintf("Training of the learning model for the estimation ", 
                           "of the variable %s completed", model_name), 
                   level = 1, progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# Save models
# ------------------------------------------------------------------------------

pipeline_message(sprintf("Save training models and features in files %s and %s ", 
                         "respectively", 
                         rel_path(CFG$XGB_MODELS_WITH_RATIOS_FILEPATH), 
                         rel_path(CFG$XGB_RATIO_FEATURE_INFO_FILEPATH)), 
                 level = 1, progress = "start", process = "save")

# Save list of models and road feature formula
saveRDS(object = models_list, 
        file   = CFG$XGB_MODELS_WITH_RATIOS_FILEPATH)

# Extract feature names from first available model to ensure consistent alignment
# All models use the same feature matrix, so any model's feature_names should work
# Priority: flow_D > truck_pct_D > speed_D > any other model
feature_names_from_training <- NULL

# Try priority base models first
for (base_model in c("flow_D", "truck_pct_D", "speed_D")) {
  if (!is.null(x = models_list[[base_model]]) && 
      !is.null(x = models_list[[base_model]]$feature_names)) {
    feature_names_from_training <- models_list[[base_model]]$feature_names
    pipeline_message(
      sprintf("Feature names extracted from model: %s", base_model),
      process = "info")
    break
  }
}

# Fallback: use any available model's feature_names
if (is.null(x = feature_names_from_training)) {
  for (model_key in names(x = models_list)) {
    if (!is.null(x = models_list[[model_key]]) && 
        !is.null(x = models_list[[model_key]]$feature_names)) {
      feature_names_from_training <- models_list[[model_key]]$feature_names
      pipeline_message(
        sprintf("Feature names extracted from fallback model: %s", model_key),
        process = "warning")
      break
    }
  }
}

if (is.null(x = feature_names_from_training)) {
  pipeline_message(
    "⚠️ No feature_names found in any model. Predictions may fail due to feature matrix misalignment.",
    process = "warning")
}

saveRDS(object = list(road_feature_formula        = road_feature_formula, 
                      all_periods                 = all_periods,
                      feature_names_from_training = feature_names_from_training), 
        file   = CFG$XGB_RATIO_FEATURE_INFO_FILEPATH)

pipeline_message("Training models and features successfully saved ", level = 1, 
                 progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Final summary
# ------------------------------------------------------------------------------

pipeline_message(sprintf("Results summary: \n", 
                         paste0(capture.output(results_summary), 
                                collapse = "\n\t\t")), 
                 level = 1, progress = "start", process = "plot")

# ------------------------------------------------------------------------------
# Feature importance summary
# ------------------------------------------------------------------------------

# Aggregate feature importance across all models
all_importance <- data.frame()
for (model_name in names(x = models_list)) {
  if (!is.null(x = models_list[[model_name]]$feature_importance)) {
    imp            <- models_list[[model_name]]$feature_importance
    imp$Model      <- model_name
    imp$ModelType  <- ifelse(test = grepl(pattern = "_D$", 
                                          x       = model_name), 
                            yes   = "Base", 
                            no    = "Ratio")
    all_importance <- rbind(all_importance, imp)
  }
}

# Top features across all models
if (nrow(x = all_importance) > 0) {
  global_importance <- all_importance %>%
    group_by(Feature) %>%
    summarise(
      AvgGain   = mean(x = Gain, na.rm = TRUE),
      AvgCover  = mean(x = Cover, na.rm = TRUE),
      TimesUsed = n(),
      .groups   = 'drop') %>%
    arrange(desc(x = AvgGain)) %>%
    head(10)
  
  for (i in 1:nrow(x = global_importance)) {
    pipeline_message(
      sprintf("%2d. %-20s | Avg Gain: %5.1f%% | Used in %2d/%2d models", 
              i, global_importance$Feature[i], 
              global_importance$AvgGain[i] * 100, 
              global_importance$TimesUsed[i], length(x = models_list)), 
      process = "info")
  }
  
  # Base models vs Ratio models feature comparison
  base_importance <- all_importance %>%
    filter(ModelType == "Base") %>%
    group_by(Feature) %>%
    summarise(AvgGain = mean(x = Gain), 
              .groups = 'drop') %>%
    arrange(desc(x = AvgGain)) %>%
    head(5)
  
  ratio_importance <- all_importance %>%
    filter(ModelType == "Ratio") %>%
    group_by(Feature) %>%
    summarise(AvgGain = mean(x = Gain), 
              .groups = 'drop') %>%
    arrange(desc(x = AvgGain)) %>%
    head(5)
  
  for (i in 1:nrow(x = base_importance)) {
    pipeline_message(sprintf("%d. %-20s (%.1f%%)", i, 
                             base_importance$Feature[i], 
                             base_importance$AvgGain[i] * 100), 
                     process = "info")
  }
  
  for (i in 1:nrow(x = ratio_importance)) {
    pipeline_message(sprintf("%d. %-20s (%.1f%%)", i, 
                             ratio_importance$Feature[i], 
                             ratio_importance$AvgGain[i] * 100), 
                     process = "info")
  }
}

# Separate base and ratio model performance
base_results  <- results_summary[grepl(pattern = "_D$", 
                                       x       = results_summary$Model), ]
ratio_results <- results_summary[grepl(pattern = "^ratio_", 
                                       x       = results_summary$Model), ]

pipeline_message(
  sprintf("R²: %.3f", mean(x = base_results$R2, na.rm = TRUE)), 
  process = "info")
pipeline_message(
  sprintf("MAPE: %.1f%%", mean(x = base_results$MAPE, na.rm = TRUE)), 
  process = "info")
pipeline_message(
  sprintf("R²: %.3f", mean(x = ratio_results$R2, na.rm = TRUE)), 
  process = "info")
pipeline_message(
  sprintf("MAPE: %.1f%%", mean(x = ratio_results$MAPE, na.rm = TRUE)), 
  process = "info")

pipeline_message("Results summary completed", level = 1, 
                 progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Error analysis in percentage
# ------------------------------------------------------------------------------

pipeline_message("Error analysis", level = 1, 
                 progress = "start", process = "plot")

# Compute percentage errors for each model
for (model_name in names(x = models_list)) {
  model_info   <- models_list[[model_name]]
  model_config <- model_info$config
  
  # Get test predictions and actual values
  test_data <- training_data %>% filter(period == model_config$period)
  y_all     <- test_data[[model_config$target]]
  valid_idx <- !is.na(x = y_all) & y_all >= model_config$min_valid
  
  if (sum(valid_idx) < 10) next
  
  data_clean <- test_data[valid_idx, ]
  y_clean    <- y_all[valid_idx]
  
  sparse_data_matrix <- safe_sparse_model_matrix(
    formula_obj = road_feature_formula,
    data_df     = data_clean)
  
  if (nrow(x = sparse_data_matrix) != nrow(x = data_clean)) {
    kept_rows  <- as.integer(x = rownames(x = sparse_data_matrix))
    y_clean    <- y_clean[kept_rows]
    data_clean <- data_clean[kept_rows, ]
  }
  
  # Apply transform
  if (!is.null(x = model_config$transform) && 
      model_config$transform == "log10") {
    transformed_training_data_target <- log10(x = pmax(y_clean, 
                                                       model_config$min_valid))
  } else {
    transformed_training_data_target <- y_clean
  }
  
  # Train/test split (same seed as training)
  set.seed(123)
  n_final   <- nrow(x = sparse_data_matrix)
  train_idx <- sample(x    = seq_len(n_final), 
                      size = floor(x = 0.8 * n_final))
  test_idx  <- setdiff(x = seq_len(n_final), 
                       y = train_idx)
  
  X_test <- sparse_data_matrix[test_idx, ]
  y_test <- transformed_training_data_target[test_idx]
  
  # Remove invalid
  invalid_test <- is.na(x = y_test) | is.infinite(x = y_test) | is.nan(x = y_test)
  if (any(invalid_test)) {
    valid_test_idx <- !invalid_test
    X_test         <- X_test[valid_test_idx, ]
    y_test         <- y_test[valid_test_idx]
  }
  
  if (length(x = y_test) < 5) {next}
  
  # Predict
  pred_test <- xgboost::predict(object  = model_info$model,  
                                newdata = X_test)
  
  # Back-transform
  if (!is.null(x = model_config$transform) && 
      model_config$transform == "log10") {
    pred_original   <- 10^pred_test
    actual_original <- 10^y_test
  } else {
    pred_original   <- pred_test
    actual_original <- y_test
  }
  
  # Compute percentage errors
  pct_errors <- ((pred_original - actual_original) / 
                pmax(actual_original, 0.01)) * 100
  pct_errors <- pct_errors[is.finite(x = pct_errors)]
  
  if (length(x = pct_errors) == 0) {next}
  
  # Statistics
  mean_pct_error   <- mean(x = pct_errors, na.rm = TRUE)
  median_pct_error <- median(x = pct_errors, na.rm = TRUE)
  mae_pct          <- mean(x = abs(x = pct_errors), na.rm = TRUE)
  
  # Quantiles
  q25 <- quantile(x     = pct_errors, 
                  probs = 0.25, 
                  na.rm = TRUE)
  q75 <- quantile(x     = pct_errors, 
                  probs = 0.75, 
                  na.rm = TRUE)
  
  # Print results
  pipeline_message(sprintf("%-30s | Period: %-2s | N=%4d", model_config$name, 
                           model_config$period, length(x = pct_errors)), 
                   process = "search")
  pipeline_message(sprintf("Mean Error: %+6.1f%% (bias)", mean_pct_error), 
                   process = "search")
  pipeline_message(sprintf("Median Error: %+6.1f%%", median_pct_error), 
                   process = "search")
  pipeline_message(sprintf("MAE: %6.1f%% (average absolute error)", mae_pct), 
                   process = "search")
  pipeline_message(sprintf("Q25-Q75: [%+6.1f%%, %+6.1f%%]", q25, q75), 
                   process = "search")
}

pipeline_message("Error analysis completed", level = 1, 
                 progress = "end", process = "valid")

pipeline_message("Successfully trained learning model", level = 0, 
                 progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Acoustic emission error analysis (dB) on test dataset
# ------------------------------------------------------------------------------

pipeline_message("Acoustic emission error analysis (dB)", 
                 level = 1, progress = "start", process = "plot")

emission_test <- data.frame()

# Strategy: use the shared sensor split to build emission test data for ALL periods
# 1. Predict flow_D/truck_pct_D/speed_D on test sensors (period D)
# 2. For each non-D period, apply ratio models to get predicted values
# 3. Get actual values from training_data for each period's test sensors
# 4. Compute CNOSSOS emission for all periods
if (all(c("flow_D", "truck_pct_D", "speed_D") %in% names(x = models_list)) &&
    !is.null(x = shared_base_test_sensors)) {

  d_data <- training_data %>%
    filter(period == "D", 
           count_point_id %in% shared_base_test_sensors, 
           !is.na(x = aggregate_flow), 
           aggregate_flow >= 1)

  if (nrow(d_data) > 0) {
    d_matrix <- safe_sparse_model_matrix(
      formula_obj = road_feature_formula, 
      data_df = d_data)

    if (nrow(d_matrix) != nrow(d_data)) {
      kept_rows <- as.integer(x = rownames(x = d_matrix))
      d_data    <- d_data[kept_rows, ]
    }

    # Align feature columns with what the model expects
    # (subset of test data may have fewer factor levels → fewer columns)
    model_features <- models_list[["flow_D"]]$model$feature_names
    if (!is.null(x = model_features)) {
      current_cols <- colnames(x = d_matrix)
      missing_cols <- setdiff(x = model_features, y = current_cols)
      if (length(x = missing_cols) > 0) {
        zero_mat <- Matrix::Matrix(data   = 0, 
                                   nrow   = nrow(d_matrix),
                                   ncol   = length(x = missing_cols),
                                   sparse = TRUE)
        colnames(x = zero_mat) <- missing_cols
        d_matrix <- cbind(d_matrix, zero_mat)
      }
      d_matrix <- d_matrix[, model_features, drop = FALSE]
    }

    if (nrow(d_matrix) > 0) {
      # --- Base predictions (period D) ---
      pred_flow_D_log         <- xgboost::predict(
                                    object  = models_list[["flow_D"]]$model, 
                                    newdata = d_matrix)
      pred_flow_D             <- 10^pred_flow_D_log
      pred_truck_D            <- xgboost::predict(
                                    object  = models_list[["truck_pct_D"]]$model, 
                                    newdata = d_matrix)
      pred_speed_ratio_to_osm <- xgboost::predict(
                                    object  = models_list[["speed_D"]]$model, 
                                    newdata = d_matrix)

      osm_speed_raw        <- suppressWarnings(expr = as.numeric(x = d_data$speed))
      osm_speed_missing    <- is.na(x = osm_speed_raw) | osm_speed_raw < 5
      speed_base_for_ratio <- osm_speed_raw
      speed_base_for_ratio[osm_speed_missing] <- CFG$DEFAULT_VEHICLE_SPEED
      pred_speed_D         <- pmax(5, pred_speed_ratio_to_osm * speed_base_for_ratio)

      # OSM maxspeed: use raw OSM speed where available, impute missing with XGBoost speed_D
      osm_speed_imputed_flag <- osm_speed_missing
      osm_speed_val          <- osm_speed_raw
      osm_speed_val[osm_speed_imputed_flag] <- pred_speed_D[osm_speed_imputed_flag]

      # Actual values for period D
      actual_speed_D <- ifelse(test = !is.na(x = d_data$aggregate_speed) & 
                                      d_data$aggregate_speed >= 5, 
                               yes  = d_data$aggregate_speed,
                               no   = speed_base_for_ratio)
      actual_truck_D <- ifelse(test = !is.na(x = d_data$truck_pct) & 
                                      d_data$truck_pct >= 0,
                               yes  = d_data$truck_pct,
                               no   = 0)

      # Build emission_test for period D
      emission_D <- data.frame(
        osm_id             = d_data$osm_id, 
        count_point_id     = d_data$count_point_id, 
        period             = "D", 
        highway            = as.character(x = d_data$highway), 
        DEGRE              = if ("DEGRE" %in% names(x = d_data)) 
                               {as.character(x = d_data$DEGRE)} 
                             else {NA_character_}, 
        pred_flow          = as.numeric(x = pred_flow_D), 
        actual_flow        = as.numeric(x = d_data$aggregate_flow), 
        pred_truck_pct     = as.numeric(x = pred_truck_D), 
        actual_truck_pct   = as.numeric(x = actual_truck_D), 
        pred_speed         = as.numeric(x = pred_speed_D), 
        actual_speed       = as.numeric(x = actual_speed_D), 
        osm_speed          = as.numeric(x = osm_speed_val), 
        osm_speed_imputed  = osm_speed_imputed_flag, 
        has_measured_speed = !is.na(x = d_data$aggregate_speed) & 
                             d_data$aggregate_speed >= 5, 
        has_measured_truck = !is.na(x = d_data$truck_pct) & 
                             d_data$truck_pct >= 0, 
        stringsAsFactors   = FALSE)
      
      emission_D <- emission_D[!is.na(x = emission_D$actual_speed) & 
                               emission_D$actual_speed >= 5, ]

      emission_parts <- list(emission_D)

      # Apply ratio models for all non-D periods ---
      non_d_periods   <- setdiff(x = all_periods, y = "D")
      # Keep only test sensor IDs that survived D filtering
      test_sensor_ids <- unique(x = emission_D$count_point_id)
      n_periods_done  <- 0

      for (p in non_d_periods) {
        # Check that all 3 ratio models exist for this period
        flow_model_key  <- paste0("ratio_flow_", p)
        truck_model_key <- paste0("ratio_truck_pct_", p)
        speed_model_key <- paste0("ratio_speed_", p)

        has_flow_ratio  <- !is.null(x = models_list[[flow_model_key]]$model)
        has_truck_ratio <- !is.null(x = models_list[[truck_model_key]]$model)
        has_speed_ratio <- !is.null(x = models_list[[speed_model_key]]$model)

        # Need at least flow ratio to proceed
        if (!has_flow_ratio) next

        # Predict ratios from the feature matrix (same d_matrix, same roads)
        ratio_flow  <- xgboost::predict(
                         object  = models_list[[flow_model_key]]$model, 
                         newdata = d_matrix)
        ratio_truck <- if (has_truck_ratio) {
          xgboost::predict(object  = models_list[[truck_model_key]]$model, 
                           newdata = d_matrix)
        } else {
          rep(x = 1, times = nrow(d_matrix))  # Fallback: same as D
        }
        ratio_speed <- if (has_speed_ratio) {
          xgboost::predict(object  = models_list[[speed_model_key]]$model, 
                           newdata = d_matrix)
        } else {
          rep(x = 1, times = nrow(d_matrix))  # Fallback: same as D
        }

        # Predicted values for period P = base_D × ratio_P
        pred_flow_P  <- pmax(0, pred_flow_D * ratio_flow)
        pred_truck_P <- pmin(100, pmax(0, pred_truck_D * ratio_truck))
        pred_speed_P <- pmax(5, pred_speed_D * ratio_speed)

        # Get actual values from training_data for this period's test sensors
        p_data <- training_data %>%
          filter(period == p,
                 count_point_id %in% test_sensor_ids)

        if (nrow(p_data) == 0) {next}

        # Match to D-data rows by count_point_id (inner join)
        # d_data rows define the feature matrix order
        match_idx   <- match(x     = p_data$count_point_id, 
                             table = d_data$count_point_id)
        valid_match <- !is.na(x = match_idx)
        p_data      <- p_data[valid_match, ]
        row_in_d    <- match_idx[valid_match]

        if (nrow(p_data) == 0) {next}

        actual_flow_P  <- ifelse(test = !is.na(x = p_data$aggregate_flow) & 
                                        p_data$aggregate_flow >= 0,
                                 yes  = p_data$aggregate_flow, 
                                 no   = NA_real_)
        actual_truck_P <- ifelse(test = !is.na(x = p_data$truck_pct) & 
                                        p_data$truck_pct >= 0,
                                 yes  = p_data$truck_pct, 
                                 no   = 0)
        actual_speed_P <- ifelse(test = !is.na(x = p_data$aggregate_speed) & 
                                        p_data$aggregate_speed >= 5,
                                 yes  = p_data$aggregate_speed, 
                                 no   = speed_base_for_ratio[row_in_d])

        emission_P <- data.frame(
          osm_id             = d_data$osm_id[row_in_d],
          count_point_id     = p_data$count_point_id,
          period             = p,
          highway            = as.character(x = d_data$highway[row_in_d]),
          DEGRE              = if ("DEGRE" %in% names(x = d_data)) {
                                 as.character(x = d_data$DEGRE[row_in_d])} 
                               else {NA_character_},
          pred_flow          = as.numeric(x = pred_flow_P[row_in_d]),
          actual_flow        = as.numeric(x = actual_flow_P),
          pred_truck_pct     = as.numeric(x = pred_truck_P[row_in_d]),
          actual_truck_pct   = as.numeric(x = actual_truck_P),
          pred_speed         = as.numeric(x = pred_speed_P[row_in_d]),
          actual_speed       = as.numeric(x = actual_speed_P),
          osm_speed          = as.numeric(x = osm_speed_val[row_in_d]),
          osm_speed_imputed  = osm_speed_imputed_flag[row_in_d],
          has_measured_speed = !is.na(x = p_data$aggregate_speed) & 
                               p_data$aggregate_speed >= 5,
          has_measured_truck = !is.na(x = p_data$truck_pct) & 
                               p_data$truck_pct >= 0,
          stringsAsFactors   = FALSE)

        # Filter out invalid rows
        emission_P <- emission_P[!is.na(x = emission_P$actual_speed) &
                                 emission_P$actual_speed >= 5 &
                                 !is.na(x = emission_P$actual_flow), ]

        if (nrow(emission_P) > 0) {
          emission_parts <- c(emission_parts, list(emission_P))
          n_periods_done <- n_periods_done + 1
        }
      }

      # Combine all periods
      emission_test <- do.call(what = rbind, 
                               args = emission_parts)
      rm(emission_parts, emission_D)

      pipeline_message(
        sprintf("Emission dB eval: %s rows across %d periods (%s test sensors)",
                fmt(nrow(emission_test)),
                n_periods_done + 1,  # +1 for D
                fmt(length(x = test_sensor_ids))),
        process = "info")
      pipeline_message(
        sprintf("Period breakdown: D=%s, E/N=%s, hourly=%s, wd=%s, we=%s",
                fmt(sum(emission_test$period == "D")),
                fmt(sum(emission_test$period %in% c("E", "N"))),
                fmt(sum(grepl("^h[0-9]+$", emission_test$period))),
                fmt(sum(grepl("_wd$", emission_test$period))),
                fmt(sum(grepl("_we$", emission_test$period)))),
        process = "info")
    }
  }
}

# Legacy fallback: merge base_test_predictions if shared split unavailable
if (nrow(emission_test) == 0 &&
    all(c("flow_D", "truck_pct_D", "speed_D") 
      %in% names(x = base_test_predictions))) {
  flow_test <- base_test_predictions[["flow_D"]]
  names(x = flow_test)[names(x = flow_test) == "pred"]   <- "pred_flow"
  names(x = flow_test)[names(x = flow_test) == "actual"] <- "actual_flow"

  truck_test <- base_test_predictions[["truck_pct_D"]][,
    c("osm_id", "count_point_id", "period", "pred", "actual")]
  names(x = truck_test)[names(x = truck_test) == "pred"]   <- "pred_truck_pct"
  names(x = truck_test)[names(x = truck_test) == "actual"] <- "actual_truck_pct"

  speed_test <- base_test_predictions[["speed_D"]][,
    c("osm_id", "count_point_id", "period", "pred", "actual")]
  names(x = speed_test)[names(x = speed_test) == "pred"]   <- "pred_speed"
  names(x = speed_test)[names(x = speed_test) == "actual"] <- "actual_speed"

  emission_test <- merge(
    x   = flow_test, 
    y   = truck_test, 
    by  = c("osm_id", "count_point_id", "period"), 
    all = FALSE)

  emission_test <- merge(
    x = emission_test, 
    y   = speed_test, 
    by  = c("osm_id", "count_point_id", "period"), 
    all = FALSE)

  # Legacy path may not carry DEGRE information
  if (!"DEGRE" %in% names(x = emission_test)) {
    emission_test$DEGRE <- NA_character_
  }

  if (nrow(x = emission_test) > 0) {
    pipeline_message(
      sprintf("Emission dB legacy path: %s rows from base_test_predictions merge", fmt(nrow(x = emission_test))),
      process = "info")
  }
}

if (nrow(x = emission_test) > 0) {
  pipeline_message(sprintf("Computing CNOSSOS-EU emission for %s test samples", 
                           fmt(nrow(x = emission_test))), 
                   process = "calc")
  # Emission with XGBoost predicted speed (real traffic speed)
  emission_test$pred_db <- compute_emission_cnossos(
    flow      = emission_test$pred_flow,
    truck_pct = emission_test$pred_truck_pct,
    speed     = emission_test$pred_speed)
  # Emission with OSM maxspeed (imputed by XGBoost when missing)
  has_osm_speed <- "osm_speed" %in% names(x = emission_test)
  if (has_osm_speed) {
    emission_test$pred_db_osm_speed <- compute_emission_cnossos(
      flow      = emission_test$pred_flow,
      truck_pct = emission_test$pred_truck_pct,
      speed     = emission_test$osm_speed)
  }
  # Reference emission from actual/observed values
  emission_test$actual_db <- compute_emission_cnossos(
    flow      = emission_test$actual_flow,
    truck_pct = emission_test$actual_truck_pct,
    speed     = emission_test$actual_speed)

  emission_test$db_error <- emission_test$pred_db - emission_test$actual_db
  emission_test$abs_db_error <- abs(x = emission_test$db_error)
  if (has_osm_speed) {
    emission_test$db_error_osm <- emission_test$pred_db_osm_speed - 
                                  emission_test$actual_db
    emission_test$abs_db_error_osm <- abs(x = emission_test$db_error_osm)
  }

  valid_db <- is.finite(x = emission_test$db_error)
  if (has_osm_speed) {
    valid_db <- valid_db & is.finite(x = emission_test$db_error_osm)
  }
  emission_test <- emission_test[valid_db, ]
}

if (nrow(x = emission_test) > 0) {
  db_bias <- mean(x     = emission_test$db_error, 
                  na.rm = TRUE)
  db_mae  <- mean(x     = abs(x = emission_test$db_error), 
                  na.rm = TRUE)
  db_rmse <- sqrt(x = mean(x     = emission_test$db_error^2, 
                           na.rm = TRUE))
  db_q50  <- median(x     = emission_test$abs_db_error, 
                    na.rm = TRUE)
  db_q90  <- as.numeric(x = quantile(x     = emission_test$abs_db_error, 
                                     probs = 0.90, 
                                     na.rm = TRUE))

  pipeline_message(
    sprintf(paste("Emission dB (test): N=%s | Bias=%+.2f dB |", 
                  "MAE=%.2f dB | RMSE=%.2f dB |", 
                  "MedAE=%.2f dB |", "P90AE=%.2f dB"), 
            fmt(nrow(x = emission_test)), 
            db_bias, db_mae, db_rmse, db_q50, db_q90), 
    process = "info")

  if ("has_measured_speed" %in% names(x = emission_test)) {
    emission_measured_speed <- emission_test[emission_test$has_measured_speed, , 
                                             drop = FALSE]
    emission_osm_fallback   <- emission_test[!emission_test$has_measured_speed, , 
                                             drop = FALSE]

    stats_measured <- compute_db_stats(emission_measured_speed)
    stats_osm      <- compute_db_stats(emission_osm_fallback)

    if (stats_measured$n > 0) {
      pipeline_message(
        sprintf(paste("Emission dB (measured speed): N=%s |", 
                      "Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB"), 
                fmt(stats_measured$n), stats_measured$bias, 
                    stats_measured$mae, stats_measured$rmse), 
        process = "info")
    }
    if (stats_osm$n > 0) {
      pipeline_message(
        sprintf(paste("Emission dB (OSM speed fallback): N=%s |", 
                      "Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB"), 
                fmt(stats_osm$n), stats_osm$bias, 
                    stats_osm$mae, stats_osm$rmse), 
        process = "info")
    }
  }

  # Highway-level summary (top classes by sample size)
  emission_dt <- as.data.table(x = emission_test)
  db_by_highway <- emission_dt[, .(
      n       = .N,
      bias_db = mean(x = db_error, na.rm = TRUE),
      mae_db  = mean(x = abs_db_error, na.rm = TRUE),
      rmse_db = sqrt(x = mean(x = db_error^2, na.rm = TRUE))), 
    by = highway][order(-n)]

  top_hw <- db_by_highway[1:min(8, nrow(x = db_by_highway))]

  # Dual-speed comparison: XGBoost predicted speed vs OSM maxspeed
  has_osm_speed <- "db_error_osm" %in% names(x = emission_test)
  if (has_osm_speed) {
    db_bias_osm <- mean(x = emission_test$db_error_osm, na.rm = TRUE)
    db_mae_osm  <- mean(x = abs(x = emission_test$db_error_osm), na.rm = TRUE)
    db_rmse_osm <- sqrt(x = mean(x = emission_test$db_error_osm^2, na.rm = TRUE))

    pipeline_message(
      sprintf(paste("Emission dB with XGBoost speed: Bias=%+.2f dB |", 
                    "MAE=%.2f dB | RMSE=%.2f dB"), 
              db_bias, db_mae, db_rmse),
      process = "info")
    pipeline_message(
      sprintf(paste("Emission dB with OSM maxspeed: Bias=%+.2f dB |", 
                    "MAE=%.2f dB | RMSE=%.2f dB"), 
              db_bias_osm, db_mae_osm, db_rmse_osm),
      process = "info")
    pipeline_message(
      sprintf(paste("Speed choice impact: \u0394Bias=%.2f dB |", 
                    "\u0394MAE=%.2f dB (OSM - XGBoost)"), 
              db_bias_osm - db_bias, db_mae_osm - db_mae),
      process = "info")
  }

  # Save report in PDF file
  if (!dir.exists(path = CFG$FIGS_DIR)) {
    dir.create(path         = CFG$FIGS_DIR, 
               recursive    = TRUE, 
               showWarnings = FALSE)
  }
  emission_pdf_path <- file.path(CFG$FIGS_DIR, "06_emission_dB_error_test.pdf")

  grDevices::pdf(file = emission_pdf_path, width = 11, height = 8.5)
  old_par <- par(no.readonly = TRUE)

  # ============================================================================
  # Helper: compute per-variable dB contribution (isolate each variable)
  # ============================================================================
  db_actual_all <- emission_test$actual_db
  db_flow_only <- compute_emission_cnossos(
    flow      = emission_test$pred_flow,
    truck_pct = emission_test$actual_truck_pct,
    speed     = emission_test$actual_speed)
  db_truck_only <- compute_emission_cnossos(
    flow      = emission_test$actual_flow,
    truck_pct = emission_test$pred_truck_pct,
    speed     = emission_test$actual_speed)
  db_speed_only <- compute_emission_cnossos(
    flow      = emission_test$actual_flow,
    truck_pct = emission_test$actual_truck_pct,
    speed     = emission_test$pred_speed)

  contrib_flow  <- db_flow_only  - db_actual_all
  contrib_truck <- db_truck_only - db_actual_all
  contrib_speed <- db_speed_only - db_actual_all

  # Truck contribution: only where truck reference is actually measured
  if ("has_measured_truck" %in% names(x = emission_test)) {
    truck_eval_idx <- !is.na(x = emission_test$has_measured_truck) & 
                      as.logical(emission_test$has_measured_truck)
  } else {
    truck_eval_idx <- rep(x = TRUE, times = length(x = contrib_truck))
  }
  truck_eval_idx     <- truck_eval_idx & is.finite(x = contrib_truck)
  contrib_truck_eval <- contrib_truck[truck_eval_idx]
  truck_bias <- if (length(x = contrib_truck_eval) > 0) {
                  mean(x = contrib_truck_eval, na.rm = TRUE)} 
                else {NA_real_}
  truck_mae  <- if (length(x = contrib_truck_eval) > 0) {
                  mean(x = abs(x = contrib_truck_eval), na.rm = TRUE)} 
                else {NA_real_}

  mean_contrib <- c(Flow      = mean(x = contrib_flow, na.rm = TRUE),
                    `Truck %` = truck_bias,
                    Speed     = mean(x = contrib_speed, na.rm = TRUE))
  mae_contrib <- c(Flow       = mean(x = abs(x = contrib_flow), na.rm = TRUE),
                   `Truck %`  = truck_mae,
                   Speed      = mean(x = abs(x = contrib_speed), na.rm = TRUE))

  pipeline_message(
    sprintf(paste("Bias decomposition: Flow=%+.2f dB |", 
                  "Truck%%=%+.2f dB (N=%s) | Speed=%+.2f dB"),
            mean_contrib["Flow"], mean_contrib["Truck %"], 
            fmt(length(x = contrib_truck_eval)), mean_contrib["Speed"]),
    process = "info")

  # Attach row indices for decomposition lookup
  emission_test$.row_idx <- seq_len(nrow(x = emission_test))

  # ============================================================================
  # PAGE 1: Overall prediction quality in dB on test sections
  # ============================================================================
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))

  # 1a: Error histogram
  hist(x        = emission_test$db_error, 
       breaks   = 50,
       main     = "Emission error distribution (dB)",
       xlab     = "Error = Predicted - Observed (dB)",
       col      = "steelblue", 
       border   = "white", 
       cex.main = 1.1)
  abline(v   = 0, 
         col = "red", 
         lwd = 2, 
         lty = 1)
  abline(v   = db_bias, 
         col = "orange", 
         lwd = 2, 
         lty = 2)
  legend(x      = "topright",
         legend = c("Zero", sprintf("Mean bias = %+.2f dB", db_bias)),
         col    = c("red", "orange"), 
         lwd    = 2, 
         lty    = c(1, 2), 
         cex    = 0.8, 
         bty    = "n")

  # 1b: Predicted vs Observed
  plot(x        = emission_test$actual_db, 
       y        = emission_test$pred_db,
       pch      = 16, 
       cex      = 0.35, 
       col      = rgb(red   = 0.2, 
                      blue  = 0.4, 
                      green = 0.8, 
                      alpha = 0.25),
       xlab     = "Observed emission CNOSSOS-EU (dB(A))",
       ylab     = "Predicted emission CNOSSOS-EU (dB(A))",
       main     = "Predicted vs Observed (test sections)", 
       cex.main = 1.1)
  abline(a   = 0, 
         b   = 1, 
         col = "red", 
         lwd = 2)
  r2 <- cor(x   = emission_test$actual_db, 
            y   = emission_test$pred_db, 
            use = "complete.obs")^2
  legend(x      = "topleft", 
         legend = sprintf("R² = %.3f", r2), 
         cex    = 0.9, 
         bty    = "n")

  # 1c: CDF of absolute error
  abs_sorted <- sort(x = emission_test$abs_db_error)
  ecdf_vals <- seq_along(along.with = abs_sorted) / length(x = abs_sorted)
  plot(x        = abs_sorted, 
       y        = ecdf_vals, 
       type     = "l", 
       lwd      = 2, 
       col      = "steelblue",
       xlab     = "Absolute error (dB)", 
       ylab     = "Cumulative proportion",
       main     = "Absolute error distribution function", 
       cex.main = 1.1)
  abline(h   = 0.5, 
         col = "grey60", 
         lty = 3)
  abline(h   = 0.9, 
         col = "grey60", 
         lty = 3)
  abline(v   = db_q50, 
         col = "orange", 
         lty = 2)
  abline(v   = db_q90, 
         col = "red", 
         lty = 2)
  legend(x      = "bottomright", 
         legend = c(sprintf("Median = %.1f dB", db_q50), 
                    sprintf("P90 = %.1f dB", db_q90)), 
         col    = c("orange", "red"), 
         lwd    = 2, 
         lty    = 2, 
         cex    = 0.8, 
         bty    = "n")

  # 1d: Summary table
  plot.new()
  title(main     = "Overall metrics on test sections", 
        cex.main = 1.1)
  summary_txt <- c(
    sprintf("Number of sections/periods evaluated: %s\n", 
            fmt(nrow(x = emission_test))),
    sprintf("Mean bias:                 %+.2f dB", db_bias),
    sprintf("Mean absolute error (MAE): %.2f dB", db_mae),
    sprintf("RMSE:                      %.2f dB", db_rmse),
    sprintf("Median absolute error:     %.2f dB", db_q50),
    sprintf("P90 absolute error:        %.2f dB", db_q90),
    "",
    "Interpretation :",
    if (db_bias > 0.5) {
      sprintf("\t-> Overestimation of %+.1f dB on average", db_bias)}
    else if (db_bias < -0.5) {
      sprintf("\t-> Underestimation of %+.1f dB on average", db_bias)}
    else {
      "\t-> Negligible bias (< 0.5 dB)"
    }
  )
  text(x          = 0.02, 
       y          = seq(from       = 0.95, 
                        to         = 0.95 - 0.075 * (length(x = summary_txt) - 1), 
                        length.out = length(x = summary_txt)), 
       labels     = summary_txt, 
       adj        = c(0, 0.5), 
       cex        = 0.90, 
       family     = "mono")

  mtext(text  = "Evaluation of CNOSSOS-EU emissions on test sections", 
        side  = 3, 
        outer = FALSE, 
        line  = -1.5, 
        cex   = 0.7, 
        font  = 3)

  # ============================================================================
  # PAGE 1bis: Data transparency and quality by subsample
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main     = "Transparency of test data and robustness of results", 
        cex.main = 1.1)

  n_all <- nrow(x = emission_test)
  has_speed_col <- "has_measured_speed" %in% names(x = emission_test)
  has_truck_col <- "has_measured_truck" %in% names(x = emission_test)

  n_speed_measured <- if (has_speed_col) {sum(emission_test$has_measured_speed, na.rm = TRUE)} 
                      else {0L}
  n_speed_fallback <- if (has_speed_col) {n_all - n_speed_measured} 
                      else {NA_integer_}
  n_truck_measured <- if (has_truck_col) {sum(emission_test$has_measured_truck, na.rm = TRUE)} 
                      else {0L}
  n_truck_fallback <- if (has_truck_col) {n_all - n_truck_measured} 
                      else {NA_integer_}

  subset_table <- data.frame(stringsAsFactors = FALSE)
  if (has_speed_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(mask  = emission_test$has_measured_speed, 
                             label = "Measured speed"),
      compute_subset_metrics(mask  = !emission_test$has_measured_speed, 
                             label = "Fallback speed (OSM/assigned)"))
  }
  if (has_truck_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(mask  = emission_test$has_measured_truck, 
                             label = "Measured %HGV"),
      compute_subset_metrics(mask  = !emission_test$has_measured_truck, 
                             label = "Fallback %HGV"))
  }
  if (has_speed_col && has_truck_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(mask  = emission_test$has_measured_speed & emission_test$has_measured_truck, 
                             label = "Measured speed + %HGV"),
      compute_subset_metrics(mask  = !emission_test$has_measured_speed & !emission_test$has_measured_truck, 
                             label = "Fallback speed + %HGV"))
  }

  intro_lines <- c(
    sprintf("Total sample evaluated: %s sections-periods\n", fmt(n_all)),
    sprintf("Coverage measured speed: %s (%s)",
            fmt(n_speed_measured), 
            if (has_speed_col) fmt_pct(n_speed_measured / n_all) else "NA"),
    sprintf("Coverage fallback speed: %s (%s)",
            if (is.na(x = n_speed_fallback)) {"NA"} else {fmt(n_speed_fallback)},
            if (has_speed_col) {fmt_pct(n_speed_fallback / n_all)} else {"NA"}),
    sprintf("Coverage measured %%HGV: %s (%s)",
            fmt(n_truck_measured), 
            if (has_truck_col) fmt_pct(n_truck_measured / n_all) else "NA"),
    sprintf("Coverage fallback %%HGV: %s (%s)\n",
            if (is.na(x = n_truck_fallback)) {"NA"} else {fmt(n_truck_fallback)},
            if (has_truck_col) {fmt_pct(n_truck_fallback / n_all)} else {"NA"}),
    "Quality per subsample (dB):", 
    "subset\t\t\t n\t bias\t MAE\t RMSE\t P50\t P90\t |e|<=1dB\t |e|<=2dB"
  )

  table_lines <- c()
  if (nrow(x = subset_table) > 0) {
    for (i in seq_len(nrow(x = subset_table))) {
      rr          <- subset_table[i, ]
      table_lines <- c(table_lines, sprintf(
        "%-34s %6s %7s %6s %7s %5s %5s %8s %8s",
        substr(x = rr$subset, start = 1, stop = 34),
        fmt(rr$n),
        ifelse(test = is.na(x = rr$bias), 
               yes  = "NA", 
               no   = sprintf("%+.2f", rr$bias)),
        ifelse(test = is.na(x = rr$mae), 
               yes  = "NA", 
               no   = sprintf("%.2f", rr$mae)),
        ifelse(test = is.na(x = rr$rmse), 
               yes  = "NA", 
               no   = sprintf("%.2f", rr$rmse)),
        ifelse(test = is.na(x = rr$q50), 
               yes  = "NA", 
               no   = sprintf("%.2f", rr$q50)),
        ifelse(test = is.na(x = rr$q90), 
               yes  = "NA", 
               no   = sprintf("%.2f", rr$q90)),
        ifelse(test = is.na(x = rr$within_1db), 
               yes  = "NA", 
               no   = fmt_pct(rr$within_1db)),
        ifelse(test = is.na(x = rr$within_2db), 
               yes  = "NA", 
               no   = fmt_pct(rr$within_2db))
      ))
    }
  }

  final_lines <- c(
    intro_lines,
    table_lines,
    paste("\nNote: the objective of the report is solely the quality of", 
          "dB emission predictions on test sections (not the error on", 
          "gross road traffic).")
    )

  text(x      = 0.02,
       y      = seq(from       = 0.96, 
                    to         = 0.96 - 0.029 * (length(x = final_lines) - 1), 
                    length.out = length(x = final_lines)),
       labels = final_lines, 
       adj    = c(0, 0.5), 
       cex    = 0.70, 
       family = "mono")

  # ============================================================================
  # PAGE 2: Search for the cause of the error. Breakdown by source (flow, HGV 
  #         percentage, speed)
  # ============================================================================
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1))

  bar_colors <- c("#4393C3", "#F4A582", "#D6604D")

  # 2a: Bias by variable
  bp <- barplot(height   = mean_contrib, 
                col      = bar_colors,
                main     = "Contribution to bias per variable",
                ylab     = "Mean bias (dB)",
                ylim     = range(c(mean_contrib, 0), 
                                 na.rm = TRUE) * c(1.4, 1.4),
                border   = NA, 
                cex.main = 1.1)
  abline(h   = 0, 
         col = "grey40")
  text(x = bp, 
       y = mean_contrib, 
       labels = sprintf("%+.2f dB", mean_contrib),
       pos    = ifelse(test = mean_contrib >= 0, 
                       yes  = 3, 
                       no   = 1), 
       cex    = 0.95, 
       font   = 2)
  mtext(text = "Bias > 0 = Overestimation, < 0 = Underestimation",
        side = 1, 
        line = 2.5, 
        cex  = 0.7, 
        font = 3)

  # 2b: MAE par variable
  bp2 <- barplot(height = mae_contrib, 
                 col    = bar_colors,
                 main   = "Mean Absolute Error (MAE) by variable",
                 ylab   = "MAE (dB)", border = NA, cex.main = 1.1)
  text(x      = bp2, 
       y      = mae_contrib, 
       labels = sprintf("%.2f dB", mae_contrib),
       pos    = 3, 
       cex    = 0.95, 
       font   = 2)
  mtext(text = paste("The higher the bar,", 
                     "the more the variable introduces error in dB"),
        side = 1, 
        line = 2.5, 
        cex  = 0.7, 
        font = 3)

  # 2c: Boxplot of contributions
  truck_box <- if (length(x = contrib_truck_eval) > 0) {
                 contrib_truck_eval} 
               else {NA_real_}
  boxplot(x        = list(Flow               = contrib_flow, 
                          `% HGV (measured)` = truck_box, 
                          Speed              = contrib_speed),
          col      = bar_colors,
          main     = "Distribution of the dB error by variable",
          ylab     = "dB Error (predicted - observed)", 
          outline  = FALSE, 
          cex.main = 1.1)
  abline(h   = 0, 
         col = "red", 
         lty = 2)
  mtext(text = paste("Each box shows the variability of the error", 
                     "due to a single variable"),
        side = 1, 
        line = 2.5, 
        cex  = 0.7, 
        font = 3)

  # 2d: Textual interpretation
  plot.new()
  title(main     = "Diagnosis: which variable causes the error?", 
        cex.main = 1.1)

  # Rank by MAE
  ord <- order(mae_contrib, decreasing = TRUE)
  var_names_fr <- c(Flow   = "the flow", 
                    `%HGV` = "the % heavy goods vehicles", 
                    Speed  = "the speed")
  direction_fr <- c(Flow = if (mean_contrib["Flow"] > 0) {"overestimated"} 
                           else {"underestimated"},
                    `Truck %` = if (!is.na(x = mean_contrib["Truck %"]) && 
                                    mean_contrib["Truck %"] > 0) {"overestimated"} 
                                else {"underestimated"},
                    Speed = if (mean_contrib["Speed"] > 0) {
                              "overestimated → too high level"} 
                            else {"underestimated → too low level"})

  diag_lines <- c(
    "Ranking of sources of error (from most impactful to least):\n\n",)
  for (i in seq_along(along.with = ord)) {
    vn         <- names(x = mae_contrib)[ord[i]]
    diag_lines <- c(diag_lines, 
                    sprintf("\t%d. %s : biais = %+.2f dB, MAE = %.2f dB (%s)", 
                            i, var_names_fr[vn], mean_contrib[vn], 
                            mae_contrib[vn], direction_fr[vn]))
  }
  diag_lines   <- c(diag_lines, 
                    sprintf(paste("\n\tHGV percentage evaluated on", 
                            "%s sensors with effective measurments.\n\n"), 
                            fmt(length(x = contrib_truck_eval))), 
                  "Conclusion :")
  dominant_var <- names(x = mae_contrib)[ord[1]]
  if (dominant_var == "Speed") {
    diag_lines <- c(diag_lines, 
                    paste("\tSpeed is the main source of error in dB", 
                          "\t-> See page 6 for the impact of OSM speed", 
                          "vs predicted."))
  } else if (dominant_var == "Flow") {
    diag_lines <- c(diag_lines, 
                    "\tFlow is the main source of error in dB", 
                    "\t-> Improving the flow model would reduce the error", 
                    "the most.")
  } else {
    diag_lines <- c(diag_lines,
                    "\tThe %%HGV is the main source of error in dB",
                    "\t-> But the reference data is limited.")
  }
  text(x          = 0.02, 
       y          = seq(from       = 0.95, 
                        to         = 0.95 - 0.055 * (length(x = diag_lines) - 1),
                        length.out = length(x = diag_lines)),
       labels     = diag_lines, 
       adj        = c(0, 0.5), 
       cex        = 0.82, 
       family     = "mono")

  # ============================================================================
  # PAGE 3: Which periods are the best/the worst?
  # ============================================================================
  if ("period" %in% names(x = emission_test)) {
    # D/E/N analysis
    den_df <- emission_test[emission_test$period %in% c("D", "E", "N"), , 
                            drop = FALSE]
    # Hourly analysis (h0...h23)
    hour_df <- emission_test[grepl(pattern = "^h([0-9]|1[0-9]|2[0-3])$", 
                                   x       = emission_test$period), , 
                             drop = FALSE]

    den_ok <- nrow(x = den_df) > 0
    hour_ok <- nrow(x = hour_df) > 0

    if (den_ok || hour_ok) {
      par(mfrow = c(2, 2), mar = c(5, 4.5, 3.5, 1))

      # 3a: Bias per period D/E/N
      if (den_ok) {
        den_stats <- summarize_group_metrics(den_df, "period")
        # Reorder D, E, N
        den_order <- match(x     = c("D", "E", "N"), 
                           table = den_stats$group)
        den_order <- den_order[!is.na(x = den_order)]
        den_stats <- den_stats[den_order, ]
        den_lbl   <- sprintf("%s\n(n=%s)", den_stats$group, fmt(den_stats$n))
        den_cols  <- c(D = "#FFD700", E = "#FF8C00", N = "#1a1a6e")
        
        bp_den <- barplot(height    = den_stats$bias_xgb, 
                          names.arg = den_lbl,
                          col       = den_cols[den_stats$group], 
                          border    = NA,
                          main      = "Bias in dB per period D/E/N",
                          ylab      = "Biais (dB)", 
                          cex.main  = 1.1)
        abline(h   = 0, 
               col = "red", 
               lty = 2)
        text(x      = bp_den, 
             y      = den_stats$bias_xgb,
             labels = sprintf("%+.2f", den_stats$bias_xgb),
             pos    = ifelse(test = den_stats$bias_xgb >= 0, 
                             yes  = 3, 
                             no   = 1), 
             cex    = 0.9, 
             font   = 2)
      } else {
        plot.new()
        title(main = "D/E/N: insufficient data")
      }

      # 3b: MAE per period D/E/N
      if (den_ok) {
        bp_den2 <- barplot(height    = den_stats$mae_xgb, 
                           names.arg = den_lbl,
                           col       = den_cols[den_stats$group], 
                           border    = NA,
                           main      = "MAE in dB per period D/E/N",
                           ylab      = "MAE (dB)", cex.main = 1.1)
        text(x      = bp_den2, 
             y      = den_stats$mae_xgb,
             labels = sprintf("%.2f", den_stats$mae_xgb),
             pos    = 3, 
             cex    = 0.9, 
             font   = 2)
      } else {
        plot.new()
        title(main = "D/E/N: insufficient data")
      }

      # 3c: MAE hourly
      if (hour_ok) {
        hour_stats          <- summarize_group_metrics(hour_df, "period")
        hour_stats$hour_num <- as.integer(
                                  x = sub(pattern     = "^h", 
                                          replacement = "", 
                                          x           =  hour_stats$group))
        hour_stats          <- hour_stats[order(hour_stats$hour_num), ]
        labels_h            <- paste0("h", sprintf("%02d", hour_stats$hour_num))

        # Color by D/E/N membership
        h_cols <- ifelse(test = hour_stats$hour_num >= 6 & 
                                hour_stats$hour_num < 18, 
                         yes  = "#FFD700", 
                         no = ifelse(test = hour_stats$hour_num >= 18 &       
                                            hour_stats$hour_num < 22, 
                                     yes  = "#FF8C00", 
                                     no   = "#1a1a6e"))

        plot(x        = hour_stats$hour_num, 
             y        = hour_stats$mae_xgb, 
             type     = "b", 
             pch      = 16,
             col      = h_cols, 
             lwd      = 2,
             xlab     = "Hour", 
             ylab     = "MAE (dB)",
             main     = "MAE per hour (h0–h23)", 
             cex.main = 1.1,
             xaxt     = "n")
        axis(side     = 1, 
             at       = hour_stats$hour_num, 
             labels   = labels_h, 
             cex.axis = 0.7, 
             las      = 2)
        # Shade D/E/N zones
        rect(xleft   = 5.5, 
             xright  = 17.5, 
             ybottom = par("usr")[3], 
             ytop    = par("usr")[4], 
             col     = rgb(red   = 1, 
                           green = 0.84, 
                           blue  = 0, 
                           alpha = 0.08), 
             border  = NA)
        rect(xleft   = 17.5, 
             xright  = 21.5, 
             ybottom = par("usr")[3], 
             ytop    = par("usr")[4], 
             col     = rgb(red   = 1, 
                           green = 0.55, 
                           blue  = 0, 
                           alpha = 0.08), 
             border  = NA)
        # Redraw points on top
        points(x   = hour_stats$hour_num, 
               y   = hour_stats$mae_xgb, 
               pch = 16, 
               col = h_cols, 
               cex = 1.2)
        lines(x   = hour_stats$hour_num, 
              y   = hour_stats$mae_xgb, 
              col = "grey40")
        legend(x      = "topright", 
               legend = c("Day (D)", "Evening (E)", "Night (N)"),
               fill   = c("#FFD700", "#FF8C00", "#1a1a6e"), 
               cex    = 0.7, 
               bty    = "n")

        # Find best/worst hours
        best_h  <- hour_stats$group[which.min(x = hour_stats$mae_xgb)]
        worst_h <- hour_stats$group[which.max(x = hour_stats$mae_xgb)]
      } else {
        plot.new()
        title(main = "Insufficient hourly data")
        best_h  <- "N/A"
        worst_h <- "N/A"
      }

      # 3d: Hourly bias with shaded D/E/N zones
      if (hour_ok) {
        plot(x        = hour_stats$hour_num, 
             y        = hour_stats$bias_xgb, 
             type     = "b", 
             pch      = 16,
             col      = h_cols, 
             lwd      = 2,
             xlab     = "Hour", 
             ylab     = "Bias (dB)",
             main     = "Bias per hour (h0–h23)", 
             cex.main = 1.1,
             xaxt     = "n")
        axis(side     = 1, 
             at       = hour_stats$hour_num, 
             labels   = labels_h, 
             cex.axis = 0.7, 
             las      = 2)
        abline(h = 0, col = "red", lty = 2, lwd = 1.5)
        rect(xleft   = 5.5, 
             xright  = 17.5, 
             ybottom = par("usr")[3], 
             ytop    = par("usr")[4], 
             col     = rgb(red   = 1, 
                           green = 0.84, 
                           blue  = 0, 
                           alpha = 0.08), 
             border  = NA)
        rect(xleft   = 17.5, 
             xright  = 21.5, 
             ybottom = par("usr")[3], 
             ytop    = par("usr")[4], 
             col     = rgb(red   = 1, 
                           green = 0.55, 
                           blue  = 0, 
                           alpha = 0.08), 
             border  = NA)
        points(x   = hour_stats$hour_num, 
               y   = hour_stats$bias_xgb, 
               pch = 16, 
               col = h_cols, 
               cex = 1.2)
        lines(x   = hour_stats$hour_num, 
              y   = hour_stats$bias_xgb, 
              col = "grey40")
      } else {
        plot.new()
        title(main = "Insufficient hourly data")
      }

      mtext(text  = "Prediction quality in dB per time period",
            side  = 3, 
            outer = FALSE, 
            line  = -1.5, 
            cex   = 0.7, 
            font  = 3)
    }

    # Page 3bis: Textual diagnosis of periods
    if (den_ok || hour_ok) {
      par(mfrow = c(1, 1), 
          mar   = c(2, 2, 3, 2))
      plot.new()
      title(main     = "Diagnosis by period: why are some better than others?",
            cex.main = 1.1)

      period_diag <- c("Analysis of temporal periods :\n")

      if (den_ok) {
        best_den    <- den_stats$group[which.min(x = den_stats$mae_xgb)]
        worst_den   <- den_stats$group[which.max(x = den_stats$mae_xgb)]
        period_diag <- c(period_diag, 
                         "\tRegulatory periods (D/E/N):", 
                         sprintf(paste("\t\tBest: %s (MAE = %.2f dB,", 
                                       "Bias = %+.2f dB)"), 
                         best_den, 
                         den_stats$mae_xgb[den_stats$group == best_den], 
                         den_stats$bias_xgb[den_stats$group == best_den]), 
                         sprintf(paste("\t\tWorst: %s (MAE = %.2f dB,", 
                                       "Bias = %+.2f dB)\n"), 
                                       worst_den, 
                                       den_stats$mae_xgb[den_stats$group == worst_den], 
                                       den_stats$bias_xgb[den_stats$group == worst_den]))

        # Decompose source for best/worst DEN
        for (pp in c(best_den, worst_den)) {
          sub_idx <- which(x = emission_test$period == pp)
          if (length(x = sub_idx) > 5) {
            sub_fl      <- mean(x     = abs(x = contrib_flow[sub_idx]), 
                                na.rm = TRUE)
            sub_sp      <- mean(x     = abs(x = contrib_speed[sub_idx]), 
                                na.rm = TRUE)
            dominant    <- if (sub_fl > sub_sp) {"flow"} else {"speed"}
            period_diag <- c(period_diag, 
                             sprintf(paste("\t\t%s: main source = %s", 
                                           "(MAE flux=%.2f,", 
                                           "MAE vitesse=%.2f dB)\n"), 
                                     pp, dominant, sub_fl, sub_sp))
          }
        }
      }

      if (hour_ok) {
        period_diag <- c(period_diag, 
                         "\tHourly periods (h0–h23) :", 
                         sprintf("\t\tBest hour : %s (MAE = %.2f dB)", 
                                 best_h, min(x = hour_stats$mae_xgb)), 
                         sprintf("\t\tWorst hour      : %s (MAE = %.2f dB)\n",
                                 worst_h, max(x = hour_stats$mae_xgb)), 
                         "\tExplanation:", 
                         "\t\tThe nighttime hours (h22–h05) often have a higher", 
                         "bias because the weak flows are relative and a small", 
                         "absolute error on the flow produces a large relative", 
                         "error in dB (log effect).", 
                         "\t\tDuring the day (h07–h18), the flows are stables", 
                         "and better predicted.")
      }

      text(x          = 0.02, 
           y          = seq(from       = 0.95, 
                            to         = 0.95 - 0.045 * (length(x = period_diag) - 1), 
                            length.out = length(x = period_diag)),
           labels     = period_diag, 
           adj        = c(0, 0.5), 
           cex        = 0.78, 
           family     = "mono")
    }

    # --- Page 3ter: Weekday vs Weekend (h*_wd / h*_we) ---
    hour_wd_df <- emission_test[grepl(pattern = "^h[0-9]+_wd$", 
                                      x       = emission_test$period), , 
                                drop = FALSE]
    hour_we_df <- emission_test[grepl(pattern = "^h[0-9]+_we$", 
                                      x = emission_test$period), , 
                                drop = FALSE]
    wd_ok <- nrow(x = hour_wd_df) > 0
    we_ok <- nrow(x = hour_we_df) > 0

    if (wd_ok && we_ok) {
      par(mfrow = c(2, 2), 
          mar   = c(5, 4.5, 3.5, 1))

      # Compute stats for weekday hours
      wd_stats          <- summarize_group_metrics(hour_wd_df, "period")
      wd_stats$hour_num <- as.integer(x = sub(pattern    = "^h([0-9]+)_wd$", 
                                              replacement = "\\1", 
                                              x           = wd_stats$group))
      wd_stats          <- wd_stats[order(wd_stats$hour_num), ]

      # Compute stats for weekend hours
      we_stats <- summarize_group_metrics(hour_we_df, "period")
      we_stats$hour_num <- as.integer(x = sub(pattern     = "^h([0-9]+)_we$", 
                                              replacement = "\\1", 
                                              x           = we_stats$group))
      we_stats <- we_stats[order(we_stats$hour_num), ]

      # 3ter-a: MAE weekday vs weekend
      y_lim_mae    <- range(c(wd_stats$mae_xgb, we_stats$mae_xgb), 
                            na.rm = TRUE)
      y_lim_mae[1] <- max(0, y_lim_mae[1] - 0.5)
      y_lim_mae[2] <- y_lim_mae[2] + 0.5
      plot(x        = wd_stats$hour_num, 
           y        = wd_stats$mae_xgb, 
           type     = "b", 
           pch      = 16,
           col      = "#2166AC", 
           lwd      = 2, 
           ylim     = y_lim_mae,
           xlab     = "Hour", 
           ylab     = "MAE (dB)",
           main     = "MAE per hour: Weekday vs Weekend", 
           cex.main = 1.1,
           xaxt     = "n")
      lines(x    = we_stats$hour_num, we_stats$mae_xgb, 
            type = "b", 
            pch  = 17,
            col  = "#B2182B", 
            lwd  = 2)
      axis(side     = 1, 
           at       = 0:23, 
           labels   = paste0("h", sprintf("%02d", 0:23)),
           cex.axis = 0.65, 
           las      = 2)
      legend(x      = "topright", 
             legend = c("Weekday (wd)", "Weekend (we)"),
             col    = c("#2166AC", "#B2182B"), 
             pch    = c(16, 17), 
             lwd    = 2,
             cex    = 0.8, 
             bty    = "n")

      # 3ter-b: Bias weekday vs weekend
      y_lim_bias <- range(c(wd_stats$bias_xgb, we_stats$bias_xgb), 
                          na.rm = TRUE)
      y_lim_bias <- c(y_lim_bias[1] - 0.5, y_lim_bias[2] + 0.5)
      plot(x        = wd_stats$hour_num, 
           y        = wd_stats$bias_xgb, 
           type     = "b", 
           pch      = 16,
           col      = "#2166AC", 
           lwd      = 2, 
           ylim     = y_lim_bias,
           xlab     = "Hour", 
           ylab     = "Bias (dB)",
           main     = "Bias per hour: Weekday vs Weekend", 
           cex.main = 1.1,
           xaxt     = "n")
      lines(x    = we_stats$hour_num, 
            y    = we_stats$bias_xgb, 
            type = "b", 
            pch  = 17,
            col  = "#B2182B", 
            lwd  = 2)
      axis(side     = 1, 
           at       = 0:23, 
           labels   = paste0("h", sprintf("%02d", 0:23)),
           cex.axis = 0.65, 
           las      = 2)
      abline(h   = 0, 
             col = "red", 
             lty = 2, 
             lwd = 1.5)
      legend(x      = "topright", 
             legend = c("Weekday (wd)", "Weekend (we)"),
             col    = c("#2166AC", "#B2182B"), 
             pch    = c(16, 17), 
             lwd    = 2,
             cex    = 0.8, 
             bty    = "n")

      # 3ter-c: Hourly workforce (wd vs we)
      n_wd   <- wd_stats$n
      n_we   <- we_stats$n
      bp_mat <- rbind(n_wd, n_we)
      colnames(x = bp_mat) <- paste0("h", sprintf("%02d", wd_stats$hour_num))
      barplot(height    = bp_mat, 
              beside    = TRUE, 
              col       = c("#2166AC", "#B2182B"),
              border    = NA, 
              las       = 2, 
              cex.names = 0.65,
              main      = "Test workforce per hour and type of day",
              ylab      = "Number of sensors")
      legend(x      = "topright", 
             legend = c("Weekday", "Weekend"),
             fill   = c("#2166AC", "#B2182B"), 
             cex    = 0.8, 
             bty    = "n")

      # 3ter-d: Textual diagnosis
      plot.new()
      title(main     = "Diagnosis Weekday/Weekend", 
            cex.main = 1.1)
      wd_global_mae  <- mean(x = hour_wd_df$abs_db_error, na.rm = TRUE)
      we_global_mae  <- mean(x = hour_we_df$abs_db_error, na.rm = TRUE)
      wd_global_bias <- mean(x = hour_wd_df$db_error, na.rm = TRUE)
      we_global_bias <- mean(x = hour_we_df$db_error, na.rm = TRUE)
      best_wd_h      <- wd_stats$group[which.min(x = wd_stats$mae_xgb)]
      worst_wd_h     <- wd_stats$group[which.max(x = wd_stats$mae_xgb)]
      best_we_h      <- we_stats$group[which.min(x = we_stats$mae_xgb)]
      worst_we_h     <- we_stats$group[which.max(x = we_stats$mae_xgb)]
      wd_we_diag     <- c(
        sprintf("\tWeekday: MAE = %.2f dB, Bias = %+.2f dB (n = %s)",
                wd_global_mae, wd_global_bias, fmt(nrow(x = hour_wd_df))),
        sprintf("\tWeekend: MAE = %.2f dB, Bias = %+.2f dB (n = %s)\n",
                we_global_mae, we_global_bias, fmt(nrow(x = hour_we_df))),
        sprintf("\tBest hour of weekday: %s (MAE = %.2f dB)", 
                best_wd_h, min(x = wd_stats$mae_xgb)),
        sprintf("\tWorst hour of weekday: %s (MAE = %.2f dB)",
                worst_wd_h, max(x = wd_stats$mae_xgb)),
        sprintf("\tBest hour of weekend: %s (MAE = %.2f dB)",
                best_we_h, min(x = we_stats$mae_xgb)),
        sprintf("\tWorst hour of weekend: %s (MAE = %.2f dB)\n",
                worst_we_h, max(x = we_stats$mae_xgb)),
        if (abs(x = wd_global_mae - we_global_mae) < 0.3) {
          "\tConclusion: Similar performance during the weekday/weekend."
        } else if (wd_global_mae < we_global_mae) {
          sprintf(paste("\tConclusion: Better performance during the weekday", 
                        "(ΔMAE = %.2f dB)."), 
                  we_global_mae - wd_global_mae)
        } else {
          sprintf(paste("\tConclusion: Better performance during the weekend", 
                        "(ΔMAE = %.2f dB)."), 
                  wd_global_mae - we_global_mae)
        }
      )
      text(x          = 0.02, 
           y          = seq(from       = 0.90, 
                            to         = 0.90 - 0.06 * (length(x = wd_we_diag) - 1), 
                            length.out = length(x = wd_we_diag)),
           length.out = length(x = wd_we_diag),
           labels     = wd_we_diag, 
           adj        = c(0, 0.5), 
           cex        = 0.82, 
           family     = "mono")

      mtext(text = "Comparison Weekday/Weekend - Quality of hourly predictions",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
    }
  }

  # ============================================================================
  # PAGE 4: Where is the prediction better/worse? Analysis by highway type
  # ============================================================================
  if ("highway" %in% names(x = emission_test)) {
    hw_stats <- summarize_group_metrics(emission_test, "highway")
    hw_stats <- hw_stats[order(-hw_stats$n), ]
    min_n_hw <- 20
    hw_stats <- hw_stats[hw_stats$n >= min_n_hw, , drop = FALSE]

    if (nrow(x = hw_stats) > 0) {
      par(mfrow = c(2, 2), 
          mar   = c(6, 4.5, 3.5, 1))

      # 4a: MAE per highway (sorted by workforce)
      top_n <- head(x = hw_stats, n = 12)
      bp_hw <- barplot(height    = top_n$mae_xgb, 
                       names.arg = sprintf("%s\n(n=%s)", 
                                           top_n$group, fmt(top_n$n)), 
                       las = 2, col = "steelblue", border = NA,
                       main = "MAE per type of route (highway)", 
                       ylab = "MAE (dB)", cex.main = 1.1)
      text(x      = bp_hw, 
           y      = top_n$mae_xgb, 
           labels = sprintf("%.1f", top_n$mae_xgb),
           pos    = 3, 
           cex    = 0.75, 
           font   = 2)

      # 4b: Bias per highway
      bias_cols <- ifelse(top_n$bias_xgb >= 0, "#D6604D", "#4393C3")
      bp_hw2 <- barplot(height    = top_n$bias_xgb,
                        names.arg = sprintf("%s\n(n=%s)", 
                                            top_n$group, fmt(top_n$n)),
                        las       = 2, 
                        col       = bias_cols, 
                        border    = NA,
                        main      = "Bias per type of road (highway)",
                        ylab      = "Bias (dB)", 
                        cex.main  = 1.1)
      abline(h   = 0, 
             col = "red", 
             lty = 2)
      text(x      = bp_hw2, 
           y      = top_n$bias_xgb,
           labels = sprintf("%+.1f", top_n$bias_xgb),
           pos = ifelse(test = top_n$bias_xgb >= 0, 
                        yes  = 3, 
                        no   = 1), 
           cex = 0.75, font = 2)

      # 4c: Main source of error per highway
      hw_source <- sapply(
                    X   = top_n$group, 
                    FUN = function(hw) {
                            sub_idx <- which(x = emission_test$highway == hw)
                            if (length(x = sub_idx) < 5) return(NA_real_)
                            mae_f <- mean(x     = abs(x = contrib_flow[sub_idx]), 
                                          na.rm = TRUE)
                            mae_s <- mean(x     = abs(x = contrib_speed[sub_idx]), 
                                          na.rm = TRUE)
                            return(mae_f - mae_s) # >0 -> flow dominates, 
                                                  # <0 -> speed dominates
                   })
      hw_source_col <- ifelse(test = hw_source > 0, 
                              yes  = "#4393C3", 
                              no   = "#D6604D")
      bp_hw3 <- barplot(height    = hw_source,
                        names.arg = sprintf("%s", top_n$group),
                        las       = 2, 
                        col       = hw_source_col, 
                        border    = NA,
                        main      = "Main source of error per highway",
                        ylab      = "MAE(flow) - MAE(speed) (dB)", 
                        cex.main  = 1.0)
      abline(h   = 0, 
             col = "grey40", 
             lty = 1)
      legend(x      = "topright", 
             legend = c("Flow dominates (>0)", "Speed dominates (<0)"),
             fill   = c("#4393C3", "#D6604D"), 
             cex    = 0.7, 
             bty    = "n")

      # 4d: ΔMAE OSM vs XGBoost per highway
      if (has_osm_speed && all(is.finite(x = top_n$mae_osm))) {
        delta_mae  <- top_n$mae_osm - top_n$mae_xgb
        delta_cols <- ifelse(test = delta_mae > 0, 
                             yes  = "#D6604D", 
                             no   = "#2ca02c")
        bp_hw4 <- barplot(height    = delta_mae,
                          names.arg = sprintf("%s", top_n$group),
                          las       = 2, 
                          col       = delta_cols, 
                          border    = NA,
                          main      = "ΔMAE = MAE(OSM) - MAE(XGBoost)",
                          ylab      = "ΔMAE (dB)", 
                          cex.main  = 1.0)
        abline(h   = 0, 
               col = "grey40")
        legend(x      = "topright", 
               legend = c("Worst OSM (>0)", "Best OSM (<0)"),
               fill   = c("#D6604D", "#2ca02c"), 
               cex    = 0.7, 
               bty    = "n")
      } else {
        plot.new()
        title(main = "OSM speed not available")
      }

      mtext(text  = "dB quality analysis by road type (highway)",
            side  = 3, 
            outer = FALSE, 
            line  = -1.5, 
            cex   = 0.7, 
            font  = 3)

      # Page 4bis: Textual diagnosis highway
      par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
      plot.new()
      title(main     = "Diagnosis per highway: where and why?", 
            cex.main = 1.1)

      best_hw  <- top_n$group[which.min(x = top_n$mae_xgb)]
      worst_hw <- top_n$group[which.max(x = top_n$mae_xgb)]
      hw_diag  <- c("Analysis per type of road:\n", 
                    sprintf(paste("\tBest prediction: '%s'", 
                                  "(MAE = %.2f dB,", 
                                  "Bias = %+.2f dB,", 
                                  "n = %s)"), 
                            best_hw,
                            top_n$mae_xgb[top_n$group == best_hw],
                            top_n$bias_xgb[top_n$group == best_hw],
                            fmt(top_n$n[top_n$group == best_hw])),
        sprintf(paste("\tWorst prediction: '%s'", 
                      "(MAE = %.2f dB,", 
                      "Bias = %+.2f dB,", 
                      "n = %s)\n"),
                worst_hw, 
                top_n$mae_xgb[top_n$group == worst_hw], 
                top_n$bias_xgb[top_n$group == worst_hw], 
                fmt(top_n$n[top_n$group == worst_hw])))

      # Per-highway source analysis
      hw_diag <- c(hw_diag, "\tMain source of error per highway:")
      for (i in seq_len(nrow(x = top_n))) {
        hw_name  <- top_n$group[i]
        sub_idx  <- which(emission_test$highway == hw_name)
        mae_f    <- mean(x = abs(x = contrib_flow[sub_idx]), na.rm = TRUE)
        mae_s    <- mean(x = abs(x = contrib_speed[sub_idx]), na.rm = TRUE)
        dominant <- if (mae_f > mae_s) "flux" else "vitesse"
        hw_diag  <- c(hw_diag, 
                      sprintf(paste("\t\t%-20s: %s", 
                                    "(MAE flow=%.2f, MAE speed=%.2f dB)\n"), 
                              hw_name, dominant, mae_f, mae_s))
      }

      hw_diag <- c(hw_diag, 
                   "\tPrimary and trunk roads often have higher error rates", 
                   "because traffic flow is more variable. Residential roads", 
                   "have low traffic flow with lower absolute error but", 
                   "higher relative error.")

      text(x          = 0.02, 
           y          = seq(from       = 0.95, 
                            to         = 0.95 - 0.04 * (length(x = hw_diag) - 1),
                            length.out = length(x = hw_diag)),
           labels     = hw_diag, 
           adj        = c(0, 0.5), 
           cex        = 0.72, 
           family     = "mono")
    }
  }

  # ============================================================================
  # PAGE 5: Analysis per DEGREE (urban density)
  # ============================================================================
  if ("DEGRE" %in% names(x = emission_test)) {
    deg_stats <- summarize_group_metrics(emission_test, "DEGRE")
    deg_stats <- deg_stats[deg_stats$group != "NA", , drop = FALSE]
    if (nrow(x = deg_stats) > 0) {
      suppressWarnings(expr = deg_num <- as.numeric(x = deg_stats$group))
      if (all(!is.na(x = deg_num))) {
        deg_stats <- deg_stats[order(x = deg_num), ]
      }
      deg_labels <- sprintf("DEGRE %s\n(n=%s)", 
                            deg_stats$group, fmt(deg_stats$n))
      deg_cols   <- colorRampPalette(
                      palette = c("#d73027", "#fee08b", "#1a9850")
                    )(nrow(x = deg_stats))

      par(mfrow = c(2, 2), mar = c(5.5, 4.5, 3.5, 1))

      # 5a: MAE per DEGREE
      bp_d <- barplot(height    = deg_stats$mae_xgb, 
                      names.arg = deg_labels, 
                      las       = 2,
                      col       = deg_cols, 
                      border    = NA,
                      main      = "MAE per DEGREE (urban density)",
                      ylab      = "MAE (dB)", 
                      cex.main  = 1.1)
      text(x      = bp_d, 
           y      = deg_stats$mae_xgb, 
           labels = sprintf("%.2f", deg_stats$mae_xgb),
           pos    = 3, 
           cex    = 0.8, 
           font   = 2)

      # 5b: Bias per DEGREE
      bias_cols_d <- ifelse(test = deg_stats$bias_xgb >= 0, 
                            yes  = "#D6604D", 
                            no   = "#4393C3")
      bp_d2 <- barplot(heigt     = deg_stats$bias_xgb, 
                       names.arg = deg_labels, 
                       las       = 2,
                       col       = bias_cols_d, 
                       border    = NA,
                       main      = "Bias per DEGREE",
                       ylab      = "Bias (dB)", 
                       cex.main  = 1.1)
      abline(h   = 0, 
             col = "red", 
             lty = 2)
      text(x      = bp_d2, 
           y      = deg_stats$bias_xgb,
           labels = sprintf("%+.2f", deg_stats$bias_xgb),
           pos    = ifelse(test = deg_stats$bias_xgb >= 0, 
                           yes  = 3, 
                           no   = 1), 
           cex    = 0.8, 
           font   = 2)

      # 5c: Main source per DEGREE
      deg_source <- sapply(
                      X = deg_stats$group, 
                      FUN = function(dg) {
                              sub_idx <- which(
                                x = as.character(x = emission_test$DEGRE) == dg)
        if (length(x = sub_idx) < 5) {return(NA_real_)}
        mae_f <- mean(x = abs(x = contrib_flow[sub_idx]), na.rm = TRUE)
        mae_s <- mean(x = abs(x = contrib_speed[sub_idx]), na.rm = TRUE)
        return(mae_f - mae_s)
      })
      deg_source_col <- ifelse(test = deg_source > 0, 
                               yes  = "#4393C3", 
                               no   = "#D6604D")
      bp_d3 <- barplot(height    = deg_source,
                       names.arg = sprintf("D%s", deg_stats$group),
                       las       = 2, 
                       col       = deg_source_col, 
                       border    = NA,
                       main      = "Main source of error per DEGREE",
                       ylab      = "MAE(flow) - MAE(speed) (dB)", 
                       cex.main  = 1.0)
      abline(h   = 0, 
             col = "grey40")
      legend(x      = "topright", 
             legend = c("Flow dominates", "Speed dominates"),
             fill   = c("#4393C3", "#D6604D"), 
             cex    = 0.7, 
             bty    = "n")

      # 5d: Comparison A vs B per DEGREE
      if (has_osm_speed && all(is.finite(x = deg_stats$mae_osm))) {
        x     <- seq_len(nrow(x = deg_stats))
        y_lim <- range(c(deg_stats$mae_xgb, deg_stats$mae_osm), na.rm = TRUE)
        plot(x        = x, 
             y        = deg_stats$mae_xgb, 
             type     = "b", 
             pch      = 16, 
             col      = "#1f77b4",
             xaxt     = "n", 
             ylim     = y_lim, 
             lwd      = 2,
             xlab     = "DEGREE", 
             ylab     = "MAE (dB)",
             main     = "MAE: predicted speed vs OSM per DEGREE", 
             cex.main = 1.0)
        lines(x    = x, 
              y    = deg_stats$mae_osm, 
              type = "b", 
              pch  = 17, 
              col  = "#d62728", 
              lwd  = 2)
        axis(side   = 1, 
             at     = x, 
             labels = paste0("D", deg_stats$group))
        legend(x      = "topleft", 
               legend = c("Predicted speed", "OSM speed"),
               col    = c("#1f77b4", "#d62728"), 
               pch    = c(16, 17), 
               lwd    = 2, 
               bty    = "n", 
               cex    = 0.8)
      } else {
        plot.new()
        title(main = "OSM speed not available")
      }

      mtext(text  = "Analysis of dB quality by urban density (DEGRE INSEE)",
            side  = 3, 
            outer = FALSE, 
            line  = -1.5, 
            cex   = 0.7, 
            font  = 3)
    }
  }

  # ============================================================================
  # PAGE 6: Regulatory speed (OSM) vs. predicted speed - impact on dB
  # ============================================================================
  if (has_osm_speed) {
    par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1))

    # 6a: Superimposed histograms of dB errors
    breaks_range <- range(c(emission_test$db_error, emission_test$db_error_osm), 
                          na.rm = TRUE)
    breaks_seq <- seq(from       = floor(x = breaks_range[1]), 
                      to         = ceiling(breaks_range[2]), 
                      length.out = 60)
    h_xgb <- hist(x      = emission_test$db_error, 
                  breaks = breaks_seq, 
                  plot   = FALSE)
    h_osm <- hist(x      = emission_test$db_error_osm, 
                  breaks = breaks_seq, 
                  plot   = FALSE)
    ylim_max <- max(x = c(h_xgb$counts, h_osm$counts))
    plot(x        = h_xgb, 
         col      = rgb(red   = 0.2, 
                        green = 0.4, 
                        blue  = 0.8, 
                        alpha = 0.5), 
         border   = "white",
         main     = "Error dB: predicted speed vs OSM speed",
         xlab     = "Erreur = Predicted - Observed (dB)", 
         ylim     = c(0, ylim_max * 1.1),
         cex.main = 1.1)
    plot(x      = h_osm, 
         col    = rgb(red   = 0.9, 
                      green = 0.4, 
                      blue  = 0.2, 
                      alpha = 0.5), 
         border = "white", 
         add    = TRUE)
    abline(v   = 0, 
           col = "red", 
           lwd = 2)
    legend(x      = "topright", 
           legend = c(sprintf("Predicted speed (bias=%+.1f dB)", db_bias), 
                      sprintf("OSM speed (bias=%+.1f dB)", db_bias_osm)),
           fill   = c(rgb(red   = 0.2, 
                          green = 0.4, 
                          blue  = 0.8, 
                          alpha = 0.5), 
                      rgb(red   = 0.9, 
                          green = 0.4, 
                          blue  = 0.2, 
                          alpha = 0.5)),
            cex   = 0.75, 
            bty   = "n")

    # 6b: CDF comparison
    abs_xgb_sorted <- sort(x = emission_test$abs_db_error)
    abs_osm_sorted <- sort(x = emission_test$abs_db_error_osm)
    ecdf_xgb       <- seq_along(along.with = abs_xgb_sorted) / 
                                             length(x = abs_xgb_sorted)
    ecdf_osm       <- seq_along(along.with = abs_osm_sorted) / 
                                             length(x = abs_osm_sorted)
    xlim_max <- max(x     = c(abs_xgb_sorted, abs_osm_sorted), 
                    na.rm = TRUE)
    plot(x        = abs_xgb_sorted, 
         y        = ecdf_xgb, 
         type     = "l", 
         lwd      = 2, 
         col      = rgb(red   = 0.2, 
                        blue  = 0.4, 
                        green = 0.8),
         xlim     = c(0, min(xlim_max, 20)),
         xlab     = "Absolute error (dB)", 
         ylab     = "Proportion cumulée",
         main     = "CDF: predicted speed vs OSM", 
         cex.main = 1.1)
    lines(x   = abs_osm_sorted, 
          y   = ecdf_osm, 
          lwd = 2, 
          col = rgb(red   = 0.9, 
                    blue  = 0.4, 
                    green = 0.2))
    abline(h   = 0.9, 
           col = "grey60", 
           lty = 2)
    legend(x      = "bottomright", 
           legend = c(sprintf("Predicted (MAE=%.1f dB)", db_mae),  
                      sprintf("OSM (MAE=%.1f dB)", db_mae_osm)), 
           lwd    = 2, 
           col    = c(rgb(red   = 0.2, 
                          blue  = 0.4, 
                          green = 0.8), 
                      rgb(red   = 0.9, 
                          blue  = 0.4, 
                          green = 0.2)), 
           cex    = 0.75, 
           bty    = "n")

    # 6c: Scatter prdicted speed vs OSM
    plot(x   = emission_test$pred_speed, 
         y   = emission_test$osm_speed,
         pch = 16, cex = 0.3, col = rgb(red   = 0, 
                                          blue  = 0, 
                                          green = 0, 
                                          alpha = 0.15),
         xlab = "Predicted speed XGBoost (km/h)",
         ylab = "OSM speed (km/h)",
         main = "Predicted speed vs OSM speed", cex.main = 1.1)
    abline(a   = 0, 
           b   = 1, 
           col = "red", 
           lwd = 2)
    speed_cor <- cor(x   = emission_test$pred_speed, 
                     y   = emission_test$osm_speed, 
                     use = "complete.obs")
    legend(x      = "topleft", 
           legend = c(sprintf("r = %.2f", speed_cor), 
                      sprintf("Median predicted = %.0f km/h", 
                              median(x = emission_test$pred_speed, na.rm = TRUE)), 
                      sprintf("Médiane OSM = %.0f km/h", 
                              median(x = emission_test$osm_speed, na.rm = TRUE))), 
            cex    = 0.75, 
            bty    = "n")

    # 6d: Verbatim verdict
    plot.new()
    title(main     = "Verdict: Does OSM speed improve or degrade the predictions?", 
          cex.main = 1.1)

    delta_bias <- db_bias_osm - db_bias
    delta_mae  <- db_mae_osm  - db_mae
    delta_rmse <- db_rmse_osm - db_rmse

    summary_df = pandas.DataFrame({
        "Metric"           : ["Bias (dB)", "MAE (dB)", "RMSE (dB)"],
        "Predicted speed"  : [db_bias, db_mae, db_rmse],
        "OSM speed"        : [db_bias_osm, db_mae_osm, db_rmse_osm],
        "Δ (OSM-Predicted)": [delta_bias, delta_mae, delta_rmse]
    })

    print("\nComparison of prediction quality in dB on the test sections:\n")
    print(summary_df.to_string(index=False))

    if (delta_mae > 0.05) {
      verdict_lines <- c(
        verdict_lines,
        sprintf("\tVERDICT: The OSM speed DEGRADES the prediction by %.2f dB (MAE).", 
                delta_mae), 
                "\t-> The XGBoost predicted speed is preferred.")
    } else if (delta_mae < -0.05) {
      verdict_lines <- c(
        verdict_lines,
        sprintf("\tVERDICT: The OSM speed IMPROVES the prediction by %.2f dB (MAE).", 
                abs(x = delta_mae)), 
                "\t-> The regulatory OSM speed would be a better choice.")
    } else {
      verdict_lines <- c(
        verdict_lines,
        "\tVERDICT: The two speeds give quasi-identical results.",
        "\t-> Difference < 0.05 dB in MAE, indifferent choice.")
    }

    # Calculate percentage of observations using OSM speed as speed_obs
    pct_osm_speed <- round(100 * mean(!emission_test$has_measured_speed))

    verdict_lines <- c(
      verdict_lines, "",
      "\tNote: 'observed' = CNOSSOS(flow_obs, HGV%_obs, speed_obs).",
      sprintf("\t%.0f%% of AVATAR sensors use OSM maxspeed as 'speed_obs'.", pct_osm_speed), 
              "\tThe speed bias therefore partially includes a reference bias.")

    text(x      = 0.02, 
         y      = seq(from       = 0.95, 
                          to         = 0.95 - 0.055 * (length(x = verdict_lines) - 1),
                          length.out = length(x = verdict_lines)),
         labels = verdict_lines, 
         adj    = c(0, 0.5), 
         cex    = 0.78, 
         family = "mono")

    mtext(text  = "Comparison of the predicted speed vs regulatory OSM speed", 
          side  = 3, 
          outer = FALSE, 
          line  = -1.5, 
          cex   = 0.7, 
          font  = 3)
  }

  # ============================================================================
  # PAGE 6bis: Top errors on the test sections (real-life examples)
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main     = "Top errors dB on the test sections (real-life examples)", 
        cex.main = 1.1)

  top_k   <- min(25L, nrow(x = emission_test))
  idx_top <- order(emission_test$abs_db_error, decreasing = TRUE)[seq_len(top_k)]
  top_err <- emission_test[idx_top, , drop = FALSE]

  # Identify dominant source of error
  src_dom <- c("Flow", "Speed", "%HGV")[
    max.col(m = abs(x = cbind(contrib_flow[idx_top], 
                              contrib_speed[idx_top], 
                              contrib_truck[idx_top])))]

  # Build summary table
  df_top <- data.frame(
              rank             = seq_len(nrow(top_err)),
              osm_id           = top_err$osm_id,
              period           = top_err$period,
              hwy              = substr(x     = top_err$highway, 
                                        start =  1, 
                                        stop  = 13),
              DEGRE            = substr(x     = top_err$DEGRE, 
                                        start = 1, 
                                        stop  = 6),
              err_dB           = top_err$db_error,
              abs_dB           = top_err$abs_db_error,
              src_dom          = src_dom,
              flow_dB          = contrib_flow[idx_top],
              speed_dB         = contrib_speed[idx_top],
              truck_dB         = contrib_truck[idx_top],
              stringsAsFactors = FALSE)

  # Header + table + explanation
  top_lines <- c(
    sprintf("Top %d absolute errors (|Pred - Obs|)\n", top_k), 
    capture.output(print(df_top, row.names = FALSE)), 
    "\nInterpretation: 'src_dom' indicates which variable contributes the most",
    "to the error in absolute terms (Flow, Speed, or %%HGV).",
    "These cases help identify where the modelling pipeline could be improved."
    )

  text(x = 0.02,
       y      = seq(from       = 0.96, 
                    to         = 0.96 - 0.03 * (length(x = top_lines) - 1), 
                    length.out = length(x = top_lines)), 
       labels = top_lines, 
       adj    = c(0, 0.5), 
       cex    = 0.66, 
       family = "mono")

  # ============================================================================
  # PAGE 7: Final summary and verdict
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main     = "Summary of the acoustic assessment on test sections",
        cex.main = 1.2)

  synth_lines <- c(
    sprintf("Data: %s sections-periods evaluated via CNOSSOS-EU 2020\s", 
            fmt(nrow(x = emission_test))),
    sprintf("of which %s with measured speed, %s with measured %%HGV\n",
            fmt(sum(emission_test$has_measured_speed, na.rm = TRUE)),
            fmt(sum(emission_test$has_measured_truck, na.rm = TRUE))),
    "\n--- Overall quality ---",
    sprintf("\tBias = %+.2f dB | MAE = %.2f dB | RMSE = %.2f dB\n", 
            db_bias, db_mae, db_rmse))

  # Source of error
  ord         <- order(mae_contrib, decreasing = TRUE)
  synth_lines <- c(synth_lines,
    "\n--- Main source of error ---",
    sprintf("\t1. %s (MAE = %.2f dB, bias = %+.2f dB)",
            c(Flow = "Flow", `Truck %` = "%HGV", Speed = "Speed")[
              names(x = mae_contrib)[ord[1]]],
            mae_contrib[ord[1]], mean_contrib[ord[1]]),
    sprintf("\t2. %s (MAE = %.2f dB, bias = %+.2f dB)",
            c(Flow = "Flow", `Truck %` = "%HGV", Speed = "Speed")[
              names(x = mae_contrib)[ord[2]]],
            mae_contrib[ord[2]], mean_contrib[ord[2]]),
    sprintf("\t3. %s (MAE = %.2f dB, bias = %+.2f dB)\n",
            c(Flow = "Flow", `Truck %` = "%HGV", Speed = "Speed")[
              names(x = mae_contrib)[ord[3]]],
            mae_contrib[ord[3]], mean_contrib[ord[3]]))

  # Verdict concerning speed
  if (has_osm_speed) {
    delta_mae_v <- db_mae_osm - db_mae
    if (delta_mae_v > 0.05) {
      synth_lines <- c(synth_lines, 
        "\t--- Regulatory speed vs predicted ---", 
        sprintf("\tUsing the OSM speed  Using OSM speed would DEGRADE the MAE of %.2f dB.", 
                delta_mae_v), 
        "\t\t-> Keep the speed predicted by XGBoost.\n")
    } else if (delta_mae_v < -0.05) {
      synth_lines <- c(synth_lines,
        "\t--- Regulatory speed vs predicted ---",
        sprintf("\tUsing the OSM speed  Using OSM speed would IMPROVE the MAE of %.2f dB.", 
                abs(x = delta_mae_v)),
        "\t\t-> Consider using the OSM speed for predictions.", "")
    } else {
      synth_lines <- c(synth_lines,
        "\t--- Regulatory speed vs predicted ---",
        "\t\tNegligible difference between the two speeds (< 0.05 dB).", "")
    }
  }

  # Best/worst
  if ("highway" %in% names(x = emission_test) && exists(x = "top_n") && nrow(x = top_n) > 0) {
    best_hw_s   <- top_n$group[which.min(x = top_n$mae_xgb)]
    worst_hw_s  <- top_n$group[which.max(x = top_n$mae_xgb)]
    synth_lines <- c(synth_lines, 
      "\n--- By road type ---",
      sprintf("\tBest: %s (MAE = %.2f dB)", best_hw_s,
              top_n$mae_xgb[top_n$group == best_hw_s]),
      sprintf("\tWorst: %s (MAE = %.2f dB)\n", worst_hw_s,
              top_n$mae_xgb[top_n$group == worst_hw_s]))
  }

  synth_lines <- c(synth_lines,
    "\n--- Reading the report ---",
    "\tBias > 0: Overestimation of sound level",
    "\tBias < 0: Underestimation of sound level",
    "\tObs = CNOSSOS(flow_obs, truck%_obs, speed_obs) on test AVATAR sensors")

  text(x     = 0.02, 
       y     = seq(from       = 0.95, 
                   to         = 0.95 - 0.032 * (length(x = synth_lines) - 1), 
                   length.out = length(x = synth_lines)), 
      labels = synth_lines, 
      adj    = c(0, 0.5), 
      cex    = 0.72, 
      family = "mono")

  par(old_par)
  grDevices::dev.off()

  pipeline_message(
    sprintf("Emission dB error report saved to %s", 
            rel_path(emission_pdf_path)),
    process = "save")
} else {
  pipeline_message(
    "Emission dB analysis skipped: no usable rows for emission comparison",
    process = "warning")
}

pipeline_message("Acoustic emission error analysis completed", 
                 level = 1, progress = "end", process = "valid")

assign(x     = "xgb_models_with_ratios", 
       value = models_list, 
       envir = .GlobalEnv)

pipeline_message("Successfully trained learning model", 
                 level = 0, progress = "end", process = "valid")
