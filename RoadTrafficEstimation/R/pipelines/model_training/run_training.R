# ==============================================================================
# RUN XGBOOST TRAINING LEARNING MODEL
# ==============================================================================

pipeline_message("Training the learning model", level = 0, 
                 progress = "start", process = "learn")

# ==============================================================================
# Architecture:
#   1. Base models for period D (day): 
#      - flow_D: Total traffic (vehicles/hour)
#      - truck_pct_D: Truck percentage (HGV/TV as percentage 0-100%)
#      - speed_D: Speed ratio to OSM maxspeed (aggregate_speed / speed)
#   2. Ratio models for other periods (E, N, h0-h23):
#      - ratio_flow_P: flow_P / flow_D
#      - ratio_truck_pct_P: truck_pct_P / truck_pct_D (ratio of percentages)
#      - ratio_speed_P: speed_P / speed_D
#   3. Final predictions:
#      - flow_P = flow_D x ratio_flow_P
#      - truck_pct_P = truck_pct_D x ratio_truck_pct_P
#      - HGV_P = flow_P x (truck_pct_P / 100)
#      - LV_P = flow_P - HGV_P
#      - speed_D_abs = osm_speed x speed_D
#      - speed_P = speed_D_abs x ratio_speed_P
# Benefits:
#   - Truck percentage more stable than absolute truck count
#   - Ratios capture temporal patterns (more trucks at night on highways)
#   - Prevents HGV > TV inconsistencies
#   - Better performance for periods with sparse data
# ==============================================================================

# ------------------------------------------------------------------------------
# Retrieving the configuration list
# ------------------------------------------------------------------------------
cfg_g <- CFG$global
cfg_data <- CFG$data_prep
cfg_train <- CFG$training

# ------------------------------------------------------------------------------
# Load training data for the learning model
# ------------------------------------------------------------------------------

pipeline_message(sprintf("Loading training data for the learning model from %s", 
                         rel_path(cfg_train$TRAINING_RDS_DATA_FILEPATH)), 
                 level = 1, progress = "start", process = "load")

if (!exists(x= 'training_data', inherits = FALSE) && 
    !file.exists(cfg_train$TRAINING_RDS_DATA_FILEPATH)){
  
  pipeline_message(
    sprintf("File %s doesn't exists. Training data is required to train the model. Run the data preparation scripts to generate the dataset.", 
            cfg_train$TRAINING_RDS_DATA_FILEPATH), 
    process = "stop")
}

training_data <- readRDS(cfg_train$TRAINING_RDS_DATA_FILEPATH)

if (!"ratio_speed_to_osm" %in% names(training_data)) {
  if (all(c("aggregate_speed", "speed") %in% names(training_data))) {
    pipeline_message(
      "ratio_speed_to_osm missing in training dataset: computing fallback from aggregate_speed/speed",
      process = "warning")
    training_data$ratio_speed_to_osm <- ifelse(
      test = !is.na(training_data$aggregate_speed) & 
             training_data$aggregate_speed >= 0 & 
             !is.na(training_data$speed) & 
             training_data$speed > 0, 
      yes = training_data$aggregate_speed / training_data$speed,
      no = NA_real_)
  } else {
    pipeline_message(
      "Missing required target ratio_speed_to_osm and cannot compute fallback (aggregate_speed/speed unavailable). Please ensure the training dataset contains the column ratio_speed_to_osm or the necessary columns to compute it", 
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

available_road_features <- intersect(candidate_road_features, names(training_data))
missing_road_features <- setdiff(candidate_road_features, available_road_features)

if (length(missing_road_features) > 0) {
  pipeline_message(
    sprintf("Some candidate features are missing and will be skipped: %s",
            paste(missing_road_features, collapse = ", ")),
    process = "warning")
}

if (length(available_road_features) == 0) {
  pipeline_message("No candidate road/network features are available in the training dataset. The model will not be able to learn meaningful patterns and predictions will be unreliable. Please ensure the training dataset contains relevant OSM/network features for the model to train on.", 
  process = "stop")}

pipeline_message(
  sprintf("Using %d road/network features: %s",
          length(available_road_features),
          paste(available_road_features, collapse = ", ")),
  process = "info")

# Build formula with existing features + interaction terms
# Interactions capture highway-specific traffic patterns by density zone,
# lane count, and network connectivity
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

if (length(interaction_terms) > 0) {
  pipeline_message(
    sprintf("Added %d interaction terms: %s",
            length(interaction_terms),
            paste(interaction_terms, collapse = ", ")),
    process = "info")
}

# Temporal periods configuration
all_periods <- c("D", "E", "N", paste0("h", 0:23),
                 paste0("h", 0:23, "_wd"), paste0("h", 0:23, "_we"))

# ***************************** #
# Training model configurations #
# ***************************** #

# Base models (period D only)
base_configs <- list(
  flow_D = list(
    name = "Traffic Flow (Day)",
    period = "D",
    target = "aggregate_flow",
    baseline = "flow_D",
    transform = "log10",  # log transform for base models
    min_valid = 1),
  truck_pct_D = list(
    name = "Truck Percentage (Day)",
    period = "D",
    target = "truck_pct",
    baseline = "truck_pct_D",
    transform = NULL,     # Already percentage 0-100
    min_valid = 0),
  speed_D = list(
    name = "Speed Ratio to OSM (Day)",
    period = "D",
    target = "ratio_speed_to_osm",
    baseline = "speed_D",
    transform = NULL,
    min_valid = 0.05))

# Ratio models (all periods except D)
ratio_configs <- list()
ratio_periods <- setdiff(all_periods, "D")

for (p in ratio_periods) {
  ratio_configs[[paste0("ratio_flow_", p)]] <- list(
    name = paste0("Flow Ratio (", p, "/D)"),
    period = p,
    target = "ratio_flow",  
    transform = NULL,  # No log transform for ratios
    min_valid = 0.01)
  
  ratio_configs[[paste0("ratio_truck_pct_", p)]] <- list(
    name = paste0("Truck % Ratio (", p, "/D)"),
    period = p,
    target = "ratio_truck_pct",
    transform = NULL,  # No log transform for ratios
    min_valid = 0.001  # More permissive for truck ratios
  )
  
  ratio_configs[[paste0("ratio_speed_", p)]] <- list(
    name = paste0("Speed Ratio (", p, "/D)"),
    period = p,
    target = "ratio_speed",
    transform = NULL,  # No log transform for ratios
    min_valid = 0.01
  )
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

models_list <- list()
results_summary <- data.frame()
base_test_predictions <- list()

# Pre-compute a shared sensor split for base models (flow_D, truck_pct_D, speed_D)
# This ensures all 3 base models use the SAME test sensors, so the emission dB
# analysis can merge their predictions without losing rows to split mismatch.
shared_base_test_sensors <- NULL
if (isTRUE(cfg_train$USE_GROUPED_SENSOR_SPLIT)) {
  d_data_for_split <- training_data %>% filter(period == "D")
  all_d_sensors <- unique(d_data_for_split$count_point_id)
  if (length(all_d_sensors) >= 5) {
    set.seed(42)  # Different seed from per-model splits to avoid correlation
    n_train <- max(1, floor(0.8 * length(all_d_sensors)))
    shared_base_train_sensors <- sample(all_d_sensors, size = n_train)
    shared_base_test_sensors <- setdiff(all_d_sensors, shared_base_train_sensors)
    pipeline_message(
      sprintf("Shared base-model sensor split: %d train / %d test sensors",
              length(shared_base_train_sensors), length(shared_base_test_sensors)),
      process = "info")
  }
}

get_quality_indicator_column <- function(target_name, available_cols) {
  if (target_name %in% c("aggregate_flow", "ratio_flow")) {
    col <- "perc_flow_predicted"
  } else if (target_name %in% c("aggregate_speed", "ratio_speed", "ratio_speed_to_osm")) {
    col <- "perc_speed_predicted"
  } else if (target_name %in% c("truck_pct", "ratio_truck_pct", 
                                "aggregate_flow_trucks", "ratio_flow_trucks")) {
    col <- "perc_flow_trucks_predicted"
  } else {
    col <- NA_character_
  }
  if (!is.na(col) && col %in% available_cols) col else NA_character_
}

safe_sparse_model_matrix <- function(formula_obj, data_df) {
  vars_in_formula <- intersect(unique(all.vars(formula_obj)), names(data_df))
  if (nrow(data_df) == 0L) {
    return(Matrix::sparse.model.matrix(object = formula_obj, data = data_df))
  }

  # Work on a copy and normalize categorical vars to character
  mm_data <- data_df
  for (v in vars_in_formula) {
    if (is.factor(mm_data[[v]])) {
      mm_data[[v]] <- as.character(mm_data[[v]])
    }
  }

  # model.matrix drops rows with NA on variables used in formula; check levels
  # on that effective subset and neutralize categorical variables with <=1 level
  mm_subset <- mm_data[, vars_in_formula, drop = FALSE]
  cc_idx <- complete.cases(mm_subset)
  cc_data <- mm_data[cc_idx, , drop = FALSE]

  for (v in vars_in_formula) {
    if (is.character(mm_data[[v]])) {
      lv <- unique(cc_data[[v]])
      lv <- lv[!is.na(lv) & nzchar(lv)]
      if (length(lv) <= 1) {
        mm_data[[v]] <- 0
      }
    }
  }

  Matrix::sparse.model.matrix(object = formula_obj, data = mm_data)
}

# ----------------------------------------------------------------------------
# CNOSSOS-EU emission via NoiseModelling Java bridge
# ----------------------------------------------------------------------------
# Uses the real CNOSSOS-EU 2020 implementation from NoiseModelling 5.x.
# The compute_emission_cnossos() function is defined in utils_model_training.R
# and calls the Java batch calculator for all octave bands (63-8000 Hz)
# with proper A-weighting.

for (model_name in names(all_configs)) {
  
  # Current configuration
  config <- all_configs[[model_name]]
  
  pipeline_message(
    sprintf("Training step [%d/%d] - Estimation of the variable %s", 
            which(names(all_configs) == model_name), 
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
  valid_idx <- !is.na(training_data_target) & 
    training_data_target >= model_config$min_valid
  
  # More permissive threshold for truck models (less data available than for 
  # light vehicles)
  min_obs_threshold <- ifelse(test = grepl(pattern = "truck", 
                                           x = model_config$target), 
                              yes = 20, 
                              no = 50)
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
  clean_training_data_target <- training_data_target[valid_idx]
  quality_col <- get_quality_indicator_column(
    target_name = model_config$target,
    available_cols = names(clean_training_data_over_period))
  quality_indicator <- if (!is.na(quality_col)) {
    as.numeric(clean_training_data_over_period[[quality_col]])
  } else {
    rep(NA_real_, nrow(clean_training_data_over_period))
  }
  
  pipeline_message("Time period selected and target data filtered", level = 2, 
                   progress = "end", process = "valid")
  
  pipeline_message("Construction of the sparse feature matrix", level = 2, 
                   progress = "start", process = "calc")
  
  # Create sparse feature matrix (may eliminate more rows due to NA in features)
  sparse_data_matrix <- safe_sparse_model_matrix(
    formula_obj = road_feature_formula,
    data_df = clean_training_data_over_period)
  
  pipeline_message("Sparse feature matrix constructed successfully", level = 2, 
                   progress = "end", process = "valid")
  
  # Check if the sparse data matrix eliminated additional rows
  if (nrow(x = sparse_data_matrix) 
      != nrow(x = clean_training_data_over_period)) {
    # Find which rows were kept by sparse.model.matrix
    kept_rows <- as.integer(x = rownames(sparse_data_matrix))
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
  if (!is.null(model_config$transform) && model_config$transform == "log10") {
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
  n_final <- nrow(x = sparse_data_matrix)
  use_grouped_split <- isTRUE(cfg_train$USE_GROUPED_SENSOR_SPLIT)
  is_base_model <- model_name %in% c("flow_D", "truck_pct_D", "speed_D")
  if (use_grouped_split && "count_point_id" %in% names(clean_training_data_over_period)) {
    sensor_ids <- clean_training_data_over_period$count_point_id
    unique_sensors <- unique(sensor_ids)
    if (is_base_model && !is.null(shared_base_test_sensors)) {
      # Use the shared split for base models so emission dB analysis
      # can merge test predictions from all 3 models without row loss
      train_idx <- which(!sensor_ids %in% shared_base_test_sensors)
      test_idx <- which(sensor_ids %in% shared_base_test_sensors)
      if (length(train_idx) == 0 || length(test_idx) == 0) {
        train_idx <- sample(x = seq_len(to = n_final),
                            size = floor(x = 0.8 * n_final))
        test_idx <- setdiff(x = seq_len(n_final), y = train_idx)
      }
    } else if (length(unique_sensors) >= 5) {
      n_train_sensors <- max(1, floor(0.8 * length(unique_sensors)))
      train_sensors <- sample(unique_sensors, size = n_train_sensors)
      train_idx <- which(sensor_ids %in% train_sensors)
      test_idx <- which(!sensor_ids %in% train_sensors)
      if (length(train_idx) == 0 || length(test_idx) == 0) {
        train_idx <- sample(x = seq_len(to = n_final), 
                            size = floor(x = 0.8 * n_final))
        test_idx <- setdiff(x = seq_len(n_final), y = train_idx)
      }
    } else {
      train_idx <- sample(x = seq_len(to = n_final), 
                          size = floor(x = 0.8 * n_final))
      test_idx <- setdiff(x = seq_len(n_final), y = train_idx)
    }
  } else {
    train_idx <- sample(x = seq_len(to = n_final), 
                        size = floor(x = 0.8 * n_final))
    test_idx <- setdiff(x = seq_len(n_final), y = train_idx)
  }
  
  X_train <- sparse_data_matrix[train_idx, ]
  X_test <- sparse_data_matrix[test_idx, ]
  y_train <- transformed_training_data_target[train_idx]
  y_test <- transformed_training_data_target[test_idx]
  quality_train <- quality_indicator[train_idx]
  quality_test <- quality_indicator[test_idx]
  test_meta <- clean_training_data_over_period[test_idx, c(
    "osm_id", "count_point_id", "period", "highway", "speed"
  )]
  
  # Check for invalid values in labels
  invalid_train <- is.na(y_train) | is.infinite(y_train) | is.nan(y_train)
  invalid_test <- is.na(y_test) | is.infinite(y_test) | is.nan(y_test)
  highway_test <- as.character(clean_training_data_over_period$highway[test_idx])
  
  if (any(invalid_train) || any(invalid_test)) {
    pipeline_message(
      sprintf("Found %d invalid train labels and %d invalid test labels", 
              sum(invalid_train), sum(invalid_test)), 
      process = "warning")
    
    # Remove invalid observations
    if (any(invalid_train)) {
      valid_train_idx <- !invalid_train
      X_train <- X_train[valid_train_idx, ]
      y_train <- y_train[valid_train_idx]
      quality_train <- quality_train[valid_train_idx]
    }
    if (any(invalid_test)) {
      valid_test_idx <- !invalid_test
      X_test <- X_test[valid_test_idx, ]
      y_test <- y_test[valid_test_idx]
      quality_test <- quality_test[valid_test_idx]
      highway_test <- highway_test[valid_test_idx]
      test_meta <- test_meta[valid_test_idx, , drop = FALSE]
    }
  }
  
  # Final check
  if (length(x = y_train) < 10 || length(x = y_test) < 5) {
    next
  }
  
  n_train <- length(x = y_train)
  n_test <- length(x = y_test)
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
  if (length(y_train) < 100 || length(y_test) < 30) {
    pipeline_message(sprintf("Small sample detected:\n\t\t", 
                             "-> train = %d\n\t\t", 
                             "-> test = %d\n\t\t", 
                             "=> Disabling early stopping and watchlist", 
                             length(y_train), length(y_test)), 
                     process = "warning")
    
    use_watchlist <- FALSE
  }
  
  use_quality_weights <- isTRUE(cfg_train$USE_AVATAR_QUALITY_WEIGHTS)
  min_weight <- if (!is.null(cfg_train$MIN_AVATAR_SAMPLE_WEIGHT)) {
    cfg_train$MIN_AVATAR_SAMPLE_WEIGHT
  } else {
    0.20
  }
  if (use_quality_weights && any(!is.na(quality_train))) {
    weight_train <- 1 - pmin(pmax(quality_train, 0), 100) / 100
    weight_train[is.na(weight_train)] <- 1
    weight_train <- pmax(weight_train, min_weight)
    dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train, weight = weight_train)
    dtest <- xgboost::xgb.DMatrix(data = X_test, label = y_test)
    pipeline_message(
      sprintf("Using Avatar quality weights: min=%.2f, mean=%.2f, max=%.2f", 
              min(weight_train), mean(weight_train), max(weight_train)),
      process = "info")
  } else {
    dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
    dtest <- xgboost::xgb.DMatrix(data = X_test, label = y_test)
  }
  
  # Choose parameters and training strategy based on model type
  start_timer()
  if (grepl(pattern = "truck", x = model_config$target)) {
    params <- cfg_train$TRUCK_PARAMS
    # For truck models with small samples, use CV for robust estimation
    if (length(x = y_train) < 200) {
      nfold <- min(5, length(x = y_train) %/% 10)
      if (nfold >= 2) {
        cv_result <- xgboost::xgb.cv(
          params = params,
          data = dtrain,
          nrounds = cfg_train$NROUNDS,
          nfold = nfold,  # Adaptive CV folds
          early_stopping_rounds = 30,
          verbose = 0,
          showsd = FALSE)
        best_rounds <- cv_result$best_iteration
        pipeline_message(sprintf("CV selected %d rounds (from max %d)", 
                                 best_rounds, cfg_train$NROUNDS), 
                         process = "clip")
      } else {
        best_rounds <- cfg_train$NROUNDS
      }
    } else {
      best_rounds <- cfg_train$NROUNDS
    }
  } else {
    params <- cfg_train$TRAINING_PARAMS
    # Cross-validation to select optimal nrounds (avoids overfitting)
    nfold <- min(5, max(2, length(x = y_train) %/% 50))
    if (nfold >= 2 && length(y_train) >= 100) {
      cv_result <- xgboost::xgb.cv(
        params = params,
        data = dtrain,
        nrounds = cfg_train$NROUNDS,
        nfold = nfold,
        early_stopping_rounds = 50,
        verbose = 0,
        showsd = FALSE)
      best_rounds <- cv_result$best_iteration
      pipeline_message(sprintf("CV(%d-fold) selected %d rounds (from max %d)",
                               nfold, best_rounds, cfg_train$NROUNDS), 
                       process = "info")
    } else {
      best_rounds <- cfg_train$NROUNDS
    }
  }
  
  # Acceptable limits for training
  min_train_xgb <- 150
  min_test_xgb  <- 50
  if (model_config$period == "D" && model_config$target == "ratio_speed_to_osm") {
    min_train_xgb <- 10
    min_test_xgb  <- 5
  }
  if (grepl(pattern = "truck", x = model_config$target)) {
    min_train_xgb <- 10
    min_test_xgb  <- 5
  }
  if (length(y_train) < min_train_xgb || length(y_test) < min_test_xgb) {
    pipeline_message(sprintf("Sample too small for XGBoost:\n\t\t", 
                             "-> train = %d\n\t\t", 
                             "-> test = %d\n\t\t", 
                             "=> Model skipped!", 
                             length(y_train), length(y_test)), 
      process = "warning")
    next
  }
  
  # Training
  xgb_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_rounds,
    watchlist = if (use_watchlist) list(train = dtrain, test = dtest) else NULL,
    early_stopping_rounds = if (use_watchlist) 50 else NULL,
    maximize = FALSE,
    verbose = 0)
  elapsed <- stop_timer()
  
  pipeline_message("Learning model trained successfully", level = 2, 
                   progress = "end", process = "valid")
  
  pipeline_message("Assessment and diagnostics of the learning model", 
                   level = 2, progress = "start", process = "search")
  
  # Evaluate
  pred_test <- predict(xgb_model, X_test)
  
  # Back-transform if needed
  if (!is.null(model_config$transform) && model_config$transform == "log10") {
    pred_original <- 10^pred_test
    actual_original <- 10^y_test
  } else {
    pred_original <- pred_test
    actual_original <- y_test
  }

  if (model_name == "speed_D" && config$target == "ratio_speed_to_osm") {
    speed_osm_test <- suppressWarnings(as.numeric(test_meta$speed))
    speed_osm_test[is.na(speed_osm_test) | speed_osm_test <= 0] <- cfg_data$DEFAULT_VEHICLE_SPEED
    pred_eval <- pmax(0, pred_original * speed_osm_test)
    actual_eval <- pmax(0, actual_original * speed_osm_test)
  } else {
    pred_eval <- pred_original
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
  mape_values <- abs(
    x = (pred_eval - actual_eval) / pmax(actual_eval, 0.01)) * 100
  medape <- median(x = mape_values[is.finite(mape_values)], 
                   na.rm = TRUE)
  
  pipeline_message(sprintf("R²=%.3f | MAPE=%.1f%% | MedAPE=%.1f%%", 
                           r2, mape, medape), 
                   process = "info")

  # Per-highway diagnostics on test fold (lightweight)
  highway_metrics <- data.table(
    highway = highway_test,
    actual = actual_eval,
    pred = pred_eval)
  highway_metrics <- highway_metrics[!is.na(highway)]
  if (nrow(highway_metrics) > 0) {
    highway_metrics_summary <- highway_metrics[, {
      m <- compute_model_metrics(y_true = actual, y_pred = pred)
      list(n = .N, r2 = m$r2, mae = m$mae, rmse = m$rmse, mape = m$mape)
    }, by = highway][order(-n)]
    top_hw_pool <- highway_metrics_summary[n >= 30]
    top_hw <- top_hw_pool[1:min(nrow(top_hw_pool), 5)]
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
  importance <- xgboost::xgb.importance(model = xgb_model)
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
    model = xgb_model,
    config = config,
    metrics = list(mae = mae, rmse = rmse, r2 = r2, mape = mape, medape = medape),
    feature_names = colnames(X_train),
    feature_importance = importance,  # Full importance table
    top_features = top_features,      # Top 5 for quick reference
    highway_metrics = as.data.frame(highway_metrics_summary),
    n_train = length(x = y_train),
    n_test = length(x = y_test),
    training_time = elapsed)

  # Keep base-model test predictions for acoustic emission diagnostics
  if (model_name %in% c("flow_D", "truck_pct_D", "speed_D")) {
    base_test_predictions[[model_name]] <- data.frame(
      osm_id = test_meta$osm_id,
      count_point_id = test_meta$count_point_id,
      period = as.character(test_meta$period),
      highway = as.character(test_meta$highway),
      pred = as.numeric(pred_eval),
      actual = as.numeric(actual_eval),
      stringsAsFactors = FALSE
    )
  }
  
  # Add to summary
  results_summary <- rbind(results_summary, 
                           data.frame(
                             Model = model_name, 
                             Target = model_config$name, 
                             N_train = length(x = y_train), 
                             N_test = length(x = y_test), 
                             R2 = round(x = r2, digits = 3), 
                             MAE = round(x = mae, digits = 2), 
                             RMSE = round(x = rmse, digits = 2), 
                             MAPE = round(x = mape, digits = 1), 
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
                         rel_path(cfg_train$XGB_MODELS_WITH_RATIOS_FILEPATH), 
                         rel_path(cfg_train$XGB_RATIO_FEATURE_INFO_FILEPATH)), 
                 level = 1, progress = "start", process = "save")

# Save list of models and road feature formula
saveRDS(object = models_list, 
        file = cfg_train$XGB_MODELS_WITH_RATIOS_FILEPATH)
saveRDS(object =  list(road_feature_formula = road_feature_formula, 
                       all_periods = all_periods), 
        file = cfg_train$XGB_RATIO_FEATURE_INFO_FILEPATH)

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
for (model_name in names(models_list)) {
  if (!is.null(models_list[[model_name]]$feature_importance)) {
    imp <- models_list[[model_name]]$feature_importance
    imp$Model <- model_name
    imp$ModelType <- ifelse(test = grepl(pattern = "_D$", x = model_name), 
                            yes = "Base", no = "Ratio")
    all_importance <- rbind(all_importance, imp)
  }
}

# Top features across all models
if (nrow(x = all_importance) > 0) {
  global_importance <- all_importance %>%
    group_by(Feature) %>%
    summarise(
      AvgGain = mean(x = Gain, na.rm = TRUE),
      AvgCover = mean(x = Cover, na.rm = TRUE),
      TimesUsed = n(),
      .groups = 'drop'
    ) %>%
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
base_results <- results_summary[grepl(pattern = "_D$", 
                                      x = results_summary$Model), ]
ratio_results <- results_summary[grepl(pattern = "^ratio_", 
                                       x = results_summary$Model), ]

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
for (model_name in names(models_list)) {
  model_info <- models_list[[model_name]]
  model_config <- model_info$config
  
  # Get test predictions and actual values
  test_data <- training_data %>% filter(period == model_config$period)
  y_all <- test_data[[model_config$target]]
  valid_idx <- !is.na(y_all) & y_all >= model_config$min_valid
  
  if (sum(valid_idx) < 10) next
  
  data_clean <- test_data[valid_idx, ]
  y_clean <- y_all[valid_idx]
  
  sparse_data_matrix <- safe_sparse_model_matrix(
    formula_obj = road_feature_formula,
    data_df = data_clean)
  
  if (nrow(x = sparse_data_matrix) != nrow(x = data_clean)) {
    kept_rows <- as.integer(rownames(sparse_data_matrix))
    y_clean <- y_clean[kept_rows]
    data_clean <- data_clean[kept_rows, ]
  }
  
  # Apply transform
  if (!is.null(model_config$transform) && model_config$transform == "log10") {
    transformed_training_data_target <- log10(pmax(y_clean, model_config$min_valid))
  } else {
    transformed_training_data_target <- y_clean
  }
  
  # Train/test split (same seed as training)
  set.seed(123)
  n_final <- nrow(x = sparse_data_matrix)
  train_idx <- sample(seq_len(n_final), size = floor(0.8 * n_final))
  test_idx <- setdiff(seq_len(n_final), train_idx)
  
  X_test <- sparse_data_matrix[test_idx, ]
  y_test <- transformed_training_data_target[test_idx]
  
  # Remove invalid
  invalid_test <- is.na(y_test) | is.infinite(y_test) | is.nan(y_test)
  if (any(invalid_test)) {
    valid_test_idx <- !invalid_test
    X_test <- X_test[valid_test_idx, ]
    y_test <- y_test[valid_test_idx]
  }
  
  if (length(x = y_test) < 5) next
  
  # Predict
  pred_test <- predict(model_info$model, X_test)
  
  # Back-transform
  if (!is.null(model_config$transform) && model_config$transform == "log10") {
    pred_original <- 10^pred_test
    actual_original <- 10^y_test
  } else {
    pred_original <- pred_test
    actual_original <- y_test
  }
  
  # Compute percentage errors
  pct_errors <- ((pred_original - actual_original) / pmax(actual_original, 0.01)) * 100
  pct_errors <- pct_errors[is.finite(pct_errors)]
  
  if (length(x = pct_errors) == 0) next
  
  # Statistics
  mean_pct_error <- mean(pct_errors, na.rm = TRUE)
  median_pct_error <- median(pct_errors, na.rm = TRUE)
  mae_pct <- mean(abs(pct_errors), na.rm = TRUE)
  
  # Quantiles
  q25 <- quantile(pct_errors, 0.25, na.rm = TRUE)
  q75 <- quantile(pct_errors, 0.75, na.rm = TRUE)
  
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

# Strategy: use the shared sensor split to build emission test data for ALL periods.
# 1. Predict flow_D/truck_pct_D/speed_D on test sensors (period D)
# 2. For each non-D period, apply ratio models to get predicted values
# 3. Get actual values from training_data for each period's test sensors
# 4. Compute CNOSSOS emission for all periods
if (all(c("flow_D", "truck_pct_D", "speed_D") %in% names(models_list)) &&
    !is.null(shared_base_test_sensors)) {

  d_data <- training_data %>%
    filter(period == "D",
           count_point_id %in% shared_base_test_sensors,
           !is.na(aggregate_flow), aggregate_flow >= 1)

  if (nrow(d_data) > 0) {
    d_matrix <- safe_sparse_model_matrix(
      formula_obj = road_feature_formula,
      data_df = d_data)

    if (nrow(d_matrix) != nrow(d_data)) {
      kept_rows <- as.integer(rownames(d_matrix))
      d_data <- d_data[kept_rows, ]
    }

    # Align feature columns with what the model expects
    # (subset of test data may have fewer factor levels → fewer columns)
    model_features <- models_list[["flow_D"]]$model$feature_names
    if (!is.null(model_features)) {
      current_cols <- colnames(d_matrix)
      missing_cols <- setdiff(model_features, current_cols)
      if (length(missing_cols) > 0) {
        zero_mat <- Matrix::Matrix(0, nrow = nrow(d_matrix),
                                   ncol = length(missing_cols),
                                   sparse = TRUE)
        colnames(zero_mat) <- missing_cols
        d_matrix <- cbind(d_matrix, zero_mat)
      }
      d_matrix <- d_matrix[, model_features, drop = FALSE]
    }

    if (nrow(d_matrix) > 0) {
      # --- Base predictions (period D) ---
      pred_flow_D_log <- predict(models_list[["flow_D"]]$model, d_matrix)
      pred_flow_D <- 10^pred_flow_D_log
      pred_truck_D <- predict(models_list[["truck_pct_D"]]$model, d_matrix)
      pred_speed_ratio_to_osm <- predict(models_list[["speed_D"]]$model, d_matrix)

      osm_speed_raw <- suppressWarnings(as.numeric(d_data$speed))
      osm_speed_missing <- is.na(osm_speed_raw) | osm_speed_raw < 5
      speed_base_for_ratio <- osm_speed_raw
      speed_base_for_ratio[osm_speed_missing] <- cfg_data$DEFAULT_VEHICLE_SPEED
      pred_speed_D <- pmax(5, pred_speed_ratio_to_osm * speed_base_for_ratio)

      # OSM maxspeed: use raw OSM speed where available, impute missing with XGBoost speed_D
      osm_speed_imputed_flag <- osm_speed_missing
      osm_speed_val <- osm_speed_raw
      osm_speed_val[osm_speed_imputed_flag] <- pred_speed_D[osm_speed_imputed_flag]

      # Actual values for period D
      actual_speed_D <- ifelse(!is.na(d_data$aggregate_speed) & d_data$aggregate_speed >= 5,
                               d_data$aggregate_speed,
                               speed_base_for_ratio)
      actual_truck_D <- ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
                               d_data$truck_pct, 0)

      # --- Build emission_test for period D ---
      emission_D <- data.frame(
        osm_id = d_data$osm_id,
        count_point_id = d_data$count_point_id,
        period = "D",
        highway = as.character(d_data$highway),
        DEGRE = if ("DEGRE" %in% names(d_data)) as.character(d_data$DEGRE) else NA_character_,
        pred_flow = as.numeric(pred_flow_D),
        actual_flow = as.numeric(d_data$aggregate_flow),
        pred_truck_pct = as.numeric(pred_truck_D),
        actual_truck_pct = as.numeric(actual_truck_D),
        pred_speed = as.numeric(pred_speed_D),
        actual_speed = as.numeric(actual_speed_D),
        osm_speed = as.numeric(osm_speed_val),
        osm_speed_imputed = osm_speed_imputed_flag,
        has_measured_speed = !is.na(d_data$aggregate_speed) & d_data$aggregate_speed >= 5,
        has_measured_truck = !is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
        stringsAsFactors = FALSE
      )
      emission_D <- emission_D[!is.na(emission_D$actual_speed) &
                                emission_D$actual_speed >= 5, ]

      emission_parts <- list(emission_D)

      # --- Apply ratio models for all non-D periods ---
      non_d_periods <- setdiff(all_periods, "D")
      # Keep only test sensor IDs that survived D filtering
      test_sensor_ids <- unique(emission_D$count_point_id)
      n_periods_done <- 0

      for (p in non_d_periods) {
        # Check that all 3 ratio models exist for this period
        flow_model_key <- paste0("ratio_flow_", p)
        truck_model_key <- paste0("ratio_truck_pct_", p)
        speed_model_key <- paste0("ratio_speed_", p)

        has_flow_ratio <- !is.null(models_list[[flow_model_key]]$model)
        has_truck_ratio <- !is.null(models_list[[truck_model_key]]$model)
        has_speed_ratio <- !is.null(models_list[[speed_model_key]]$model)

        # Need at least flow ratio to proceed
        if (!has_flow_ratio) next

        # Predict ratios from the feature matrix (same d_matrix, same roads)
        ratio_flow <- predict(models_list[[flow_model_key]]$model, d_matrix)
        ratio_truck <- if (has_truck_ratio) {
          predict(models_list[[truck_model_key]]$model, d_matrix)
        } else {
          rep(1, nrow(d_matrix))  # Fallback: same as D
        }
        ratio_speed <- if (has_speed_ratio) {
          predict(models_list[[speed_model_key]]$model, d_matrix)
        } else {
          rep(1, nrow(d_matrix))  # Fallback: same as D
        }

        # Predicted values for period P = base_D × ratio_P
        pred_flow_P <- pmax(0, pred_flow_D * ratio_flow)
        pred_truck_P <- pmin(100, pmax(0, pred_truck_D * ratio_truck))
        pred_speed_P <- pmax(5, pred_speed_D * ratio_speed)

        # Get actual values from training_data for this period's test sensors
        p_data <- training_data %>%
          filter(period == p,
                 count_point_id %in% test_sensor_ids)

        if (nrow(p_data) == 0) next

        # Match to D-data rows by count_point_id (inner join)
        # d_data rows define the feature matrix order
        match_idx <- match(p_data$count_point_id, d_data$count_point_id)
        valid_match <- !is.na(match_idx)
        p_data <- p_data[valid_match, ]
        row_in_d <- match_idx[valid_match]

        if (nrow(p_data) == 0) next

        actual_flow_P <- ifelse(!is.na(p_data$aggregate_flow) & p_data$aggregate_flow >= 0,
                                p_data$aggregate_flow, NA_real_)
        actual_truck_P <- ifelse(!is.na(p_data$truck_pct) & p_data$truck_pct >= 0,
                                 p_data$truck_pct, 0)
        actual_speed_P <- ifelse(!is.na(p_data$aggregate_speed) & p_data$aggregate_speed >= 5,
                                 p_data$aggregate_speed,
                                 speed_base_for_ratio[row_in_d])

        emission_P <- data.frame(
          osm_id = d_data$osm_id[row_in_d],
          count_point_id = p_data$count_point_id,
          period = p,
          highway = as.character(d_data$highway[row_in_d]),
          DEGRE = if ("DEGRE" %in% names(d_data)) as.character(d_data$DEGRE[row_in_d]) else NA_character_,
          pred_flow = as.numeric(pred_flow_P[row_in_d]),
          actual_flow = as.numeric(actual_flow_P),
          pred_truck_pct = as.numeric(pred_truck_P[row_in_d]),
          actual_truck_pct = as.numeric(actual_truck_P),
          pred_speed = as.numeric(pred_speed_P[row_in_d]),
          actual_speed = as.numeric(actual_speed_P),
          osm_speed = as.numeric(osm_speed_val[row_in_d]),
          osm_speed_imputed = osm_speed_imputed_flag[row_in_d],
          has_measured_speed = !is.na(p_data$aggregate_speed) & p_data$aggregate_speed >= 5,
          has_measured_truck = !is.na(p_data$truck_pct) & p_data$truck_pct >= 0,
          stringsAsFactors = FALSE
        )

        # Filter out invalid rows
        emission_P <- emission_P[!is.na(emission_P$actual_speed) &
                                  emission_P$actual_speed >= 5 &
                                  !is.na(emission_P$actual_flow), ]

        if (nrow(emission_P) > 0) {
          emission_parts <- c(emission_parts, list(emission_P))
          n_periods_done <- n_periods_done + 1
        }
      }

      # Combine all periods
      emission_test <- do.call(rbind, emission_parts)
      rm(emission_parts, emission_D)

      pipeline_message(
        sprintf("Emission dB eval: %s rows across %d periods (%s test sensors)",
                fmt(nrow(emission_test)),
                n_periods_done + 1,  # +1 for D
                fmt(length(test_sensor_ids))),
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
    all(c("flow_D", "truck_pct_D", "speed_D") %in% names(base_test_predictions))) {
  flow_test <- base_test_predictions[["flow_D"]]
  names(flow_test)[names(flow_test) == "pred"] <- "pred_flow"
  names(flow_test)[names(flow_test) == "actual"] <- "actual_flow"

  truck_test <- base_test_predictions[["truck_pct_D"]][,
    c("osm_id", "count_point_id", "period", "pred", "actual")]
  names(truck_test)[names(truck_test) == "pred"] <- "pred_truck_pct"
  names(truck_test)[names(truck_test) == "actual"] <- "actual_truck_pct"

  speed_test <- base_test_predictions[["speed_D"]][,
    c("osm_id", "count_point_id", "period", "pred", "actual")]
  names(speed_test)[names(speed_test) == "pred"] <- "pred_speed"
  names(speed_test)[names(speed_test) == "actual"] <- "actual_speed"

  emission_test <- merge(
    x = flow_test,
    y = truck_test,
    by = c("osm_id", "count_point_id", "period"),
    all = FALSE)

  emission_test <- merge(
    x = emission_test,
    y = speed_test,
    by = c("osm_id", "count_point_id", "period"),
    all = FALSE)

  # Legacy path may not carry DEGRE information
  if (!"DEGRE" %in% names(emission_test)) {
    emission_test$DEGRE <- NA_character_
  }

  if (nrow(emission_test) > 0) {
    pipeline_message(
      sprintf("Emission dB legacy path: %s rows from base_test_predictions merge", fmt(nrow(emission_test))),
      process = "info")
  }
}

if (nrow(emission_test) > 0) {
  pipeline_message(sprintf("Computing CNOSSOS-EU emission for %s test samples", 
                           fmt(nrow(emission_test))), 
                   process = "calc")
  # Emission with XGBoost predicted speed (real traffic speed)
  emission_test$pred_db <- compute_emission_cnossos(
    flow = emission_test$pred_flow,
    truck_pct = emission_test$pred_truck_pct,
    speed = emission_test$pred_speed)
  # Emission with OSM maxspeed (imputed by XGBoost when missing)
  has_osm_speed <- "osm_speed" %in% names(emission_test)
  if (has_osm_speed) {
    emission_test$pred_db_osm_speed <- compute_emission_cnossos(
      flow = emission_test$pred_flow,
      truck_pct = emission_test$pred_truck_pct,
      speed = emission_test$osm_speed)
  }
  # Reference emission from actual/observed values
  emission_test$actual_db <- compute_emission_cnossos(
    flow = emission_test$actual_flow,
    truck_pct = emission_test$actual_truck_pct,
    speed = emission_test$actual_speed)

  emission_test$db_error <- emission_test$pred_db - emission_test$actual_db
  emission_test$abs_db_error <- abs(emission_test$db_error)
  if (has_osm_speed) {
    emission_test$db_error_osm <- emission_test$pred_db_osm_speed - emission_test$actual_db
    emission_test$abs_db_error_osm <- abs(emission_test$db_error_osm)
  }

  valid_db <- is.finite(emission_test$db_error)
  if (has_osm_speed) valid_db <- valid_db & is.finite(emission_test$db_error_osm)
  emission_test <- emission_test[valid_db, ]
}

if (nrow(emission_test) > 0) {
  db_bias <- mean(emission_test$db_error, na.rm = TRUE)
  db_mae <- mean(abs(emission_test$db_error), na.rm = TRUE)
  db_rmse <- sqrt(mean(emission_test$db_error^2, na.rm = TRUE))
  db_q50 <- median(emission_test$abs_db_error, na.rm = TRUE)
  db_q90 <- as.numeric(quantile(emission_test$abs_db_error, probs = 0.90, na.rm = TRUE))

  compute_db_stats <- function(df) {
    if (nrow(df) == 0) {
      return(list(n = 0L, bias = NA_real_, mae = NA_real_, rmse = NA_real_))
    }
    list(
      n = nrow(df),
      bias = mean(df$db_error, na.rm = TRUE),
      mae = mean(abs(df$db_error), na.rm = TRUE),
      rmse = sqrt(mean(df$db_error^2, na.rm = TRUE))
    )
  }

  pipeline_message(
    sprintf("Emission dB (test): N=%s | Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB | MedAE=%.2f dB | P90AE=%.2f dB", 
            fmt(nrow(emission_test)), db_bias, db_mae, db_rmse, db_q50, db_q90),
    process = "info")

  if ("has_measured_speed" %in% names(emission_test)) {
    emission_measured_speed <- emission_test[emission_test$has_measured_speed, , drop = FALSE]
    emission_osm_fallback <- emission_test[!emission_test$has_measured_speed, , drop = FALSE]

    stats_measured <- compute_db_stats(emission_measured_speed)
    stats_osm <- compute_db_stats(emission_osm_fallback)

    if (stats_measured$n > 0) {
      pipeline_message(
        sprintf("Emission dB (measured speed): N=%s | Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB", 
                fmt(stats_measured$n), stats_measured$bias, stats_measured$mae, stats_measured$rmse),
        process = "info")
    }
    if (stats_osm$n > 0) {
      pipeline_message(
        sprintf("Emission dB (OSM speed fallback): N=%s | Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB", 
                fmt(stats_osm$n), stats_osm$bias, stats_osm$mae, stats_osm$rmse),
        process = "info")
    }
  }

  # Highway-level summary (top classes by sample size)
  emission_dt <- as.data.table(emission_test)
  db_by_highway <- emission_dt[, .(
    n = .N,
    bias_db = mean(db_error, na.rm = TRUE),
    mae_db = mean(abs_db_error, na.rm = TRUE),
    rmse_db = sqrt(mean(db_error^2, na.rm = TRUE))
  ), by = highway][order(-n)]

  top_hw <- db_by_highway[1:min(8, nrow(db_by_highway))]

  # Dual-speed comparison: XGBoost predicted speed vs OSM maxspeed
  has_osm_speed <- "db_error_osm" %in% names(emission_test)
  if (has_osm_speed) {
    db_bias_osm <- mean(emission_test$db_error_osm, na.rm = TRUE)
    db_mae_osm <- mean(abs(emission_test$db_error_osm), na.rm = TRUE)
    db_rmse_osm <- sqrt(mean(emission_test$db_error_osm^2, na.rm = TRUE))

    pipeline_message(
      sprintf("Emission dB with XGBoost speed: Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB", 
              db_bias, db_mae, db_rmse),
      process = "info")
    pipeline_message(
      sprintf("Emission dB with OSM maxspeed:   Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB", 
              db_bias_osm, db_mae_osm, db_rmse_osm),
      process = "info")
    pipeline_message(
      sprintf("Speed choice impact: \u0394Bias=%.2f dB | \u0394MAE=%.2f dB (OSM - XGBoost)", 
              db_bias_osm - db_bias, db_mae_osm - db_mae),
      process = "info")
  }

  # Save report PDF
  if (!dir.exists(cfg_g$FIGS_DIR)) {
    dir.create(cfg_g$FIGS_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  emission_pdf_path <- file.path(cfg_g$FIGS_DIR, "06_emission_dB_error_test.pdf")

  grDevices::pdf(file = emission_pdf_path, width = 11, height = 8.5)
  old_par <- par(no.readonly = TRUE)

  # ============================================================================
  # Helper: compute per-variable dB contribution (isolate each variable)
  # ============================================================================
  db_actual_all <- emission_test$actual_db
  db_flow_only <- compute_emission_cnossos(
    flow = emission_test$pred_flow,
    truck_pct = emission_test$actual_truck_pct,
    speed = emission_test$actual_speed)
  db_truck_only <- compute_emission_cnossos(
    flow = emission_test$actual_flow,
    truck_pct = emission_test$pred_truck_pct,
    speed = emission_test$actual_speed)
  db_speed_only <- compute_emission_cnossos(
    flow = emission_test$actual_flow,
    truck_pct = emission_test$actual_truck_pct,
    speed = emission_test$pred_speed)

  contrib_flow  <- db_flow_only - db_actual_all
  contrib_truck <- db_truck_only - db_actual_all
  contrib_speed <- db_speed_only - db_actual_all

  # Truck contribution: only where truck reference is actually measured
  if ("has_measured_truck" %in% names(emission_test)) {
    truck_eval_idx <- !is.na(emission_test$has_measured_truck) & as.logical(emission_test$has_measured_truck)
  } else {
    truck_eval_idx <- rep(TRUE, length(contrib_truck))
  }
  truck_eval_idx <- truck_eval_idx & is.finite(contrib_truck)
  contrib_truck_eval <- contrib_truck[truck_eval_idx]
  truck_bias <- if (length(contrib_truck_eval) > 0) mean(contrib_truck_eval, na.rm = TRUE) else NA_real_
  truck_mae  <- if (length(contrib_truck_eval) > 0) mean(abs(contrib_truck_eval), na.rm = TRUE) else NA_real_

  mean_contrib <- c(Flow = mean(contrib_flow, na.rm = TRUE),
                    `Truck %` = truck_bias,
                    Speed = mean(contrib_speed, na.rm = TRUE))
  mae_contrib <- c(Flow = mean(abs(contrib_flow), na.rm = TRUE),
                   `Truck %` = truck_mae,
                   Speed = mean(abs(contrib_speed), na.rm = TRUE))

  pipeline_message(
    sprintf("Bias decomposition: Flow=%+.2f dB | Truck%%=%+.2f dB (N=%s) | Speed=%+.2f dB",
            mean_contrib["Flow"], mean_contrib["Truck %"], 
            fmt(length(contrib_truck_eval)), mean_contrib["Speed"]),
    process = "info")

  # Helper: summarize metrics by group
  summarize_group_metrics <- function(df, group_col) {
    dt <- as.data.table(df)
    out <- dt[, .(
      n = .N,
      bias_xgb = mean(db_error, na.rm = TRUE),
      mae_xgb = mean(abs_db_error, na.rm = TRUE),
      rmse_xgb = sqrt(mean(db_error^2, na.rm = TRUE)),
      bias_osm = if ("db_error_osm" %in% names(.SD)) mean(db_error_osm, na.rm = TRUE) else NA_real_,
      mae_osm = if ("abs_db_error_osm" %in% names(.SD)) mean(abs_db_error_osm, na.rm = TRUE) else NA_real_
    ), by = .(group = as.character(get(group_col)))]
    out[!is.na(group) & group != ""]
  }

  fmt_pct <- function(x) {
    if (is.na(x) || !is.finite(x)) return("NA")
    sprintf("%.1f%%", 100 * x)
  }

  compute_subset_metrics <- function(mask, label) {
    idx <- !is.na(mask) & as.logical(mask)
    if (sum(idx) == 0) {
      return(data.frame(
        subset = label,
        n = 0L,
        bias = NA_real_,
        mae = NA_real_,
        rmse = NA_real_,
        q50 = NA_real_,
        q90 = NA_real_,
        within_1db = NA_real_,
        within_2db = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    err <- emission_test$db_error[idx]
    abse <- emission_test$abs_db_error[idx]
    data.frame(
      subset = label,
      n = sum(idx),
      bias = mean(err, na.rm = TRUE),
      mae = mean(abse, na.rm = TRUE),
      rmse = sqrt(mean(err^2, na.rm = TRUE)),
      q50 = median(abse, na.rm = TRUE),
      q90 = as.numeric(quantile(abse, probs = 0.90, na.rm = TRUE)),
      within_1db = mean(abse <= 1, na.rm = TRUE),
      within_2db = mean(abse <= 2, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  # Helper: identify dominant error source for a subset
  identify_dominant_source <- function(df) {
    if (nrow(df) == 0) return("N/A")
    fl <- db_flow_only[as.integer(rownames(df))] - db_actual_all[as.integer(rownames(df))]
    sp <- db_speed_only[as.integer(rownames(df))] - db_actual_all[as.integer(rownames(df))]
    mae_f <- mean(abs(fl), na.rm = TRUE)
    mae_s <- mean(abs(sp), na.rm = TRUE)
    if (mae_f > mae_s) return("Flux") else return("Vitesse")
  }

  # Attach row indices for decomposition lookup
  emission_test$.row_idx <- seq_len(nrow(emission_test))

  # ============================================================================
  # PAGE 1: Qualité globale de la prédiction en dB sur les tronçons tests
  # ============================================================================
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))

  # 1a: Histogramme des erreurs
  hist(emission_test$db_error, breaks = 50,
       main = "Distribution de l'erreur d'émission (dB)",
       xlab = "Erreur = Prédit - Observé (dB)",
       col = "steelblue", border = "white", cex.main = 1.1)
  abline(v = 0, col = "red", lwd = 2, lty = 1)
  abline(v = db_bias, col = "orange", lwd = 2, lty = 2)
  legend("topright",
         legend = c("Zéro", sprintf("Biais moyen = %+.2f dB", db_bias)),
         col = c("red", "orange"), lwd = 2, lty = c(1, 2), cex = 0.8, bty = "n")

  # 1b: Prédit vs Observé
  plot(emission_test$actual_db, emission_test$pred_db,
       pch = 16, cex = 0.35, col = rgb(0.2, 0.4, 0.8, 0.25),
       xlab = "Émission observée CNOSSOS-EU (dB(A))",
       ylab = "Émission prédite CNOSSOS-EU (dB(A))",
       main = "Prédit vs Observé (tronçons tests)", cex.main = 1.1)
  abline(a = 0, b = 1, col = "red", lwd = 2)
  r2 <- cor(emission_test$actual_db, emission_test$pred_db, use = "complete.obs")^2
  legend("topleft", legend = sprintf("R² = %.3f", r2), cex = 0.9, bty = "n")

  # 1c: CDF de l'erreur absolue
  abs_sorted <- sort(emission_test$abs_db_error)
  ecdf_vals <- seq_along(abs_sorted) / length(abs_sorted)
  plot(abs_sorted, ecdf_vals, type = "l", lwd = 2, col = "steelblue",
       xlab = "Erreur absolue (dB)", ylab = "Proportion cumulée",
       main = "Fonction de répartition de l'erreur absolue", cex.main = 1.1)
  abline(h = 0.5, col = "grey60", lty = 3)
  abline(h = 0.9, col = "grey60", lty = 3)
  abline(v = db_q50, col = "orange", lty = 2)
  abline(v = db_q90, col = "red", lty = 2)
  legend("bottomright", legend = c(
    sprintf("Médiane = %.1f dB", db_q50),
    sprintf("P90 = %.1f dB", db_q90)),
    col = c("orange", "red"), lwd = 2, lty = 2, cex = 0.8, bty = "n")

  # 1d: Tableau synthèse
  plot.new()
  title(main = "Métriques globales sur tronçons tests", cex.main = 1.1)
  summary_txt <- c(
    sprintf("Nombre de tronçons-périodes évalués : %s", fmt(nrow(emission_test))),
    "",
    sprintf("Biais moyen           : %+.2f dB", db_bias),
    sprintf("Erreur absolue moyenne (MAE)  : %.2f dB", db_mae),
    sprintf("RMSE                  : %.2f dB", db_rmse),
    sprintf("Erreur absolue médiane : %.2f dB", db_q50),
    sprintf("Erreur absolue P90     : %.2f dB", db_q90),
    "",
    "Interprétation :",
    if (db_bias > 0.5) sprintf("  -> Surestimation de %+.1f dB en moyenne", db_bias)
    else if (db_bias < -0.5) sprintf("  -> Sous-estimation de %+.1f dB en moyenne", db_bias)
    else "  -> Biais quasi nul (< 0.5 dB)"
  )
  text(0.02, seq(0.95, 0.95 - 0.075 * (length(summary_txt) - 1),
       length.out = length(summary_txt)),
       labels = summary_txt, adj = c(0, 0.5), cex = 0.90, family = "mono")

  mtext("Évaluation émission CNOSSOS-EU sur tronçons tests",
        side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)

  # ============================================================================
  # PAGE 1bis: Transparence des données et qualité par sous-échantillon
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main = "Transparence des données test et robustesse des résultats", cex.main = 1.1)

  n_all <- nrow(emission_test)
  has_speed_col <- "has_measured_speed" %in% names(emission_test)
  has_truck_col <- "has_measured_truck" %in% names(emission_test)

  n_speed_measured <- if (has_speed_col) sum(emission_test$has_measured_speed, na.rm = TRUE) else 0L
  n_speed_fallback <- if (has_speed_col) n_all - n_speed_measured else NA_integer_
  n_truck_measured <- if (has_truck_col) sum(emission_test$has_measured_truck, na.rm = TRUE) else 0L
  n_truck_fallback <- if (has_truck_col) n_all - n_truck_measured else NA_integer_

  subset_table <- data.frame(stringsAsFactors = FALSE)
  if (has_speed_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(emission_test$has_measured_speed, "Vitesse mesurée"),
      compute_subset_metrics(!emission_test$has_measured_speed, "Vitesse fallback (OSM/imputée)")
    )
  }
  if (has_truck_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(emission_test$has_measured_truck, "%PL mesuré"),
      compute_subset_metrics(!emission_test$has_measured_truck, "%PL fallback")
    )
  }
  if (has_speed_col && has_truck_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(emission_test$has_measured_speed & emission_test$has_measured_truck,
                             "Vitesse + %PL mesurés"),
      compute_subset_metrics(!emission_test$has_measured_speed & !emission_test$has_measured_truck,
                             "Vitesse + %PL fallback")
    )
  }

  intro_lines <- c(
    sprintf("Échantillon total évalué : %s tronçons-périodes", fmt(n_all)),
    "",
    sprintf("Couverture vitesse mesurée  : %s (%s)",
            fmt(n_speed_measured), if (has_speed_col) fmt_pct(n_speed_measured / n_all) else "NA"),
    sprintf("Couverture vitesse fallback : %s (%s)",
            if (is.na(n_speed_fallback)) "NA" else fmt(n_speed_fallback),
            if (has_speed_col) fmt_pct(n_speed_fallback / n_all) else "NA"),
    sprintf("Couverture %%PL mesuré       : %s (%s)",
            fmt(n_truck_measured), if (has_truck_col) fmt_pct(n_truck_measured / n_all) else "NA"),
    sprintf("Couverture %%PL fallback     : %s (%s)",
            if (is.na(n_truck_fallback)) "NA" else fmt(n_truck_fallback),
            if (has_truck_col) fmt_pct(n_truck_fallback / n_all) else "NA"),
    "",
    "Qualité par sous-échantillon (dB) :",
    "subset                               n      bias    MAE    RMSE   P50   P90   |e|<=1dB |e|<=2dB"
  )

  table_lines <- c()
  if (nrow(subset_table) > 0) {
    for (i in seq_len(nrow(subset_table))) {
      rr <- subset_table[i, ]
      table_lines <- c(table_lines, sprintf(
        "%-34s %6s %7s %6s %7s %5s %5s %8s %8s",
        substr(rr$subset, 1, 34),
        fmt(rr$n),
        ifelse(is.na(rr$bias), "NA", sprintf("%+.2f", rr$bias)),
        ifelse(is.na(rr$mae), "NA", sprintf("%.2f", rr$mae)),
        ifelse(is.na(rr$rmse), "NA", sprintf("%.2f", rr$rmse)),
        ifelse(is.na(rr$q50), "NA", sprintf("%.2f", rr$q50)),
        ifelse(is.na(rr$q90), "NA", sprintf("%.2f", rr$q90)),
        ifelse(is.na(rr$within_1db), "NA", fmt_pct(rr$within_1db)),
        ifelse(is.na(rr$within_2db), "NA", fmt_pct(rr$within_2db))
      ))
    }
  }

  final_lines <- c(
    intro_lines,
    table_lines,
    "",
    "Note: l'objectif du rapport est uniquement la qualité de prédiction des émissions dB",
    "sur tronçons tests (pas l'erreur sur trafic brut)."
  )

  text(0.02,
       seq(0.96, 0.96 - 0.029 * (length(final_lines) - 1), length.out = length(final_lines)),
       labels = final_lines, adj = c(0, 0.5), cex = 0.70, family = "mono")

  # ============================================================================
  # PAGE 2: Pourquoi l'erreur ? Décomposition par source (flux, truck%, vitesse)
  # ============================================================================
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1))

  # 2a: Biais par variable
  bar_cols <- c("#4393C3", "#F4A582", "#D6604D")
  bp <- barplot(mean_contrib, col = bar_cols,
                main = "Contribution au biais par variable",
                ylab = "Biais moyen (dB)",
                ylim = range(c(mean_contrib, 0), na.rm = TRUE) * c(1.4, 1.4),
                border = NA, cex.main = 1.1)
  abline(h = 0, col = "grey40")
  text(bp, mean_contrib, labels = sprintf("%+.2f dB", mean_contrib),
       pos = ifelse(mean_contrib >= 0, 3, 1), cex = 0.95, font = 2)
  mtext("Biais > 0 = surestimation, < 0 = sous-estimation",
        side = 1, line = 2.5, cex = 0.7, font = 3)

  # 2b: MAE par variable
  bp2 <- barplot(mae_contrib, col = bar_cols,
                 main = "Erreur absolue moyenne (MAE) par variable",
                 ylab = "MAE (dB)", border = NA, cex.main = 1.1)
  text(bp2, mae_contrib, labels = sprintf("%.2f dB", mae_contrib),
       pos = 3, cex = 0.95, font = 2)
  mtext("Plus la barre est haute, plus la variable introduit d'erreur",
        side = 1, line = 2.5, cex = 0.7, font = 3)

  # 2c: Boxplot des contributions
  truck_box <- if (length(contrib_truck_eval) > 0) contrib_truck_eval else NA_real_
  boxplot(list(Flux = contrib_flow, `% PL (mesuré)` = truck_box, Vitesse = contrib_speed),
          col = bar_cols,
          main = "Distribution de l'erreur dB par variable",
          ylab = "Erreur dB (prédit - observé)", outline = FALSE, cex.main = 1.1)
  abline(h = 0, col = "red", lty = 2)
  mtext("Chaque boîte montre la variabilité de l'erreur due à une seule variable",
        side = 1, line = 2.5, cex = 0.7, font = 3)

  # 2d: Interprétation textuelle
  plot.new()
  title(main = "Diagnostic : quelle variable cause l'erreur ?", cex.main = 1.1)

  # Rank by MAE
  ord <- order(mae_contrib, decreasing = TRUE)
  var_names_fr <- c(Flow = "le flux", `Truck %` = "le % poids lourds", Speed = "la vitesse")
  direction_fr <- c(Flow = if (mean_contrib["Flow"] > 0) "surestimé" else "sous-estimé",
                    `Truck %` = if (!is.na(mean_contrib["Truck %"]) && mean_contrib["Truck %"] > 0) "surestimé" else "sous-estimé",
                    Speed = if (mean_contrib["Speed"] > 0) "surestimé → niveau trop fort" else "sous-estimé → niveau trop faible")

  diag_lines <- c(
    "Classement des sources d'erreur (de la plus impactante à la moins) :",
    ""
  )
  for (i in seq_along(ord)) {
    vn <- names(mae_contrib)[ord[i]]
    diag_lines <- c(diag_lines, sprintf(
      "  %d. %s : biais = %+.2f dB, MAE = %.2f dB (%s)",
      i, var_names_fr[vn], mean_contrib[vn], mae_contrib[vn], direction_fr[vn]))
  }
  diag_lines <- c(diag_lines, "",
    sprintf("  Truck %% évalué sur %s capteurs avec mesure effective.", fmt(length(contrib_truck_eval))),
    "",
    "Conclusion :")
  dominant_var <- names(mae_contrib)[ord[1]]
  if (dominant_var == "Speed") {
    diag_lines <- c(diag_lines,
      "  La vitesse est la source principale d'erreur en dB.",
      "  -> Voir page 6 pour l'impact de la vitesse OSM vs prédite.")
  } else if (dominant_var == "Flow") {
    diag_lines <- c(diag_lines,
      "  Le flux est la source principale d'erreur en dB.",
      "  -> L'amélioration du modèle de flux réduirait le plus l'erreur.")
  } else {
    diag_lines <- c(diag_lines,
      "  Le % poids lourds est la source principale d'erreur en dB.",
      "  -> Mais les données de référence sont limitées.")
  }
  text(0.02, seq(0.95, 0.95 - 0.055 * (length(diag_lines) - 1),
       length.out = length(diag_lines)),
       labels = diag_lines, adj = c(0, 0.5), cex = 0.82, family = "mono")

  # ============================================================================
  # PAGE 3: Quelles périodes sont les meilleures / pires ?
  # ============================================================================
  if ("period" %in% names(emission_test)) {
    # D/E/N analysis
    den_df <- emission_test[emission_test$period %in% c("D", "E", "N"), , drop = FALSE]
    # Hourly analysis (h0..h23)
    hour_df <- emission_test[grepl("^h([0-9]|1[0-9]|2[0-3])$", emission_test$period), , drop = FALSE]

    den_ok <- nrow(den_df) > 0
    hour_ok <- nrow(hour_df) > 0

    if (den_ok || hour_ok) {
      par(mfrow = c(2, 2), mar = c(5, 4.5, 3.5, 1))

      # 3a: Bias par période D/E/N
      if (den_ok) {
        den_stats <- summarize_group_metrics(den_df, "period")
        # Reorder D, E, N
        den_order <- match(c("D", "E", "N"), den_stats$group)
        den_order <- den_order[!is.na(den_order)]
        den_stats <- den_stats[den_order, ]
        den_lbl <- sprintf("%s\n(n=%s)", den_stats$group, fmt(den_stats$n))
        den_cols <- c(D = "#FFD700", E = "#FF8C00", N = "#1a1a6e")
        bp_den <- barplot(den_stats$bias_xgb, names.arg = den_lbl,
                          col = den_cols[den_stats$group], border = NA,
                          main = "Biais dB par période D / E / N",
                          ylab = "Biais (dB)", cex.main = 1.1)
        abline(h = 0, col = "red", lty = 2)
        text(bp_den, den_stats$bias_xgb,
             labels = sprintf("%+.2f", den_stats$bias_xgb),
             pos = ifelse(den_stats$bias_xgb >= 0, 3, 1), cex = 0.9, font = 2)
      } else {
        plot.new()
        title(main = "D / E / N : données insuffisantes")
      }

      # 3b: MAE par période D/E/N
      if (den_ok) {
        bp_den2 <- barplot(den_stats$mae_xgb, names.arg = den_lbl,
                           col = den_cols[den_stats$group], border = NA,
                           main = "MAE dB par période D / E / N",
                           ylab = "MAE (dB)", cex.main = 1.1)
        text(bp_den2, den_stats$mae_xgb,
             labels = sprintf("%.2f", den_stats$mae_xgb),
             pos = 3, cex = 0.9, font = 2)
      } else {
        plot.new()
        title(main = "D / E / N : données insuffisantes")
      }

      # 3c: MAE horaire
      if (hour_ok) {
        hour_stats <- summarize_group_metrics(hour_df, "period")
        hour_stats$hour_num <- as.integer(sub("^h", "", hour_stats$group))
        hour_stats <- hour_stats[order(hour_stats$hour_num), ]
        labels_h <- paste0("h", sprintf("%02d", hour_stats$hour_num))

        # Color by D/E/N membership
        h_cols <- ifelse(hour_stats$hour_num >= 6 & hour_stats$hour_num < 18, "#FFD700",
                  ifelse(hour_stats$hour_num >= 18 & hour_stats$hour_num < 22, "#FF8C00", "#1a1a6e"))

        plot(hour_stats$hour_num, hour_stats$mae_xgb, type = "b", pch = 16,
             col = h_cols, lwd = 2,
             xlab = "Heure", ylab = "MAE (dB)",
             main = "MAE par heure (h0–h23)", cex.main = 1.1,
             xaxt = "n")
        axis(1, at = hour_stats$hour_num, labels = labels_h, cex.axis = 0.7, las = 2)
        # Shade D/E/N zones
        rect(5.5, par("usr")[3], 17.5, par("usr")[4], col = rgb(1, 0.84, 0, 0.08), border = NA)
        rect(17.5, par("usr")[3], 21.5, par("usr")[4], col = rgb(1, 0.55, 0, 0.08), border = NA)
        # Redraw points on top
        points(hour_stats$hour_num, hour_stats$mae_xgb, pch = 16, col = h_cols, cex = 1.2)
        lines(hour_stats$hour_num, hour_stats$mae_xgb, col = "grey40")
        legend("topright", legend = c("Jour (D)", "Soirée (E)", "Nuit (N)"),
               fill = c("#FFD700", "#FF8C00", "#1a1a6e"), cex = 0.7, bty = "n")

        # Find best / worst hours
        best_h <- hour_stats$group[which.min(hour_stats$mae_xgb)]
        worst_h <- hour_stats$group[which.max(hour_stats$mae_xgb)]
      } else {
        plot.new()
        title(main = "Données horaires insuffisantes")
        best_h <- "N/A"
        worst_h <- "N/A"
      }

      # 3d: Biais horaire
      if (hour_ok) {
        plot(hour_stats$hour_num, hour_stats$bias_xgb, type = "b", pch = 16,
             col = h_cols, lwd = 2,
             xlab = "Heure", ylab = "Biais (dB)",
             main = "Biais par heure (h0–h23)", cex.main = 1.1,
             xaxt = "n")
        axis(1, at = hour_stats$hour_num, labels = labels_h, cex.axis = 0.7, las = 2)
        abline(h = 0, col = "red", lty = 2, lwd = 1.5)
        rect(5.5, par("usr")[3], 17.5, par("usr")[4], col = rgb(1, 0.84, 0, 0.08), border = NA)
        rect(17.5, par("usr")[3], 21.5, par("usr")[4], col = rgb(1, 0.55, 0, 0.08), border = NA)
        points(hour_stats$hour_num, hour_stats$bias_xgb, pch = 16, col = h_cols, cex = 1.2)
        lines(hour_stats$hour_num, hour_stats$bias_xgb, col = "grey40")
      } else {
        plot.new()
        title(main = "Données horaires insuffisantes")
      }

      mtext("Qualité de la prédiction dB par période temporelle",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
    }

    # --- Page 3bis: Diagnostic textuel des périodes ---
    if (den_ok || hour_ok) {
      par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
      plot.new()
      title(main = "Diagnostic par période : pourquoi certaines sont meilleures ?",
            cex.main = 1.1)

      period_diag <- c("Analyse des périodes temporelles :", "")

      if (den_ok) {
        best_den <- den_stats$group[which.min(den_stats$mae_xgb)]
        worst_den <- den_stats$group[which.max(den_stats$mae_xgb)]
        period_diag <- c(period_diag,
          "  Périodes réglementaires (D / E / N) :",
          sprintf("    Meilleure : %s (MAE = %.2f dB, Biais = %+.2f dB)",
                  best_den,
                  den_stats$mae_xgb[den_stats$group == best_den],
                  den_stats$bias_xgb[den_stats$group == best_den]),
          sprintf("    Pire      : %s (MAE = %.2f dB, Biais = %+.2f dB)",
                  worst_den,
                  den_stats$mae_xgb[den_stats$group == worst_den],
                  den_stats$bias_xgb[den_stats$group == worst_den]),
          "")

        # Decompose source for best/worst DEN
        for (pp in c(best_den, worst_den)) {
          sub_idx <- which(emission_test$period == pp)
          if (length(sub_idx) > 5) {
            sub_fl <- mean(abs(contrib_flow[sub_idx]), na.rm = TRUE)
            sub_sp <- mean(abs(contrib_speed[sub_idx]), na.rm = TRUE)
            dominant <- if (sub_fl > sub_sp) "flux" else "vitesse"
            period_diag <- c(period_diag,
              sprintf("    %s : source dominante = %s (MAE flux=%.2f, MAE vitesse=%.2f dB)",
                      pp, dominant, sub_fl, sub_sp))
          }
        }
        period_diag <- c(period_diag, "")
      }

      if (hour_ok) {
        period_diag <- c(period_diag,
          "  Périodes horaires (h0–h23) :",
          sprintf("    Meilleure heure : %s (MAE = %.2f dB)",
                  best_h, min(hour_stats$mae_xgb)),
          sprintf("    Pire heure      : %s (MAE = %.2f dB)",
                  worst_h, max(hour_stats$mae_xgb)),
          "",
          "  Explication :",
          "    Les heures nocturnes (h22–h05) ont souvent un biais plus élevé car",
          "    les flux faibles sont relatifs et une petite erreur absolue sur le",
          "    flux produit une grande erreur relative en dB (effet log).",
          "    En journée (h07–h18), les flux sont stables et mieux prédits."
        )
      }

      text(0.02, seq(0.95, 0.95 - 0.045 * (length(period_diag) - 1),
           length.out = length(period_diag)),
           labels = period_diag, adj = c(0, 0.5), cex = 0.78, family = "mono")
    }

    # --- Page 3ter: Semaine vs Weekend (h*_wd / h*_we) ---
    hour_wd_df <- emission_test[grepl("^h[0-9]+_wd$", emission_test$period), , drop = FALSE]
    hour_we_df <- emission_test[grepl("^h[0-9]+_we$", emission_test$period), , drop = FALSE]
    wd_ok <- nrow(hour_wd_df) > 0
    we_ok <- nrow(hour_we_df) > 0

    if (wd_ok && we_ok) {
      par(mfrow = c(2, 2), mar = c(5, 4.5, 3.5, 1))

      # Compute stats for weekday hours
      wd_stats <- summarize_group_metrics(hour_wd_df, "period")
      wd_stats$hour_num <- as.integer(sub("^h([0-9]+)_wd$", "\\1", wd_stats$group))
      wd_stats <- wd_stats[order(wd_stats$hour_num), ]

      # Compute stats for weekend hours
      we_stats <- summarize_group_metrics(hour_we_df, "period")
      we_stats$hour_num <- as.integer(sub("^h([0-9]+)_we$", "\\1", we_stats$group))
      we_stats <- we_stats[order(we_stats$hour_num), ]

      # 3ter-a: MAE semaine vs weekend
      y_lim_mae <- range(c(wd_stats$mae_xgb, we_stats$mae_xgb), na.rm = TRUE)
      y_lim_mae[1] <- max(0, y_lim_mae[1] - 0.5)
      y_lim_mae[2] <- y_lim_mae[2] + 0.5
      plot(wd_stats$hour_num, wd_stats$mae_xgb, type = "b", pch = 16,
           col = "#2166AC", lwd = 2, ylim = y_lim_mae,
           xlab = "Heure", ylab = "MAE (dB)",
           main = "MAE par heure : Semaine vs Weekend", cex.main = 1.1,
           xaxt = "n")
      lines(we_stats$hour_num, we_stats$mae_xgb, type = "b", pch = 17,
            col = "#B2182B", lwd = 2)
      axis(1, at = 0:23, labels = paste0("h", sprintf("%02d", 0:23)),
           cex.axis = 0.65, las = 2)
      legend("topright", legend = c("Semaine (wd)", "Weekend (we)"),
             col = c("#2166AC", "#B2182B"), pch = c(16, 17), lwd = 2,
             cex = 0.8, bty = "n")

      # 3ter-b: Biais semaine vs weekend
      y_lim_bias <- range(c(wd_stats$bias_xgb, we_stats$bias_xgb), na.rm = TRUE)
      y_lim_bias <- c(y_lim_bias[1] - 0.5, y_lim_bias[2] + 0.5)
      plot(wd_stats$hour_num, wd_stats$bias_xgb, type = "b", pch = 16,
           col = "#2166AC", lwd = 2, ylim = y_lim_bias,
           xlab = "Heure", ylab = "Biais (dB)",
           main = "Biais par heure : Semaine vs Weekend", cex.main = 1.1,
           xaxt = "n")
      lines(we_stats$hour_num, we_stats$bias_xgb, type = "b", pch = 17,
            col = "#B2182B", lwd = 2)
      axis(1, at = 0:23, labels = paste0("h", sprintf("%02d", 0:23)),
           cex.axis = 0.65, las = 2)
      abline(h = 0, col = "red", lty = 2, lwd = 1.5)
      legend("topright", legend = c("Semaine (wd)", "Weekend (we)"),
             col = c("#2166AC", "#B2182B"), pch = c(16, 17), lwd = 2,
             cex = 0.8, bty = "n")

      # 3ter-c: Effectif par heure (wd vs we)
      n_wd <- wd_stats$n
      n_we <- we_stats$n
      bp_mat <- rbind(n_wd, n_we)
      colnames(bp_mat) <- paste0("h", sprintf("%02d", wd_stats$hour_num))
      barplot(bp_mat, beside = TRUE, col = c("#2166AC", "#B2182B"),
              border = NA, las = 2, cex.names = 0.65,
              main = "Effectif test par heure et type de jour",
              ylab = "Nombre de capteurs")
      legend("topright", legend = c("Semaine", "Weekend"),
             fill = c("#2166AC", "#B2182B"), cex = 0.8, bty = "n")

      # 3ter-d: Diagnostic textuel
      plot.new()
      title(main = "Diagnostic Semaine / Weekend", cex.main = 1.1)
      wd_global_mae <- mean(hour_wd_df$abs_db_error, na.rm = TRUE)
      we_global_mae <- mean(hour_we_df$abs_db_error, na.rm = TRUE)
      wd_global_bias <- mean(hour_wd_df$db_error, na.rm = TRUE)
      we_global_bias <- mean(hour_we_df$db_error, na.rm = TRUE)
      best_wd_h <- wd_stats$group[which.min(wd_stats$mae_xgb)]
      worst_wd_h <- wd_stats$group[which.max(wd_stats$mae_xgb)]
      best_we_h <- we_stats$group[which.min(we_stats$mae_xgb)]
      worst_we_h <- we_stats$group[which.max(we_stats$mae_xgb)]
      wd_we_diag <- c(
        sprintf("  Semaine : MAE = %.2f dB, Biais = %+.2f dB (n = %s)",
                wd_global_mae, wd_global_bias, fmt(nrow(hour_wd_df))),
        sprintf("  Weekend : MAE = %.2f dB, Biais = %+.2f dB (n = %s)",
                we_global_mae, we_global_bias, fmt(nrow(hour_we_df))),
        "",
        sprintf("  Meilleure heure semaine : %s (MAE = %.2f dB)",
                best_wd_h, min(wd_stats$mae_xgb)),
        sprintf("  Pire heure semaine      : %s (MAE = %.2f dB)",
                worst_wd_h, max(wd_stats$mae_xgb)),
        sprintf("  Meilleure heure weekend : %s (MAE = %.2f dB)",
                best_we_h, min(we_stats$mae_xgb)),
        sprintf("  Pire heure weekend      : %s (MAE = %.2f dB)",
                worst_we_h, max(we_stats$mae_xgb)),
        "",
        if (abs(wd_global_mae - we_global_mae) < 0.3) {
          "  Conclusion : performances similaires semaine/weekend."
        } else if (wd_global_mae < we_global_mae) {
          sprintf("  Conclusion : meilleure performance en semaine (ΔMAE = %.2f dB).",
                  we_global_mae - wd_global_mae)
        } else {
          sprintf("  Conclusion : meilleure performance le weekend (ΔMAE = %.2f dB).",
                  wd_global_mae - we_global_mae)
        }
      )
      text(0.02, seq(0.90, 0.90 - 0.06 * (length(wd_we_diag) - 1),
           length.out = length(wd_we_diag)),
           labels = wd_we_diag, adj = c(0, 0.5), cex = 0.82, family = "mono")

      mtext("Comparaison Semaine / Weekend — Qualité des prédictions horaires",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
    }
  }

  # ============================================================================
  # PAGE 4: Où la prédiction est meilleure/pire ? Analyse par highway
  # ============================================================================
  if ("highway" %in% names(emission_test)) {
    hw_stats <- summarize_group_metrics(emission_test, "highway")
    hw_stats <- hw_stats[order(-hw_stats$n), ]
    min_n_hw <- 20
    hw_stats <- hw_stats[hw_stats$n >= min_n_hw, , drop = FALSE]

    if (nrow(hw_stats) > 0) {
      par(mfrow = c(2, 2), mar = c(6, 4.5, 3.5, 1))

      # 4a: MAE par highway (trié par effectif)
      top_n <- head(hw_stats, 12)
      bp_hw <- barplot(top_n$mae_xgb,
                       names.arg = sprintf("%s\n(n=%s)", top_n$group, fmt(top_n$n)),
                       las = 2, col = "steelblue", border = NA,
                       main = "MAE par type de route (highway)",
                       ylab = "MAE (dB)", cex.main = 1.1)
      text(bp_hw, top_n$mae_xgb, labels = sprintf("%.1f", top_n$mae_xgb),
           pos = 3, cex = 0.75, font = 2)

      # 4b: Biais par highway
      bias_cols <- ifelse(top_n$bias_xgb >= 0, "#D6604D", "#4393C3")
      bp_hw2 <- barplot(top_n$bias_xgb,
                        names.arg = sprintf("%s\n(n=%s)", top_n$group, fmt(top_n$n)),
                        las = 2, col = bias_cols, border = NA,
                        main = "Biais par type de route (highway)",
                        ylab = "Biais (dB)", cex.main = 1.1)
      abline(h = 0, col = "red", lty = 2)
      text(bp_hw2, top_n$bias_xgb,
           labels = sprintf("%+.1f", top_n$bias_xgb),
           pos = ifelse(top_n$bias_xgb >= 0, 3, 1), cex = 0.75, font = 2)

      # 4c: Source dominante d'erreur par highway
      hw_source <- sapply(top_n$group, function(hw) {
        sub_idx <- which(emission_test$highway == hw)
        if (length(sub_idx) < 5) return(NA_real_)
        mae_f <- mean(abs(contrib_flow[sub_idx]), na.rm = TRUE)
        mae_s <- mean(abs(contrib_speed[sub_idx]), na.rm = TRUE)
        return(mae_f - mae_s)  # >0 means flux dominates, <0 speed dominates
      })
      hw_source_col <- ifelse(hw_source > 0, "#4393C3", "#D6604D")
      bp_hw3 <- barplot(hw_source,
                        names.arg = sprintf("%s", top_n$group),
                        las = 2, col = hw_source_col, border = NA,
                        main = "Source dominante d'erreur par highway",
                        ylab = "MAE(flux) - MAE(vitesse) (dB)", cex.main = 1.0)
      abline(h = 0, col = "grey40", lty = 1)
      legend("topright", legend = c("Flux domine (>0)", "Vitesse domine (<0)"),
             fill = c("#4393C3", "#D6604D"), cex = 0.7, bty = "n")

      # 4d: ΔMAE OSM vs XGBoost par highway
      if (has_osm_speed && all(is.finite(top_n$mae_osm))) {
        delta_mae <- top_n$mae_osm - top_n$mae_xgb
        delta_cols <- ifelse(delta_mae > 0, "#D6604D", "#2ca02c")
        bp_hw4 <- barplot(delta_mae,
                          names.arg = sprintf("%s", top_n$group),
                          las = 2, col = delta_cols, border = NA,
                          main = "ΔMAE = MAE(OSM) − MAE(XGBoost)",
                          ylab = "ΔMAE (dB)", cex.main = 1.0)
        abline(h = 0, col = "grey40")
        legend("topright", legend = c("OSM pire (>0)", "OSM meilleur (<0)"),
               fill = c("#D6604D", "#2ca02c"), cex = 0.7, bty = "n")
      } else {
        plot.new()
        title(main = "Vitesse OSM non disponible")
      }

      mtext("Analyse de la qualité dB par type de route (highway)",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)

      # --- Page 4bis: Diagnostic textuel highway ---
      par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
      plot.new()
      title(main = "Diagnostic par highway : où et pourquoi ?", cex.main = 1.1)

      best_hw <- top_n$group[which.min(top_n$mae_xgb)]
      worst_hw <- top_n$group[which.max(top_n$mae_xgb)]
      hw_diag <- c(
        "Analyse par type de route :", "",
        sprintf("  Meilleure prédiction : '%s' (MAE = %.2f dB, Biais = %+.2f dB, n = %s)",
                best_hw,
                top_n$mae_xgb[top_n$group == best_hw],
                top_n$bias_xgb[top_n$group == best_hw],
                fmt(top_n$n[top_n$group == best_hw])),
        sprintf("  Pire prédiction      : '%s' (MAE = %.2f dB, Biais = %+.2f dB, n = %s)",
                worst_hw,
                top_n$mae_xgb[top_n$group == worst_hw],
                top_n$bias_xgb[top_n$group == worst_hw],
                fmt(top_n$n[top_n$group == worst_hw])),
        ""
      )

      # Per-highway source analysis
      hw_diag <- c(hw_diag, "  Source dominante d'erreur par highway :")
      for (i in seq_len(nrow(top_n))) {
        hw_name <- top_n$group[i]
        sub_idx <- which(emission_test$highway == hw_name)
        mae_f <- mean(abs(contrib_flow[sub_idx]), na.rm = TRUE)
        mae_s <- mean(abs(contrib_speed[sub_idx]), na.rm = TRUE)
        dominant <- if (mae_f > mae_s) "flux" else "vitesse"
        hw_diag <- c(hw_diag, sprintf(
          "    %-20s : %s (MAE flux=%.2f, MAE vit=%.2f dB)",
          hw_name, dominant, mae_f, mae_s))
      }

      hw_diag <- c(hw_diag, "",
        "  Les routes principales (primary, trunk) ont souvent plus d'erreur car",
        "  le flux y est plus variable. Les routes résidentielles ont des flux",
        "  faibles avec une erreur absolue moindre mais relative plus grande.")

      text(0.02, seq(0.95, 0.95 - 0.04 * (length(hw_diag) - 1),
           length.out = length(hw_diag)),
           labels = hw_diag, adj = c(0, 0.5), cex = 0.72, family = "mono")
    }
  }

  # ============================================================================
  # PAGE 5: Analyse par DEGRE (densité urbaine)
  # ============================================================================
  if ("DEGRE" %in% names(emission_test)) {
    deg_stats <- summarize_group_metrics(emission_test, "DEGRE")
    deg_stats <- deg_stats[deg_stats$group != "NA", , drop = FALSE]
    if (nrow(deg_stats) > 0) {
      suppressWarnings(deg_num <- as.numeric(deg_stats$group))
      if (all(!is.na(deg_num))) deg_stats <- deg_stats[order(deg_num), ]

      deg_labels <- sprintf("DEGRE %s\n(n=%s)", deg_stats$group, fmt(deg_stats$n))
      deg_cols <- colorRampPalette(c("#d73027", "#fee08b", "#1a9850"))(nrow(deg_stats))

      par(mfrow = c(2, 2), mar = c(5.5, 4.5, 3.5, 1))

      # 5a: MAE par DEGRE
      bp_d <- barplot(deg_stats$mae_xgb, names.arg = deg_labels, las = 2,
                      col = deg_cols, border = NA,
                      main = "MAE par DEGRE (densité urbaine)",
                      ylab = "MAE (dB)", cex.main = 1.1)
      text(bp_d, deg_stats$mae_xgb, labels = sprintf("%.2f", deg_stats$mae_xgb),
           pos = 3, cex = 0.8, font = 2)

      # 5b: Biais par DEGRE
      bias_cols_d <- ifelse(deg_stats$bias_xgb >= 0, "#D6604D", "#4393C3")
      bp_d2 <- barplot(deg_stats$bias_xgb, names.arg = deg_labels, las = 2,
                       col = bias_cols_d, border = NA,
                       main = "Biais par DEGRE",
                       ylab = "Biais (dB)", cex.main = 1.1)
      abline(h = 0, col = "red", lty = 2)
      text(bp_d2, deg_stats$bias_xgb,
           labels = sprintf("%+.2f", deg_stats$bias_xgb),
           pos = ifelse(deg_stats$bias_xgb >= 0, 3, 1), cex = 0.8, font = 2)

      # 5c: Source dominante par DEGRE
      deg_source <- sapply(deg_stats$group, function(dg) {
        sub_idx <- which(as.character(emission_test$DEGRE) == dg)
        if (length(sub_idx) < 5) return(NA_real_)
        mae_f <- mean(abs(contrib_flow[sub_idx]), na.rm = TRUE)
        mae_s <- mean(abs(contrib_speed[sub_idx]), na.rm = TRUE)
        return(mae_f - mae_s)
      })
      deg_source_col <- ifelse(deg_source > 0, "#4393C3", "#D6604D")
      bp_d3 <- barplot(deg_source,
                       names.arg = sprintf("D%s", deg_stats$group),
                       las = 2, col = deg_source_col, border = NA,
                       main = "Source dominante d'erreur par DEGRE",
                       ylab = "MAE(flux) - MAE(vitesse) (dB)", cex.main = 1.0)
      abline(h = 0, col = "grey40")
      legend("topright", legend = c("Flux domine", "Vitesse domine"),
             fill = c("#4393C3", "#D6604D"), cex = 0.7, bty = "n")

      # 5d: Comparaison A vs B par DEGRE
      if (has_osm_speed && all(is.finite(deg_stats$mae_osm))) {
        x <- seq_len(nrow(deg_stats))
        y_lim <- range(c(deg_stats$mae_xgb, deg_stats$mae_osm), na.rm = TRUE)
        plot(x, deg_stats$mae_xgb, type = "b", pch = 16, col = "#1f77b4",
             xaxt = "n", ylim = y_lim, lwd = 2,
             xlab = "DEGRE", ylab = "MAE (dB)",
             main = "MAE : vitesse prédite vs OSM par DEGRE", cex.main = 1.0)
        lines(x, deg_stats$mae_osm, type = "b", pch = 17, col = "#d62728", lwd = 2)
        axis(1, at = x, labels = paste0("D", deg_stats$group))
        legend("topleft", legend = c("Vitesse prédite", "Vitesse OSM"),
               col = c("#1f77b4", "#d62728"), pch = c(16, 17), lwd = 2, bty = "n", cex = 0.8)
      } else {
        plot.new()
        title(main = "Vitesse OSM non disponible")
      }

      mtext("Analyse de la qualité dB par densité urbaine (DEGRE INSEE)",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
    }
  }

  # ============================================================================
  # PAGE 6: Vitesse réglementaire (OSM) vs vitesse prédite — impact sur les dB
  # ============================================================================
  if (has_osm_speed) {
    par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1))

    # 6a: Histogrammes superposés des erreurs dB
    breaks_range <- range(c(emission_test$db_error, emission_test$db_error_osm), na.rm = TRUE)
    breaks_seq <- seq(floor(breaks_range[1]), ceiling(breaks_range[2]), length.out = 60)
    h_xgb <- hist(emission_test$db_error, breaks = breaks_seq, plot = FALSE)
    h_osm <- hist(emission_test$db_error_osm, breaks = breaks_seq, plot = FALSE)
    ylim_max <- max(c(h_xgb$counts, h_osm$counts))
    plot(h_xgb, col = rgb(0.2, 0.4, 0.8, 0.5), border = "white",
         main = "Erreur dB : vitesse prédite vs vitesse OSM",
         xlab = "Erreur = Prédit - Observé (dB)", ylim = c(0, ylim_max * 1.1),
         cex.main = 1.1)
    plot(h_osm, col = rgb(0.9, 0.4, 0.2, 0.5), border = "white", add = TRUE)
    abline(v = 0, col = "red", lwd = 2)
    legend("topright", legend = c(
      sprintf("Vitesse prédite (biais=%+.1f dB)", db_bias),
      sprintf("Vitesse OSM (biais=%+.1f dB)", db_bias_osm)),
      fill = c(rgb(0.2, 0.4, 0.8, 0.5), rgb(0.9, 0.4, 0.2, 0.5)),
      cex = 0.75, bty = "n")

    # 6b: CDF comparison
    abs_xgb_sorted <- sort(emission_test$abs_db_error)
    abs_osm_sorted <- sort(emission_test$abs_db_error_osm)
    ecdf_xgb <- seq_along(abs_xgb_sorted) / length(abs_xgb_sorted)
    ecdf_osm <- seq_along(abs_osm_sorted) / length(abs_osm_sorted)
    xlim_max <- max(c(abs_xgb_sorted, abs_osm_sorted), na.rm = TRUE)
    plot(abs_xgb_sorted, ecdf_xgb, type = "l", lwd = 2, col = rgb(0.2, 0.4, 0.8),
         xlim = c(0, min(xlim_max, 20)),
         xlab = "Erreur absolue (dB)", ylab = "Proportion cumulée",
         main = "CDF : vitesse prédite vs OSM", cex.main = 1.1)
    lines(abs_osm_sorted, ecdf_osm, lwd = 2, col = rgb(0.9, 0.4, 0.2))
    abline(h = 0.9, col = "grey60", lty = 2)
    legend("bottomright", legend = c(
      sprintf("Prédite (MAE=%.1f dB)", db_mae),
      sprintf("OSM (MAE=%.1f dB)", db_mae_osm)),
      lwd = 2, col = c(rgb(0.2, 0.4, 0.8), rgb(0.9, 0.4, 0.2)),
      cex = 0.75, bty = "n")

    # 6c: Scatter vitesse prédite vs OSM
    plot(emission_test$pred_speed, emission_test$osm_speed,
         pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.15),
         xlab = "Vitesse prédite XGBoost (km/h)",
         ylab = "Vitesse réglementaire OSM (km/h)",
         main = "Vitesse prédite vs vitesse OSM", cex.main = 1.1)
    abline(a = 0, b = 1, col = "red", lwd = 2)
    speed_cor <- cor(emission_test$pred_speed, emission_test$osm_speed, use = "complete.obs")
    legend("topleft", legend = c(
      sprintf("r = %.2f", speed_cor),
      sprintf("Médiane prédite = %.0f km/h", median(emission_test$pred_speed, na.rm = TRUE)),
      sprintf("Médiane OSM = %.0f km/h", median(emission_test$osm_speed, na.rm = TRUE))),
      cex = 0.75, bty = "n")

    # 6d: Verdict textuel
    plot.new()
    title(main = "Verdict : vitesse OSM améliore ou dégrade ?", cex.main = 1.1)

    delta_bias <- db_bias_osm - db_bias
    delta_mae <- db_mae_osm - db_mae
    delta_rmse <- db_rmse_osm - db_rmse

    verdict_lines <- c(
      "Comparaison de la qualité dB sur les tronçons tests :", "",
      sprintf("                    Vitesse prédite    Vitesse OSM     Δ (OSM-Préd)"),
      sprintf("  Biais (dB)      : %+.2f              %+.2f            %+.2f",
              db_bias, db_bias_osm, delta_bias),
      sprintf("  MAE (dB)        : %.2f               %.2f             %+.2f",
              db_mae, db_mae_osm, delta_mae),
      sprintf("  RMSE (dB)       : %.2f               %.2f             %+.2f",
              db_rmse, db_rmse_osm, delta_rmse),
      ""
    )

    if (delta_mae > 0.05) {
      verdict_lines <- c(verdict_lines,
        sprintf("  VERDICT : La vitesse OSM DÉGRADE la prédiction de %.2f dB (MAE).", delta_mae),
        "  -> La vitesse prédite par XGBoost est préférable.")
    } else if (delta_mae < -0.05) {
      verdict_lines <- c(verdict_lines,
        sprintf("  VERDICT : La vitesse OSM AMÉLIORE la prédiction de %.2f dB (MAE).", abs(delta_mae)),
        "  -> La vitesse réglementaire OSM serait un meilleur choix.")
    } else {
      verdict_lines <- c(verdict_lines,
        "  VERDICT : Les deux vitesses donnent des résultats quasi identiques.",
        "  -> Différence < 0.05 dB en MAE, choix indifférent.")
    }

    verdict_lines <- c(verdict_lines, "",
      "  Note : 'observé' = CNOSSOS(flow_obs, truck%_obs, speed_obs).",
      sprintf("  78%% des capteurs AVATAR utilisent maxspeed OSM comme 'speed_obs'."),
      "  Le biais vitesse inclut donc partiellement un biais de référence."
    )

    text(0.02, seq(0.95, 0.95 - 0.055 * (length(verdict_lines) - 1),
         length.out = length(verdict_lines)),
         labels = verdict_lines, adj = c(0, 0.5), cex = 0.78, family = "mono")

    mtext("Comparaison vitesse prédite vs vitesse réglementaire OSM",
          side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
  }

  # ============================================================================
  # PAGE 6bis: Top erreurs sur tronçons tests (diagnostic concret)
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main = "Top erreurs dB sur tronçons tests (cas concrets)", cex.main = 1.1)

  top_k <- min(25L, nrow(emission_test))
  idx_top <- order(emission_test$abs_db_error, decreasing = TRUE)[seq_len(top_k)]
  top_err <- emission_test[idx_top, , drop = FALSE]

  top_lines <- c(
    sprintf("Top %d erreurs absolues (|Pred-Obs|)", top_k),
    "",
    "rank  osm_id        period  hwy            DEGRE   err_dB  abs_dB  src_dom  flow_dB  speed_dB  truck_dB"
  )

  for (i in seq_len(nrow(top_err))) {
    rr <- top_err[i, ]
    row_idx <- idx_top[i]
    c_flow <- contrib_flow[row_idx]
    c_speed <- contrib_speed[row_idx]
    c_truck <- contrib_truck[row_idx]
    src_dom <- c("Flux", "Vitesse", "%PL")[which.max(c(abs(c_flow), abs(c_speed), abs(c_truck)))]

    top_lines <- c(top_lines, sprintf(
      "%3d  %-12s %-6s %-13s %-6s %7.2f %7.2f %-8s %8.2f %9.2f %9.2f",
      i,
      as.character(rr$osm_id),
      as.character(rr$period),
      substr(as.character(rr$highway), 1, 13),
      substr(as.character(rr$DEGRE), 1, 6),
      rr$db_error,
      rr$abs_db_error,
      src_dom,
      c_flow,
      c_speed,
      c_truck
    ))
  }

  top_lines <- c(
    top_lines,
    "",
    "Lecture: src_dom = variable qui explique la plus grande part de l'erreur en valeur absolue.",
    "Ces cas permettent d'identifier précisément où améliorer le pipeline."
  )

  text(0.02,
       seq(0.96, 0.96 - 0.03 * (length(top_lines) - 1), length.out = length(top_lines)),
       labels = top_lines, adj = c(0, 0.5), cex = 0.66, family = "mono")

  # ============================================================================
  # PAGE 7: Synthèse finale
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main = "Synthèse de l'évaluation acoustique sur tronçons tests",
        cex.main = 1.2)

  synth_lines <- c(
    sprintf("Données : %s tronçons-périodes évalués via CNOSSOS-EU 2020", fmt(nrow(emission_test))),
    sprintf("  dont %s avec vitesse mesurée, %s avec %% PL mesuré",
            fmt(sum(emission_test$has_measured_speed, na.rm = TRUE)),
            fmt(sum(emission_test$has_measured_truck, na.rm = TRUE))),
    "",
    "═══ Qualité globale ═══",
    sprintf("  Biais = %+.2f dB | MAE = %.2f dB | RMSE = %.2f dB", db_bias, db_mae, db_rmse),
    ""
  )

  # Source d'erreur
  ord <- order(mae_contrib, decreasing = TRUE)
  synth_lines <- c(synth_lines,
    "═══ Source principale d'erreur ═══",
    sprintf("  1. %s (MAE = %.2f dB, biais = %+.2f dB)",
            c(Flow = "Flux", `Truck %` = "% Poids lourds", Speed = "Vitesse")[names(mae_contrib)[ord[1]]],
            mae_contrib[ord[1]], mean_contrib[ord[1]]),
    sprintf("  2. %s (MAE = %.2f dB, biais = %+.2f dB)",
            c(Flow = "Flux", `Truck %` = "% Poids lourds", Speed = "Vitesse")[names(mae_contrib)[ord[2]]],
            mae_contrib[ord[2]], mean_contrib[ord[2]]),
    sprintf("  3. %s (MAE = %.2f dB, biais = %+.2f dB)",
            c(Flow = "Flux", `Truck %` = "% Poids lourds", Speed = "Vitesse")[names(mae_contrib)[ord[3]]],
            mae_contrib[ord[3]], mean_contrib[ord[3]]),
    ""
  )

  # Vitesse verdict
  if (has_osm_speed) {
    delta_mae_v <- db_mae_osm - db_mae
    if (delta_mae_v > 0.05) {
      synth_lines <- c(synth_lines,
        "═══ Vitesse réglementaire vs prédite ═══",
        sprintf("  Utiliser la vitesse OSM DÉGRADERAIT la MAE de %.2f dB.", delta_mae_v),
        "  -> Conserver la vitesse prédite par XGBoost.", "")
    } else if (delta_mae_v < -0.05) {
      synth_lines <- c(synth_lines,
        "═══ Vitesse réglementaire vs prédite ═══",
        sprintf("  Utiliser la vitesse OSM AMÉLIORERAIT la MAE de %.2f dB.", abs(delta_mae_v)),
        "  -> Envisager la vitesse OSM pour les prédictions.", "")
    } else {
      synth_lines <- c(synth_lines,
        "═══ Vitesse réglementaire vs prédite ═══",
        "  Différence négligeable entre les deux vitesses (< 0.05 dB).", "")
    }
  }

  # Best/worst
  if ("highway" %in% names(emission_test) && exists("top_n") && nrow(top_n) > 0) {
    best_hw_s <- top_n$group[which.min(top_n$mae_xgb)]
    worst_hw_s <- top_n$group[which.max(top_n$mae_xgb)]
    synth_lines <- c(synth_lines,
      "═══ Par type de route ═══",
      sprintf("  Meilleur : %s (MAE = %.2f dB)", best_hw_s,
              top_n$mae_xgb[top_n$group == best_hw_s]),
      sprintf("  Pire     : %s (MAE = %.2f dB)", worst_hw_s,
              top_n$mae_xgb[top_n$group == worst_hw_s]),
      "")
  }

  synth_lines <- c(synth_lines,
    "═══ Lecture du rapport ═══",
    "  Biais > 0 : surestimation du niveau sonore",
    "  Biais < 0 : sous-estimation du niveau sonore",
    "  Obs = CNOSSOS(flow_obs, truck%_obs, speed_obs) sur capteurs AVATAR test"
  )

  text(0.02, seq(0.95, 0.95 - 0.032 * (length(synth_lines) - 1),
       length.out = length(synth_lines)),
       labels = synth_lines, adj = c(0, 0.5), cex = 0.72, family = "mono")

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

assign(x = "xgb_models_with_ratios", value = models_list, envir = .GlobalEnv)

pipeline_message("Successfully trained learning model", 
                 level = 0, progress = "end", process = "valid")
