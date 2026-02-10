# ==============================================================================
# STAGE 6D: XGBOOST TRAINING LEARNING MODEL
# ==============================================================================

pipeline_message(text = "Training the learning model", 
                 level = 0, progress = "start", process = "learn")

# ==============================================================================
# Architecture:
#   1. Base models for period D (day): 
#      - flow_D: Total traffic (vehicles/hour)
#      - truck_pct_D: Truck percentage (HGV/TV as percentage 0-100%)
#      - speed_D: Average speed (km/h)
#   2. Ratio models for other periods (E, N, h0-h23):
#      - ratio_flow_P: flow_P / flow_D
#      - ratio_truck_pct_P: truck_pct_P / truck_pct_D (ratio of percentages)
#      - ratio_speed_P: speed_P / speed_D
#   3. Final predictions:
#      - flow_P = flow_D x ratio_flow_P
#      - truck_pct_P = truck_pct_D x ratio_truck_pct_P
#      - HGV_P = flow_P x (truck_pct_P / 100)
#      - LV_P = flow_P - HGV_P
#      - speed_P = speed_D x ratio_speed_P
# Benefits:
#   - Truck percentage more stable than absolute truck count
#   - Ratios capture temporal patterns (more trucks at night on highways)
#   - Prevents HGV > TV inconsistencies
#   - Better performance for periods with sparse data
# ==============================================================================

# ------------------------------------------------------------------------------
# Load training data for the learning model
# ------------------------------------------------------------------------------

pipeline_message(
  text = sprintf("Loading training data for the learning model from %s", 
                 rel_path(CONFIG$TRAINING_RDS_DATA_FILEPATH)), 
  level = 1, progress = "start", process = "load")

if (!file.exists(CONFIG$TRAINING_RDS_DATA_FILEPATH)) {
  pipeline_message(
    text = sprintf("File %s doesn't exists. Training data is required to train 
                   the model. Run the data preparation scripts to generate the 
                   dataset.", CONFIG$TRAINING_RDS_DATA_FILEPATH), 
    process = "stop")
}

training_data <- readRDS(CONFIG$TRAINING_RDS_DATA_FILEPATH)

pipeline_message(text = describe_df(training_data), process = "info")

pipeline_message(text = "Training data successfully loaded", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Global configuration of features and periods
# ------------------------------------------------------------------------------

pipeline_message(text = "Configuring training model", 
                 level = 1, progress = "start", process = "configure")

# ****************************** #
# OSM variables used as features #
# ****************************** #

# Check available OSM network features
available_road_features <- c("highway", "DEGRE", "ref_letter", "first_word", 
                             "oneway_osm", "lanes_osm", "speed", 
                             "connectivity", "betweenness", 
                             "closeness", "pagerank")

# Build formula with existing features
road_feature_formula <- as.formula(
  object = paste("~", paste(available_road_features, collapse = " + ")))

# Temporal periods configuration
all_periods <- c("D", "E", "N", paste0("h", 0:23))

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
    name = "Speed (Day)",
    period = "D",
    target = "aggregate_speed",
    baseline = "speed_D",
    transform = NULL,
    min_valid = 5))

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

pipeline_message(text = sprintf("Number of variables to be estimated: %d",  
                                length(x = all_configs)), 
                 process = "info")

pipeline_message(text = "Training model successfully configured", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Train models
# ------------------------------------------------------------------------------

models_list <- list()
results_summary <- data.frame()

for (model_name in names(all_configs)) {
  
  # Current configuration
  config <- all_configs[[model_name]]
  
  pipeline_message(text = sprintf("Training step [%d/%d] - Estimation of the variable %s", 
                                  which(names(all_configs) == model_name), 
                                  length(x = all_configs), model_name), 
                   level = 1, progress = "start", process = "wait")
  
  pipeline_message(text = "Time period selection and target filtering", 
                   level = 2, progress = "start", process = "configure")
  
  # Filter data for this period
  training_data_over_period <- training_data %>% 
    filter(period == config$period)
  
  # Extract target variable
  training_data_target <- training_data_over_period[[config$target]]
  
  # Filter valid target data
  valid_idx <- !is.na(training_data_target) & 
    training_data_target >= config$min_valid
  
  # More permissive threshold for truck models (less data available than for 
  # light vehicles)
  min_obs_threshold <- ifelse(test = grepl(pattern = "truck", 
                                           x = config$target), 
                              yes = 20, 
                              no = 50)
  # Verification of the number of observations
  if (sum(valid_idx) < min_obs_threshold) {
    pipeline_message(
      text = sprintf("Too few valid observations for trucks (< %d). Skipping!", 
                     min_obs_threshold), 
      process = "warning")
    next
  }
  
  pipeline_message(
    text = sprintf("Number of valid observations: %d", sum(valid_idx)), 
    process = "info")
  
  # Filter data
  clean_training_data_over_period <- training_data_over_period[valid_idx, ]
  clean_training_data_target <- training_data_target[valid_idx]
  
  pipeline_message(text = "Time period selected and target data filtered", 
                   level = 2, progress = "end", process = "valid")
  
  pipeline_message(text = "Construction of the sparse feature matrix", 
                   level = 2, progress = "start", process = "calc")
  
  # Create sparse feature matrix (may eliminate more rows due to NA in features)
  sparse_data_matrix <- Matrix::sparse.model.matrix(
    object = road_feature_formula, 
    data = clean_training_data_over_period)
  
  pipeline_message(text = "Sparse feature matrix constructed successfully", 
                   level = 2, progress = "end", process = "valid")
  
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
  }
  
  pipeline_message(
    text = "Target transformation and split of the training and testing data", 
    level = 2, progress = "start", process = "calc")
  
  # Apply log transform to aligned target
  if (!is.null(config$transform) && config$transform == "log10") {
    transformed_training_data_target <- log10(
      x = pmax(clean_training_data_target, config$min_valid))
  } else {
    transformed_training_data_target <- clean_training_data_target
  }
  
  # Verify dimensions are now synchronized
  pipeline_message(
    text = sprintf("Final data: %d rows (Sparse matrix: %d x %d | Target data: %d)", 
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
  train_idx <- sample(x = seq_len(to = n_final), 
                      size = floor(x = 0.8 * n_final))
  test_idx <- setdiff(x = seq_len(n_final), 
                      y = train_idx)
  
  X_train <- sparse_data_matrix[train_idx, ]
  X_test <- sparse_data_matrix[test_idx, ]
  y_train <- transformed_training_data_target[train_idx]
  y_test <- transformed_training_data_target[test_idx]
  
  # Check for invalid values in labels
  invalid_train <- is.na(y_train) | is.infinite(y_train) | is.nan(y_train)
  invalid_test <- is.na(y_test) | is.infinite(y_test) | is.nan(y_test)
  
  if (any(invalid_train) || any(invalid_test)) {
    pipeline_message(
      text = sprintf("Found %d invalid train labels and %d invalid test labels", 
                     sum(invalid_train), sum(invalid_test)), 
      process = "warning")
    
    # Remove invalid observations
    if (any(invalid_train)) {
      valid_train_idx <- !invalid_train
      X_train <- X_train[valid_train_idx, ]
      y_train <- y_train[valid_train_idx]
    }
    if (any(invalid_test)) {
      valid_test_idx <- !invalid_test
      X_test <- X_test[valid_test_idx, ]
      y_test <- y_test[valid_test_idx]
    }
  }
  
  # Final check
  if (length(x = y_train) < 10 || length(x = y_test) < 5) {
    next
  }
  
  n_train <- length(x = y_train)
  n_test <- length(x = y_test)
  n_total <- n_train + n_test
  
  pipeline_message(text = sprintf("Training: %s observations (%.1f%%)", 
                                  fmt(n_train), 
                                  100 * n_train / n_total), 
                   process = "info")
  pipeline_message(text = sprintf("Test: %s observations (%.1f%%)", 
                                  fmt(n_test), 
                                  100 * n_test / n_total), 
                   process = "info")
  pipeline_message(text = sprintf("Range of learning values: [%.3f, %.3f]", 
                                  min(y_train, na.rm=TRUE), 
                                  max(y_train, na.rm=TRUE)), 
                   process = "info")
  
  pipeline_message(
    text = "Target transformed and training/testing data splitted", 
    level = 2, progress = "end", process = "valid")
  
  # Train model
  pipeline_message(
    text = "Training of the learning model", 
    level = 2, progress = "start", process = "learn")
  
  # Adaptive training strategy for small samples
    use_watchlist <- TRUE
  if (length(y_train) < 100 || length(y_test) < 30) {
    pipeline_message(
      text = sprintf("Small sample detected:\n\t\t 
                     -> train = %d\n\t\t 
                     -> test = %d\n\t\t 
                     ===> Disabling early stopping and watchlist", 
                     length(y_train), length(y_test)), 
      process = "warning")
    
    use_watchlist <- FALSE
  }
  
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train) 
  dtest <- xgboost::xgb.DMatrix(data = X_test, label = y_test)
  
  # Choose parameters and training strategy based on model type
  start_timer()
  if (grepl(pattern = "truck", x = config$target)) {
    params <- CONFIG$TRUCK_PARAMS
    # For truck models with small samples, use CV for robust estimation
    if (length(x = y_train) < 200) {
      nfold <- min(5, length(x = y_train) %/% 10)
      if (nfold >= 2) {
        cv_result <- xgboost::xgb.cv(
          params = params,
          data = dtrain,
          nrounds = CONFIG$NROUNDS,
          nfold = nfold,  # Adaptive CV folds
          early_stopping_rounds = 30,
          verbose = 0,
          showsd = FALSE)
        best_rounds <- cv_result$best_iteration
        pipeline_message(text = sprintf("CV selected %d rounds (from max %d)", 
                                        best_rounds, CONFIG$NROUNDS), 
                         process = "clip")
      } else {
        best_rounds <- CONFIG$NROUNDS
      }
    } else {
      best_rounds <- CONFIG$NROUNDS
    }
  } else {
    params <- CONFIG$TRAINING_PARAMS
    best_rounds <- CONFIG$NROUNDS
  }
  
  # Acceptable limits for training
  min_train_xgb <- 150
  min_test_xgb  <- 50
  if (config$period == "D" && config$target %in% c("truck_pct", "aggregate_speed")) {
    min_train_xgb <- 10
    min_test_xgb  <- 5
  }
  if (length(y_train) < min_train_xgb || length(y_test) < min_test_xgb) {
    pipeline_message(
      text = sprintf("Sample too small for XGBoost:\n\t\t 
                     -> train = %d\n\t\t 
                     -> test = %d\n\t\t 
                     ===> Model skipped!", 
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
  
  pipeline_message(
    text = "Learning model trained successfully", 
    level = 2, progress = "end", process = "valid")
  
  pipeline_message(
    text = "Assessment and diagnostics of the learning model", 
    level = 2, progress = "start", process = "search")
  
  # Evaluate
  pred_test <- predict(xgb_model, X_test)
  
  # Back-transform if needed
  if (!is.null(config$transform) && config$transform == "log10") {
    pred_original <- 10^pred_test
    actual_original <- 10^y_test
  } else {
    pred_original <- pred_test
    actual_original <- y_test
  }
  
  # Metrics
  mae <- mean(x = abs(x = pred_original - actual_original), 
              na.rm = TRUE)
  rmse <- sqrt(x = mean(x = (pred_original - actual_original)^2, 
                        na.rm = TRUE))
  r2 <- cor(x = pred_original, y = actual_original, use = "complete.obs")^2
  mape_values <- abs(
    x = (pred_original - actual_original) / pmax(actual_original, 0.01)) * 100
  mape <- median(x = mape_values[is.finite(mape_values)], 
                 na.rm = TRUE)
  
  pipeline_message(text = sprintf("R²=%.3f | MAPE=%.1f%%", r2, mape), 
                   process = "info")
  
  # Feature importance analysis
  importance <- xgboost::xgb.importance(model = xgb_model)
  top_features <- head(x = importance, 5)  # Top 5 most important features
  
  pipeline_message(
    text = paste("Top 5 most important features:", 
                 paste0(sprintf("\t\t\t%d. %-15s (%.1f%%)", 
                                seq_len(nrow(top_features)), 
                                top_features$Feature, 
                                top_features$Gain * 100), collapse = "\n"), 
                 sep = "\n"), 
    process = "info")
  
  pipeline_message(
    text = "Learning model evaluated and diagnosed", 
    level = 2, progress = "end", process = "valid")
  
  pipeline_message(
    text = "Storage of the model and statistical evaluation indicators", 
    level = 2, progress = "start", process = "save")
  
  # Store model
  models_list[[model_name]] <- list(
    model = xgb_model,
    config = config,
    metrics = list(mae = mae, rmse = rmse, r2 = r2, mape = mape),
    feature_names = colnames(X_train),
    feature_importance = importance,  # Full importance table
    top_features = top_features,      # Top 5 for quick reference
    n_train = length(x = y_train),
    n_test = length(x = y_test),
    training_time = elapsed)
  
  # Add to summary
  results_summary <- rbind(results_summary, 
                           data.frame(
                             Model = model_name, 
                             Target = config$name, 
                             N_train = length(x = y_train), 
                             N_test = length(x = y_test), 
                             R2 = round(x = r2, digits = 3), 
                             MAE = round(x = mae, digits = 2), 
                             RMSE = round(x = rmse, digits = 2), 
                             MAPE = round(x = mape, digits = 1), 
                             Time_min = round(x = elapsed, digits = 2)))
  
  pipeline_message(
    text = "Model and statistical evaluation indicators successfully stored", 
    level = 2, progress = "end", process = "valid")
  
  pipeline_message(text = sprintf("Training of the learning model for the 
                                  estimation of the variable %s completed", 
                                  model_name), 
                   level = 1, progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# Save models
# ------------------------------------------------------------------------------

pipeline_message(
  text = sprintf("Save training models and features in files %s and %s 
                 respectively", 
                 rel_path(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH), 
                 rel_path(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)), 
  level = 1, progress = "start", process = "save")

# Save list of models and road feature formula
saveRDS(object = models_list, 
        file = CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
saveRDS(object =  list(road_feature_formula = road_feature_formula, 
                       all_periods = all_periods), 
        file = CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)

pipeline_message(
  text = "Training models and features successfully saved in *.rds files", 
  level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Final summary
# ------------------------------------------------------------------------------

pipeline_message(
  text = sprintf("Results summary: \n", 
                 paste0(capture.output(results_summary), collapse = "\n\t\t")), 
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
      text = sprintf("%2d. %-20s | Avg Gain: %5.1f%% | Used in %2d/%2d models", 
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
    pipeline_message(
      text = sprintf("%d. %-20s (%.1f%%)", i, base_importance$Feature[i], 
                     base_importance$AvgGain[i] * 100), 
      process = "info")
  }
  
  for (i in 1:nrow(x = ratio_importance)) {
    pipeline_message(text = sprintf("%d. %-20s (%.1f%%)", i, 
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
  text = sprintf("R²: %.3f", mean(x = base_results$R2, na.rm = TRUE)), 
                 process = "info")
pipeline_message(
  text = sprintf("MAPE: %.1f%%", mean(x = base_results$MAPE, na.rm = TRUE)), 
                 process = "info")
pipeline_message(
  text = sprintf("R²: %.3f", mean(x = ratio_results$R2, na.rm = TRUE)), 
  process = "info")
pipeline_message(
  text = sprintf("MAPE: %.1f%%", mean(x = ratio_results$MAPE, na.rm = TRUE)), 
  process = "info")

pipeline_message(text = "Results summary completed", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Error analysis in percentage
# ------------------------------------------------------------------------------

pipeline_message(text = "Error analysis", 
                 level = 1, progress = "start", process = "plot")

# Compute percentage errors for each model
for (model_name in names(models_list)) {
  model_info <- models_list[[model_name]]
  config <- model_info$config
  
  # Get test predictions and actual values
  test_data <- training_data %>% filter(period == config$period)
  y_all <- test_data[[config$target]]
  valid_idx <- !is.na(y_all) & y_all >= config$min_valid
  
  if (sum(valid_idx) < 10) next
  
  data_clean <- test_data[valid_idx, ]
  y_clean <- y_all[valid_idx]
  
  sparse_data_matrix <- Matrix::sparse.model.matrix(object = road_feature_formula, 
                                       data = data_clean)
  
  if (nrow(x = sparse_data_matrix) != nrow(x = data_clean)) {
    kept_rows <- as.integer(rownames(sparse_data_matrix))
    y_clean <- y_clean[kept_rows]
    data_clean <- data_clean[kept_rows, ]
  }
  
  # Apply transform
  if (!is.null(config$transform) && config$transform == "log10") {
    transformed_training_data_target <- log10(pmax(y_clean, config$min_valid))
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
  if (!is.null(config$transform) && config$transform == "log10") {
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
  pipeline_message(text = sprintf("%-30s | Period: %-2s | N=%4d", config$name, 
                                  config$period, length(x = pct_errors)), 
                   process = "search")
  pipeline_message(text = sprintf("Mean Error: %+6.1f%% (bias)", 
                                  mean_pct_error), 
                   process = "search")
  pipeline_message(text = sprintf("Median Error: %+6.1f%%", median_pct_error), 
                   process = "search")
  pipeline_message(text = sprintf("MAE: %6.1f%% (average absolute error)", 
                                  mae_pct), 
                   process = "search")
  pipeline_message(text = sprintf("Q25-Q75: [%+6.1f%%, %+6.1f%%]", q25, q75), 
                   process = "search")
}

pipeline_message(text = "Error analysis completed", 
                 level = 1, progress = "end", process = "valid")

assign("xgb_models_with_ratios", models_list, envir = .GlobalEnv)

pipeline_message(text = "Successfully trained learning model", 
                 level = 0, progress = "end", process = "valid")