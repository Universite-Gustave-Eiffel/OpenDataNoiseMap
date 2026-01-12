# =============================================================================
# STAGE 6D: XGBOOST TRAINING WITH RATIO MODELS
# =============================================================================
# Architecture:
#   1. Base models for period D (day): 
#      - flow_D: Total traffic (vehicles/hour)
#      - truck_pct_D: Truck percentage (HGV/TV as percentage 0-100%)
#      - speed_D: Average speed (km/h)
#   
#   2. Ratio models for other periods (E, N, h0-h23):
#      - ratio_flow_P: flow_P / flow_D
#      - ratio_truck_pct_P: truck_pct_P / truck_pct_D (ratio of percentages)
#      - ratio_speed_P: speed_P / speed_D
#   
#   3. Final predictions:
#      - flow_P = flow_D × ratio_flow_P
#      - truck_pct_P = truck_pct_D × ratio_truck_pct_P
#      - HGV_P = flow_P × (truck_pct_P / 100)
#      - LV_P = flow_P - HGV_P
#      - speed_P = speed_D × ratio_speed_P
#
# Benefits:
#   - Truck percentage more stable than absolute truck count
#   - Ratios capture temporal patterns (more trucks at night on highways)
#   - Prevents HGV > TV inconsistencies
#   - Better performance for periods with sparse data
# =============================================================================

library(xgboost)
library(dplyr)
library(Matrix)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Training mode
QUICK_TRAIN_MODE <- TRUE
RUN_HYPERPARAMETER_TUNING <- FALSE

# HIGH PERFORMANCE PARAMS (better accuracy, slower training)
QUICK_PARAMS <- list(
  max_depth = 10,           # Deeper trees for complex patterns
  eta = 0.05,              # Lower learning rate for finer optimization  
  subsample = 0.9,         # More data per tree
  colsample_bytree = 0.9,  # More features per tree
  min_child_weight = 2,    # Better regularization
  gamma = 0.1,             # Minimum loss reduction
  reg_alpha = 0.01,        # L1 regularization
  reg_lambda = 0.01,       # L2 regularization
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# ENHANCED TRUCK PARAMS (balanced between complexity and small sample)
TRUCK_PARAMS <- list(
  max_depth = 6,           # Deeper than before but controlled
  eta = 0.1,               # Moderate learning rate
  subsample = 0.95,        # Use most data (small sample)
  colsample_bytree = 0.9,  # Use most features
  min_child_weight = 1,    # Less strict regularization
  gamma = 0.05,            # Small loss reduction threshold
  reg_alpha = 0.005,       # Light L1 regularization
  reg_lambda = 0.01,       # Light L2 regularization
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

NROUNDS <- 1500  # More boosting rounds for better convergence


if (QUICK_TRAIN_MODE) {
} else {
}

# =============================================================================
# LOAD DATA
# =============================================================================

training_data <- readRDS("data/training_data.rds")

# Check which network features are available
available_features <- c("highway", "DEGRE", "ref_letter", "first_word", "oneway_osm", 
                       "lanes_osm", "speed", "connectivity", "betweenness", "closeness", "pagerank")

# Build formula with existing features
feature_formula <- as.formula(paste("~", paste(available_features, collapse = " + ")))

# =============================================================================
# TARGET CONFIGURATIONS  
# =============================================================================

# Define all periods
all_periods <- c("D", "E", "N", paste0("h", 0:23))

# Base models (period D only)
base_configs <- list(
  flow_D = list(
    name = "Traffic Flow (Day)",
    period = "D",
    target = "aggregate_flow",
    baseline = "flow_D",
    transform = "log10",
    min_valid = 1
  ),
  truck_pct_D = list(
    name = "Truck Percentage (Day)",
    period = "D",
    target = "truck_pct",
    baseline = "truck_pct_D",
    transform = NULL,  # Already percentage 0-100
    min_valid = 0
  ),
  speed_D = list(
    name = "Speed (Day)",
    period = "D",
    target = "aggregate_speed",
    baseline = "speed_D",
    transform = NULL,
    min_valid = 5
  )
)

# Ratio models (all periods except D)
ratio_configs <- list()
ratio_periods <- setdiff(all_periods, "D")

for (p in ratio_periods) {
  ratio_configs[[paste0("ratio_flow_", p)]] <- list(
    name = paste0("Flow Ratio (", p, "/D)"),
    period = p,
    target = "ratio_flow",
    transform = NULL,  # No log transform for ratios!
    min_valid = 0.01
  )
  
  ratio_configs[[paste0("ratio_truck_pct_", p)]] <- list(
    name = paste0("Truck % Ratio (", p, "/D)"),
    period = p,
    target = "ratio_truck_pct",
    transform = NULL,  # No log transform for ratios!
    min_valid = 0.001  # More permissive for truck ratios (very small values possible)
  )
  
  ratio_configs[[paste0("ratio_speed_", p)]] <- list(
    name = paste0("Speed Ratio (", p, "/D)"),
    period = p,
    target = "ratio_speed",
    transform = NULL,  # No log transform for ratios!
    min_valid = 0.01
  )
}

all_configs <- c(base_configs, ratio_configs)

# =============================================================================
# TRAIN MODELS
# =============================================================================


models_list <- list()
results_summary <- data.frame()

for (model_name in names(all_configs)) {
  config <- all_configs[[model_name]]
  
  cat(sprintf("──────────────────────────────────────────────────────────────── [%d/%d]\n", 
              which(names(all_configs) == model_name), length(all_configs)))
  
  # Filter data for this period
  period_data <- training_data %>% filter(period == config$period)
  
  # Extract target variable
  y_all <- period_data[[config$target]]
  
  # Filter valid data
  valid_idx <- !is.na(y_all) & y_all >= config$min_valid
  
  # More permissive threshold for truck models (they have less data)
  min_obs_threshold <- ifelse(grepl("truck", config$target), 20, 50)
  
  if (sum(valid_idx) < min_obs_threshold) {
    cat(sprintf("  ⚠️  WARNING: Too few valid observations (<%d), skipping\n\n", min_obs_threshold))
    next
  }
  
  cat(sprintf("  • Valid observations: %s\n", format(sum(valid_idx), big.mark=",")))
  
  # Filter data first
  data_clean <- period_data[valid_idx, ]
  y_clean <- y_all[valid_idx]
  
  # Create feature matrix (may eliminate more rows due to NA in features)
  X_all <- sparse.model.matrix(feature_formula, data = data_clean)
  
  # Check if sparse.model.matrix eliminated additional rows
  if (nrow(X_all) != nrow(data_clean)) {
    # Find which rows were kept by sparse.model.matrix
    kept_rows <- as.integer(rownames(X_all))
    
    # Align y and data with X_all
    y_clean <- y_clean[kept_rows]
    data_clean <- data_clean[kept_rows, ]
  }
  
  # Apply transform to aligned target
  if (!is.null(config$transform) && config$transform == "log10") {
    y_transformed <- log10(pmax(y_clean, config$min_valid))
  } else {
    
    y_transformed <- y_clean
  }
  
  # Verify dimensions are now synchronized
  cat(sprintf("  • Final data: %d rows (X_all: %d × %d, y: %d)\n",
             nrow(data_clean), nrow(X_all), ncol(X_all), length(y_transformed)))
  
  if (nrow(X_all) != length(y_transformed)) {
    next
  }
  
  # Train/test split on synchronized data
  set.seed(123)
  n_final <- nrow(X_all)
  train_idx <- sample(seq_len(n_final), size = floor(0.8 * n_final))
  test_idx <- setdiff(seq_len(n_final), train_idx)
  
  X_train <- X_all[train_idx, ]
  X_test <- X_all[test_idx, ]
  y_train <- y_transformed[train_idx]
  y_test <- y_transformed[test_idx]
  
  # Check for invalid values in labels
  invalid_train <- is.na(y_train) | is.infinite(y_train) | is.nan(y_train)
  invalid_test <- is.na(y_test) | is.infinite(y_test) | is.nan(y_test)
  
  if (any(invalid_train) || any(invalid_test)) {
    cat(sprintf("  ⚠️  WARNING: Found %d invalid train labels, %d invalid test labels\n",
               sum(invalid_train), sum(invalid_test)))
    
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
  if (length(y_train) < 10 || length(y_test) < 5) {
    next
  }
  
  cat(sprintf("  • Training: %s observations (80%%)\n", format(length(y_train), big.mark=",")))
  cat(sprintf("  • Test: %s observations (20%%)\n", format(length(y_test), big.mark=",")))
  cat(sprintf("  • Y range: [%.3f, %.3f]\n\n", min(y_train, na.rm=TRUE), max(y_train, na.rm=TRUE)))
  
  # Train model
  start_time <- Sys.time()
  
  # Choose parameters and training strategy based on model type
  if (grepl("truck", config$target)) {
    params <- TRUCK_PARAMS
    
    # For truck models with small samples, use CV for robust estimation
    if (length(y_train) < 200) {
      cv_result <- xgb.cv(
        params = params,
        data = dtrain,
        nrounds = NROUNDS,
        nfold = min(5, length(y_train) %/% 10),  # Adaptive CV folds
        early_stopping_rounds = 30,
        verbose = 0,
        showsd = FALSE
      )
      best_rounds <- cv_result$best_iteration
      cat(sprintf("  📈 CV selected %d rounds (from max %d)\n", best_rounds, NROUNDS))
    } else {
      best_rounds <- NROUNDS
    }
  } else {
    params <- QUICK_PARAMS
    best_rounds <- NROUNDS
  }
  
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_rounds,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 50,  # Stop if no improvement for 50 rounds
    maximize = FALSE,            # Minimize RMSE
    verbose = 0
  )
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  
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
  mae <- mean(abs(pred_original - actual_original), na.rm = TRUE)
  rmse <- sqrt(mean((pred_original - actual_original)^2, na.rm = TRUE))
  r2 <- cor(pred_original, actual_original, use = "complete.obs")^2
  mape_values <- abs((pred_original - actual_original) / pmax(actual_original, 0.01)) * 100
  mape <- median(mape_values[is.finite(mape_values)], na.rm = TRUE)
  
  cat(sprintf("  ✅ Done in %.2f min | R²=%.3f | MAPE=%.1f%%\n", elapsed, r2, mape))
  
  # Feature importance analysis
  importance <- xgb.importance(model = xgb_model)
  top_features <- head(importance, 5)  # Top 5 most important features
  
  for (i in 1:nrow(top_features)) {
    cat(sprintf("     %d. %-15s (%.1f%%)\n", 
               i, top_features$Feature[i], top_features$Gain[i] * 100))
  }
  
  # Store model
  models_list[[model_name]] <- list(
    model = xgb_model,
    config = config,
    metrics = list(mae = mae, rmse = rmse, r2 = r2, mape = mape),
    feature_names = colnames(X_train),
    feature_importance = importance,  # Full importance table
    top_features = top_features,      # Top 5 for quick reference
    n_train = length(y_train),
    n_test = length(y_test),
    training_time = elapsed
  )
  
  # Add to summary
  results_summary <- rbind(results_summary, data.frame(
    Model = model_name,
    Target = config$name,
    N_train = length(y_train),
    N_test = length(y_test),
    R2 = round(r2, 3),
    MAE = round(mae, 2),
    RMSE = round(rmse, 2),
    MAPE = round(mape, 1),
    Time_min = round(elapsed, 2)
  ))
  
}

# =============================================================================
# SAVE MODELS
# =============================================================================


saveRDS(models_list, "rdsFiles/xgb_models_with_ratios.rds")

saveRDS(list(
  feature_formula = feature_formula,
  all_periods = all_periods
), "rdsFiles/xgb_ratio_feature_info.rds")

# =============================================================================
# FINAL SUMMARY
# =============================================================================


print(results_summary, row.names = FALSE)

# =============================================================================
# FEATURE IMPORTANCE SUMMARY
# =============================================================================


# Aggregate feature importance across all models
all_importance <- data.frame()
for (model_name in names(models_list)) {
  if (!is.null(models_list[[model_name]]$feature_importance)) {
    imp <- models_list[[model_name]]$feature_importance
    imp$Model <- model_name
    imp$ModelType <- ifelse(grepl("_D$", model_name), "Base", "Ratio")
    all_importance <- rbind(all_importance, imp)
  }
}

# Top features across all models
if (nrow(all_importance) > 0) {
  global_importance <- all_importance %>%
    group_by(Feature) %>%
    summarise(
      AvgGain = mean(Gain, na.rm = TRUE),
      AvgCover = mean(Cover, na.rm = TRUE),
      TimesUsed = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(AvgGain)) %>%
    head(10)
  
  for (i in 1:nrow(global_importance)) {
    cat(sprintf("  %2d. %-20s | Avg Gain: %5.1f%% | Used in %2d/%2d models\n",
               i, global_importance$Feature[i], 
               global_importance$AvgGain[i] * 100,
               global_importance$TimesUsed[i], length(models_list)))
  }
  
  # Base models vs Ratio models feature comparison
  base_importance <- all_importance %>%
    filter(ModelType == "Base") %>%
    group_by(Feature) %>%
    summarise(AvgGain = mean(Gain), .groups = 'drop') %>%
    arrange(desc(AvgGain)) %>%
    head(5)
  
  ratio_importance <- all_importance %>%
    filter(ModelType == "Ratio") %>%
    group_by(Feature) %>%
    summarise(AvgGain = mean(Gain), .groups = 'drop') %>%
    arrange(desc(AvgGain)) %>%
    head(5)
  
  for (i in 1:nrow(base_importance)) {
    cat(sprintf("   %d. %-20s (%.1f%%)\n", i, base_importance$Feature[i], 
               base_importance$AvgGain[i] * 100))
  }
  
  for (i in 1:nrow(ratio_importance)) {
    cat(sprintf("   %d. %-20s (%.1f%%)\n", i, ratio_importance$Feature[i], 
               ratio_importance$AvgGain[i] * 100))
  }
}

# Separate base and ratio model performance
base_results <- results_summary[grepl("_D$", results_summary$Model), ]
ratio_results <- results_summary[grepl("^ratio_", results_summary$Model), ]

cat(sprintf("   • R²: %.3f\n", mean(base_results$R2, na.rm = TRUE)))
cat(sprintf("   • MAPE: %.1f%%\n\n", mean(base_results$MAPE, na.rm = TRUE)))

cat(sprintf("   • R²: %.3f\n", mean(ratio_results$R2, na.rm = TRUE)))
cat(sprintf("   • MAPE: %.1f%%\n\n", mean(ratio_results$MAPE, na.rm = TRUE)))

# =============================================================================
# ERROR ANALYSIS IN PERCENTAGE
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("                    ERROR ANALYSIS (% of values)                  \n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

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
  
  X_all <- sparse.model.matrix(feature_formula, data = data_clean)
  
  if (nrow(X_all) != nrow(data_clean)) {
    kept_rows <- as.integer(rownames(X_all))
    y_clean <- y_clean[kept_rows]
    data_clean <- data_clean[kept_rows, ]
  }
  
  # Apply transform
  if (!is.null(config$transform) && config$transform == "log10") {
    y_transformed <- log10(pmax(y_clean, config$min_valid))
  } else {
    y_transformed <- y_clean
  }
  
  # Train/test split (same seed as training)
  set.seed(123)
  n_final <- nrow(X_all)
  train_idx <- sample(seq_len(n_final), size = floor(0.8 * n_final))
  test_idx <- setdiff(seq_len(n_final), train_idx)
  
  X_test <- X_all[test_idx, ]
  y_test <- y_transformed[test_idx]
  
  # Remove invalid
  invalid_test <- is.na(y_test) | is.infinite(y_test) | is.nan(y_test)
  if (any(invalid_test)) {
    valid_test_idx <- !invalid_test
    X_test <- X_test[valid_test_idx, ]
    y_test <- y_test[valid_test_idx]
  }
  
  if (length(y_test) < 5) next
  
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
  
  if (length(pct_errors) == 0) next
  
  # Statistics
  mean_pct_error <- mean(pct_errors, na.rm = TRUE)
  median_pct_error <- median(pct_errors, na.rm = TRUE)
  mae_pct <- mean(abs(pct_errors), na.rm = TRUE)
  
  # Quantiles
  q25 <- quantile(pct_errors, 0.25, na.rm = TRUE)
  q75 <- quantile(pct_errors, 0.75, na.rm = TRUE)
  
  # Print results
  cat(sprintf("%-30s | Period: %-2s | N=%4d\n", config$name, config$period, length(pct_errors)))
  cat(sprintf("   Mean Error:   %+6.1f%%  (bias)\n", mean_pct_error))
  cat(sprintf("   Median Error: %+6.1f%%\n", median_pct_error))
  cat(sprintf("   MAE:          %6.1f%%  (average absolute error)\n", mae_pct))
  cat(sprintf("   Q25-Q75:      [%+6.1f%%, %+6.1f%%]\n\n", q25, q75))
}

assign("xgb_models_with_ratios", models_list, envir = .GlobalEnv)

