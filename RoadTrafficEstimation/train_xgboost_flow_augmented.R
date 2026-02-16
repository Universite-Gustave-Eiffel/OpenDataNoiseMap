# ==============================================================================
# Flow-Augmented Speed Model Training
# ==============================================================================
# Strategy: Train flow_D FIRST, then use predicted flow as an additional
# feature for speed models. This gives the speed model information about
# real traffic load (fundamental diagram: speed = f(flow, capacity)).
#
# Approach:
#   1. Train flow_D with standard 14 OSM features (identical to standard)
#   2. Generate out-of-fold flow predictions via 5-fold CV on training data
#      → avoids data leakage (each sensor's prediction comes from a model
#        that never saw that sensor during training)
#   3. Add pred_log10_flow and flow_per_lane as extra columns to the sparse
#      matrix for speed_D AND all speed ratio models
#   4. Train truck_pct_D normally (no flow augmentation needed)
#   5. Train all ratio models: speed ratios get augmented features, rest normal
#   6. Save augmented models to separate RDS file
#
# For prediction scripts: must predict flow_D first, then inject pred_flow
# into the feature matrix before predicting speed.
#
# Usage: Rscript train_xgboost_flow_augmented.R
# ==============================================================================

# --- Bootstrap & config ---
setwd("/home/aumond/Documents/github/OpenDataNoiseMap/RoadTrafficEstimation")
source("bootstrap.R")
source("config_pipeline.R")

pipeline_message(text = "Flow-augmented speed model training",
                 level = 0, progress = "start", process = "learn")

# ==============================================================================
# 1. Load training data
# ==============================================================================

pipeline_message(text = "Loading training data and feature info",
                 level = 1, progress = "start", process = "load")

training_data <- readRDS(CONFIG$TRAINING_RDS_DATA_FILEPATH)
pipeline_message(text = describe_df(training_data), process = "info")

# Ensure ratio_speed_to_osm exists
if (!"ratio_speed_to_osm" %in% names(training_data)) {
  if (all(c("aggregate_speed", "speed") %in% names(training_data))) {
    training_data$ratio_speed_to_osm <- ifelse(
      !is.na(training_data$aggregate_speed) & training_data$aggregate_speed >= 0 &
        !is.na(training_data$speed) & training_data$speed > 0,
      training_data$aggregate_speed / training_data$speed,
      NA_real_)
  }
}

pipeline_message(text = "Training data loaded",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 2. Build feature formula (same 14 features + 3 interactions as standard)
# ==============================================================================

pipeline_message(text = "Building feature formula",
                 level = 1, progress = "start", process = "configure")

candidate_road_features <- c("highway", "DEGRE", "ref_letter", "first_word",
                              "oneway_osm", "lanes_osm", "speed",
                              "junction_osm", "lane_number",
                              "connectivity", "betweenness", "closeness",
                              "pagerank", "coreness", "dead_end_score",
                              "edge_length_m")

available_road_features <- intersect(candidate_road_features, names(training_data))
pipeline_message(
  text = sprintf("Using %d features: %s", length(available_road_features),
                 paste(available_road_features, collapse = ", ")),
  process = "info")

# Interaction terms
interaction_terms <- character(0)
if (all(c("highway", "DEGRE") %in% available_road_features))
  interaction_terms <- c(interaction_terms, "highway:DEGRE")
if (all(c("highway", "lanes_osm") %in% available_road_features))
  interaction_terms <- c(interaction_terms, "highway:lanes_osm")
if (all(c("DEGRE", "connectivity") %in% available_road_features))
  interaction_terms <- c(interaction_terms, "DEGRE:connectivity")
if (all(c("junction_osm", "highway") %in% available_road_features))
  interaction_terms <- c(interaction_terms, "junction_osm:highway")

formula_parts <- c(available_road_features, interaction_terms)
road_feature_formula <- as.formula(paste("~", paste(formula_parts, collapse = " + ")))

pipeline_message(text = sprintf("Formula: %s", deparse(road_feature_formula)),
                 process = "info")
pipeline_message(text = "Feature formula built",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 3. Configure models (same structure as standard training)
# ==============================================================================

all_periods <- c("D", "E", "N", paste0("h", 0:23),
                 paste0("h", 0:23, "_wd"), paste0("h", 0:23, "_we"))

base_configs <- list(
  flow_D = list(
    name = "Traffic Flow (Day)", period = "D",
    target = "aggregate_flow", baseline = "flow_D",
    transform = "log10", min_valid = 1),
  truck_pct_D = list(
    name = "Truck Percentage (Day)", period = "D",
    target = "truck_pct", baseline = "truck_pct_D",
    transform = NULL, min_valid = 0),
  speed_D = list(
    name = "Speed Ratio to OSM (Day)", period = "D",
    target = "ratio_speed_to_osm", baseline = "speed_D",
    transform = NULL, min_valid = 0.05))

ratio_configs <- list()
for (p in setdiff(all_periods, "D")) {
  ratio_configs[[paste0("ratio_flow_", p)]] <- list(
    name = paste0("Flow Ratio (", p, "/D)"), period = p,
    target = "ratio_flow", transform = NULL, min_valid = 0.01)
  ratio_configs[[paste0("ratio_truck_pct_", p)]] <- list(
    name = paste0("Truck % Ratio (", p, "/D)"), period = p,
    target = "ratio_truck_pct", transform = NULL, min_valid = 0.001)
  ratio_configs[[paste0("ratio_speed_", p)]] <- list(
    name = paste0("Speed Ratio (", p, "/D)"), period = p,
    target = "ratio_speed", transform = NULL, min_valid = 0.01)
}

all_configs <- c(base_configs, ratio_configs)
pipeline_message(text = sprintf("Total models to train: %d", length(all_configs)),
                 process = "info")

# ==============================================================================
# 4. Shared sensor split (same seed=42 as standard for comparability)
# ==============================================================================

pipeline_message(text = "Setting up shared sensor split",
                 level = 1, progress = "start", process = "calc")

shared_base_test_sensors <- NULL
shared_base_train_sensors <- NULL
d_data_for_split <- training_data %>% filter(period == "D")
all_d_sensors <- unique(d_data_for_split$count_point_id)

set.seed(42)
n_train <- max(1, floor(0.8 * length(all_d_sensors)))
shared_base_train_sensors <- sample(all_d_sensors, size = n_train)
shared_base_test_sensors <- setdiff(all_d_sensors, shared_base_train_sensors)

pipeline_message(
  text = sprintf("Shared split: %d train / %d test sensors",
                 length(shared_base_train_sensors),
                 length(shared_base_test_sensors)),
  process = "info")
pipeline_message(text = "Sensor split configured",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 5. Helper functions
# ==============================================================================

get_quality_indicator_column <- function(target_name, available_cols) {
  if (target_name %in% c("aggregate_flow", "ratio_flow")) {
    col <- "perc_flow_predicted"
  } else if (target_name %in% c("aggregate_speed", "ratio_speed", "ratio_speed_to_osm")) {
    col <- "perc_speed_predicted"
  } else if (target_name %in% c("truck_pct", "ratio_truck_pct")) {
    col <- "perc_flow_trucks_predicted"
  } else {
    col <- NA_character_
  }
  if (!is.na(col) && col %in% available_cols) col else NA_character_
}

safe_sparse_model_matrix <- function(formula_obj, data_df) {
  vars_in_formula <- intersect(unique(all.vars(formula_obj)), names(data_df))
  if (nrow(data_df) == 0L) {
    return(Matrix::sparse.model.matrix(formula_obj, data_df))
  }

  mm_data <- data_df
  for (v in vars_in_formula) {
    if (is.factor(mm_data[[v]])) {
      mm_data[[v]] <- as.character(mm_data[[v]])
    }
  }

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

  Matrix::sparse.model.matrix(formula_obj, mm_data)
}

# Align sparse matrix columns to model's expected features
align_matrix_to_model <- function(mat, model_obj) {
  expected <- model_obj$feature_names
  if (is.null(expected)) return(mat)
  cur <- colnames(mat)
  missing <- setdiff(expected, cur)
  if (length(missing) > 0) {
    zero_mat <- Matrix::Matrix(0, nrow = nrow(mat), ncol = length(missing),
                                sparse = TRUE)
    colnames(zero_mat) <- missing
    mat <- cbind(mat, zero_mat)
  }
  extra <- setdiff(colnames(mat), expected)
  if (length(extra) > 0) {
    mat <- mat[, expected, drop = FALSE]
  }
  mat
}

# Add flow-augmented features (pred_log10_flow and flow_per_lane) to a
# sparse matrix. pred_flow_vec must have same length as nrow(mat).
augment_with_flow <- function(mat, pred_flow_vec, lanes_vec) {
  n <- nrow(mat)
  stopifnot(length(pred_flow_vec) == n)
  stopifnot(length(lanes_vec) == n)
  
  # log10(pred_flow + 1) — same scale as the flow model target
  log_flow <- log10(pmax(pred_flow_vec, 1))
  # flow per lane — proxy for saturation (capacity ~ 1800 veh/h/lane)
  safe_lanes <- pmax(lanes_vec, 1)
  flow_per_lane <- pred_flow_vec / safe_lanes
  
  aug_mat <- Matrix::Matrix(
    cbind(log_flow, flow_per_lane),
    nrow = n, sparse = TRUE)
  colnames(aug_mat) <- c("pred_log10_flow", "flow_per_lane")
  
  cbind(mat, aug_mat)
}

# ==============================================================================
# 6. Train flow_D FIRST + generate out-of-fold predictions
# ==============================================================================

pipeline_message(
  text = "PHASE 1: Training flow_D and generating out-of-fold predictions",
  level = 1, progress = "start", process = "learn")

# --- 6a. Prepare period-D flow data ---
d_data <- training_data %>%
  dplyr::filter(period == "D") %>%
  dplyr::filter(!is.na(aggregate_flow), aggregate_flow >= 1)

pipeline_message(text = sprintf("Period D flow data: %d sensors", nrow(d_data)),
                 process = "info")

# Sparse matrix for flow features
d_matrix_full <- safe_sparse_model_matrix(road_feature_formula, d_data)
if (nrow(d_matrix_full) != nrow(d_data)) {
  kept <- as.integer(rownames(d_matrix_full))
  d_data <- d_data[kept, ]
}

# Target: log10(flow)
y_flow_full <- log10(pmax(d_data$aggregate_flow, 1))

# --- 6b. Out-of-fold predictions (5-fold CV on sensors) ---
# Each sensor gets a flow prediction from a model that never trained on it.
# This avoids leakage when using pred_flow as a feature for speed.

pipeline_message(text = "Generating out-of-fold flow predictions (5-fold sensor CV)",
                 level = 2, progress = "start", process = "calc")

sensor_ids <- d_data$count_point_id
unique_sensors <- unique(sensor_ids)
n_folds <- 5

set.seed(123)  # Separate seed for CV folds (not the train/test split seed)
fold_assignment <- sample(rep(1:n_folds, length.out = length(unique_sensors)))
names(fold_assignment) <- unique_sensors

# Initialize OOF prediction vector (on log10 scale)
oof_pred_log10_flow <- rep(NA_real_, nrow(d_data))

for (fold in 1:n_folds) {
  fold_test_sensors <- unique_sensors[fold_assignment == fold]
  fold_test_idx <- which(sensor_ids %in% fold_test_sensors)
  fold_train_idx <- which(!sensor_ids %in% fold_test_sensors)
  
  if (length(fold_train_idx) < 100 || length(fold_test_idx) < 10) {
    pipeline_message(text = sprintf("Fold %d: too few samples, skipping", fold),
                     process = "warning")
    next
  }
  
  X_fold_train <- d_matrix_full[fold_train_idx, ]
  y_fold_train <- y_flow_full[fold_train_idx]
  X_fold_test <- d_matrix_full[fold_test_idx, ]
  
  # Quality weights
  quality_col <- get_quality_indicator_column("aggregate_flow", names(d_data))
  if (!is.na(quality_col)) {
    q_train <- as.numeric(d_data[[quality_col]][fold_train_idx])
    w_train <- 1 - pmin(pmax(q_train, 0), 100) / 100
    w_train[is.na(w_train)] <- 1
    w_train <- pmax(w_train, CONFIG$MIN_AVATAR_SAMPLE_WEIGHT)
    dtrain_fold <- xgb.DMatrix(data = X_fold_train, label = y_fold_train,
                                weight = w_train)
  } else {
    dtrain_fold <- xgb.DMatrix(data = X_fold_train, label = y_fold_train)
  }
  
  # Train with same params as standard
  cv_result <- xgb.cv(
    params = CONFIG$TRAINING_PARAMS,
    data = dtrain_fold,
    nrounds = CONFIG$NROUNDS,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = 0)
  best_rounds <- cv_result$best_iteration
  
  fold_model <- xgb.train(
    params = CONFIG$TRAINING_PARAMS,
    data = dtrain_fold,
    nrounds = best_rounds,
    verbose = 0)
  
  # Predict on held-out fold
  oof_pred_log10_flow[fold_test_idx] <- predict(fold_model, X_fold_test)
  
  pipeline_message(
    text = sprintf("Fold %d/%d: %d train, %d test sensors, %d rounds",
                   fold, n_folds, length(fold_train_idx),
                   length(fold_test_idx), best_rounds),
    process = "info")
  
  rm(fold_model, dtrain_fold, X_fold_train, X_fold_test)
}

# Convert OOF predictions to natural scale
oof_pred_flow <- 10^oof_pred_log10_flow

# Check coverage
oof_valid <- sum(!is.na(oof_pred_flow))
pipeline_message(
  text = sprintf("OOF flow predictions: %d/%d valid (%.1f%%)",
                 oof_valid, length(oof_pred_flow),
                 100 * oof_valid / length(oof_pred_flow)),
  process = "info")

# For any remaining NAs, use median (should be 0 if all folds worked)
if (any(is.na(oof_pred_flow))) {
  med_flow <- median(oof_pred_flow, na.rm = TRUE)
  oof_pred_flow[is.na(oof_pred_flow)] <- med_flow
  pipeline_message(text = sprintf("Imputed %d missing OOF predictions with median=%.0f",
                                  sum(is.na(oof_pred_flow)), med_flow),
                   process = "warning")
}

pipeline_message(text = "Out-of-fold flow predictions complete",
                 level = 2, progress = "end", process = "valid")

# --- 6c. Train final flow_D model on 80% train split ---
pipeline_message(text = "Training final flow_D model (train/test split)",
                 level = 2, progress = "start", process = "learn")

train_idx_flow <- which(!sensor_ids %in% shared_base_test_sensors)
test_idx_flow <- which(sensor_ids %in% shared_base_test_sensors)

X_train_flow <- d_matrix_full[train_idx_flow, ]
X_test_flow <- d_matrix_full[test_idx_flow, ]
y_train_flow <- y_flow_full[train_idx_flow]
y_test_flow <- y_flow_full[test_idx_flow]

# Quality weights for flow
quality_col <- get_quality_indicator_column("aggregate_flow", names(d_data))
if (!is.na(quality_col)) {
  q_train_flow <- as.numeric(d_data[[quality_col]][train_idx_flow])
  w_train_flow <- 1 - pmin(pmax(q_train_flow, 0), 100) / 100
  w_train_flow[is.na(w_train_flow)] <- 1
  w_train_flow <- pmax(w_train_flow, CONFIG$MIN_AVATAR_SAMPLE_WEIGHT)
  dtrain_flow <- xgb.DMatrix(X_train_flow, label = y_train_flow,
                              weight = w_train_flow)
} else {
  dtrain_flow <- xgb.DMatrix(X_train_flow, label = y_train_flow)
}
dtest_flow <- xgb.DMatrix(X_test_flow, label = y_test_flow)

start_timer()
cv_result <- xgb.cv(params = CONFIG$TRAINING_PARAMS, data = dtrain_flow,
                      nrounds = CONFIG$NROUNDS, nfold = 5,
                      early_stopping_rounds = 50, verbose = 0)
best_rounds_flow <- cv_result$best_iteration

flow_model <- xgb.train(
  params = CONFIG$TRAINING_PARAMS,
  data = dtrain_flow,
  nrounds = best_rounds_flow,
  watchlist = list(train = dtrain_flow, test = dtest_flow),
  early_stopping_rounds = 50,
  maximize = FALSE,
  verbose = 0)
elapsed_flow <- stop_timer()

# Evaluate flow_D
pred_flow_test <- 10^predict(flow_model, X_test_flow)
actual_flow_test <- 10^y_test_flow
flow_metrics <- compute_model_metrics(actual_flow_test, pred_flow_test)

pipeline_message(
  text = sprintf("flow_D: R²=%.3f, MAE=%.1f, RMSE=%.1f, MAPE=%.1f%% (%d rounds, %.1f min)",
                 flow_metrics$r2, flow_metrics$mae, flow_metrics$rmse,
                 flow_metrics$mape, best_rounds_flow, elapsed_flow),
  process = "info")

# Store flow_D model
models_list <- list()
results_summary <- data.frame()
base_test_predictions <- list()

flow_importance <- xgb.importance(model = flow_model)

models_list[["flow_D"]] <- list(
  model = flow_model,
  config = base_configs$flow_D,
  metrics = list(mae = flow_metrics$mae, rmse = flow_metrics$rmse,
                 r2 = flow_metrics$r2, mape = flow_metrics$mape),
  feature_names = colnames(X_train_flow),
  feature_importance = flow_importance,
  top_features = head(flow_importance, 5),
  n_train = length(y_train_flow),
  n_test = length(y_test_flow),
  training_time = elapsed_flow)

# Store test predictions for emission analysis
osm_speed_raw <- suppressWarnings(as.numeric(d_data$speed[test_idx_flow]))
osm_speed_raw[is.na(osm_speed_raw) | osm_speed_raw < 5] <- CONFIG$DEFAULT_VEHICLE_SPEED

base_test_predictions[["flow_D"]] <- data.frame(
  osm_id = d_data$osm_id[test_idx_flow],
  count_point_id = d_data$count_point_id[test_idx_flow],
  period = "D",
  highway = as.character(d_data$highway[test_idx_flow]),
  pred = as.numeric(pred_flow_test),
  actual = as.numeric(actual_flow_test),
  stringsAsFactors = FALSE)

results_summary <- rbind(results_summary, data.frame(
  Model = "flow_D", Target = "Traffic Flow (Day)",
  N_train = length(y_train_flow), N_test = length(y_test_flow),
  R2 = round(flow_metrics$r2, 3), MAE = round(flow_metrics$mae, 2),
  RMSE = round(flow_metrics$rmse, 2), MAPE = round(flow_metrics$mape, 1),
  Time_min = round(elapsed_flow, 2)))

pipeline_message(text = "flow_D model trained",
                 level = 2, progress = "end", process = "valid")

# --- 6d. Predict flow for ALL period-D sensors using the final model ---
# For TRAIN sensors: use OOF predictions (no leakage)
# For TEST sensors: use the final model (they were never in training)
pipeline_message(text = "Building flow prediction map for all sensors",
                 level = 2, progress = "start", process = "calc")

# Start with OOF predictions for all sensors
pred_flow_all <- oof_pred_flow

# For test sensors, overwrite with final model predictions (more accurate)
pred_flow_test_all <- 10^predict(flow_model, d_matrix_full[test_idx_flow, ])
pred_flow_all[test_idx_flow] <- pred_flow_test_all

# Build lookup: count_point_id → predicted flow
flow_lookup <- data.frame(
  count_point_id = d_data$count_point_id,
  pred_flow_D = pred_flow_all,
  stringsAsFactors = FALSE)

# Also store lanes for the saturation feature
if ("lanes_osm" %in% names(d_data)) {
  lanes_lookup <- data.frame(
    count_point_id = d_data$count_point_id,
    lanes_osm = as.numeric(d_data$lanes_osm),
    stringsAsFactors = FALSE)
  lanes_lookup$lanes_osm[is.na(lanes_lookup$lanes_osm) |
                            lanes_lookup$lanes_osm < 1] <- 1
} else {
  lanes_lookup <- data.frame(
    count_point_id = d_data$count_point_id,
    lanes_osm = 1, stringsAsFactors = FALSE)
}

pipeline_message(
  text = sprintf("Flow predictions available for %d sensors (range: %.0f–%.0f veh/D)",
                 nrow(flow_lookup),
                 min(flow_lookup$pred_flow_D),
                 max(flow_lookup$pred_flow_D)),
  process = "info")
pipeline_message(text = "Flow prediction map built",
                 level = 2, progress = "end", process = "valid")

pipeline_message(text = "PHASE 1 complete: flow_D trained + OOF predictions ready",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 7. Train remaining models (truck_pct_D, speed_D, all ratios)
# ==============================================================================

pipeline_message(text = "PHASE 2: Training remaining 224 models (speed models get flow features)",
                 level = 1, progress = "start", process = "learn")

# Which models get flow augmentation?
is_speed_model <- function(model_name) {
  model_name == "speed_D" || grepl("^ratio_speed_", model_name)
}

# Skip flow_D (already done)
remaining_configs <- all_configs[names(all_configs) != "flow_D"]

n_trained <- 1  # flow_D already counted
n_skipped <- 0
n_total <- length(all_configs)

for (model_name in names(remaining_configs)) {
  config <- remaining_configs[[model_name]]
  model_idx <- which(names(all_configs) == model_name)
  augment_speed <- is_speed_model(model_name)
  
  pipeline_message(
    text = sprintf("[%d/%d] %s%s",
                   model_idx, n_total, model_name,
                   if (augment_speed) " (+flow features)" else ""),
    level = 1, progress = "start", process = "wait")
  
  # --- Filter period data ---
  period_data <- training_data %>%
    dplyr::filter(period == config$period)
  
  target_vec <- period_data[[config$target]]
  valid_idx <- !is.na(target_vec) & target_vec >= config$min_valid
  
  min_obs <- ifelse(grepl("truck", config$target), 20, 50)
  if (sum(valid_idx) < min_obs) {
    pipeline_message(text = sprintf("Too few valid obs (%d < %d), skipping",
                                    sum(valid_idx), min_obs),
                     process = "warning")
    n_skipped <- n_skipped + 1
    next
  }
  
  clean_data <- period_data[valid_idx, ]
  clean_target <- target_vec[valid_idx]
  
  quality_col <- get_quality_indicator_column(config$target, names(clean_data))
  quality_indicator <- if (!is.na(quality_col)) {
    as.numeric(clean_data[[quality_col]])
  } else {
    rep(NA_real_, nrow(clean_data))
  }
  
  # --- Build sparse matrix (standard features) ---
  sparse_mat <- safe_sparse_model_matrix(road_feature_formula, clean_data)
  if (nrow(sparse_mat) != nrow(clean_data)) {
    kept <- as.integer(rownames(sparse_mat))
    clean_data <- clean_data[kept, ]
    clean_target <- clean_target[kept]
    quality_indicator <- quality_indicator[kept]
  }
  
  # --- Add flow features for speed models ---
  if (augment_speed) {
    # Match sensors to flow predictions
    match_idx <- match(clean_data$count_point_id, flow_lookup$count_point_id)
    has_flow <- !is.na(match_idx)
    
    if (sum(has_flow) < 50) {
      pipeline_message(text = sprintf("Only %d sensors matched flow predictions, skipping augmentation",
                                      sum(has_flow)), process = "warning")
      augment_speed <- FALSE
    } else {
      # For unmatched sensors (shouldn't happen but safety), use median flow
      pred_flow_vec <- rep(median(flow_lookup$pred_flow_D), nrow(sparse_mat))
      pred_flow_vec[has_flow] <- flow_lookup$pred_flow_D[match_idx[has_flow]]
      
      # Get lanes
      match_lanes <- match(clean_data$count_point_id, lanes_lookup$count_point_id)
      lanes_vec <- rep(1, nrow(sparse_mat))
      has_lanes <- !is.na(match_lanes)
      lanes_vec[has_lanes] <- lanes_lookup$lanes_osm[match_lanes[has_lanes]]
      
      # Augment sparse matrix
      sparse_mat <- augment_with_flow(sparse_mat, pred_flow_vec, lanes_vec)
      
      n_unmatched <- sum(!has_flow)
      if (n_unmatched > 0) {
        pipeline_message(
          text = sprintf("Flow augmentation: %d matched, %d imputed with median",
                         sum(has_flow), n_unmatched),
          process = "info")
      } else {
        pipeline_message(text = "Flow augmentation: all sensors matched", process = "info")
      }
    }
  }
  
  # --- Apply transform ---
  if (!is.null(config$transform) && config$transform == "log10") {
    y_transformed <- log10(pmax(clean_target, config$min_valid))
  } else {
    y_transformed <- clean_target
  }
  
  if (nrow(sparse_mat) != length(y_transformed)) next
  
  # --- Train/test split ---
  set.seed(123)
  n_final <- nrow(sparse_mat)
  is_base_model <- model_name %in% c("truck_pct_D", "speed_D")
  sensor_ids_here <- clean_data$count_point_id
  
  if ("count_point_id" %in% names(clean_data)) {
    unique_sensors_here <- unique(sensor_ids_here)
    if (is_base_model && !is.null(shared_base_test_sensors)) {
      train_idx <- which(!sensor_ids_here %in% shared_base_test_sensors)
      test_idx <- which(sensor_ids_here %in% shared_base_test_sensors)
      if (length(train_idx) == 0 || length(test_idx) == 0) {
        train_idx <- sample(seq_len(n_final), floor(0.8 * n_final))
        test_idx <- setdiff(seq_len(n_final), train_idx)
      }
    } else if (length(unique_sensors_here) >= 5) {
      n_train_s <- max(1, floor(0.8 * length(unique_sensors_here)))
      train_sensors_here <- sample(unique_sensors_here, size = n_train_s)
      train_idx <- which(sensor_ids_here %in% train_sensors_here)
      test_idx <- which(!sensor_ids_here %in% train_sensors_here)
      if (length(train_idx) == 0 || length(test_idx) == 0) {
        train_idx <- sample(seq_len(n_final), floor(0.8 * n_final))
        test_idx <- setdiff(seq_len(n_final), train_idx)
      }
    } else {
      train_idx <- sample(seq_len(n_final), floor(0.8 * n_final))
      test_idx <- setdiff(seq_len(n_final), train_idx)
    }
  } else {
    train_idx <- sample(seq_len(n_final), floor(0.8 * n_final))
    test_idx <- setdiff(seq_len(n_final), train_idx)
  }
  
  X_train <- sparse_mat[train_idx, ]
  X_test <- sparse_mat[test_idx, ]
  y_train <- y_transformed[train_idx]
  y_test <- y_transformed[test_idx]
  q_train <- quality_indicator[train_idx]
  highway_test <- as.character(clean_data$highway[test_idx])
  test_meta <- clean_data[test_idx, c("osm_id", "count_point_id", "period",
                                       "highway", "speed")]
  
  # Remove invalid labels
  invalid_train <- is.na(y_train) | is.infinite(y_train)
  invalid_test <- is.na(y_test) | is.infinite(y_test)
  if (any(invalid_train)) {
    valid <- !invalid_train
    X_train <- X_train[valid, ]; y_train <- y_train[valid]; q_train <- q_train[valid]
  }
  if (any(invalid_test)) {
    valid <- !invalid_test
    X_test <- X_test[valid, ]; y_test <- y_test[valid]
    highway_test <- highway_test[valid]
    test_meta <- test_meta[valid, , drop = FALSE]
  }
  
  # Size checks
  min_train_xgb <- 150; min_test_xgb <- 50
  if (model_name == "speed_D") { min_train_xgb <- 10; min_test_xgb <- 5 }
  if (grepl("truck", config$target)) { min_train_xgb <- 10; min_test_xgb <- 5 }
  
  if (length(y_train) < min_train_xgb || length(y_test) < min_test_xgb) {
    pipeline_message(text = sprintf("Sample too small (train=%d, test=%d), skipping",
                                    length(y_train), length(y_test)),
                     process = "warning")
    n_skipped <- n_skipped + 1
    next
  }
  
  # --- Build DMatrices with quality weights ---
  use_quality_weights <- isTRUE(CONFIG$USE_AVATAR_QUALITY_WEIGHTS)
  min_weight <- if (!is.null(CONFIG$MIN_AVATAR_SAMPLE_WEIGHT)) {
    CONFIG$MIN_AVATAR_SAMPLE_WEIGHT } else { 0.20 }
  
  if (use_quality_weights && any(!is.na(q_train))) {
    w_train <- 1 - pmin(pmax(q_train, 0), 100) / 100
    w_train[is.na(w_train)] <- 1
    w_train <- pmax(w_train, min_weight)
    dtrain <- xgb.DMatrix(X_train, label = y_train, weight = w_train)
  } else {
    dtrain <- xgb.DMatrix(X_train, label = y_train)
  }
  dtest <- xgb.DMatrix(X_test, label = y_test)
  
  # --- Select params and nrounds ---
  if (grepl("truck", config$target)) {
    params <- CONFIG$TRUCK_PARAMS
    if (length(y_train) < 200) {
      nfold <- min(5, length(y_train) %/% 10)
      if (nfold >= 2) {
        cv_res <- xgb.cv(params = params, data = dtrain, nrounds = CONFIG$NROUNDS,
                          nfold = nfold, early_stopping_rounds = 30, verbose = 0)
        best_rounds <- cv_res$best_iteration
      } else {
        best_rounds <- CONFIG$NROUNDS
      }
    } else {
      best_rounds <- CONFIG$NROUNDS
    }
  } else {
    params <- CONFIG$TRAINING_PARAMS
    nfold <- min(5, max(2, length(y_train) %/% 50))
    if (nfold >= 2 && length(y_train) >= 100) {
      cv_res <- xgb.cv(params = params, data = dtrain, nrounds = CONFIG$NROUNDS,
                        nfold = nfold, early_stopping_rounds = 50, verbose = 0)
      best_rounds <- cv_res$best_iteration
    } else {
      best_rounds <- CONFIG$NROUNDS
    }
  }
  
  # --- Train ---
  use_watchlist <- length(y_train) >= 100 && length(y_test) >= 30
  
  start_timer()
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_rounds,
    watchlist = if (use_watchlist) list(train = dtrain, test = dtest) else NULL,
    early_stopping_rounds = if (use_watchlist) 50 else NULL,
    maximize = FALSE,
    verbose = 0)
  elapsed <- stop_timer()
  
  # --- Evaluate ---
  pred_test <- predict(xgb_model, X_test)
  if (!is.null(config$transform) && config$transform == "log10") {
    pred_original <- 10^pred_test
    actual_original <- 10^y_test
  } else {
    pred_original <- pred_test
    actual_original <- y_test
  }
  
  if (model_name == "speed_D") {
    speed_osm_test <- suppressWarnings(as.numeric(test_meta$speed))
    speed_osm_test[is.na(speed_osm_test) | speed_osm_test <= 0] <- CONFIG$DEFAULT_VEHICLE_SPEED
    pred_eval <- pmax(0, pred_original * speed_osm_test)
    actual_eval <- pmax(0, actual_original * speed_osm_test)
  } else {
    pred_eval <- pred_original
    actual_eval <- actual_original
  }
  
  model_metrics <- compute_model_metrics(actual_eval, pred_eval)
  importance <- xgb.importance(model = xgb_model)
  top_feat <- head(importance, 5)
  
  # Log top features (especially interesting for speed models with flow features)
  if (augment_speed && nrow(importance) > 0) {
    flow_feat_rows <- importance[grepl("pred_log10_flow|flow_per_lane", importance$Feature), ]
    if (nrow(flow_feat_rows) > 0) {
      flow_rank <- which(importance$Feature %in% flow_feat_rows$Feature)
      pipeline_message(
        text = sprintf("Flow features importance: %s",
                       paste(sprintf("%s=%.1f%% (rank %d)",
                                     flow_feat_rows$Feature,
                                     flow_feat_rows$Gain * 100,
                                     flow_rank), collapse = ", ")),
        process = "info")
    }
  }
  
  pipeline_message(
    text = sprintf("R²=%.3f, MAE=%.2f, MAPE=%.1f%% [%d rounds, %.1f min]",
                   model_metrics$r2, model_metrics$mae, model_metrics$mape,
                   best_rounds, elapsed),
    process = "info")
  
  # --- Store model ---
  models_list[[model_name]] <- list(
    model = xgb_model,
    config = config,
    metrics = list(mae = model_metrics$mae, rmse = model_metrics$rmse,
                   r2 = model_metrics$r2, mape = model_metrics$mape),
    feature_names = colnames(X_train),
    feature_importance = importance,
    top_features = top_feat,
    n_train = length(y_train),
    n_test = length(y_test),
    training_time = elapsed,
    flow_augmented = augment_speed)  # Flag for prediction scripts
  
  # Base model test predictions for emission analysis
  if (model_name %in% c("truck_pct_D", "speed_D")) {
    base_test_predictions[[model_name]] <- data.frame(
      osm_id = test_meta$osm_id,
      count_point_id = test_meta$count_point_id,
      period = as.character(test_meta$period),
      highway = as.character(test_meta$highway),
      pred = as.numeric(pred_eval),
      actual = as.numeric(actual_eval),
      stringsAsFactors = FALSE)
  }
  
  results_summary <- rbind(results_summary, data.frame(
    Model = model_name, Target = config$name,
    N_train = length(y_train), N_test = length(y_test),
    R2 = round(model_metrics$r2, 3), MAE = round(model_metrics$mae, 2),
    RMSE = round(model_metrics$rmse, 2), MAPE = round(model_metrics$mape, 1),
    Time_min = round(elapsed, 2)))
  
  n_trained <- n_trained + 1
  
  pipeline_message(
    text = sprintf("[%d/%d] %s complete", model_idx, n_total, model_name),
    level = 1, progress = "end", process = "valid")
  
  # Memory cleanup
  rm(xgb_model, dtrain, dtest, sparse_mat, X_train, X_test)
  if (n_trained %% 30 == 0) gc(verbose = FALSE)
}

gc(verbose = FALSE)

pipeline_message(
  text = sprintf("PHASE 2 complete: %d trained, %d skipped",
                 n_trained, n_skipped),
  level = 1, progress = "end", process = "valid")

# ==============================================================================
# 8. Save models and feature info
# ==============================================================================

pipeline_message(text = "Saving flow-augmented models",
                 level = 1, progress = "start", process = "save")

output_dir <- file.path(CONFIG$TRAINING_DATA_DIR, "rds")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

models_path <- file.path(output_dir, "06_xgboost_trained_models_flow_augmented.rds")
saveRDS(models_list, models_path)
pipeline_message(text = sprintf("Saved %d models to %s (%.0f MB)",
                                length(models_list), rel_path(models_path),
                                file.size(models_path) / 1e6),
                 process = "info")

# Also save the flow lookup for use in prediction scripts
flow_lookup_path <- file.path(output_dir, "06_flow_augmented_lookup.rds")
saveRDS(list(
  flow_lookup = flow_lookup,
  lanes_lookup = lanes_lookup,
  road_feature_formula = road_feature_formula,
  all_periods = all_periods
), flow_lookup_path)
pipeline_message(text = sprintf("Saved flow lookup to %s", rel_path(flow_lookup_path)),
                 process = "info")

pipeline_message(text = "Models saved",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 9. Print summary
# ==============================================================================

pipeline_message(text = "Training summary",
                 level = 1, progress = "start", process = "search")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║              FLOW-AUGMENTED XGBoost Training Summary                       ║\n")
cat("╠══════════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  Total models: %d trained, %d skipped                                     ║\n",
            n_trained, n_skipped))
cat("╠══════════════════════════════════════════════════════════════════════════════╣\n")

# Show base models
cat("║  BASE MODELS (period D):                                                  ║\n")
base_names <- c("flow_D", "truck_pct_D", "speed_D")
for (bn in base_names) {
  if (bn %in% names(models_list)) {
    m <- models_list[[bn]]
    aug_flag <- if (isTRUE(m$flow_augmented)) " +FLOW" else ""
    cat(sprintf("║    %-15s R²=%.3f  MAE=%8.2f  MAPE=%5.1f%%  %s\n",
                bn, m$metrics$r2, m$metrics$mae, m$metrics$mape, aug_flag))
  }
}

# Speed model feature importance (key diagnostic)
if ("speed_D" %in% names(models_list)) {
  cat("║                                                                            ║\n")
  cat("║  speed_D Top 5 Features:                                                   ║\n")
  imp <- models_list[["speed_D"]]$feature_importance
  top5 <- head(imp, 5)
  for (i in 1:nrow(top5)) {
    cat(sprintf("║    %d. %-25s  Gain=%.1f%%\n",
                i, top5$Feature[i], top5$Gain[i] * 100))
  }
  
  # Check flow feature ranking
  flow_feats <- imp[grepl("pred_log10_flow|flow_per_lane", imp$Feature), ]
  if (nrow(flow_feats) > 0) {
    cat("║                                                                            ║\n")
    cat("║  Flow feature importance in speed_D:                                      ║\n")
    for (j in 1:nrow(flow_feats)) {
      rank_j <- which(imp$Feature == flow_feats$Feature[j])
      cat(sprintf("║    %-25s  Gain=%.1f%%  Rank=%d/%d\n",
                  flow_feats$Feature[j], flow_feats$Gain[j] * 100,
                  rank_j, nrow(imp)))
    }
  }
}

cat("╚══════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

pipeline_message(text = "Training summary complete",
                 level = 1, progress = "end", process = "valid")

pipeline_message(text = "Flow-augmented training complete",
                 level = 0, progress = "end", process = "valid")
