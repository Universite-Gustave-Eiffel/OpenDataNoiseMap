# ==============================================================================
# Comparison: Standard vs Flow-Augmented XGBoost — Emission dB analysis
# ==============================================================================
# Runs CNOSSOS emission analysis on the test set for BOTH model sets and
# produces a side-by-side comparison PDF. Special focus on speed bias
# (the main target of the flow-augmented approach).
#
# Usage: Rscript run_flow_augmented_comparison.R
# ==============================================================================

# --- Bootstrap & config ---
setwd("/home/aumond/Documents/github/OpenDataNoiseMap/RoadTrafficEstimation")
source("bootstrap.R")
source("config_pipeline.R")

pipeline_message(text = "Standard vs Flow-Augmented emission comparison",
                 level = 0, progress = "start", process = "plot")

# ==============================================================================
# 1. Load artifacts
# ==============================================================================

pipeline_message(text = "Loading models and training data",
                 level = 1, progress = "start", process = "load")

# Standard models
std_models <- readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
pipeline_message(text = sprintf("Standard models: %d", length(std_models)),
                 process = "info")

# Flow-augmented models
fa_models_path <- file.path(CONFIG$TRAINING_DATA_DIR, "rds",
                             "06_xgboost_trained_models_flow_augmented.rds")
if (!file.exists(fa_models_path)) {
  stop("Flow-augmented models not found: ", fa_models_path)
}
fa_models <- readRDS(fa_models_path)
pipeline_message(text = sprintf("Flow-augmented models: %d", length(fa_models)),
                 process = "info")

# Flow lookup (for augmenting speed features at prediction time)
fa_lookup_path <- file.path(CONFIG$TRAINING_DATA_DIR, "rds",
                             "06_flow_augmented_lookup.rds")
fa_lookup <- readRDS(fa_lookup_path)
flow_lookup <- fa_lookup$flow_lookup
lanes_lookup <- fa_lookup$lanes_lookup

# Feature info (for standard formula)
feature_info <- readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)
road_feature_formula <- feature_info$road_feature_formula
all_periods <- feature_info$all_periods

# Training data
training_data <- readRDS(CONFIG$TRAINING_RDS_DATA_FILEPATH)
if (!"ratio_speed_to_osm" %in% names(training_data)) {
  training_data$ratio_speed_to_osm <- ifelse(
    !is.na(training_data$aggregate_speed) & training_data$aggregate_speed >= 0 &
      !is.na(training_data$speed) & training_data$speed > 0,
    training_data$aggregate_speed / training_data$speed, NA_real_)
}
pipeline_message(text = describe_df(training_data), process = "info")

pipeline_message(text = "Artifacts loaded",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 2. Reconstruct shared sensor split (same seed=42 as training)
# ==============================================================================

pipeline_message(text = "Reconstructing test sensor set",
                 level = 1, progress = "start", process = "calc")

d_data_all <- training_data %>% filter(period == "D")
all_d_sensors <- unique(d_data_all$count_point_id)
set.seed(42)
n_train <- max(1, floor(0.8 * length(all_d_sensors)))
shared_train_sensors <- sample(all_d_sensors, size = n_train)
shared_test_sensors <- setdiff(all_d_sensors, shared_train_sensors)

pipeline_message(text = sprintf("Test sensors: %d", length(shared_test_sensors)),
                 process = "info")
pipeline_message(text = "Test sensors reconstructed",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 3. Build emission test data for BOTH model sets
# ==============================================================================

# Helpers
align_matrix_to_model <- function(mat, model) {
  expected <- model$feature_names
  if (is.null(expected)) return(mat)
  cur <- colnames(mat)
  missing <- setdiff(expected, cur)
  if (length(missing) > 0) {
    zero_mat <- Matrix::Matrix(0, nrow = nrow(mat), ncol = length(missing),
                                sparse = TRUE)
    colnames(zero_mat) <- missing
    mat <- cbind(mat, zero_mat)
  }
  mat[, expected, drop = FALSE]
}

# Augment with flow features (for flow-augmented speed models)
augment_with_flow <- function(mat, pred_flow_vec, lanes_vec) {
  n <- nrow(mat)
  log_flow <- log10(pmax(pred_flow_vec, 1))
  safe_lanes <- pmax(lanes_vec, 1)
  flow_per_lane <- pred_flow_vec / safe_lanes
  aug_mat <- Matrix::Matrix(cbind(log_flow, flow_per_lane), nrow = n, sparse = TRUE)
  colnames(aug_mat) <- c("pred_log10_flow", "flow_per_lane")
  cbind(mat, aug_mat)
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

# ---------- Standard emission predictions (same as run_db_comparison.R) ------
run_emission_standard <- function(models_list, label) {
  pipeline_message(text = sprintf("Running emission predictions [%s]", label),
                   level = 1, progress = "start", process = "calc")
  
  d_data <- training_data %>%
    filter(period == "D",
           count_point_id %in% shared_test_sensors,
           !is.na(aggregate_flow), aggregate_flow >= 1)
  
  d_matrix <- safe_sparse_model_matrix(road_feature_formula, d_data)
  if (nrow(d_matrix) != nrow(d_data)) {
    kept <- as.integer(rownames(d_matrix))
    d_data <- d_data[kept, ]
  }
  
  # Base predictions (D)
  pred_flow_D <- 10^predict(models_list[["flow_D"]]$model,
                             align_matrix_to_model(d_matrix, models_list[["flow_D"]]$model))
  if (!is.null(models_list[["truck_pct_D"]]$model)) {
    pred_truck_D <- predict(models_list[["truck_pct_D"]]$model,
                            align_matrix_to_model(d_matrix, models_list[["truck_pct_D"]]$model))
  } else {
    pred_truck_D <- ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
                           d_data$truck_pct, 0)
    pipeline_message(
      text = sprintf("[%s] Missing base model truck_pct_D: using observed truck_pct as fallback", label),
      process = "warning")
  }
  pred_speed_ratio <- predict(models_list[["speed_D"]]$model,
                               align_matrix_to_model(d_matrix, models_list[["speed_D"]]$model))
  
  osm_speed_raw <- suppressWarnings(as.numeric(d_data$speed))
  osm_speed_raw[is.na(osm_speed_raw) | osm_speed_raw < 5] <- CONFIG$DEFAULT_VEHICLE_SPEED
  pred_speed_D <- pmax(5, pred_speed_ratio * osm_speed_raw)
  
  actual_speed <- ifelse(!is.na(d_data$aggregate_speed) & d_data$aggregate_speed >= 5,
                          d_data$aggregate_speed, osm_speed_raw)
  actual_truck <- ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
                          d_data$truck_pct, 0)
  
  emission_D <- data.frame(
    count_point_id = d_data$count_point_id,
    period = "D", highway = as.character(d_data$highway),
    pred_flow = as.numeric(pred_flow_D),
    actual_flow = as.numeric(d_data$aggregate_flow),
    pred_truck_pct = as.numeric(pred_truck_D),
    actual_truck_pct = as.numeric(actual_truck),
    pred_speed = as.numeric(pred_speed_D),
    actual_speed = as.numeric(actual_speed),
    osm_speed = as.numeric(osm_speed_raw),
    stringsAsFactors = FALSE)
  emission_D <- emission_D[!is.na(emission_D$actual_speed) & emission_D$actual_speed >= 5, ]
  
  emission_parts <- list(emission_D)
  test_sensor_ids <- unique(emission_D$count_point_id)
  n_periods_done <- 0
  
  non_d_periods <- setdiff(all_periods, "D")
  for (p in non_d_periods) {
    flow_key <- paste0("ratio_flow_", p)
    truck_key <- paste0("ratio_truck_pct_", p)
    speed_key <- paste0("ratio_speed_", p)
    if (is.null(models_list[[flow_key]]$model)) next
    
    ratio_flow <- predict(models_list[[flow_key]]$model,
                           align_matrix_to_model(d_matrix, models_list[[flow_key]]$model))
    ratio_truck <- if (!is.null(models_list[[truck_key]]$model)) {
      predict(models_list[[truck_key]]$model,
              align_matrix_to_model(d_matrix, models_list[[truck_key]]$model))
    } else rep(1, nrow(d_matrix))
    ratio_speed <- if (!is.null(models_list[[speed_key]]$model)) {
      predict(models_list[[speed_key]]$model,
              align_matrix_to_model(d_matrix, models_list[[speed_key]]$model))
    } else rep(1, nrow(d_matrix))
    
    pred_flow_P <- pmax(0, pred_flow_D * ratio_flow)
    pred_truck_P <- pmin(100, pmax(0, pred_truck_D * ratio_truck))
    pred_speed_P <- pmax(5, pred_speed_D * ratio_speed)
    
    p_data <- training_data %>%
      filter(period == p, count_point_id %in% test_sensor_ids)
    if (nrow(p_data) == 0) next
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
                              p_data$aggregate_speed, osm_speed_raw[row_in_d])
    
    emission_P <- data.frame(
      count_point_id = p_data$count_point_id,
      period = p, highway = as.character(d_data$highway[row_in_d]),
      pred_flow = as.numeric(pred_flow_P[row_in_d]),
      actual_flow = as.numeric(actual_flow_P),
      pred_truck_pct = as.numeric(pred_truck_P[row_in_d]),
      actual_truck_pct = as.numeric(actual_truck_P),
      pred_speed = as.numeric(pred_speed_P[row_in_d]),
      actual_speed = as.numeric(actual_speed_P),
      osm_speed = as.numeric(osm_speed_raw[row_in_d]),
      stringsAsFactors = FALSE)
    emission_P <- emission_P[!is.na(emission_P$actual_speed) & emission_P$actual_speed >= 5 &
                               !is.na(emission_P$actual_flow), ]
    if (nrow(emission_P) > 0) {
      emission_parts <- c(emission_parts, list(emission_P))
      n_periods_done <- n_periods_done + 1
    }
  }
  
  emission_test <- do.call(rbind, emission_parts)
  rm(emission_parts)
  
  pipeline_message(text = sprintf("[%s] Emission rows: %s across %d periods",
                                  label, fmt(nrow(emission_test)), n_periods_done + 1),
                   process = "info")
  
  # CNOSSOS dB
  emission_test$pred_db <- compute_emission_cnossos(
    emission_test$pred_flow, emission_test$pred_truck_pct, emission_test$pred_speed)
  emission_test$actual_db <- compute_emission_cnossos(
    emission_test$actual_flow, emission_test$actual_truck_pct, emission_test$actual_speed)
  emission_test$db_error <- emission_test$pred_db - emission_test$actual_db
  
  # Per-variable dB contributions
  emission_test$contrib_flow <- compute_emission_cnossos(
    emission_test$pred_flow, emission_test$actual_truck_pct,
    emission_test$actual_speed) - emission_test$actual_db
  emission_test$contrib_truck <- compute_emission_cnossos(
    emission_test$actual_flow, emission_test$pred_truck_pct,
    emission_test$actual_speed) - emission_test$actual_db
  emission_test$contrib_speed <- compute_emission_cnossos(
    emission_test$actual_flow, emission_test$actual_truck_pct,
    emission_test$pred_speed) - emission_test$actual_db
  
  valid <- is.finite(emission_test$db_error)
  emission_test <- emission_test[valid, ]
  
  pipeline_message(text = sprintf("[%s] Done: %s valid samples",
                                  label, fmt(nrow(emission_test))),
                   level = 1, progress = "end", process = "valid")
  emission_test
}

# ------ Flow-augmented emission predictions (speed models need flow input) ----
run_emission_flow_augmented <- function(models_list, label) {
  pipeline_message(text = sprintf("Running emission predictions [%s]", label),
                   level = 1, progress = "start", process = "calc")
  
  d_data <- training_data %>%
    filter(period == "D",
           count_point_id %in% shared_test_sensors,
           !is.na(aggregate_flow), aggregate_flow >= 1)
  
  d_matrix <- safe_sparse_model_matrix(road_feature_formula, d_data)
  if (nrow(d_matrix) != nrow(d_data)) {
    kept <- as.integer(rownames(d_matrix))
    d_data <- d_data[kept, ]
  }
  
  # Step 1: predict flow first (uses standard features only)
  pred_flow_D <- 10^predict(models_list[["flow_D"]]$model,
                             align_matrix_to_model(d_matrix, models_list[["flow_D"]]$model))
  
  # Step 2: augment matrix with flow for speed models
  match_lanes <- match(d_data$count_point_id, lanes_lookup$count_point_id)
  lanes_vec <- rep(1, nrow(d_matrix))
  has_lanes <- !is.na(match_lanes)
  lanes_vec[has_lanes] <- lanes_lookup$lanes_osm[match_lanes[has_lanes]]
  
  d_matrix_augmented <- augment_with_flow(d_matrix, pred_flow_D, lanes_vec)
  
  # Step 3: predict truck (standard) and speed (augmented)
  if (!is.null(models_list[["truck_pct_D"]]$model)) {
    pred_truck_D <- predict(models_list[["truck_pct_D"]]$model,
                            align_matrix_to_model(d_matrix, models_list[["truck_pct_D"]]$model))
  } else {
    pred_truck_D <- ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
                           d_data$truck_pct, 0)
    pipeline_message(
      text = sprintf("[%s] Missing base model truck_pct_D: using observed truck_pct as fallback", label),
      process = "warning")
  }
  
  # Speed model uses augmented matrix (if flow_augmented flag is set)
  if (isTRUE(models_list[["speed_D"]]$flow_augmented)) {
    pred_speed_ratio <- predict(models_list[["speed_D"]]$model,
                                 align_matrix_to_model(d_matrix_augmented,
                                                        models_list[["speed_D"]]$model))
  } else {
    pred_speed_ratio <- predict(models_list[["speed_D"]]$model,
                                 align_matrix_to_model(d_matrix, models_list[["speed_D"]]$model))
  }
  
  osm_speed_raw <- suppressWarnings(as.numeric(d_data$speed))
  osm_speed_raw[is.na(osm_speed_raw) | osm_speed_raw < 5] <- CONFIG$DEFAULT_VEHICLE_SPEED
  pred_speed_D <- pmax(5, pred_speed_ratio * osm_speed_raw)
  
  actual_speed <- ifelse(!is.na(d_data$aggregate_speed) & d_data$aggregate_speed >= 5,
                          d_data$aggregate_speed, osm_speed_raw)
  actual_truck <- ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
                          d_data$truck_pct, 0)
  
  emission_D <- data.frame(
    count_point_id = d_data$count_point_id,
    period = "D", highway = as.character(d_data$highway),
    pred_flow = as.numeric(pred_flow_D),
    actual_flow = as.numeric(d_data$aggregate_flow),
    pred_truck_pct = as.numeric(pred_truck_D),
    actual_truck_pct = as.numeric(actual_truck),
    pred_speed = as.numeric(pred_speed_D),
    actual_speed = as.numeric(actual_speed),
    osm_speed = as.numeric(osm_speed_raw),
    stringsAsFactors = FALSE)
  emission_D <- emission_D[!is.na(emission_D$actual_speed) & emission_D$actual_speed >= 5, ]
  
  emission_parts <- list(emission_D)
  test_sensor_ids <- unique(emission_D$count_point_id)
  n_periods_done <- 0
  
  non_d_periods <- setdiff(all_periods, "D")
  for (p in non_d_periods) {
    flow_key <- paste0("ratio_flow_", p)
    truck_key <- paste0("ratio_truck_pct_", p)
    speed_key <- paste0("ratio_speed_", p)
    if (is.null(models_list[[flow_key]]$model)) next
    
    ratio_flow <- predict(models_list[[flow_key]]$model,
                           align_matrix_to_model(d_matrix, models_list[[flow_key]]$model))
    ratio_truck <- if (!is.null(models_list[[truck_key]]$model)) {
      predict(models_list[[truck_key]]$model,
              align_matrix_to_model(d_matrix, models_list[[truck_key]]$model))
    } else rep(1, nrow(d_matrix))
    
    # Speed ratio models may also be flow-augmented
    if (isTRUE(models_list[[speed_key]]$flow_augmented)) {
      ratio_speed <- predict(models_list[[speed_key]]$model,
                              align_matrix_to_model(d_matrix_augmented,
                                                     models_list[[speed_key]]$model))
    } else if (!is.null(models_list[[speed_key]]$model)) {
      ratio_speed <- predict(models_list[[speed_key]]$model,
                              align_matrix_to_model(d_matrix, models_list[[speed_key]]$model))
    } else {
      ratio_speed <- rep(1, nrow(d_matrix))
    }
    
    pred_flow_P <- pmax(0, pred_flow_D * ratio_flow)
    pred_truck_P <- pmin(100, pmax(0, pred_truck_D * ratio_truck))
    pred_speed_P <- pmax(5, pred_speed_D * ratio_speed)
    
    p_data <- training_data %>%
      filter(period == p, count_point_id %in% test_sensor_ids)
    if (nrow(p_data) == 0) next
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
                              p_data$aggregate_speed, osm_speed_raw[row_in_d])
    
    emission_P <- data.frame(
      count_point_id = p_data$count_point_id,
      period = p, highway = as.character(d_data$highway[row_in_d]),
      pred_flow = as.numeric(pred_flow_P[row_in_d]),
      actual_flow = as.numeric(actual_flow_P),
      pred_truck_pct = as.numeric(pred_truck_P[row_in_d]),
      actual_truck_pct = as.numeric(actual_truck_P),
      pred_speed = as.numeric(pred_speed_P[row_in_d]),
      actual_speed = as.numeric(actual_speed_P),
      osm_speed = as.numeric(osm_speed_raw[row_in_d]),
      stringsAsFactors = FALSE)
    emission_P <- emission_P[!is.na(emission_P$actual_speed) & emission_P$actual_speed >= 5 &
                               !is.na(emission_P$actual_flow), ]
    if (nrow(emission_P) > 0) {
      emission_parts <- c(emission_parts, list(emission_P))
      n_periods_done <- n_periods_done + 1
    }
  }
  
  emission_test <- do.call(rbind, emission_parts)
  rm(emission_parts)
  
  pipeline_message(text = sprintf("[%s] Emission rows: %s across %d periods",
                                  label, fmt(nrow(emission_test)), n_periods_done + 1),
                   process = "info")
  
  # CNOSSOS dB
  emission_test$pred_db <- compute_emission_cnossos(
    emission_test$pred_flow, emission_test$pred_truck_pct, emission_test$pred_speed)
  emission_test$actual_db <- compute_emission_cnossos(
    emission_test$actual_flow, emission_test$actual_truck_pct, emission_test$actual_speed)
  emission_test$db_error <- emission_test$pred_db - emission_test$actual_db
  
  emission_test$contrib_flow <- compute_emission_cnossos(
    emission_test$pred_flow, emission_test$actual_truck_pct,
    emission_test$actual_speed) - emission_test$actual_db
  emission_test$contrib_truck <- compute_emission_cnossos(
    emission_test$actual_flow, emission_test$pred_truck_pct,
    emission_test$actual_speed) - emission_test$actual_db
  emission_test$contrib_speed <- compute_emission_cnossos(
    emission_test$actual_flow, emission_test$actual_truck_pct,
    emission_test$pred_speed) - emission_test$actual_db
  
  valid <- is.finite(emission_test$db_error)
  emission_test <- emission_test[valid, ]
  
  pipeline_message(text = sprintf("[%s] Done: %s valid samples",
                                  label, fmt(nrow(emission_test))),
                   level = 1, progress = "end", process = "valid")
  emission_test
}

# Run for both model sets
emission_std <- run_emission_standard(std_models, "STANDARD")
gc(verbose = FALSE)
emission_fa <- run_emission_flow_augmented(fa_models, "FLOW-AUG")
gc(verbose = FALSE)

# ==============================================================================
# 4. Compute and print comparison metrics
# ==============================================================================

pipeline_message(text = "Computing comparison metrics",
                 level = 1, progress = "start", process = "search")

compute_stats <- function(df) {
  list(
    n = nrow(df),
    bias = mean(df$db_error, na.rm = TRUE),
    mae = mean(abs(df$db_error), na.rm = TRUE),
    rmse = sqrt(mean(df$db_error^2, na.rm = TRUE)),
    medae = median(abs(df$db_error), na.rm = TRUE),
    p90ae = as.numeric(quantile(abs(df$db_error), 0.90, na.rm = TRUE)),
    bias_flow = mean(df$contrib_flow, na.rm = TRUE),
    bias_truck = mean(df$contrib_truck, na.rm = TRUE),
    bias_speed = mean(df$contrib_speed, na.rm = TRUE),
    mae_flow = mean(abs(df$contrib_flow), na.rm = TRUE),
    mae_truck = mean(abs(df$contrib_truck), na.rm = TRUE),
    mae_speed = mean(abs(df$contrib_speed), na.rm = TRUE)
  )
}

s_std <- compute_stats(emission_std)
s_fa <- compute_stats(emission_fa)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║       COMPARISON: STANDARD vs FLOW-AUGMENTED (emission dB test)      ║\n")
cat("╠════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %12s  %12s  ║\n", "Metric", "STANDARD", "FLOW-AUG"))
cat("╠════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %12d  %12d  ║\n", "N test samples", s_std$n, s_fa$n))
cat("╠════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %+11.2f  %+12.2f  ║\n", "Bias (dB)", s_std$bias, s_fa$bias))
cat(sprintf("║  %-32s  %12.2f  %12.2f  ║\n", "MAE (dB)", s_std$mae, s_fa$mae))
cat(sprintf("║  %-32s  %12.2f  %12.2f  ║\n", "RMSE (dB)", s_std$rmse, s_fa$rmse))
cat(sprintf("║  %-32s  %12.2f  %12.2f  ║\n", "MedAE (dB)", s_std$medae, s_fa$medae))
cat(sprintf("║  %-32s  %12.2f  %12.2f  ║\n", "P90 |Error| (dB)", s_std$p90ae, s_fa$p90ae))
cat("╠════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %+11.2f  %+12.2f  ║\n", "Bias — Flow contrib (dB)", s_std$bias_flow, s_fa$bias_flow))
cat(sprintf("║  %-32s  %+11.2f  %+12.2f  ║\n", "Bias — Truck% contrib (dB)", s_std$bias_truck, s_fa$bias_truck))
cat(sprintf("║  %-32s  %+11.2f  %+12.2f  ║\n", "Bias — Speed contrib (dB) ★", s_std$bias_speed, s_fa$bias_speed))
cat("╠════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %12.2f  %12.2f  ║\n", "MAE — Flow contrib (dB)", s_std$mae_flow, s_fa$mae_flow))
cat(sprintf("║  %-32s  %12.2f  %12.2f  ║\n", "MAE — Truck% contrib (dB)", s_std$mae_truck, s_fa$mae_truck))
cat(sprintf("║  %-32s  %12.2f  %12.2f  ║\n", "MAE — Speed contrib (dB) ★", s_std$mae_speed, s_fa$mae_speed))
cat("╚════════════════════════════════════════════════════════════════════════╝\n")

# Improvement summary
cat("\n")
cat("Key changes (★ = target of flow augmentation):\n")
cat(sprintf("  Δ Bias:       %+.2f → %+.2f dB  (Δ = %+.2f)\n",
            s_std$bias, s_fa$bias, s_fa$bias - s_std$bias))
cat(sprintf("  Δ MAE:        %.2f → %.2f dB  (Δ = %+.2f, %+.1f%%)\n",
            s_std$mae, s_fa$mae, s_fa$mae - s_std$mae,
            (s_fa$mae - s_std$mae) / s_std$mae * 100))
cat(sprintf("  Δ RMSE:       %.2f → %.2f dB  (Δ = %+.2f, %+.1f%%)\n",
            s_std$rmse, s_fa$rmse, s_fa$rmse - s_std$rmse,
            (s_fa$rmse - s_std$rmse) / s_std$rmse * 100))
cat(sprintf("  ★ Δ Speed bias: %+.2f → %+.2f dB  (Δ = %+.2f)\n",
            s_std$bias_speed, s_fa$bias_speed,
            s_fa$bias_speed - s_std$bias_speed))
cat(sprintf("  ★ Δ Speed MAE:  %.2f → %.2f dB  (Δ = %+.2f, %+.1f%%)\n",
            s_std$mae_speed, s_fa$mae_speed,
            s_fa$mae_speed - s_std$mae_speed,
            (s_fa$mae_speed - s_std$mae_speed) / s_std$mae_speed * 100))
cat("\n")

pipeline_message(text = "Comparison metrics computed",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 5. Correlation with speed (KEY diagnostic — does flow break the dependency?)
# ==============================================================================

pipeline_message(text = "Correlation analysis (key diagnostic)",
                 level = 1, progress = "start", process = "search")

cor_std <- cor(emission_std$db_error, emission_std$actual_speed, use = "complete.obs")
cor_fa <- cor(emission_fa$db_error, emission_fa$actual_speed, use = "complete.obs")

cor_speed_std <- cor(emission_std$contrib_speed, emission_std$actual_speed, use = "complete.obs")
cor_speed_fa <- cor(emission_fa$contrib_speed, emission_fa$actual_speed, use = "complete.obs")

cat(sprintf("Correlation r(dB_error, actual_speed):\n"))
cat(sprintf("  Standard:     r = %.3f\n", cor_std))
cat(sprintf("  Flow-augmented: r = %.3f\n", cor_fa))
cat(sprintf("  Change:       Δr = %+.3f  (%s)\n", cor_fa - cor_std,
            if (abs(cor_fa) < abs(cor_std)) "IMPROVED ✓" else "worse"))

cat(sprintf("\nCorrelation r(speed_contrib_dB, actual_speed):\n"))
cat(sprintf("  Standard:     r = %.3f\n", cor_speed_std))
cat(sprintf("  Flow-augmented: r = %.3f\n", cor_speed_fa))
cat(sprintf("  Change:       Δr = %+.3f  (%s)\n", cor_speed_fa - cor_speed_std,
            if (abs(cor_speed_fa) < abs(cor_speed_std)) "IMPROVED ✓" else "worse"))

pipeline_message(text = "Correlation analysis complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 6. Per-highway comparison
# ==============================================================================

pipeline_message(text = "Per-highway breakdown",
                 level = 1, progress = "start", process = "search")

hw_std <- as.data.table(emission_std)[, .(
  n = .N,
  bias_std = mean(db_error), mae_std = mean(abs(db_error)),
  speed_bias_std = mean(contrib_speed), speed_mae_std = mean(abs(contrib_speed))
), by = highway]

hw_fa <- as.data.table(emission_fa)[, .(
  bias_fa = mean(db_error), mae_fa = mean(abs(db_error)),
  speed_bias_fa = mean(contrib_speed), speed_mae_fa = mean(abs(contrib_speed))
), by = highway]

hw_comp <- merge(hw_std, hw_fa, by = "highway", all.x = TRUE)
hw_comp <- hw_comp[order(-n)]
hw_comp$delta_mae <- hw_comp$mae_fa - hw_comp$mae_std
hw_comp$delta_speed_mae <- hw_comp$speed_mae_fa - hw_comp$speed_mae_std

top_hw <- hw_comp[1:min(10, nrow(hw_comp))]

cat("\nPer-highway comparison (★ = speed MAE change):\n")
for (i in 1:nrow(top_hw)) {
  h <- top_hw[i]
  cat(sprintf("  %-15s (n=%5d): MAE %.2f→%.2f dB (Δ%+.2f) | ★ Speed MAE %.2f→%.2f (Δ%+.2f)\n",
              h$highway, h$n, h$mae_std, h$mae_fa, h$delta_mae,
              h$speed_mae_std, h$speed_mae_fa, h$delta_speed_mae))
}

pipeline_message(text = "Per-highway breakdown complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 7. Speed-binned comparison
# ==============================================================================

pipeline_message(text = "Speed-binned comparison",
                 level = 1, progress = "start", process = "search")

speed_breaks <- c(0, 30, 50, 70, 90, 110, 200)
speed_labels <- c("0-30", "30-50", "50-70", "70-90", "90-110", "110+")

emission_std$speed_bin <- cut(emission_std$actual_speed, speed_breaks, speed_labels, right = FALSE)
emission_fa$speed_bin <- cut(emission_fa$actual_speed, speed_breaks, speed_labels, right = FALSE)

sb_std <- as.data.table(emission_std)[!is.na(speed_bin), .(
  n = .N, bias_std = mean(db_error), mae_std = mean(abs(db_error)),
  speed_bias_std = mean(contrib_speed)
), by = speed_bin]

sb_fa <- as.data.table(emission_fa)[!is.na(speed_bin), .(
  bias_fa = mean(db_error), mae_fa = mean(abs(db_error)),
  speed_bias_fa = mean(contrib_speed)
), by = speed_bin]

sb_comp <- merge(sb_std, sb_fa, by = "speed_bin", all.x = TRUE)

cat("\nSpeed-binned comparison:\n")
cat(sprintf("  %-10s %6s  %8s %8s  %8s %8s  %10s %10s\n",
            "Speed", "N", "MAE_std", "MAE_fa", "Bias_std", "Bias_fa",
            "SpBias_std", "SpBias_fa"))
for (i in 1:nrow(sb_comp)) {
  s <- sb_comp[i]
  cat(sprintf("  %-10s %6d  %7.2f  %7.2f   %+7.2f  %+7.2f   %+9.2f  %+9.2f\n",
              as.character(s$speed_bin), s$n,
              s$mae_std, s$mae_fa, s$bias_std, s$bias_fa,
              s$speed_bias_std, s$speed_bias_fa))
}

pipeline_message(text = "Speed-binned comparison complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 8. Speed prediction comparison (direct, not just dB)
# ==============================================================================

pipeline_message(text = "Direct speed prediction comparison (period D only)",
                 level = 1, progress = "start", process = "search")

d_std <- emission_std[emission_std$period == "D", ]
d_fa <- emission_fa[emission_fa$period == "D", ]

speed_rmse_std <- sqrt(mean((d_std$pred_speed - d_std$actual_speed)^2, na.rm = TRUE))
speed_rmse_fa <- sqrt(mean((d_fa$pred_speed - d_fa$actual_speed)^2, na.rm = TRUE))
speed_mae_std <- mean(abs(d_std$pred_speed - d_std$actual_speed), na.rm = TRUE)
speed_mae_fa <- mean(abs(d_fa$pred_speed - d_fa$actual_speed), na.rm = TRUE)
speed_bias_std <- mean(d_std$pred_speed - d_std$actual_speed, na.rm = TRUE)
speed_bias_fa <- mean(d_fa$pred_speed - d_fa$actual_speed, na.rm = TRUE)
speed_r2_std <- 1 - sum((d_std$pred_speed - d_std$actual_speed)^2) /
  sum((d_std$actual_speed - mean(d_std$actual_speed))^2)
speed_r2_fa <- 1 - sum((d_fa$pred_speed - d_fa$actual_speed)^2) /
  sum((d_fa$actual_speed - mean(d_fa$actual_speed))^2)

cat(sprintf("\nDirect speed prediction comparison (period D, km/h):\n"))
cat(sprintf("  %-25s  %10s  %10s  %10s\n", "Metric", "Standard", "Flow-aug", "Δ"))
cat(sprintf("  %-25s  %10.1f  %10.1f  %+9.1f\n",
            "Bias (km/h)", speed_bias_std, speed_bias_fa, speed_bias_fa - speed_bias_std))
cat(sprintf("  %-25s  %10.1f  %10.1f  %+9.1f\n",
            "MAE (km/h)", speed_mae_std, speed_mae_fa, speed_mae_fa - speed_mae_std))
cat(sprintf("  %-25s  %10.1f  %10.1f  %+9.1f\n",
            "RMSE (km/h)", speed_rmse_std, speed_rmse_fa, speed_rmse_fa - speed_rmse_std))
cat(sprintf("  %-25s  %10.3f  %10.3f  %+9.3f\n",
            "R²", speed_r2_std, speed_r2_fa, speed_r2_fa - speed_r2_std))

pipeline_message(text = "Speed prediction comparison complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 9. Generate comparison PDF
# ==============================================================================

pipeline_message(text = "Generating comparison PDF",
                 level = 1, progress = "start", process = "plot")

pdf_path <- file.path(CONFIG$FIGS_DIR, "09_standard_vs_flow_augmented_comparison.pdf")
if (!dir.exists(CONFIG$FIGS_DIR)) dir.create(CONFIG$FIGS_DIR, recursive = TRUE)

col_std <- rgb(0.2, 0.4, 0.8, 0.7)
col_fa <- rgb(0.1, 0.7, 0.3, 0.7)

grDevices::pdf(file = pdf_path, width = 14, height = 10)
old_par <- par(no.readonly = TRUE)

# --- Page 1: Side-by-side error distributions & error vs speed ---
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

hist(emission_std$db_error, breaks = 80, col = rgb(0.2, 0.4, 0.8, 0.5),
     main = "dB Error — Standard",
     xlab = "Prediction error (dB)", ylab = "Count", xlim = c(-20, 20))
abline(v = s_std$bias, col = "red", lwd = 2, lty = 2)
abline(v = 0, col = "black", lwd = 1)
legend("topright", legend = sprintf("Bias=%+.2f\nMAE=%.2f\nRMSE=%.2f",
       s_std$bias, s_std$mae, s_std$rmse), bty = "n", cex = 1.1)

hist(emission_fa$db_error, breaks = 80, col = rgb(0.1, 0.7, 0.3, 0.5),
     main = "dB Error — Flow-Augmented",
     xlab = "Prediction error (dB)", ylab = "Count", xlim = c(-20, 20))
abline(v = s_fa$bias, col = "red", lwd = 2, lty = 2)
abline(v = 0, col = "black", lwd = 1)
legend("topright", legend = sprintf("Bias=%+.2f\nMAE=%.2f\nRMSE=%.2f",
       s_fa$bias, s_fa$mae, s_fa$rmse), bty = "n", cex = 1.1)

plot(emission_std$actual_speed, emission_std$db_error,
     pch = ".", col = rgb(0, 0, 1, 0.1), cex = 2,
     main = sprintf("dB Error vs Speed — Standard (r=%.3f)", cor_std),
     xlab = "Actual speed (km/h)", ylab = "dB error", ylim = c(-20, 20))
abline(h = 0, col = "gray50")
lines(lowess(emission_std$actual_speed, emission_std$db_error, f = 0.3),
      col = "red", lwd = 3)

plot(emission_fa$actual_speed, emission_fa$db_error,
     pch = ".", col = rgb(0, 0.5, 0, 0.1), cex = 2,
     main = sprintf("dB Error vs Speed — Flow-Aug (r=%.3f)", cor_fa),
     xlab = "Actual speed (km/h)", ylab = "dB error", ylim = c(-20, 20))
abline(h = 0, col = "gray50")
lines(lowess(emission_fa$actual_speed, emission_fa$db_error, f = 0.3),
      col = "darkgreen", lwd = 3)

# --- Page 2: Bias decomposition & speed-binned ---
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

bias_data <- matrix(c(s_std$bias_flow, s_std$bias_truck, s_std$bias_speed,
                       s_fa$bias_flow, s_fa$bias_truck, s_fa$bias_speed),
                     nrow = 2, byrow = TRUE)
colnames(bias_data) <- c("Flow", "Truck%", "Speed ★")
barplot(bias_data, beside = TRUE, col = c(col_std, col_fa),
        main = "Bias Decomposition by Variable",
        ylab = "Bias (dB)", ylim = c(min(bias_data) * 1.3, max(bias_data) * 1.3))
abline(h = 0, col = "gray50")
legend("topleft", legend = c("Standard", "Flow-Augmented"),
       fill = c(col_std, col_fa))

mae_data <- matrix(c(s_std$mae_flow, s_std$mae_truck, s_std$mae_speed,
                      s_fa$mae_flow, s_fa$mae_truck, s_fa$mae_speed),
                    nrow = 2, byrow = TRUE)
colnames(mae_data) <- c("Flow", "Truck%", "Speed ★")
barplot(mae_data, beside = TRUE, col = c(col_std, col_fa),
        main = "MAE Decomposition by Variable",
        ylab = "MAE (dB)")
legend("topleft", legend = c("Standard", "Flow-Augmented"),
       fill = c(col_std, col_fa))

# Speed-binned MAE
if (nrow(sb_comp) > 0) {
  sb_mat <- matrix(c(sb_comp$mae_std, sb_comp$mae_fa), nrow = 2, byrow = TRUE)
  colnames(sb_mat) <- as.character(sb_comp$speed_bin)
  barplot(sb_mat, beside = TRUE, col = c(col_std, col_fa),
          main = "MAE by Speed Bin",
          xlab = "Speed (km/h)", ylab = "MAE (dB)")
  legend("topleft", legend = c("Standard", "Flow-Augmented"),
         fill = c(col_std, col_fa))
  
  sb_bias_mat <- matrix(c(sb_comp$speed_bias_std, sb_comp$speed_bias_fa),
                         nrow = 2, byrow = TRUE)
  colnames(sb_bias_mat) <- as.character(sb_comp$speed_bin)
  barplot(sb_bias_mat, beside = TRUE, col = c(col_std, col_fa),
          main = "Speed Contribution Bias by Speed Bin ★",
          xlab = "Speed (km/h)", ylab = "Speed bias (dB)",
          ylim = c(min(sb_bias_mat) * 1.3, max(sb_bias_mat) * 1.3))
  abline(h = 0, col = "gray50")
  legend("topleft", legend = c("Standard", "Flow-Augmented"),
         fill = c(col_std, col_fa))
}

# --- Page 3: Per-highway comparison ---
par(mfrow = c(1, 1), mar = c(8, 5, 4, 2))
if (nrow(top_hw) > 0) {
  hw_mat <- matrix(c(top_hw$mae_std, top_hw$mae_fa), nrow = 2, byrow = TRUE)
  colnames(hw_mat) <- paste0(top_hw$highway, "\n(n=", top_hw$n, ")")
  barplot(hw_mat, beside = TRUE, col = c(col_std, col_fa),
          main = "MAE by Highway Type — Standard vs Flow-Augmented",
          ylab = "MAE (dB)", las = 2, cex.names = 0.8)
  legend("topright", legend = c("Standard", "Flow-Augmented"),
         fill = c(col_std, col_fa))
}

# --- Page 4: Pred vs Actual speed (period D only) ---
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
lim_sp <- c(0, max(c(d_std$actual_speed, d_std$pred_speed,
                      d_fa$pred_speed), na.rm = TRUE) * 1.05)

plot(d_std$actual_speed, d_std$pred_speed,
     pch = 20, col = rgb(0, 0, 1, 0.2), cex = 0.8,
     main = sprintf("Speed: Pred vs Actual — Standard\nR²=%.3f, RMSE=%.1f km/h",
                     speed_r2_std, speed_rmse_std),
     xlab = "Actual speed (km/h)", ylab = "Predicted speed (km/h)",
     xlim = lim_sp, ylim = lim_sp)
abline(0, 1, col = "red", lwd = 2)
abline(h = 50, v = 50, col = "gray80", lty = 3)

plot(d_fa$actual_speed, d_fa$pred_speed,
     pch = 20, col = rgb(0, 0.5, 0, 0.2), cex = 0.8,
     main = sprintf("Speed: Pred vs Actual — Flow-Aug\nR²=%.3f, RMSE=%.1f km/h",
                     speed_r2_fa, speed_rmse_fa),
     xlab = "Actual speed (km/h)", ylab = "Predicted speed (km/h)",
     xlim = lim_sp, ylim = lim_sp)
abline(0, 1, col = "red", lwd = 2)
abline(h = 50, v = 50, col = "gray80", lty = 3)

# --- Page 5: Pred vs Actual dB ---
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
lim_db <- range(c(emission_std$actual_db, emission_std$pred_db,
                   emission_fa$actual_db, emission_fa$pred_db), na.rm = TRUE)

db_r2_std <- 1 - sum((emission_std$pred_db - emission_std$actual_db)^2) /
  sum((emission_std$actual_db - mean(emission_std$actual_db))^2)
db_r2_fa <- 1 - sum((emission_fa$pred_db - emission_fa$actual_db)^2) /
  sum((emission_fa$actual_db - mean(emission_fa$actual_db))^2)

plot(emission_std$actual_db, emission_std$pred_db,
     pch = ".", col = rgb(0, 0, 1, 0.1), cex = 2,
     main = sprintf("Pred vs Actual Lw — Standard\nR²=%.3f", db_r2_std),
     xlab = "Actual Lw (dB)", ylab = "Predicted Lw (dB)",
     xlim = lim_db, ylim = lim_db)
abline(0, 1, col = "red", lwd = 2)

plot(emission_fa$actual_db, emission_fa$pred_db,
     pch = ".", col = rgb(0, 0.5, 0, 0.1), cex = 2,
     main = sprintf("Pred vs Actual Lw — Flow-Aug\nR²=%.3f", db_r2_fa),
     xlab = "Actual Lw (dB)", ylab = "Predicted Lw (dB)",
     xlim = lim_db, ylim = lim_db)
abline(0, 1, col = "red", lwd = 2)

par(old_par)
dev.off()

pipeline_message(text = sprintf("PDF saved: %s", rel_path(pdf_path)),
                 level = 1, progress = "end", process = "valid")

pipeline_message(text = "Standard vs Flow-Augmented comparison complete",
                 level = 0, progress = "end", process = "valid")
