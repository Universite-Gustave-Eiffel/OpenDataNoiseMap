# ==============================================================================
# Comparison: Standard vs dB-aware vs Flow-augmented — OSM SPEED ONLY
# ==============================================================================
# Same as run_flow_comparison.R but using osm_speed for ALL CNOSSOS calculations.
# Both predicted and actual emissions use osm_speed → speed error is ZERO.
# This isolates the dB error contribution from flow and truck_pct predictions.
#
# Usage: Rscript run_osm_speed_comparison.R
# ==============================================================================

# --- Bootstrap & config ---
setwd("/home/aumond/Documents/github/OpenDataNoiseMap/RoadTrafficEstimation")
source("bootstrap.R")
source("config_pipeline.R")

pipeline_message(text = "OSM-SPEED-ONLY comparison (flow + truck_pct errors only)",
                 level = 0, progress = "start", process = "plot")

# ==============================================================================
# 1. Load artifacts
# ==============================================================================

pipeline_message(text = "Loading models and training data",
                 level = 1, progress = "start", process = "load")

std_models <- readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
pipeline_message(text = sprintf("Standard models: %d", length(std_models)), process = "info")

db_models_path <- file.path(CONFIG$TRAINING_DATA_DIR, "rds",
                             "06_xgboost_trained_models_db_aware.rds")
db_models <- readRDS(db_models_path)
pipeline_message(text = sprintf("dB-aware models: %d", length(db_models)), process = "info")

fa_models_path <- file.path(CONFIG$TRAINING_DATA_DIR, "rds",
                             "06_xgboost_trained_models_flow_augmented.rds")
fa_models <- readRDS(fa_models_path)
pipeline_message(text = sprintf("Flow-augmented models: %d", length(fa_models)), process = "info")

feature_info <- readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)
road_feature_formula <- feature_info$road_feature_formula
all_periods <- feature_info$all_periods

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
# 2. Reconstruct shared sensor split (same seed=42)
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
# 3. Helper functions
# ==============================================================================

align_matrix_to_model <- function(mat, model) {
  expected <- model$feature_names
  if (is.null(expected)) return(mat)
  cur <- colnames(mat)
  missing <- setdiff(expected, cur)
  if (length(missing) > 0) {
    zero_mat <- Matrix::Matrix(0, nrow = nrow(mat), ncol = length(missing), sparse = TRUE)
    colnames(zero_mat) <- missing
    mat <- cbind(mat, zero_mat)
  }
  mat[, expected, drop = FALSE]
}

safe_sparse_model_matrix <- function(formula_obj, data_df) {
  vars_in_formula <- intersect(unique(all.vars(formula_obj)), names(data_df))
  if (nrow(data_df) == 0L) return(Matrix::sparse.model.matrix(formula_obj, data_df))
  mm_data <- data_df
  for (v in vars_in_formula) {
    if (is.factor(mm_data[[v]])) mm_data[[v]] <- as.character(mm_data[[v]])
  }
  mm_subset <- mm_data[, vars_in_formula, drop = FALSE]
  cc_idx <- complete.cases(mm_subset)
  cc_data <- mm_data[cc_idx, , drop = FALSE]
  for (v in vars_in_formula) {
    if (is.character(mm_data[[v]])) {
      lv <- unique(cc_data[[v]])
      lv <- lv[!is.na(lv) & nzchar(lv)]
      if (length(lv) <= 1) mm_data[[v]] <- 0
    }
  }
  Matrix::sparse.model.matrix(formula_obj, mm_data)
}

augment_with_flow <- function(mat, pred_flow_vec, lanes_vec) {
  n <- nrow(mat)
  log_flow <- log10(pmax(pred_flow_vec, 1))
  safe_lanes <- pmax(lanes_vec, 1)
  flow_per_lane <- pred_flow_vec / safe_lanes
  aug_mat <- Matrix::Matrix(cbind(log_flow, flow_per_lane), nrow = n, sparse = TRUE)
  colnames(aug_mat) <- c("pred_log10_flow", "flow_per_lane")
  cbind(mat, aug_mat)
}

# ==============================================================================
# 4. Emission prediction — OSM_SPEED for all CNOSSOS calls
# ==============================================================================
# Key difference: speed = osm_speed for BOTH pred_db and actual_db
# → speed contribution = 0, only flow + truck_pct errors matter

run_emission_osm_speed <- function(models_list, label, is_flow_augmented = FALSE) {
  pipeline_message(text = sprintf("Running emission predictions [%s] (osm_speed only)", label),
                   level = 1, progress = "start", process = "calc")

  d_data <- training_data %>%
    dplyr::filter(period == "D",
                  count_point_id %in% shared_test_sensors,
                  !is.na(aggregate_flow), aggregate_flow >= 1)

  d_matrix <- safe_sparse_model_matrix(road_feature_formula, d_data)
  if (nrow(d_matrix) != nrow(d_data)) {
    kept <- as.integer(rownames(d_matrix))
    d_data <- d_data[kept, ]
  }

  # --- Base predictions (period D) ---
  pred_flow_D <- 10^predict(models_list[["flow_D"]]$model,
                             align_matrix_to_model(d_matrix, models_list[["flow_D"]]$model))

  pred_truck_D <- if (!is.null(models_list[["truck_pct_D"]]$model)) {
    predict(models_list[["truck_pct_D"]]$model,
            align_matrix_to_model(d_matrix, models_list[["truck_pct_D"]]$model))
  } else {
    pipeline_message(text = sprintf("[%s] truck_pct_D model missing — using observed truck_pct", label), process = "info")
    ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0, d_data$truck_pct, 0)
  }

  # OSM speed — used for ALL CNOSSOS calls, not predicted speed
  osm_speed_raw <- suppressWarnings(as.numeric(d_data$speed))
  osm_speed_raw[is.na(osm_speed_raw) | osm_speed_raw < 5] <- CONFIG$DEFAULT_VEHICLE_SPEED

  actual_truck <- ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
                          d_data$truck_pct, 0)

  # Period D frame — speed is ALWAYS osm_speed
  emission_D <- data.frame(
    count_point_id = d_data$count_point_id,
    period = "D",
    highway = as.character(d_data$highway),
    pred_flow = as.numeric(pred_flow_D),
    actual_flow = as.numeric(d_data$aggregate_flow),
    pred_truck_pct = as.numeric(pred_truck_D),
    actual_truck_pct = as.numeric(actual_truck),
    osm_speed = as.numeric(osm_speed_raw),
    stringsAsFactors = FALSE
  )

  emission_parts <- list(emission_D)
  test_sensor_ids <- unique(emission_D$count_point_id)
  n_periods_done <- 0

  # --- Non-D periods via ratio models ---
  non_d_periods <- setdiff(all_periods, "D")
  for (p in non_d_periods) {
    flow_key <- paste0("ratio_flow_", p)
    truck_key <- paste0("ratio_truck_pct_", p)

    if (is.null(models_list[[flow_key]]$model)) next

    ratio_flow <- predict(models_list[[flow_key]]$model,
                           align_matrix_to_model(d_matrix, models_list[[flow_key]]$model))
    ratio_truck <- if (!is.null(models_list[[truck_key]]$model)) {
      predict(models_list[[truck_key]]$model,
              align_matrix_to_model(d_matrix, models_list[[truck_key]]$model))
    } else rep(1, nrow(d_matrix))

    pred_flow_P <- pmax(0, pred_flow_D * ratio_flow)
    pred_truck_P <- pmin(100, pmax(0, pred_truck_D * ratio_truck))

    p_data <- training_data %>%
      dplyr::filter(period == p, count_point_id %in% test_sensor_ids)
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

    emission_P <- data.frame(
      count_point_id = p_data$count_point_id,
      period = p,
      highway = as.character(d_data$highway[row_in_d]),
      pred_flow = as.numeric(pred_flow_P[row_in_d]),
      actual_flow = as.numeric(actual_flow_P),
      pred_truck_pct = as.numeric(pred_truck_P[row_in_d]),
      actual_truck_pct = as.numeric(actual_truck_P),
      osm_speed = as.numeric(osm_speed_raw[row_in_d]),
      stringsAsFactors = FALSE
    )
    emission_P <- emission_P[!is.na(emission_P$actual_flow), ]
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

  # ========================================================================
  # CNOSSOS with osm_speed for BOTH sides
  # ========================================================================
  emission_test$pred_db <- compute_emission_cnossos(
    flow = emission_test$pred_flow,
    truck_pct = emission_test$pred_truck_pct,
    speed = emission_test$osm_speed)  # ← OSM speed, not predicted speed

  emission_test$actual_db <- compute_emission_cnossos(
    flow = emission_test$actual_flow,
    truck_pct = emission_test$actual_truck_pct,
    speed = emission_test$osm_speed)  # ← OSM speed, not actual speed

  emission_test$db_error <- emission_test$pred_db - emission_test$actual_db

  valid <- is.finite(emission_test$db_error)
  emission_test <- emission_test[valid, ]

  # Per-variable contributions (with osm_speed as common speed)
  emission_test$contrib_flow <- compute_emission_cnossos(
    emission_test$pred_flow, emission_test$actual_truck_pct,
    emission_test$osm_speed) - emission_test$actual_db
  emission_test$contrib_truck <- compute_emission_cnossos(
    emission_test$actual_flow, emission_test$pred_truck_pct,
    emission_test$osm_speed) - emission_test$actual_db

  pipeline_message(text = sprintf("[%s] Done: %s valid samples",
                                  label, fmt(nrow(emission_test))),
                   level = 1, progress = "end", process = "valid")

  emission_test
}

# ==============================================================================
# 5. Run for all 3 model sets
# ==============================================================================

emission_std <- run_emission_osm_speed(std_models, "STANDARD")
gc(verbose = FALSE)
emission_db <- run_emission_osm_speed(db_models, "dB-AWARE")
gc(verbose = FALSE)
emission_fa <- run_emission_osm_speed(fa_models, "FLOW-AUG", is_flow_augmented = TRUE)
gc(verbose = FALSE)

# ==============================================================================
# 6. Compute and print metrics
# ==============================================================================

pipeline_message(text = "Computing comparison metrics (osm_speed only)",
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
    mae_flow = mean(abs(df$contrib_flow), na.rm = TRUE),
    mae_truck = mean(abs(df$contrib_truck), na.rm = TRUE)
  )
}

s_std <- compute_stats(emission_std)
s_db <- compute_stats(emission_db)
s_fa <- compute_stats(emission_fa)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════════╗\n")
cat("║  OSM-SPEED-ONLY: Standard vs dB-aware vs Flow-augmented (speed = osm_speed)     ║\n")
cat("║  Speed is IDENTICAL for pred and actual → speed error contribution = 0           ║\n")
cat("║  Only flow + truck_pct prediction errors contribute to dB error                  ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %12s  %12s  %12s  ║\n", "Metric", "STANDARD", "dB-AWARE", "FLOW-AUG"))
cat("╠═══════════════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %12d  %12d  %12d  ║\n", "N test samples", s_std$n, s_db$n, s_fa$n))
cat("╠═══════════════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %+11.2f  %+12.2f  %+12.2f  ║\n", "Bias (dB)", s_std$bias, s_db$bias, s_fa$bias))
cat(sprintf("║  %-32s  %12.2f  %12.2f  %12.2f  ║\n", "MAE (dB)", s_std$mae, s_db$mae, s_fa$mae))
cat(sprintf("║  %-32s  %12.2f  %12.2f  %12.2f  ║\n", "RMSE (dB)", s_std$rmse, s_db$rmse, s_fa$rmse))
cat(sprintf("║  %-32s  %12.2f  %12.2f  %12.2f  ║\n", "MedAE (dB)", s_std$medae, s_db$medae, s_fa$medae))
cat(sprintf("║  %-32s  %12.2f  %12.2f  %12.2f  ║\n", "P90 |Error| (dB)", s_std$p90ae, s_db$p90ae, s_fa$p90ae))
cat("╠═══════════════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %+11.2f  %+12.2f  %+12.2f  ║\n", "Bias — Flow contrib (dB)", s_std$bias_flow, s_db$bias_flow, s_fa$bias_flow))
cat(sprintf("║  %-32s  %+11.2f  %+12.2f  %+12.2f  ║\n", "Bias — Truck% contrib (dB)", s_std$bias_truck, s_db$bias_truck, s_fa$bias_truck))
cat("╠═══════════════════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  %-32s  %12.2f  %12.2f  %12.2f  ║\n", "MAE — Flow contrib (dB)", s_std$mae_flow, s_db$mae_flow, s_fa$mae_flow))
cat(sprintf("║  %-32s  %12.2f  %12.2f  %12.2f  ║\n", "MAE — Truck% contrib (dB)", s_std$mae_truck, s_db$mae_truck, s_fa$mae_truck))
cat("╚═══════════════════════════════════════════════════════════════════════════════════╝\n")

cat("\n")
cat("Note: The 3 model sets differ ONLY in flow + truck_pct predictions here.\n")
cat("      (speed_D and ratio_speed_* models are NOT used in this analysis)\n")
cat("      All 3 should give nearly identical results since flow and truck models\n")
cat("      are trained the same way across standard/dB-aware/flow-augmented.\n\n")

pipeline_message(text = "Comparison metrics computed",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 7. Per-highway breakdown
# ==============================================================================

pipeline_message(text = "Per-highway breakdown (osm_speed)",
                 level = 1, progress = "start", process = "search")

hw_std <- as.data.table(emission_std)[, .(
  n = .N, bias_std = mean(db_error), mae_std = mean(abs(db_error)),
  rmse_std = sqrt(mean(db_error^2))
), by = highway]

hw_db <- as.data.table(emission_db)[, .(
  bias_db = mean(db_error), mae_db = mean(abs(db_error))
), by = highway]

hw_fa <- as.data.table(emission_fa)[, .(
  bias_fa = mean(db_error), mae_fa = mean(abs(db_error))
), by = highway]

hw_comp <- merge(hw_std, hw_db, by = "highway", all.x = TRUE)
hw_comp <- merge(hw_comp, hw_fa, by = "highway", all.x = TRUE)
hw_comp <- hw_comp[order(-n)]
top_hw <- hw_comp[1:min(10, nrow(hw_comp))]

cat("\nPer-highway MAE (osm_speed only, top 10):\n")
cat(sprintf("  %-15s %6s  %8s %8s %8s  %8s %8s %8s\n",
            "Highway", "N", "Bias_std", "Bias_dB", "Bias_FA",
            "MAE_std", "MAE_dB", "MAE_FA"))
for (i in 1:nrow(top_hw)) {
  h <- top_hw[i]
  cat(sprintf("  %-15s %6d  %+7.2f  %+7.2f  %+7.2f   %7.2f  %7.2f  %7.2f\n",
              h$highway, h$n, h$bias_std, h$bias_db, h$bias_fa,
              h$mae_std, h$mae_db, h$mae_fa))
}

pipeline_message(text = "Per-highway breakdown complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 8. OSM speed-binned breakdown (to check if flow error varies with road type)
# ==============================================================================

pipeline_message(text = "OSM speed-binned breakdown",
                 level = 1, progress = "start", process = "search")

speed_breaks <- c(0, 30, 50, 70, 90, 110, 200)
speed_labels <- c("0-30", "30-50", "50-70", "70-90", "90-110", "110+")

emission_std$speed_bin <- cut(emission_std$osm_speed, breaks = speed_breaks,
                               labels = speed_labels, right = FALSE)
emission_fa$speed_bin <- cut(emission_fa$osm_speed, breaks = speed_breaks,
                              labels = speed_labels, right = FALSE)

sb_std <- as.data.table(emission_std)[!is.na(speed_bin), .(
  n = .N, bias = mean(db_error), mae = mean(abs(db_error)),
  rmse = sqrt(mean(db_error^2))
), by = speed_bin]

sb_fa <- as.data.table(emission_fa)[!is.na(speed_bin), .(
  bias_fa = mean(db_error), mae_fa = mean(abs(db_error))
), by = speed_bin]

sb_comp <- merge(sb_std, sb_fa, by = "speed_bin", all.x = TRUE)

cat("\nOSM speed-binned dB error (osm_speed used for CNOSSOS):\n")
cat(sprintf("  %-10s %6s  %8s %8s  %8s %8s\n",
            "OSM speed", "N", "Bias_std", "Bias_FA", "MAE_std", "MAE_FA"))
for (i in 1:nrow(sb_comp)) {
  s <- sb_comp[i]
  cat(sprintf("  %-10s %6d  %+7.2f  %+7.2f   %7.2f  %7.2f\n",
              as.character(s$speed_bin), s$n,
              s$bias, s$bias_fa, s$mae, s$mae_fa))
}

pipeline_message(text = "OSM speed-binned breakdown complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 9. Generate comparison PDF
# ==============================================================================

pipeline_message(text = "Generating OSM-speed-only comparison PDF",
                 level = 1, progress = "start", process = "plot")

pdf_path <- file.path(CONFIG$FIGS_DIR, "10_osm_speed_only_comparison.pdf")
if (!dir.exists(CONFIG$FIGS_DIR)) dir.create(CONFIG$FIGS_DIR, recursive = TRUE)

col_std <- rgb(0.2, 0.4, 0.8, 0.7)
col_db  <- rgb(0.8, 0.3, 0.2, 0.7)
col_fa  <- rgb(0.2, 0.7, 0.3, 0.7)
cols3 <- c(col_std, col_db, col_fa)

grDevices::pdf(file = pdf_path, width = 16, height = 10)
old_par <- par(no.readonly = TRUE)

# --- Page 1: Error distributions + scatter pred vs actual dB ---
par(mfrow = c(2, 3), mar = c(5, 5, 4, 2))

hist(emission_std$db_error, breaks = 80, col = col_std,
     main = "dB Error (osm_speed) — Standard",
     xlab = "Prediction error (dB)", ylab = "Count", xlim = c(-20, 20))
abline(v = s_std$bias, col = "red", lwd = 2, lty = 2); abline(v = 0)
legend("topright", legend = sprintf("Bias=%+.2f\nMAE=%.2f\nRMSE=%.2f",
       s_std$bias, s_std$mae, s_std$rmse), bty = "n", cex = 1.0)

hist(emission_db$db_error, breaks = 80, col = col_db,
     main = "dB Error (osm_speed) — dB-aware",
     xlab = "Prediction error (dB)", ylab = "Count", xlim = c(-20, 20))
abline(v = s_db$bias, col = "red", lwd = 2, lty = 2); abline(v = 0)
legend("topright", legend = sprintf("Bias=%+.2f\nMAE=%.2f\nRMSE=%.2f",
       s_db$bias, s_db$mae, s_db$rmse), bty = "n", cex = 1.0)

hist(emission_fa$db_error, breaks = 80, col = col_fa,
     main = "dB Error (osm_speed) — Flow-augmented",
     xlab = "Prediction error (dB)", ylab = "Count", xlim = c(-20, 20))
abline(v = s_fa$bias, col = "red", lwd = 2, lty = 2); abline(v = 0)
legend("topright", legend = sprintf("Bias=%+.2f\nMAE=%.2f\nRMSE=%.2f",
       s_fa$bias, s_fa$mae, s_fa$rmse), bty = "n", cex = 1.0)

# Pred vs Actual dB (3 panels)
lim <- range(c(emission_std$actual_db, emission_std$pred_db,
               emission_fa$actual_db, emission_fa$pred_db), na.rm = TRUE)

r2_std <- 1 - sum((emission_std$pred_db - emission_std$actual_db)^2) /
              sum((emission_std$actual_db - mean(emission_std$actual_db))^2)
r2_db  <- 1 - sum((emission_db$pred_db - emission_db$actual_db)^2) /
              sum((emission_db$actual_db - mean(emission_db$actual_db))^2)
r2_fa  <- 1 - sum((emission_fa$pred_db - emission_fa$actual_db)^2) /
              sum((emission_fa$actual_db - mean(emission_fa$actual_db))^2)

plot(emission_std$actual_db, emission_std$pred_db,
     pch = ".", col = rgb(0, 0, 1, 0.1), cex = 2,
     main = sprintf("Pred vs Actual Lw (osm_speed) — Standard\nR²=%.3f", r2_std),
     xlab = "Actual Lw (dB)", ylab = "Predicted Lw (dB)",
     xlim = lim, ylim = lim)
abline(0, 1, col = "red", lwd = 2)

plot(emission_db$actual_db, emission_db$pred_db,
     pch = ".", col = rgb(1, 0, 0, 0.1), cex = 2,
     main = sprintf("Pred vs Actual Lw (osm_speed) — dB-aware\nR²=%.3f", r2_db),
     xlab = "Actual Lw (dB)", ylab = "Predicted Lw (dB)",
     xlim = lim, ylim = lim)
abline(0, 1, col = "red", lwd = 2)

plot(emission_fa$actual_db, emission_fa$pred_db,
     pch = ".", col = rgb(0, 0.5, 0, 0.1), cex = 2,
     main = sprintf("Pred vs Actual Lw (osm_speed) — Flow-aug\nR²=%.3f", r2_fa),
     xlab = "Actual Lw (dB)", ylab = "Predicted Lw (dB)",
     xlim = lim, ylim = lim)
abline(0, 1, col = "red", lwd = 2)

# --- Page 2: Bias & MAE decomposition + highway + speed bins ---
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

# Bias decomposition (flow + truck only, no speed)
bias_data <- matrix(c(s_std$bias_flow, s_std$bias_truck,
                       s_db$bias_flow, s_db$bias_truck,
                       s_fa$bias_flow, s_fa$bias_truck),
                     nrow = 3, byrow = TRUE)
colnames(bias_data) <- c("Flow", "Truck%")
barplot(bias_data, beside = TRUE, col = cols3,
        main = "Bias Decomposition (osm_speed, no speed error)",
        ylab = "Bias (dB)",
        ylim = c(min(bias_data) * 1.3, max(bias_data) * 1.3))
abline(h = 0, col = "gray50")
legend("topleft", legend = c("Standard", "dB-aware", "Flow-aug"),
       fill = cols3, cex = 0.9)

# MAE decomposition
mae_data <- matrix(c(s_std$mae_flow, s_std$mae_truck,
                      s_db$mae_flow, s_db$mae_truck,
                      s_fa$mae_flow, s_fa$mae_truck),
                    nrow = 3, byrow = TRUE)
colnames(mae_data) <- c("Flow", "Truck%")
barplot(mae_data, beside = TRUE, col = cols3,
        main = "MAE Decomposition (osm_speed, no speed error)",
        ylab = "MAE (dB)")
legend("topleft", legend = c("Standard", "dB-aware", "Flow-aug"),
       fill = cols3, cex = 0.9)

# Per-highway MAE
if (nrow(top_hw) > 0) {
  hw_mat <- matrix(c(top_hw$mae_std, top_hw$mae_db, top_hw$mae_fa),
                    nrow = 3, byrow = TRUE)
  colnames(hw_mat) <- paste0(top_hw$highway, "\n(n=", top_hw$n, ")")
  barplot(hw_mat, beside = TRUE, col = cols3,
          main = "MAE by Highway (osm_speed)",
          ylab = "MAE (dB)", las = 2, cex.names = 0.7)
  legend("topright", legend = c("Standard", "dB-aware", "Flow-aug"),
         fill = cols3, cex = 0.9)
}

# OSM speed-binned MAE
if (nrow(sb_comp) > 0) {
  sb_mat <- matrix(c(sb_comp$mae, sb_comp$mae_fa),
                    nrow = 2, byrow = TRUE)
  colnames(sb_mat) <- as.character(sb_comp$speed_bin)
  barplot(sb_mat, beside = TRUE, col = c(col_std, col_fa),
          main = "MAE by OSM Speed Bin (osm_speed)",
          xlab = "OSM Speed (km/h)", ylab = "MAE (dB)")
  legend("topleft", legend = c("Standard", "Flow-aug"),
         fill = c(col_std, col_fa), cex = 0.9)
}

# --- Page 3: Summary ---
par(mfrow = c(1, 1), mar = c(2, 2, 4, 2))
plot.new()
title(main = "Summary: OSM-Speed-Only Analysis\n(speed error neutralized, flow + truck_pct only)",
      cex.main = 1.3)

summary_text <- sprintf(
  "Emission dB Metrics — osm_speed for all CNOSSOS calculations\n(speed contribution = 0)\n\n
%-25s  %12s  %12s  %12s
%-25s  %+11.2f  %+12.2f  %+12.2f
%-25s  %12.2f  %12.2f  %12.2f
%-25s  %12.2f  %12.2f  %12.2f
%-25s  %12.2f  %12.2f  %12.2f

Bias decomposition (flow + truck only):
%-25s  %+11.2f  %+12.2f  %+12.2f
%-25s  %+11.2f  %+12.2f  %+12.2f

MAE decomposition:
%-25s  %12.2f  %12.2f  %12.2f
%-25s  %12.2f  %12.2f  %12.2f

Note: The 3 model sets use the same flow + truck models.
Only speed models differ → results should be nearly identical.",
  "Metric", "STANDARD", "dB-AWARE", "FLOW-AUG",
  "Bias (dB)", s_std$bias, s_db$bias, s_fa$bias,
  "MAE (dB)", s_std$mae, s_db$mae, s_fa$mae,
  "RMSE (dB)", s_std$rmse, s_db$rmse, s_fa$rmse,
  "P90 |Error| (dB)", s_std$p90ae, s_db$p90ae, s_fa$p90ae,
  "Flow (dB)", s_std$bias_flow, s_db$bias_flow, s_fa$bias_flow,
  "Truck% (dB)", s_std$bias_truck, s_db$bias_truck, s_fa$bias_truck,
  "Flow (dB)", s_std$mae_flow, s_db$mae_flow, s_fa$mae_flow,
  "Truck% (dB)", s_std$mae_truck, s_db$mae_truck, s_fa$mae_truck)

text(0.5, 0.5, summary_text, family = "mono", cex = 0.7, adj = c(0.5, 0.5))

par(old_par)
dev.off()

pipeline_message(text = sprintf("PDF saved: %s", rel_path(pdf_path)),
                 level = 1, progress = "end", process = "valid")

pipeline_message(text = "OSM-speed-only comparison complete",
                 level = 0, progress = "end", process = "valid")
