# ==============================================================================
# Standalone: Acoustic emission error analysis (dB) on test dataset
# ==============================================================================
# This script runs ONLY the acoustic emission dB analysis from the training
# pipeline, using saved artifacts (models, training data, feature info).
# No model re-training is performed.
#
# Usage: Rscript run_emission_analysis.R
# ==============================================================================

# --- Bootstrap & config ---
setwd("/home/aumond/Documents/github/OpenDataNoiseMap/RoadTrafficEstimation")
source("bootstrap.R")
source("config_pipeline.R")

pipeline_message(text = "Standalone acoustic emission error analysis",
                 level = 0, progress = "start", process = "plot")

# --- Load saved artifacts ---
pipeline_message(text = "Loading saved models and training data",
                 level = 1, progress = "start", process = "load")

models_list <- readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
pipeline_message(text = sprintf("Loaded %d models from %s",
                                length(models_list),
                                rel_path(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)),
                 process = "info")

feature_info <- readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)
road_feature_formula <- feature_info$road_feature_formula
all_periods <- feature_info$all_periods
pipeline_message(text = sprintf("Formula: %s", deparse(road_feature_formula)),
                 process = "info")
pipeline_message(text = sprintf("Periods: %d", length(all_periods)),
                 process = "info")

training_data <- readRDS(CONFIG$TRAINING_RDS_DATA_FILEPATH)
pipeline_message(text = describe_df(training_data), process = "info")

# Ensure ratio_speed_to_osm exists
if (!"ratio_speed_to_osm" %in% names(training_data)) {
  if (all(c("aggregate_speed", "speed") %in% names(training_data))) {
    training_data$ratio_speed_to_osm <- ifelse(
      !is.na(training_data$aggregate_speed) & training_data$aggregate_speed >= 0 &
        !is.na(training_data$speed) & training_data$speed > 0,
      training_data$aggregate_speed / training_data$speed,
      NA_real_
    )
  }
}

pipeline_message(text = "Artifacts loaded successfully",
                 level = 1, progress = "end", process = "valid")

# --- Reconstruct shared sensor split (same logic & seed as training) ---
pipeline_message(text = "Reconstructing sensor train/test split",
                 level = 1, progress = "start", process = "calc")

shared_base_test_sensors <- NULL
if (isTRUE(CONFIG$USE_GROUPED_SENSOR_SPLIT)) {
  d_data_for_split <- training_data %>% filter(period == "D")
  all_d_sensors <- unique(d_data_for_split$count_point_id)
  if (length(all_d_sensors) >= 5) {
    set.seed(42)
    n_train <- max(1, floor(0.8 * length(all_d_sensors)))
    shared_base_train_sensors <- sample(all_d_sensors, size = n_train)
    shared_base_test_sensors <- setdiff(all_d_sensors, shared_base_train_sensors)
    pipeline_message(
      text = sprintf("Shared base-model sensor split: %d train / %d test sensors",
                     length(shared_base_train_sensors),
                     length(shared_base_test_sensors)),
      process = "info")
  }
}

pipeline_message(text = "Sensor split reconstructed",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# Acoustic emission error analysis (dB) on test dataset
# ==============================================================================
# (Extracted from train_xgboost_models.R lines 960-2403)

pipeline_message(text = "Acoustic emission error analysis (dB)",
                 level = 1, progress = "start", process = "plot")

emission_test <- data.frame()

if (all(c("flow_D", "speed_D") %in% names(models_list)) &&
    !is.null(shared_base_test_sensors)) {

  d_data <- training_data %>%
    filter(period == "D",
           count_point_id %in% shared_base_test_sensors,
           !is.na(aggregate_flow), aggregate_flow >= 1)

  if (nrow(d_data) > 0) {
    # Safe sparse matrix: handle single-level categoricals after NA removal
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

    d_matrix <- safe_sparse_model_matrix(
      formula_obj = road_feature_formula,
      data_df = d_data)

    if (nrow(d_matrix) != nrow(d_data)) {
      kept_rows <- as.integer(rownames(d_matrix))
      d_data <- d_data[kept_rows, ]
    }

    # Align feature columns with what the model expects
    model_features <- models_list[["flow_D"]]$model$feature_names
    if (!is.null(model_features)) {
      current_cols <- colnames(d_matrix)
      missing_cols <- setdiff(model_features, current_cols)
      if (length(missing_cols) > 0) {
        pipeline_message(
          text = sprintf("Aligning features: adding %d missing columns (%s)",
                         length(missing_cols),
                         paste(head(missing_cols, 5), collapse = ", ")),
          process = "warning")
        zero_mat <- Matrix::Matrix(0, nrow = nrow(d_matrix),
                                   ncol = length(missing_cols),
                                   sparse = TRUE)
        colnames(zero_mat) <- missing_cols
        d_matrix <- cbind(d_matrix, zero_mat)
      }
      extra_cols <- setdiff(colnames(d_matrix), model_features)
      if (length(extra_cols) > 0) {
        pipeline_message(
          text = sprintf("Dropping %d extra columns not in model: %s",
                         length(extra_cols),
                         paste(head(extra_cols, 5), collapse = ", ")),
          process = "info")
      }
      d_matrix <- d_matrix[, model_features, drop = FALSE]
    }

    pipeline_message(
      text = sprintf("Feature matrix: %d rows x %d cols (model expects %d features)",
                     nrow(d_matrix), ncol(d_matrix),
                     length(model_features)),
      process = "info")

    # Helper: align a sparse matrix to a model's expected features
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

    if (nrow(d_matrix) > 0) {
      # --- Base predictions (period D) ---
      d_matrix_flow <- align_matrix_to_model(d_matrix, models_list[["flow_D"]]$model)
      d_matrix_speed <- align_matrix_to_model(d_matrix, models_list[["speed_D"]]$model)

      pred_flow_D_log <- predict(models_list[["flow_D"]]$model, d_matrix_flow)
      pred_flow_D <- 10^pred_flow_D_log
      pred_truck_D <- if (!is.null(models_list[["truck_pct_D"]]$model)) {
        d_matrix_truck <- align_matrix_to_model(d_matrix, models_list[["truck_pct_D"]]$model)
        predict(models_list[["truck_pct_D"]]$model, d_matrix_truck)
      } else {
        pipeline_message(text = "truck_pct_D model missing — using observed truck_pct", process = "info")
        ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0, d_data$truck_pct, 0)
      }
      pred_speed_ratio_to_osm <- predict(models_list[["speed_D"]]$model, d_matrix_speed)

      osm_speed_raw <- suppressWarnings(as.numeric(d_data$speed))
      osm_speed_missing <- is.na(osm_speed_raw) | osm_speed_raw < 5
      speed_base_for_ratio <- osm_speed_raw
      speed_base_for_ratio[osm_speed_missing] <- CONFIG$DEFAULT_VEHICLE_SPEED
      pred_speed_D <- pmax(5, pred_speed_ratio_to_osm * speed_base_for_ratio)

      osm_speed_imputed_flag <- osm_speed_missing
      osm_speed_val <- osm_speed_raw
      osm_speed_val[osm_speed_imputed_flag] <- pred_speed_D[osm_speed_imputed_flag]

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
      test_sensor_ids <- unique(emission_D$count_point_id)
      n_periods_done <- 0

      for (p in non_d_periods) {
        flow_model_key <- paste0("ratio_flow_", p)
        truck_model_key <- paste0("ratio_truck_pct_", p)
        speed_model_key <- paste0("ratio_speed_", p)

        has_flow_ratio <- !is.null(models_list[[flow_model_key]]$model)
        has_truck_ratio <- !is.null(models_list[[truck_model_key]]$model)
        has_speed_ratio <- !is.null(models_list[[speed_model_key]]$model)

        if (!has_flow_ratio) next

        ratio_flow <- predict(models_list[[flow_model_key]]$model,
                              align_matrix_to_model(d_matrix, models_list[[flow_model_key]]$model))
        ratio_truck <- if (has_truck_ratio) {
          predict(models_list[[truck_model_key]]$model,
                  align_matrix_to_model(d_matrix, models_list[[truck_model_key]]$model))
        } else {
          rep(1, nrow(d_matrix))
        }
        ratio_speed <- if (has_speed_ratio) {
          predict(models_list[[speed_model_key]]$model,
                  align_matrix_to_model(d_matrix, models_list[[speed_model_key]]$model))
        } else {
          rep(1, nrow(d_matrix))
        }

        pred_flow_P <- pmax(0, pred_flow_D * ratio_flow)
        pred_truck_P <- pmin(100, pmax(0, pred_truck_D * ratio_truck))
        pred_speed_P <- pmax(5, pred_speed_D * ratio_speed)

        p_data <- training_data %>%
          filter(period == p,
                 count_point_id %in% test_sensor_ids)

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
        text = sprintf("Emission dB eval: %s rows across %d periods (%s test sensors)",
                       fmt(nrow(emission_test)),
                       n_periods_done + 1,
                       fmt(length(test_sensor_ids))),
        process = "info")
      pipeline_message(
        text = sprintf("  Period breakdown: D=%s, E/N=%s, hourly=%s, wd=%s, we=%s",
                       fmt(sum(emission_test$period == "D")),
                       fmt(sum(emission_test$period %in% c("E", "N"))),
                       fmt(sum(grepl("^h[0-9]+$", emission_test$period))),
                       fmt(sum(grepl("_wd$", emission_test$period))),
                       fmt(sum(grepl("_we$", emission_test$period)))),
        process = "info")
    }
  }
}

if (nrow(emission_test) == 0) {
  pipeline_message(
    text = "No emission test data could be built — aborting.",
    process = "stop")
  stop("Emission analysis failed: no test data.")
}

# ==============================================================================
# CNOSSOS emission computation + PDF report
# ==============================================================================
# (This is the same code as in train_xgboost_models.R from the emission_test
#  computation onwards)

# Legacy path may not carry DEGRE information
if (!"DEGRE" %in% names(emission_test)) {
  emission_test$DEGRE <- NA_character_
}

if (nrow(emission_test) > 0) {
  pipeline_message(
    text = sprintf("Emission dB legacy path: %s rows from base_test_predictions merge",
                   fmt(nrow(emission_test))),
    process = "info")
}

if (nrow(emission_test) > 0) {
  pipeline_message(text = sprintf("Computing CNOSSOS-EU emission for %s test samples...",
                   fmt(nrow(emission_test))), process = "calc")
  emission_test$pred_db <- compute_emission_cnossos(
    flow = emission_test$pred_flow,
    truck_pct = emission_test$pred_truck_pct,
    speed = emission_test$pred_speed)
  has_osm_speed <- "osm_speed" %in% names(emission_test)
  if (has_osm_speed) {
    emission_test$pred_db_osm_speed <- compute_emission_cnossos(
      flow = emission_test$pred_flow,
      truck_pct = emission_test$pred_truck_pct,
      speed = emission_test$osm_speed)
  }
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
    text = sprintf("Emission dB (test): N=%s | Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB | MedAE=%.2f dB | P90AE=%.2f dB",
                   fmt(nrow(emission_test)), db_bias, db_mae, db_rmse, db_q50, db_q90),
    process = "info")

  if ("has_measured_speed" %in% names(emission_test)) {
    emission_measured_speed <- emission_test[emission_test$has_measured_speed, , drop = FALSE]
    emission_osm_fallback <- emission_test[!emission_test$has_measured_speed, , drop = FALSE]

    stats_measured <- compute_db_stats(emission_measured_speed)
    stats_osm <- compute_db_stats(emission_osm_fallback)

    if (stats_measured$n > 0) {
      pipeline_message(
        text = sprintf("Emission dB (measured speed): N=%s | Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB",
                       fmt(stats_measured$n), stats_measured$bias, stats_measured$mae, stats_measured$rmse),
        process = "info")
    }
    if (stats_osm$n > 0) {
      pipeline_message(
        text = sprintf("Emission dB (OSM speed fallback): N=%s | Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB",
                       fmt(stats_osm$n), stats_osm$bias, stats_osm$mae, stats_osm$rmse),
        process = "info")
    }
  }

  # Highway-level summary
  emission_dt <- as.data.table(emission_test)
  db_by_highway <- emission_dt[, .(
    n = .N,
    bias_db = mean(db_error, na.rm = TRUE),
    mae_db = mean(abs_db_error, na.rm = TRUE),
    rmse_db = sqrt(mean(db_error^2, na.rm = TRUE))
  ), by = highway][order(-n)]

  top_hw <- db_by_highway[1:min(8, nrow(db_by_highway))]

  # Dual-speed comparison
  has_osm_speed <- "db_error_osm" %in% names(emission_test)
  if (has_osm_speed) {
    db_bias_osm <- mean(emission_test$db_error_osm, na.rm = TRUE)
    db_mae_osm <- mean(abs(emission_test$db_error_osm), na.rm = TRUE)
    db_rmse_osm <- sqrt(mean(emission_test$db_error_osm^2, na.rm = TRUE))

    pipeline_message(
      text = sprintf("Emission dB with XGBoost speed: Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB",
                     db_bias, db_mae, db_rmse),
      process = "info")
    pipeline_message(
      text = sprintf("Emission dB with OSM maxspeed:   Bias=%+.2f dB | MAE=%.2f dB | RMSE=%.2f dB",
                     db_bias_osm, db_mae_osm, db_rmse_osm),
      process = "info")
    pipeline_message(
      text = sprintf("Speed choice impact: \u0394Bias=%.2f dB | \u0394MAE=%.2f dB (OSM - XGBoost)",
                     db_bias_osm - db_bias, db_mae_osm - db_mae),
      process = "info")
  }

  # Save report PDF
  if (!dir.exists(CONFIG$FIGS_DIR)) {
    dir.create(CONFIG$FIGS_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  emission_pdf_path <- file.path(CONFIG$FIGS_DIR, "06_emission_dB_error_test.pdf")

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
    text = sprintf("Bias decomposition: Flow=%+.2f dB | Truck%%=%+.2f dB (N=%s) | Speed=%+.2f dB",
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
        subset = label, n = 0L, bias = NA_real_, mae = NA_real_, rmse = NA_real_,
        q50 = NA_real_, q90 = NA_real_, within_1db = NA_real_, within_2db = NA_real_,
        stringsAsFactors = FALSE))
    }
    err <- emission_test$db_error[idx]
    abse <- emission_test$abs_db_error[idx]
    data.frame(
      subset = label, n = sum(idx),
      bias = mean(err, na.rm = TRUE), mae = mean(abse, na.rm = TRUE),
      rmse = sqrt(mean(err^2, na.rm = TRUE)),
      q50 = median(abse, na.rm = TRUE),
      q90 = as.numeric(quantile(abse, probs = 0.90, na.rm = TRUE)),
      within_1db = mean(abse <= 1, na.rm = TRUE),
      within_2db = mean(abse <= 2, na.rm = TRUE),
      stringsAsFactors = FALSE)
  }

  identify_dominant_source <- function(df) {
    if (nrow(df) == 0) return("N/A")
    fl <- db_flow_only[as.integer(rownames(df))] - db_actual_all[as.integer(rownames(df))]
    sp <- db_speed_only[as.integer(rownames(df))] - db_actual_all[as.integer(rownames(df))]
    mae_f <- mean(abs(fl), na.rm = TRUE)
    mae_s <- mean(abs(sp), na.rm = TRUE)
    if (mae_f > mae_s) return("Flux") else return("Vitesse")
  }

  emission_test$.row_idx <- seq_len(nrow(emission_test))

  # ============================================================================
  # PAGE 1: Qualite globale de la prediction en dB sur les troncons tests
  # ============================================================================
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))

  # 1a: Histogramme des erreurs
  hist(emission_test$db_error, breaks = 50,
       main = "Distribution de l'erreur d'emission (dB)",
       xlab = "Erreur = Predit - Observe (dB)",
       col = "steelblue", border = "white", cex.main = 1.1)
  abline(v = 0, col = "red", lwd = 2, lty = 1)
  abline(v = db_bias, col = "orange", lwd = 2, lty = 2)
  legend("topright",
         legend = c("Zero", sprintf("Biais moyen = %+.2f dB", db_bias)),
         col = c("red", "orange"), lwd = 2, lty = c(1, 2), cex = 0.8, bty = "n")

  # 1b: Predit vs Observe
  plot(emission_test$actual_db, emission_test$pred_db,
       pch = 16, cex = 0.35, col = rgb(0.2, 0.4, 0.8, 0.25),
       xlab = "Emission observee CNOSSOS-EU (dB(A))",
       ylab = "Emission predite CNOSSOS-EU (dB(A))",
       main = "Predit vs Observe (troncons tests)", cex.main = 1.1)
  abline(a = 0, b = 1, col = "red", lwd = 2)
  r2 <- cor(emission_test$actual_db, emission_test$pred_db, use = "complete.obs")^2
  legend("topleft", legend = sprintf("R2 = %.3f", r2), cex = 0.9, bty = "n")

  # 1c: CDF de l'erreur absolue
  abs_sorted <- sort(emission_test$abs_db_error)
  ecdf_vals <- seq_along(abs_sorted) / length(abs_sorted)
  plot(abs_sorted, ecdf_vals, type = "l", lwd = 2, col = "steelblue",
       xlab = "Erreur absolue (dB)", ylab = "Proportion cumulee",
       main = "Fonction de repartition de l'erreur absolue", cex.main = 1.1)
  abline(h = 0.5, col = "grey60", lty = 3)
  abline(h = 0.9, col = "grey60", lty = 3)
  abline(v = db_q50, col = "orange", lty = 2)
  abline(v = db_q90, col = "red", lty = 2)
  legend("bottomright", legend = c(
    sprintf("Mediane = %.1f dB", db_q50),
    sprintf("P90 = %.1f dB", db_q90)),
    col = c("orange", "red"), lwd = 2, lty = 2, cex = 0.8, bty = "n")

  # 1d: Tableau synthese
  plot.new()
  title(main = "Metriques globales sur troncons tests", cex.main = 1.1)
  summary_txt <- c(
    sprintf("Nombre de troncons-periodes evalues : %s", fmt(nrow(emission_test))),
    "",
    sprintf("Biais moyen           : %+.2f dB", db_bias),
    sprintf("Erreur absolue moyenne (MAE)  : %.2f dB", db_mae),
    sprintf("RMSE                  : %.2f dB", db_rmse),
    sprintf("Erreur absolue mediane : %.2f dB", db_q50),
    sprintf("Erreur absolue P90     : %.2f dB", db_q90),
    "",
    "Interpretation :",
    if (db_bias > 0.5) sprintf("  -> Surestimation de %+.1f dB en moyenne", db_bias)
    else if (db_bias < -0.5) sprintf("  -> Sous-estimation de %+.1f dB en moyenne", db_bias)
    else "  -> Biais quasi nul (< 0.5 dB)"
  )
  text(0.02, seq(0.95, 0.95 - 0.075 * (length(summary_txt) - 1),
       length.out = length(summary_txt)),
       labels = summary_txt, adj = c(0, 0.5), cex = 0.90, family = "mono")

  mtext("Evaluation emission CNOSSOS-EU sur troncons tests",
        side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)

  # ============================================================================
  # PAGE 1bis: Transparence des donnees et qualite par sous-echantillon
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main = "Transparence des donnees test et robustesse des resultats", cex.main = 1.1)

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
      compute_subset_metrics(emission_test$has_measured_speed, "Vitesse mesuree"),
      compute_subset_metrics(!emission_test$has_measured_speed, "Vitesse fallback (OSM/imputee)")
    )
  }
  if (has_truck_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(emission_test$has_measured_truck, "%PL mesure"),
      compute_subset_metrics(!emission_test$has_measured_truck, "%PL fallback")
    )
  }
  if (has_speed_col && has_truck_col) {
    subset_table <- rbind(
      subset_table,
      compute_subset_metrics(emission_test$has_measured_speed & emission_test$has_measured_truck,
                             "Vitesse + %PL mesures"),
      compute_subset_metrics(!emission_test$has_measured_speed & !emission_test$has_measured_truck,
                             "Vitesse + %PL fallback")
    )
  }

  intro_lines <- c(
    sprintf("Echantillon total evalue : %s troncons-periodes", fmt(n_all)),
    "",
    sprintf("Couverture vitesse mesuree  : %s (%s)",
            fmt(n_speed_measured), if (has_speed_col) fmt_pct(n_speed_measured / n_all) else "NA"),
    sprintf("Couverture vitesse fallback : %s (%s)",
            if (is.na(n_speed_fallback)) "NA" else fmt(n_speed_fallback),
            if (has_speed_col) fmt_pct(n_speed_fallback / n_all) else "NA"),
    sprintf("Couverture %%PL mesure       : %s (%s)",
            fmt(n_truck_measured), if (has_truck_col) fmt_pct(n_truck_measured / n_all) else "NA"),
    sprintf("Couverture %%PL fallback     : %s (%s)",
            if (is.na(n_truck_fallback)) "NA" else fmt(n_truck_fallback),
            if (has_truck_col) fmt_pct(n_truck_fallback / n_all) else "NA"),
    "",
    "Qualite par sous-echantillon (dB) :",
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
    "Note: l'objectif du rapport est uniquement la qualite de prediction des emissions dB",
    "sur troncons tests (pas l'erreur sur trafic brut)."
  )

  text(0.02,
       seq(0.96, 0.96 - 0.029 * (length(final_lines) - 1), length.out = length(final_lines)),
       labels = final_lines, adj = c(0, 0.5), cex = 0.70, family = "mono")

  # ============================================================================
  # PAGE 2: Decomposition par source (flux, truck%, vitesse)
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
  boxplot(list(Flux = contrib_flow, `% PL (mesure)` = truck_box, Vitesse = contrib_speed),
          col = bar_cols,
          main = "Distribution de l'erreur dB par variable",
          ylab = "Erreur dB (predit - observe)", outline = FALSE, cex.main = 1.1)
  abline(h = 0, col = "red", lty = 2)
  mtext("Chaque boite montre la variabilite de l'erreur due a une seule variable",
        side = 1, line = 2.5, cex = 0.7, font = 3)

  # 2d: Interpretation textuelle
  plot.new()
  title(main = "Diagnostic : quelle variable cause l'erreur ?", cex.main = 1.1)

  ord <- order(mae_contrib, decreasing = TRUE)
  var_names_fr <- c(Flow = "le flux", `Truck %` = "le % poids lourds", Speed = "la vitesse")
  direction_fr <- c(Flow = if (mean_contrib["Flow"] > 0) "surestime" else "sous-estime",
                    `Truck %` = if (!is.na(mean_contrib["Truck %"]) && mean_contrib["Truck %"] > 0) "surestime" else "sous-estime",
                    Speed = if (mean_contrib["Speed"] > 0) "surestime -> niveau trop fort" else "sous-estime -> niveau trop faible")

  diag_lines <- c(
    "Classement des sources d'erreur (de la plus impactante a la moins) :",
    ""
  )
  for (i in seq_along(ord)) {
    vn <- names(mae_contrib)[ord[i]]
    diag_lines <- c(diag_lines, sprintf(
      "  %d. %s : biais = %+.2f dB, MAE = %.2f dB (%s)",
      i, var_names_fr[vn], mean_contrib[vn], mae_contrib[vn], direction_fr[vn]))
  }
  diag_lines <- c(diag_lines, "",
    sprintf("  Truck %% evalue sur %s capteurs avec mesure effective.", fmt(length(contrib_truck_eval))),
    "",
    "Conclusion :")
  dominant_var <- names(mae_contrib)[ord[1]]
  if (dominant_var == "Speed") {
    diag_lines <- c(diag_lines,
      "  La vitesse est la source principale d'erreur en dB.",
      "  -> Voir page 6 pour l'impact de la vitesse OSM vs predite.")
  } else if (dominant_var == "Flow") {
    diag_lines <- c(diag_lines,
      "  Le flux est la source principale d'erreur en dB.",
      "  -> L'amelioration du modele de flux reduirait le plus l'erreur.")
  } else {
    diag_lines <- c(diag_lines,
      "  Le % poids lourds est la source principale d'erreur en dB.",
      "  -> Mais les donnees de reference sont limitees.")
  }
  text(0.02, seq(0.95, 0.95 - 0.055 * (length(diag_lines) - 1),
       length.out = length(diag_lines)),
       labels = diag_lines, adj = c(0, 0.5), cex = 0.82, family = "mono")

  # ============================================================================
  # PAGE 3: Quelles periodes sont les meilleures / pires ?
  # ============================================================================
  if ("period" %in% names(emission_test)) {
    den_df <- emission_test[emission_test$period %in% c("D", "E", "N"), , drop = FALSE]
    hour_df <- emission_test[grepl("^h([0-9]|1[0-9]|2[0-3])$", emission_test$period), , drop = FALSE]

    den_ok <- nrow(den_df) > 0
    hour_ok <- nrow(hour_df) > 0

    if (den_ok || hour_ok) {
      par(mfrow = c(2, 2), mar = c(5, 4.5, 3.5, 1))

      if (den_ok) {
        den_stats <- summarize_group_metrics(den_df, "period")
        den_order <- match(c("D", "E", "N"), den_stats$group)
        den_order <- den_order[!is.na(den_order)]
        den_stats <- den_stats[den_order, ]
        den_lbl <- sprintf("%s\n(n=%s)", den_stats$group, fmt(den_stats$n))
        den_cols <- c(D = "#FFD700", E = "#FF8C00", N = "#1a1a6e")
        bp_den <- barplot(den_stats$bias_xgb, names.arg = den_lbl,
                          col = den_cols[den_stats$group], border = NA,
                          main = "Biais dB par periode D / E / N",
                          ylab = "Biais (dB)", cex.main = 1.1)
        abline(h = 0, col = "red", lty = 2)
        text(bp_den, den_stats$bias_xgb,
             labels = sprintf("%+.2f", den_stats$bias_xgb),
             pos = ifelse(den_stats$bias_xgb >= 0, 3, 1), cex = 0.9, font = 2)
      } else {
        plot.new(); title(main = "D / E / N : donnees insuffisantes")
      }

      if (den_ok) {
        bp_den2 <- barplot(den_stats$mae_xgb, names.arg = den_lbl,
                           col = den_cols[den_stats$group], border = NA,
                           main = "MAE dB par periode D / E / N",
                           ylab = "MAE (dB)", cex.main = 1.1)
        text(bp_den2, den_stats$mae_xgb,
             labels = sprintf("%.2f", den_stats$mae_xgb),
             pos = 3, cex = 0.9, font = 2)
      } else {
        plot.new(); title(main = "D / E / N : donnees insuffisantes")
      }

      if (hour_ok) {
        hour_stats <- summarize_group_metrics(hour_df, "period")
        hour_stats$hour_num <- as.integer(sub("^h", "", hour_stats$group))
        hour_stats <- hour_stats[order(hour_stats$hour_num), ]
        labels_h <- paste0("h", sprintf("%02d", hour_stats$hour_num))

        h_cols <- ifelse(hour_stats$hour_num >= 6 & hour_stats$hour_num < 18, "#FFD700",
                  ifelse(hour_stats$hour_num >= 18 & hour_stats$hour_num < 22, "#FF8C00", "#1a1a6e"))

        plot(hour_stats$hour_num, hour_stats$mae_xgb, type = "b", pch = 16,
             col = h_cols, lwd = 2,
             xlab = "Heure", ylab = "MAE (dB)",
             main = "MAE par heure (h0-h23)", cex.main = 1.1,
             xaxt = "n")
        axis(1, at = hour_stats$hour_num, labels = labels_h, cex.axis = 0.7, las = 2)
        rect(5.5, par("usr")[3], 17.5, par("usr")[4], col = rgb(1, 0.84, 0, 0.08), border = NA)
        rect(17.5, par("usr")[3], 21.5, par("usr")[4], col = rgb(1, 0.55, 0, 0.08), border = NA)
        points(hour_stats$hour_num, hour_stats$mae_xgb, pch = 16, col = h_cols, cex = 1.2)
        lines(hour_stats$hour_num, hour_stats$mae_xgb, col = "grey40")
        legend("topright", legend = c("Jour (D)", "Soiree (E)", "Nuit (N)"),
               fill = c("#FFD700", "#FF8C00", "#1a1a6e"), cex = 0.7, bty = "n")

        best_h <- hour_stats$group[which.min(hour_stats$mae_xgb)]
        worst_h <- hour_stats$group[which.max(hour_stats$mae_xgb)]
      } else {
        plot.new(); title(main = "Donnees horaires insuffisantes")
        best_h <- "N/A"; worst_h <- "N/A"
      }

      if (hour_ok) {
        plot(hour_stats$hour_num, hour_stats$bias_xgb, type = "b", pch = 16,
             col = h_cols, lwd = 2,
             xlab = "Heure", ylab = "Biais (dB)",
             main = "Biais par heure (h0-h23)", cex.main = 1.1,
             xaxt = "n")
        axis(1, at = hour_stats$hour_num, labels = labels_h, cex.axis = 0.7, las = 2)
        abline(h = 0, col = "red", lty = 2, lwd = 1.5)
        rect(5.5, par("usr")[3], 17.5, par("usr")[4], col = rgb(1, 0.84, 0, 0.08), border = NA)
        rect(17.5, par("usr")[3], 21.5, par("usr")[4], col = rgb(1, 0.55, 0, 0.08), border = NA)
        points(hour_stats$hour_num, hour_stats$bias_xgb, pch = 16, col = h_cols, cex = 1.2)
        lines(hour_stats$hour_num, hour_stats$bias_xgb, col = "grey40")
      } else {
        plot.new(); title(main = "Donnees horaires insuffisantes")
      }

      mtext("Qualite de la prediction dB par periode temporelle",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
    }

    # --- Page 3bis: Diagnostic textuel des periodes ---
    if (den_ok || hour_ok) {
      par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
      plot.new()
      title(main = "Diagnostic par periode : pourquoi certaines sont meilleures ?",
            cex.main = 1.1)

      period_diag <- c("Analyse des periodes temporelles :", "")

      if (den_ok) {
        best_den <- den_stats$group[which.min(den_stats$mae_xgb)]
        worst_den <- den_stats$group[which.max(den_stats$mae_xgb)]
        period_diag <- c(period_diag,
          "  Periodes reglementaires (D / E / N) :",
          sprintf("    Meilleure : %s (MAE = %.2f dB, Biais = %+.2f dB)",
                  best_den,
                  den_stats$mae_xgb[den_stats$group == best_den],
                  den_stats$bias_xgb[den_stats$group == best_den]),
          sprintf("    Pire      : %s (MAE = %.2f dB, Biais = %+.2f dB)",
                  worst_den,
                  den_stats$mae_xgb[den_stats$group == worst_den],
                  den_stats$bias_xgb[den_stats$group == worst_den]),
          "")

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
          "  Periodes horaires (h0-h23) :",
          sprintf("    Meilleure heure : %s (MAE = %.2f dB)",
                  best_h, min(hour_stats$mae_xgb)),
          sprintf("    Pire heure      : %s (MAE = %.2f dB)",
                  worst_h, max(hour_stats$mae_xgb)),
          "",
          "  Explication :",
          "    Les heures nocturnes (h22-h05) ont souvent un biais plus eleve car",
          "    les flux faibles sont relatifs et une petite erreur absolue sur le",
          "    flux produit une grande erreur relative en dB (effet log).",
          "    En journee (h07-h18), les flux sont stables et mieux predits."
        )
      }

      text(0.02, seq(0.95, 0.95 - 0.045 * (length(period_diag) - 1),
           length.out = length(period_diag)),
           labels = period_diag, adj = c(0, 0.5), cex = 0.78, family = "mono")
    }

    # --- Page 3ter: Semaine vs Weekend ---
    hour_wd_df <- emission_test[grepl("^h[0-9]+_wd$", emission_test$period), , drop = FALSE]
    hour_we_df <- emission_test[grepl("^h[0-9]+_we$", emission_test$period), , drop = FALSE]
    wd_ok <- nrow(hour_wd_df) > 0
    we_ok <- nrow(hour_we_df) > 0

    if (wd_ok && we_ok) {
      par(mfrow = c(2, 2), mar = c(5, 4.5, 3.5, 1))

      wd_stats <- summarize_group_metrics(hour_wd_df, "period")
      wd_stats$hour_num <- as.integer(sub("^h([0-9]+)_wd$", "\\1", wd_stats$group))
      wd_stats <- wd_stats[order(wd_stats$hour_num), ]

      we_stats <- summarize_group_metrics(hour_we_df, "period")
      we_stats$hour_num <- as.integer(sub("^h([0-9]+)_we$", "\\1", we_stats$group))
      we_stats <- we_stats[order(we_stats$hour_num), ]

      # MAE semaine vs weekend
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

      # Biais semaine vs weekend
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

      # Effectif par heure
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

      # Diagnostic textuel
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
          sprintf("  Conclusion : meilleure performance en semaine (dMAE = %.2f dB).",
                  we_global_mae - wd_global_mae)
        } else {
          sprintf("  Conclusion : meilleure performance le weekend (dMAE = %.2f dB).",
                  wd_global_mae - we_global_mae)
        }
      )
      text(0.02, seq(0.90, 0.90 - 0.06 * (length(wd_we_diag) - 1),
           length.out = length(wd_we_diag)),
           labels = wd_we_diag, adj = c(0, 0.5), cex = 0.82, family = "mono")

      mtext("Comparaison Semaine / Weekend - Qualite des predictions horaires",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
    }
  }

  # ============================================================================
  # PAGE 4: Analyse par highway
  # ============================================================================
  if ("highway" %in% names(emission_test)) {
    hw_stats <- summarize_group_metrics(emission_test, "highway")
    hw_stats <- hw_stats[order(-hw_stats$n), ]
    min_n_hw <- 20
    hw_stats <- hw_stats[hw_stats$n >= min_n_hw, , drop = FALSE]

    if (nrow(hw_stats) > 0) {
      par(mfrow = c(2, 2), mar = c(6, 4.5, 3.5, 1))

      top_n <- head(hw_stats, 12)
      bp_hw <- barplot(top_n$mae_xgb,
                       names.arg = sprintf("%s\n(n=%s)", top_n$group, fmt(top_n$n)),
                       las = 2, col = "steelblue", border = NA,
                       main = "MAE par type de route (highway)",
                       ylab = "MAE (dB)", cex.main = 1.1)
      text(bp_hw, top_n$mae_xgb, labels = sprintf("%.1f", top_n$mae_xgb),
           pos = 3, cex = 0.75, font = 2)

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

      hw_source <- sapply(top_n$group, function(hw) {
        sub_idx <- which(emission_test$highway == hw)
        if (length(sub_idx) < 5) return(NA_real_)
        mae_f <- mean(abs(contrib_flow[sub_idx]), na.rm = TRUE)
        mae_s <- mean(abs(contrib_speed[sub_idx]), na.rm = TRUE)
        return(mae_f - mae_s)
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

      if (has_osm_speed && all(is.finite(top_n$mae_osm))) {
        delta_mae <- top_n$mae_osm - top_n$mae_xgb
        delta_cols <- ifelse(delta_mae > 0, "#D6604D", "#2ca02c")
        bp_hw4 <- barplot(delta_mae,
                          names.arg = sprintf("%s", top_n$group),
                          las = 2, col = delta_cols, border = NA,
                          main = "dMAE = MAE(OSM) - MAE(XGBoost)",
                          ylab = "dMAE (dB)", cex.main = 1.0)
        abline(h = 0, col = "grey40")
        legend("topright", legend = c("OSM pire (>0)", "OSM meilleur (<0)"),
               fill = c("#D6604D", "#2ca02c"), cex = 0.7, bty = "n")
      } else {
        plot.new(); title(main = "Vitesse OSM non disponible")
      }

      mtext("Analyse de la qualite dB par type de route (highway)",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)

      # --- Page 4bis: Diagnostic textuel highway ---
      par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
      plot.new()
      title(main = "Diagnostic par highway : ou et pourquoi ?", cex.main = 1.1)

      best_hw <- top_n$group[which.min(top_n$mae_xgb)]
      worst_hw <- top_n$group[which.max(top_n$mae_xgb)]
      hw_diag <- c(
        "Analyse par type de route :", "",
        sprintf("  Meilleure prediction : '%s' (MAE = %.2f dB, Biais = %+.2f dB, n = %s)",
                best_hw,
                top_n$mae_xgb[top_n$group == best_hw],
                top_n$bias_xgb[top_n$group == best_hw],
                fmt(top_n$n[top_n$group == best_hw])),
        sprintf("  Pire prediction      : '%s' (MAE = %.2f dB, Biais = %+.2f dB, n = %s)",
                worst_hw,
                top_n$mae_xgb[top_n$group == worst_hw],
                top_n$bias_xgb[top_n$group == worst_hw],
                fmt(top_n$n[top_n$group == worst_hw])),
        ""
      )

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
        "  le flux y est plus variable. Les routes residentielles ont des flux",
        "  faibles avec une erreur absolue moindre mais relative plus grande.")

      text(0.02, seq(0.95, 0.95 - 0.04 * (length(hw_diag) - 1),
           length.out = length(hw_diag)),
           labels = hw_diag, adj = c(0, 0.5), cex = 0.72, family = "mono")
    }
  }

  # ============================================================================
  # PAGE 5: Analyse par DEGRE (densite urbaine)
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

      bp_d <- barplot(deg_stats$mae_xgb, names.arg = deg_labels, las = 2,
                      col = deg_cols, border = NA,
                      main = "MAE par DEGRE (densite urbaine)",
                      ylab = "MAE (dB)", cex.main = 1.1)
      text(bp_d, deg_stats$mae_xgb, labels = sprintf("%.2f", deg_stats$mae_xgb),
           pos = 3, cex = 0.8, font = 2)

      bias_cols_d <- ifelse(deg_stats$bias_xgb >= 0, "#D6604D", "#4393C3")
      bp_d2 <- barplot(deg_stats$bias_xgb, names.arg = deg_labels, las = 2,
                       col = bias_cols_d, border = NA,
                       main = "Biais par DEGRE",
                       ylab = "Biais (dB)", cex.main = 1.1)
      abline(h = 0, col = "red", lty = 2)
      text(bp_d2, deg_stats$bias_xgb,
           labels = sprintf("%+.2f", deg_stats$bias_xgb),
           pos = ifelse(deg_stats$bias_xgb >= 0, 3, 1), cex = 0.8, font = 2)

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

      if (has_osm_speed && all(is.finite(deg_stats$mae_osm))) {
        x <- seq_len(nrow(deg_stats))
        y_lim <- range(c(deg_stats$mae_xgb, deg_stats$mae_osm), na.rm = TRUE)
        plot(x, deg_stats$mae_xgb, type = "b", pch = 16, col = "#1f77b4",
             xaxt = "n", ylim = y_lim, lwd = 2,
             xlab = "DEGRE", ylab = "MAE (dB)",
             main = "MAE : vitesse predite vs OSM par DEGRE", cex.main = 1.0)
        lines(x, deg_stats$mae_osm, type = "b", pch = 17, col = "#d62728", lwd = 2)
        axis(1, at = x, labels = paste0("D", deg_stats$group))
        legend("topleft", legend = c("Vitesse predite", "Vitesse OSM"),
               col = c("#1f77b4", "#d62728"), pch = c(16, 17), lwd = 2, bty = "n", cex = 0.8)
      } else {
        plot.new(); title(main = "Vitesse OSM non disponible")
      }

      mtext("Analyse de la qualite dB par densite urbaine (DEGRE INSEE)",
            side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
    }
  }

  # ============================================================================
  # PAGE 6: Vitesse OSM vs predite — impact sur les dB
  # ============================================================================
  if (has_osm_speed) {
    par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1))

    breaks_range <- range(c(emission_test$db_error, emission_test$db_error_osm), na.rm = TRUE)
    breaks_seq <- seq(floor(breaks_range[1]), ceiling(breaks_range[2]), length.out = 60)
    h_xgb <- hist(emission_test$db_error, breaks = breaks_seq, plot = FALSE)
    h_osm <- hist(emission_test$db_error_osm, breaks = breaks_seq, plot = FALSE)
    ylim_max <- max(c(h_xgb$counts, h_osm$counts))
    plot(h_xgb, col = rgb(0.2, 0.4, 0.8, 0.5), border = "white",
         main = "Erreur dB : vitesse predite vs vitesse OSM",
         xlab = "Erreur = Predit - Observe (dB)", ylim = c(0, ylim_max * 1.1),
         cex.main = 1.1)
    plot(h_osm, col = rgb(0.9, 0.4, 0.2, 0.5), border = "white", add = TRUE)
    abline(v = 0, col = "red", lwd = 2)
    legend("topright", legend = c(
      sprintf("Vitesse predite (biais=%+.1f dB)", db_bias),
      sprintf("Vitesse OSM (biais=%+.1f dB)", db_bias_osm)),
      fill = c(rgb(0.2, 0.4, 0.8, 0.5), rgb(0.9, 0.4, 0.2, 0.5)),
      cex = 0.75, bty = "n")

    abs_xgb_sorted <- sort(emission_test$abs_db_error)
    abs_osm_sorted <- sort(emission_test$abs_db_error_osm)
    ecdf_xgb <- seq_along(abs_xgb_sorted) / length(abs_xgb_sorted)
    ecdf_osm <- seq_along(abs_osm_sorted) / length(abs_osm_sorted)
    xlim_max <- max(c(abs_xgb_sorted, abs_osm_sorted), na.rm = TRUE)
    plot(abs_xgb_sorted, ecdf_xgb, type = "l", lwd = 2, col = rgb(0.2, 0.4, 0.8),
         xlim = c(0, min(xlim_max, 20)),
         xlab = "Erreur absolue (dB)", ylab = "Proportion cumulee",
         main = "CDF : vitesse predite vs OSM", cex.main = 1.1)
    lines(abs_osm_sorted, ecdf_osm, lwd = 2, col = rgb(0.9, 0.4, 0.2))
    abline(h = 0.9, col = "grey60", lty = 2)
    legend("bottomright", legend = c(
      sprintf("Predite (MAE=%.1f dB)", db_mae),
      sprintf("OSM (MAE=%.1f dB)", db_mae_osm)),
      lwd = 2, col = c(rgb(0.2, 0.4, 0.8), rgb(0.9, 0.4, 0.2)),
      cex = 0.75, bty = "n")

    plot(emission_test$pred_speed, emission_test$osm_speed,
         pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.15),
         xlab = "Vitesse predite XGBoost (km/h)",
         ylab = "Vitesse reglementaire OSM (km/h)",
         main = "Vitesse predite vs vitesse OSM", cex.main = 1.1)
    abline(a = 0, b = 1, col = "red", lwd = 2)
    speed_cor <- cor(emission_test$pred_speed, emission_test$osm_speed, use = "complete.obs")
    legend("topleft", legend = c(
      sprintf("r = %.2f", speed_cor),
      sprintf("Mediane predite = %.0f km/h", median(emission_test$pred_speed, na.rm = TRUE)),
      sprintf("Mediane OSM = %.0f km/h", median(emission_test$osm_speed, na.rm = TRUE))),
      cex = 0.75, bty = "n")

    # Verdict
    plot.new()
    title(main = "Verdict : vitesse OSM ameliore ou degrade ?", cex.main = 1.1)

    delta_bias <- db_bias_osm - db_bias
    delta_mae <- db_mae_osm - db_mae
    delta_rmse <- db_rmse_osm - db_rmse

    verdict_lines <- c(
      "Comparaison de la qualite dB sur les troncons tests :", "",
      sprintf("                    Vitesse predite    Vitesse OSM     d (OSM-Pred)"),
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
        sprintf("  VERDICT : La vitesse OSM DEGRADE la prediction de %.2f dB (MAE).", delta_mae),
        "  -> La vitesse predite par XGBoost est preferable.")
    } else if (delta_mae < -0.05) {
      verdict_lines <- c(verdict_lines,
        sprintf("  VERDICT : La vitesse OSM AMELIORE la prediction de %.2f dB (MAE).", abs(delta_mae)),
        "  -> La vitesse reglementaire OSM serait un meilleur choix.")
    } else {
      verdict_lines <- c(verdict_lines,
        "  VERDICT : Les deux vitesses donnent des resultats quasi identiques.",
        "  -> Difference < 0.05 dB en MAE, choix indifferent.")
    }

    verdict_lines <- c(verdict_lines, "",
      "  Note : 'observe' = CNOSSOS(flow_obs, truck%_obs, speed_obs).",
      sprintf("  78%% des capteurs AVATAR utilisent maxspeed OSM comme 'speed_obs'."),
      "  Le biais vitesse inclut donc partiellement un biais de reference."
    )

    text(0.02, seq(0.95, 0.95 - 0.055 * (length(verdict_lines) - 1),
         length.out = length(verdict_lines)),
         labels = verdict_lines, adj = c(0, 0.5), cex = 0.78, family = "mono")

    mtext("Comparaison vitesse predite vs vitesse reglementaire OSM",
          side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)
  }

  # ============================================================================
  # PAGE 6bis: Top erreurs
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main = "Top erreurs dB sur troncons tests (cas concrets)", cex.main = 1.1)

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
    "Ces cas permettent d'identifier precisement ou ameliorer le pipeline."
  )

  text(0.02,
       seq(0.96, 0.96 - 0.03 * (length(top_lines) - 1), length.out = length(top_lines)),
       labels = top_lines, adj = c(0, 0.5), cex = 0.66, family = "mono")

  # ============================================================================
  # PAGE 7: Synthese finale
  # ============================================================================
  par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
  plot.new()
  title(main = "Synthese de l'evaluation acoustique sur troncons tests",
        cex.main = 1.2)

  synth_lines <- c(
    sprintf("Donnees : %s troncons-periodes evalues via CNOSSOS-EU 2020", fmt(nrow(emission_test))),
    sprintf("  dont %s avec vitesse mesuree, %s avec %% PL mesure",
            fmt(sum(emission_test$has_measured_speed, na.rm = TRUE)),
            fmt(sum(emission_test$has_measured_truck, na.rm = TRUE))),
    "",
    "=== Qualite globale ===",
    sprintf("  Biais = %+.2f dB | MAE = %.2f dB | RMSE = %.2f dB", db_bias, db_mae, db_rmse),
    ""
  )

  ord <- order(mae_contrib, decreasing = TRUE)
  synth_lines <- c(synth_lines,
    "=== Source principale d'erreur ===",
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

  if (has_osm_speed) {
    delta_mae_v <- db_mae_osm - db_mae
    if (delta_mae_v > 0.05) {
      synth_lines <- c(synth_lines,
        "=== Vitesse reglementaire vs predite ===",
        sprintf("  Utiliser la vitesse OSM DEGRADERAIT la MAE de %.2f dB.", delta_mae_v),
        "  -> Conserver la vitesse predite par XGBoost.", "")
    } else if (delta_mae_v < -0.05) {
      synth_lines <- c(synth_lines,
        "=== Vitesse reglementaire vs predite ===",
        sprintf("  Utiliser la vitesse OSM AMELIORERAIT la MAE de %.2f dB.", abs(delta_mae_v)),
        "  -> Envisager la vitesse OSM pour les predictions.", "")
    } else {
      synth_lines <- c(synth_lines,
        "=== Vitesse reglementaire vs predite ===",
        "  Difference negligeable entre les deux vitesses (< 0.05 dB).", "")
    }
  }

  if ("highway" %in% names(emission_test) && exists("top_n") && nrow(top_n) > 0) {
    best_hw_s <- top_n$group[which.min(top_n$mae_xgb)]
    worst_hw_s <- top_n$group[which.max(top_n$mae_xgb)]
    synth_lines <- c(synth_lines,
      "=== Par type de route ===",
      sprintf("  Meilleur : %s (MAE = %.2f dB)", best_hw_s,
              top_n$mae_xgb[top_n$group == best_hw_s]),
      sprintf("  Pire     : %s (MAE = %.2f dB)", worst_hw_s,
              top_n$mae_xgb[top_n$group == worst_hw_s]),
      "")
  }

  synth_lines <- c(synth_lines,
    "=== Lecture du rapport ===",
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
    text = sprintf("Emission dB error report saved to %s", rel_path(emission_pdf_path)),
    process = "save")
} else {
  pipeline_message(
    text = "Emission dB analysis skipped: no usable rows for emission comparison",
    process = "warning")
}

pipeline_message(text = "Acoustic emission error analysis completed",
                 level = 1, progress = "end", process = "valid")

pipeline_message(text = "Standalone emission analysis finished",
                 level = 0, progress = "end", process = "valid")
