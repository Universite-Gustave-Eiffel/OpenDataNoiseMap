# ==============================================================================
# MODEL TRAINING PHASE UTILITIES
# ==============================================================================
#' 
# -------------------------------------------------------------------------------
# Quality indicator helper
# -------------------------------------------------------------------------------
#' @title Quality indicator helper
#' @description This function returns the name of the quality indicator column 
#'              for a given target variable.
#' @param target_name Character. Name of the target variable.
#' @param available_cols Character vector. Names of available columns.
#' @return Character. Name of the quality indicator column.
#' @export
get_quality_indicator_column <- function(target_name, available_cols) {
  if (target_name %in% c("aggregate_flow", "ratio_flow")) {
    col <- "perc_flow_predicted"
  } else if (target_name %in% c("aggregate_speed", "ratio_speed", 
                                "ratio_speed_to_osm")) {
    col <- "perc_speed_predicted"
  } else if (target_name %in% c("truck_pct", "ratio_truck_pct", 
                                "aggregate_flow_trucks", "ratio_flow_trucks")) {
    col <- "perc_flow_trucks_predicted"
  } else {
    col <- NA_character_
  }
  if (!is.na(col) && col %in% available_cols) { col } else { NA_character_ }
}
#' 
# -------------------------------------------------------------------------------
# Safe sparse model matrix
# -------------------------------------------------------------------------------
#' @title Safe sparse model matrix
#' @description Safe sparse model matrix: handle single-level categoricals after 
#'              NA removal.
#' @param formula_obj formula object
#' @param data_df data.frame
#' @return sparse model matrix
#' @export
safe_sparse_model_matrix <- function(formula_obj, data_df) {
  vars_in_formula <- intersect(x = unique(x = all.vars(formula_obj)), 
                               y = names(x = data_df))
  if (nrow(x = data_df) == 0L) {
    return(Matrix::sparse.model.matrix(object = formula_obj, 
                                       data   = data_df))
  }

  # Work on a copy and normalize categorical vars to character
  mm_data <- data_df
  for (v in vars_in_formula) {
    if (is.factor(x = mm_data[[v]])) {
      mm_data[[v]] <- as.character(x = mm_data[[v]])
    }
  }

  # model.matrix drops rows with NA on variables used in formula; check levels
  # on that effective subset and neutralize categorical variables with <=1 level
  mm_subset <- mm_data[, vars_in_formula, drop = FALSE]
  cc_idx    <- complete.cases(mm_subset)
  cc_data   <- mm_data[cc_idx, , drop = FALSE]

  for (v in vars_in_formula) {
    if (is.character(x = mm_data[[v]])) {
      lv <- unique(x = cc_data[[v]])
      lv <- lv[!is.na(lv) & nzchar(lv)]
      if (length(x = lv) <= 1) {
        mm_data[[v]] <- 0
      }
    }
  }
  # Rebuild sparse model matrix
  Matrix::sparse.model.matrix(object = formula_obj, 
                              data   = mm_data)
}
#' 
# -------------------------------------------------------------------------------
# CNOSSOS-EU Emission Calculator (via NoiseModelling Java bridge)
# -------------------------------------------------------------------------------
#' @title CNOSSOS-EU Emission Calculator
#' @description Compute road noise emission level in dB(A)/m using CNOSSOS-EU. 
#'              Calls the NoiseModelling 5.x Java batch calculator via system().
#'              Computes emission over 8 octave bands (63–8000 Hz) with 
#'              A-weighting.
#' @param flow numeric vector — total vehicle flow (veh/h)
#' @param truck_pct numeric vector — percentage of heavy vehicles (0–100)
#' @param speed numeric vector — vehicle speed (km/h)
#' @param temperature numeric scalar — air temperature in °C (default 20)
#' @param road_surface character — CNOSSOS road surface code (default "NL08")
#' @return numeric vector of emission levels in dB(A)/m
#' @export
compute_emission_cnossos <- function(flow, truck_pct, speed,
                                     temperature = 20,
                                     road_surface = "NL08") {
  n <- length(x = flow)
  if (n == 0) return(numeric(0))

  # Path to the NoiseModelling emission JAR directory
  jar_dir <- file.path(getwd(), "NoiseModellingEmission")
  if (!file.exists(jar_dir)) {
    jar_dir <- file.path(dirname(getwd()), "NoiseModellingEmission")
  }
  if (!file.exists(jar_dir)) {
    # Try from CONFIG if available
    jar_dir <- file.path(CONFIG$BASE_DIR, "NoiseModellingEmission")
  }

  jar_files <- c("noisemodelling-emission-5.0.2-SNAPSHOT.jar",
                 "jackson-core-2.16.1.jar",
                 "jackson-databind-2.16.1.jar",
                 "jackson-annotations-2.16.1.jar")

  classpath <- paste(c(jar_dir, file.path(jar_dir, jar_files)), 
                     collapse = ":")

  # Build CSV input
  csv_lines <- sprintf("%.6f,%.6f,%.6f,%.1f,%s",
                       pmax(as.numeric(flow), 0),
                       pmin(100, pmax(0, as.numeric(truck_pct))),
                       pmax(as.numeric(speed), 5),
                       temperature,
                       road_surface)

  # Write to temp file for large batches
  tmp_in <- tempfile(fileext = ".csv")
  on.exit(expr = unlink(x = tmp_in), 
          add  = TRUE)

  writeLines(text = csv_lines, con = tmp_in)

  cmd <- sprintf("java -cp '%s' CnossosEmissionBatch < '%s'", classpath, tmp_in)
  result <- tryCatch(
    system(command       = cmd, 
           intern        = TRUE, 
           ignore.stderr = TRUE),
    error = function(e) {
      pipeline_message(sprintf("CNOSSOS Java bridge failed: %s", 
                               conditionMessage(e)),
                     process = "warning")
      return(NULL)
    },
  )

  if (is.null(x = result)) {
    return(rep(NA_real_, n))
  }

  values <- suppressWarnings(as.numeric(result))

  if (length(x = values) != n) {
    pipeline_message(
      sprintf("CNOSSOS output length mismatch: expected %d, got %d", 
              n, length(x = values)),
      process = "warning")
    # Pad with NA if needed
    if (length(x = values) < n) {
      values <- c(values, rep(NA_real_, n - length(x = values)))
    } else {
      values <- values[1:n]
    }
  }

  values
}
#' 
# ------------------------------------------------------------------------------
# Compute model metrics (R², MAE, RMSE, MAPE)
# ------------------------------------------------------------------------------
#' @title Compute model metrics
#' @description Compute R^2, MAE, RMSE, and MAPE metrics for a given set of 
#'              true and predicted values.
#' @param y_true vector of true values
#' @param y_pred vector of predicted values
#' @return list with metrics
#' @export
compute_model_metrics <- function(y_true, y_pred) {
  residuals <- y_true - y_pred
  ss_res    <- sum(residuals^2)
  ss_tot    <- sum((y_true - mean(x = y_true))^2)
  
  r2 <- if (is.finite(ss_tot) && ss_tot > 0) {
    1 - (ss_res / ss_tot)
  } else {
    NA_real_
  }
  mae  <- mean(x = abs(x = residuals))
  rmse <- sqrt(x = mean(x = residuals^2))
  mape <- mean(x = abs(x = residuals / pmax(abs(x = y_true), 0.01))) * 100
  return(list(r2   = r2,
              mae  = mae,
              rmse = rmse,
              mape = mape,
              n    = length(x = y_true))
}
#' 
# ------------------------------------------------------------------------------
#' Extract top features by importance
# ------------------------------------------------------------------------------
#' @title Extract top features by importance
#' @description Extract top features by importance from an XGBoost model.
#' @param importance_matrix matrix from xgb.importance()
#' @param n_top number of top features to extract
#' @return data.frame with top features
#' @export
get_top_features <- function(importance_matrix, n_top = 10) {
  if (is.null(x = importance_matrix) || nrow(x = importance_matrix) == 0) {
    return(NULL)
  }
  n_top <- min(n_top, nrow(x = importance_matrix))
  importance_matrix[1:n_top, ]
}
#' 
# ------------------------------------------------------------------------------
#' Validate training dataset structure
# ------------------------------------------------------------------------------
#' @title Validate training dataset structure
#' @description Validate training dataset structure. Checks whether the training 
#'              dataset contains the required columns and whether it is non-empty.
#' @param training_data data.frame with training data
#' @return logical TRUE if valid, FALSE otherwise
#' @export
validate_training_data <- function(training_data) {
  required_cols <- c("osm_id", "count_point_id", "period", 
                     "aggregate_flow", "highway", "DEGRE")
  
  missing_cols <- setdiff(x = required_cols, y = names(x = training_data))
  
  if (length(x = missing_cols) > 0) {
    pipeline_message(
      text = sprintf("Missing required columns: %s", 
                     paste(missing_cols, collapse = ", ")),
      process = "error")
    return(FALSE)
  }
  
  if (nrow(x = training_data) == 0) {
    pipeline_message(text = "Training data is empty", process = "error")
    return(FALSE)
  }
  
  # Check for NA in critical columns
  critical_cols <- c("aggregate_flow", "highway", "period")
  for (col in critical_cols) {
    n_na <- sum(is.na(training_data[[col]]))
    if (n_na > 0) {
      pipeline_message(
        text = sprintf("Column '%s' has %s NA values", col, n_na),
        process = "warn")
    }
  }
  
  return(TRUE)
}
#' 
# ------------------------------------------------------------------------------
#' Check if all expected models were trained
# ------------------------------------------------------------------------------
#' @title Check if all expected models were trained
#' @description Check if all expected models were trained.
#' @param models_list list of trained models
#' @return list with validation results
#' @export
validate_model_suite <- function(models_list) {
  expected_base_models <- c("flow_D", "truck_pct_D", "speed_D")
  expected_periods     <- c("D", "E", "N", paste0("h", 0:23))
  
  available_models     <- names(x = models_list)
  
  # Check base models
  missing_base <- setdiff(x = expected_base_models, y= available_models)
  
  # Check ratio models
  ratio_prefixes <- c("ratio_flow_", "ratio_truck_pct_", "ratio_speed_")
  expected_ratio_models <- c()
  for (prefix in ratio_prefixes) {
    for (period in setdiff(x = expected_periods, y = "D")) {
      expected_ratio_models <- c(expected_ratio_models, 
                                 paste0(prefix, period))
    }
  }
  
  missing_ratio <- setdiff(x = expected_ratio_models, y = available_models)
  
  return(list(total_models         = length(x = available_models), 
              expected_models      = length(x = expected_base_models) + 
                                     length(x = expected_ratio_models), 
              missing_base_models  = missing_base, 
              missing_ratio_models = missing_ratio, 
              is_complete          = length(x = missing_base) == 0 && 
                                     length(x = missing_ratio) == 0)
}
#' 
# ------------------------------------------------------------------------------
# Summarize metrics by group
# ------------------------------------------------------------------------------
#' @title Summarize metrics by group
#' @description Summarize metrics by group. Used in `summarize_group_metrics()` and 
#'              `compute_group_metrics()`.
#' @param df data.frame with metrics
#' @param group_col name of column to group by
#' @return data.frame with summarized metrics
#' @export
summarize_group_metrics <- function(df, group_col) {
  dt <- as.data.table(x = df)
  out <- dt[, .(n        = .N,
                bias_xgb = mean(x = db_error, na.rm = TRUE),
                mae_xgb  = mean(x = abs_db_error, na.rm = TRUE),
                rmse_xgb = sqrt(x = mean(x = db_error^2, na.rm = TRUE)),
                bias_osm = ifelse(test = "db_error_osm" %in% names(x = .SD), 
                                  yes  = mean(x     = db_error_osm, 
                                              na.rm = TRUE), 
                                  no   = NA_real_),
                mae_osm  = ifelse(test = "abs_db_error_osm" %in% names(x = .SD), 
                                  yes  = mean(x     = abs_db_error_osm, 
                                              na.rm = TRUE), 
                                  no   = NA_real_),
            by = .(group = as.character(x = get(group_col)))]
  out[!is.na(x = group) & group != ""]
}
#' 
# ------------------------------------------------------------------------------
# Compute metrics for subset of data
# ------------------------------------------------------------------------------
#' @title Compute metrics for subset of data
#' @description Compute metrics for subset of data. Used in `compute_group_metrics()`.
#' @param mask A logical vector indicating which rows to include in the subset.
#' @param label A character string indicating the label for the subset.
#' @return A data.frame with metrics for the subset.
#' @export
compute_subset_metrics <- function(mask, label) {
  idx <- !is.na(x = mask) & as.logical(mask)
  if (sum(idx) == 0) {
    return(data.frame(subset = label,
                      n                = 0L,
                      bias             = NA_real_,
                      mae              = NA_real_,
                      rmse             = NA_real_,
                      q50              = NA_real_,
                      q90              = NA_real_,
                      within_1db       = NA_real_,
                      within_2db       = NA_real_,
                      stringsAsFactors = FALSE))
  }
  err  <- emission_test$db_error[idx]
  abse <- emission_test$abs_db_error[idx]
  return(data.frame(subset           = label,
                    n                = sum(idx),
                    bias             = mean(x = err, na.rm = TRUE),
                    mae              = mean(x = abse, na.rm = TRUE),
                    rmse             = sqrt(x = mean(x = err^2, na.rm = TRUE)),
                    q50              = median(x = abse, na.rm = TRUE),
                    q90              = as.numeric(x = quantile(x     = abse, 
                                                                probs = 0.90, 
                                                                na.rm = TRUE)),
                    within_1db       = mean(x = abse <= 1, na.rm = TRUE),
                    within_2db       = mean(x = abse <= 2, na.rm = TRUE),
                    stringsAsFactors = FALSE)
}
#' 
# ------------------------------------------------------------------------------
# Identify dominant source of error
# ------------------------------------------------------------------------------
#' @title Identify dominant source of error
#' @description Identify dominant source of error for a group of data. Used in 
#'              `compute_group_metrics()`.
#' @param df A data.frame with metrics.
#' @return A character string indicating dominant source of error.
#' @export
identify_dominant_source <- function(df) {
  if (nrow(x = df) == 0) return("N/A")
  fl    <- db_flow_only[as.integer(x = rownames(x = df))] - 
           db_actual_all[as.integer(x = rownames(x = df))]
  sp    <- db_speed_only[as.integer(x = rownames(x = df))] - 
           db_actual_all[as.integer(x = rownames(x = df))]
  mae_f <- mean(x = abs(x = fl), na.rm = TRUE)
  mae_s <- mean(x = abs(x = sp), na.rm = TRUE)
  if (mae_f > mae_s) return("Flow") else return("Speed")
}
#' 
# ------------------------------------------------------------------------------
# Compute metrics for all data
# ------------------------------------------------------------------------------
#' @title Compute metrics for all data
#' @description Compute metrics for all data. Used in `compute_group_metrics()`.
#' @param df A data.frame with metrics.
#' @return A list with metrics for all data.
#' @export
compute_db_stats <- function(df) {
  if (nrow(x = df) == 0) {
    return(list(n    = 0L, 
                bias = NA_real_, 
                mae  = NA_real_, 
                rmse = NA_real_))
  }
  return(list(n    = nrow(x = df),
              bias = mean(x = df$db_error, na.rm = TRUE),
              mae  = mean(x = abs(x = df$db_error), na.rm = TRUE),
              rmse = sqrt(x = mean(x = df$db_error^2, na.rm = TRUE)))
}