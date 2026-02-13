# ==============================================================================
# UTILITIES: MODEL TRAINING PHASE
# ==============================================================================
# Module d'utilitaires spécifiques à la phase d'entraînement des modèles.
# Contient les fonctions pour l'entraînement XGBoost, l'évaluation des modèles,
# et l'analyse de l'importance des features.
# ==============================================================================

# Note: utils_io.R is already loaded by bootstrap.R

# ------------------------------------------------------------------------------
# CNOSSOS-EU Emission Calculator (via NoiseModelling Java bridge)
# ------------------------------------------------------------------------------

#' Compute road noise emission level in dB(A)/m using CNOSSOS-EU 2020.
#'
#' Calls the NoiseModelling 5.x Java batch calculator via system().
#' Computes emission over 8 octave bands (63–8000 Hz) with A-weighting.
#'
#' @param flow numeric vector — total vehicle flow (veh/h)
#' @param truck_pct numeric vector — percentage of heavy vehicles (0–100)
#' @param speed numeric vector — vehicle speed (km/h)
#' @param temperature numeric scalar — air temperature in °C (default 20)
#' @param road_surface character — CNOSSOS road surface code (default "NL08")
#' @return numeric vector of emission levels in dB(A)/m
compute_emission_cnossos <- function(flow, truck_pct, speed,
                                      temperature = 20,
                                      road_surface = "NL08") {
  n <- length(flow)
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

  jar_files <- c(
    "noisemodelling-emission-5.0.2-SNAPSHOT.jar",
    "jackson-core-2.16.1.jar",
    "jackson-databind-2.16.1.jar",
    "jackson-annotations-2.16.1.jar"
  )

  classpath <- paste(c(jar_dir, file.path(jar_dir, jar_files)), collapse = ":")

  # Build CSV input
  csv_lines <- sprintf("%.6f,%.6f,%.6f,%.1f,%s",
                        pmax(as.numeric(flow), 0),
                        pmin(100, pmax(0, as.numeric(truck_pct))),
                        pmax(as.numeric(speed), 5),
                        temperature,
                        road_surface)

  # Write to temp file for large batches
  tmp_in <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_in), add = TRUE)

  writeLines(csv_lines, tmp_in)

  cmd <- sprintf("java -cp '%s' CnossosEmissionBatch < '%s'", classpath, tmp_in)
  result <- tryCatch(
    system(cmd, intern = TRUE, ignore.stderr = TRUE),
    error = function(e) {
      warning("CNOSSOS Java bridge failed: ", conditionMessage(e))
      return(NULL)
    },
    warning = function(w) {
      # system() returns a warning for non-zero exit code, but output is still usable
      result <- suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE))
      return(result)
    }
  )

  if (is.null(result)) {
    return(rep(NA_real_, n))
  }

  values <- suppressWarnings(as.numeric(result))

  if (length(values) != n) {
    warning(sprintf("CNOSSOS output length mismatch: expected %d, got %d", n, length(values)))
    # Pad with NA if needed
    if (length(values) < n) {
      values <- c(values, rep(NA_real_, n - length(values)))
    } else {
      values <- values[1:n]
    }
  }

  values
}

# ------------------------------------------------------------------------------
# Fonctions d'entraînement et d'évaluation
# ------------------------------------------------------------------------------

#' Compute model metrics (R², MAE, RMSE, MAPE)
#'
#' @param y_true vector of true values
#' @param y_pred vector of predicted values
#' @return list with metrics
compute_model_metrics <- function(y_true, y_pred) {
  residuals <- y_true - y_pred
  ss_res <- sum(residuals^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  
  r2 <- if (is.finite(ss_tot) && ss_tot > 0) {
    1 - (ss_res / ss_tot)
  } else {
    NA_real_
  }
  mae <- mean(abs(residuals))
  rmse <- sqrt(mean(residuals^2))
  mape <- mean(abs(residuals / pmax(abs(y_true), 0.01))) * 100
  
  list(
    r2 = r2,
    mae = mae,
    rmse = rmse,
    mape = mape,
    n = length(y_true)
  )
}

#' Extract top features by importance
#'
#' @param importance_matrix matrix from xgb.importance()
#' @param n_top number of top features to extract
#' @return data.frame with top features
get_top_features <- function(importance_matrix, n_top = 10) {
  if (is.null(importance_matrix) || nrow(importance_matrix) == 0) {
    return(NULL)
  }
  
  n_top <- min(n_top, nrow(importance_matrix))
  importance_matrix[1:n_top, ]
}

#' Validate training dataset structure
#'
#' @param training_data data.frame with training data
#' @return logical TRUE if valid, FALSE otherwise
validate_training_data <- function(training_data) {
  required_cols <- c(
    "osm_id", "count_point_id", "period",
    "aggregate_flow", "highway", "DEGRE"
  )
  
  missing_cols <- setdiff(required_cols, names(training_data))
  
  if (length(missing_cols) > 0) {
    pipeline_message(
      text = sprintf("Missing required columns: %s", 
                     paste(missing_cols, collapse = ", ")),
      process = "error")
    return(FALSE)
  }
  
  if (nrow(training_data) == 0) {
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

#' Check if all expected models were trained
#'
#' @param models_list list of trained models
#' @return list with validation results
validate_model_suite <- function(models_list) {
  expected_base_models <- c("flow_D", "truck_pct_D", "speed_D")
  expected_periods <- c("D", "E", "N", paste0("h", 0:23))
  
  available_models <- names(models_list)
  
  # Check base models
  missing_base <- setdiff(expected_base_models, available_models)
  
  # Check ratio models
  ratio_prefixes <- c("ratio_flow_", "ratio_truck_pct_", "ratio_speed_")
  expected_ratio_models <- c()
  for (prefix in ratio_prefixes) {
    for (period in setdiff(expected_periods, "D")) {
      expected_ratio_models <- c(expected_ratio_models, 
                                  paste0(prefix, period))
    }
  }
  
  missing_ratio <- setdiff(expected_ratio_models, available_models)
  
  list(
    total_models = length(available_models),
    expected_models = length(expected_base_models) + length(expected_ratio_models),
    missing_base_models = missing_base,
    missing_ratio_models = missing_ratio,
    is_complete = length(missing_base) == 0 && length(missing_ratio) == 0
  )
}
