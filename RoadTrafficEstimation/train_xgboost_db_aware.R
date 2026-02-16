# ==============================================================================
# ALTERNATIVE TRAINING: dB-AWARE XGBOOST
# ==============================================================================
# This script retrains all 225 XGBoost models using CNOSSOS-EU emission
# sensitivity weights. The idea: standard XGBoost minimizes RMSE in native
# units (flow, speed, truck_pct), but the final acoustic output is in dB via
# the non-linear CNOSSOS transform. Errors in speed at high speed cost more dB
# than at low speed (∂Lw/∂v is non-linear).
#
# Solution: for each training sample, compute the numerical partial derivative
# |∂Lw/∂target| via CNOSSOS and use it as sample weight. This makes XGBoost's
# squared-error loss equivalent to minimizing dB² error:
#
#   weighted MSE = Σ w_i × (ŷ_i − y_i)²
#   with w_i = (∂Lw/∂target_i)²
#   ≈ Σ (ΔLw_i)²  (first-order Taylor approximation)
#
# Usage: Rscript train_xgboost_db_aware.R
#
# Outputs:
#   - data/output/training/rds/06_xgboost_trained_models_db_aware.rds
#   - data/output/training/rds/06_xgboost_feature_info_db_aware.rds
# ==============================================================================

# --- Bootstrap & config ---
setwd("/home/aumond/Documents/github/OpenDataNoiseMap/RoadTrafficEstimation")
source("bootstrap.R")
source("config_pipeline.R")

pipeline_message(text = "dB-aware XGBoost training",
                 level = 0, progress = "start", process = "learn")

# --- Output paths (distinct from standard training) ---
DB_AWARE_MODELS_FILEPATH <- file.path(
  CONFIG$TRAINING_DATA_DIR, "rds", "06_xgboost_trained_models_db_aware.rds")
DB_AWARE_FEATURE_INFO_FILEPATH <- file.path(
  CONFIG$TRAINING_DATA_DIR, "rds", "06_xgboost_feature_info_db_aware.rds")

# ==============================================================================
# 1. Load training data
# ==============================================================================

pipeline_message(text = sprintf("Loading training data from %s",
                                rel_path(CONFIG$TRAINING_RDS_DATA_FILEPATH)),
                 level = 1, progress = "start", process = "load")

training_data <- readRDS(CONFIG$TRAINING_RDS_DATA_FILEPATH)

# Ensure ratio_speed_to_osm exists (fallback)
if (!"ratio_speed_to_osm" %in% names(training_data)) {
  if (all(c("aggregate_speed", "speed") %in% names(training_data))) {
    training_data$ratio_speed_to_osm <- ifelse(
      !is.na(training_data$aggregate_speed) & training_data$aggregate_speed >= 0 &
        !is.na(training_data$speed) & training_data$speed > 0,
      training_data$aggregate_speed / training_data$speed,
      NA_real_
    )
  } else {
    stop("Missing ratio_speed_to_osm and cannot compute fallback")
  }
}

pipeline_message(text = describe_df(training_data), process = "info")
pipeline_message(text = "Training data loaded",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 2. Configure features and periods (identical to standard training)
# ==============================================================================

pipeline_message(text = "Configuring features and model configs",
                 level = 1, progress = "start", process = "configure")

# --- Features (same 14 as standard) ---
candidate_road_features <- c(
  "highway", "DEGRE", "ref_letter", "first_word",
  "oneway_osm", "lanes_osm", "speed",
  "junction_osm", "lane_number",
  "connectivity", "betweenness", "closeness", "pagerank",
  "coreness", "dead_end_score", "edge_length_m")

available_road_features <- intersect(candidate_road_features, names(training_data))
missing_road_features <- setdiff(candidate_road_features, available_road_features)

if (length(missing_road_features) > 0) {
  pipeline_message(text = sprintf("Missing features (skipped): %s",
                                  paste(missing_road_features, collapse = ", ")),
                   process = "warning")
}

# Build formula with interactions (same as standard)
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

# --- Temporal periods ---
all_periods <- c("D", "E", "N", paste0("h", 0:23),
                 paste0("h", 0:23, "_wd"), paste0("h", 0:23, "_we"))

# --- Model configurations (identical to standard) ---
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
ratio_periods <- setdiff(all_periods, "D")

for (p in ratio_periods) {
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

pipeline_message(text = sprintf("Total model configurations: %d", length(all_configs)),
                 process = "info")
pipeline_message(text = "Configuration complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 3. Compute CNOSSOS sensitivity weights for the training data
# ==============================================================================

pipeline_message(text = "Computing CNOSSOS sensitivity weights",
                 level = 1, progress = "start", process = "calc")

# We need flow_D, truck_pct_D, and absolute speed for each sensor at period D
# to compute the CNOSSOS sensitivity at the "operating point" of each sample.
#
# For the 3 base models (period D):
#   - flow model:      w_i = |∂Lw/∂Q|  evaluated at (Q_i, T_i, v_i)
#   - truck_pct model: w_i = |∂Lw/∂T|  evaluated at (Q_i, T_i, v_i)
#   - speed model:     w_i = |∂Lw/∂v|  evaluated at (Q_i, T_i, v_i)
#
# For ratio models (periods ≠ D), we propagate the base-model sensitivity
# through the ratio multiplication:
#   flow_P = flow_D × ratio_flow_P
#   ∂Lw/∂ratio_flow = ∂Lw/∂flow_P × ∂flow_P/∂ratio_flow = ∂Lw/∂flow_P × flow_D
# But this requires knowing the predicted D values, which we don't have during
# training. So for ratio models, we use the same sensitivity pattern as the
# corresponding base model (evaluated at period-D operating point), scaled by
# the base-model value. This is a reasonable approximation since the sensitivity
# shape is dominated by the operating point, not the ratio.

# Extract period D data for CNOSSOS evaluation
d_data <- training_data %>% filter(period == "D")

# Build the operating point for each sensor at period D
# We need: flow, truck_pct, absolute_speed
sensor_operating_points <- data.frame(
  count_point_id = d_data$count_point_id,
  flow = pmax(d_data$aggregate_flow, 1),
  truck_pct = pmin(100, pmax(d_data$truck_pct, 0)),
  stringsAsFactors = FALSE
)

# Absolute speed = ratio_speed_to_osm × osm_speed
osm_speed <- suppressWarnings(as.numeric(d_data$speed))
osm_speed[is.na(osm_speed) | osm_speed <= 0] <- CONFIG$DEFAULT_VEHICLE_SPEED
ratio_speed <- d_data$ratio_speed_to_osm
ratio_speed[is.na(ratio_speed) | ratio_speed <= 0] <- 1.0
sensor_operating_points$abs_speed <- pmax(ratio_speed * osm_speed, 5)
sensor_operating_points$osm_speed <- osm_speed

pipeline_message(text = sprintf("Operating points: %d sensors at period D",
                                nrow(sensor_operating_points)),
                 process = "info")

# --- Compute numerical sensitivities via CNOSSOS ---
# Perturbation step sizes
delta_flow <- 10        # ±10 veh/h
delta_truck_pct <- 1    # ±1 %
delta_speed <- 1        # ±1 km/h

n_pts <- nrow(sensor_operating_points)
Q <- sensor_operating_points$flow
T_pct <- sensor_operating_points$truck_pct
V <- sensor_operating_points$abs_speed

pipeline_message(text = "Computing CNOSSOS at 7 perturbation points per sample...",
                 process = "calc")

# Central point
Lw_central <- compute_emission_cnossos(flow = Q, truck_pct = T_pct, speed = V)

# Perturbation: flow ± delta
Lw_flow_plus  <- compute_emission_cnossos(flow = pmax(Q + delta_flow, 1),
                                           truck_pct = T_pct, speed = V)
Lw_flow_minus <- compute_emission_cnossos(flow = pmax(Q - delta_flow, 1),
                                           truck_pct = T_pct, speed = V)

# Perturbation: truck_pct ± delta
Lw_truck_plus  <- compute_emission_cnossos(flow = Q,
                                            truck_pct = pmin(T_pct + delta_truck_pct, 100),
                                            speed = V)
Lw_truck_minus <- compute_emission_cnossos(flow = Q,
                                            truck_pct = pmax(T_pct - delta_truck_pct, 0),
                                            speed = V)

# Perturbation: speed ± delta
Lw_speed_plus  <- compute_emission_cnossos(flow = Q, truck_pct = T_pct,
                                            speed = pmax(V + delta_speed, 5))
Lw_speed_minus <- compute_emission_cnossos(flow = Q, truck_pct = T_pct,
                                            speed = pmax(V - delta_speed, 5))

pipeline_message(text = "CNOSSOS perturbation computations complete",
                 process = "valid")

# Compute numerical partial derivatives (central difference)
# |∂Lw/∂Q|, |∂Lw/∂T|, |∂Lw/∂v|  in dB per unit
dLw_dflow  <- abs(Lw_flow_plus - Lw_flow_minus) / (2 * delta_flow)
dLw_dtruck <- abs(Lw_truck_plus - Lw_truck_minus) / (2 * delta_truck_pct)
dLw_dspeed <- abs(Lw_speed_plus - Lw_speed_minus) / (2 * delta_speed)

# Handle NAs from CNOSSOS failures
dLw_dflow[is.na(dLw_dflow)]   <- median(dLw_dflow, na.rm = TRUE)
dLw_dtruck[is.na(dLw_dtruck)] <- median(dLw_dtruck, na.rm = TRUE)
dLw_dspeed[is.na(dLw_dspeed)] <- median(dLw_dspeed, na.rm = TRUE)

# Store sensitivity per sensor
sensor_operating_points$sens_flow  <- dLw_dflow
sensor_operating_points$sens_truck <- dLw_dtruck
sensor_operating_points$sens_speed <- dLw_dspeed

# Summary statistics
pipeline_message(text = sprintf(
  "Sensitivity ∂Lw/∂flow:  min=%.4f, median=%.4f, max=%.4f dB/(veh/h)",
  min(dLw_dflow, na.rm = TRUE), median(dLw_dflow, na.rm = TRUE),
  max(dLw_dflow, na.rm = TRUE)), process = "info")
pipeline_message(text = sprintf(
  "Sensitivity ∂Lw/∂truck: min=%.4f, median=%.4f, max=%.4f dB/%%",
  min(dLw_dtruck, na.rm = TRUE), median(dLw_dtruck, na.rm = TRUE),
  max(dLw_dtruck, na.rm = TRUE)), process = "info")
pipeline_message(text = sprintf(
  "Sensitivity ∂Lw/∂speed: min=%.4f, median=%.4f, max=%.4f dB/(km/h)",
  min(dLw_dspeed, na.rm = TRUE), median(dLw_dspeed, na.rm = TRUE),
  max(dLw_dspeed, na.rm = TRUE)), process = "info")

# Build lookup: count_point_id → sensitivity
# For sensors with multiple D observations, average the sensitivities
sens_lookup <- aggregate(
  cbind(sens_flow, sens_truck, sens_speed) ~ count_point_id,
  data = sensor_operating_points,
  FUN = mean, na.rm = TRUE)

pipeline_message(text = sprintf("Sensitivity lookup: %d unique sensors",
                                nrow(sens_lookup)),
                 process = "info")

pipeline_message(text = "CNOSSOS sensitivity weights computed",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 4. Helper: get sensitivity weight for a model config
# ==============================================================================

get_cnossos_weight_for_target <- function(target_name) {
  # Map target name → sensitivity column
  if (target_name %in% c("aggregate_flow", "ratio_flow")) {
    return("sens_flow")
  } else if (target_name %in% c("truck_pct", "ratio_truck_pct")) {
    return("sens_truck")
  } else if (target_name %in% c("ratio_speed_to_osm", "ratio_speed",
                                "aggregate_speed")) {
    return("sens_speed")
  } else {
    return(NA_character_)
  }
}

# ==============================================================================
# 5. Shared sensor split (same as standard training)
# ==============================================================================

pipeline_message(text = "Setting up shared sensor split for base models",
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

# Quality indicator helper (same as standard)
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
  if (!is.na(col) && col %in% available_cols) col else NA_character_
}

safe_sparse_model_matrix <- function(formula_obj, data_df) {
  vars_in_formula <- intersect(unique(all.vars(formula_obj)), names(data_df))
  if (nrow(data_df) == 0L) {
    return(Matrix::sparse.model.matrix(object = formula_obj, data = data_df))
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

  Matrix::sparse.model.matrix(object = formula_obj, data = mm_data)
}

pipeline_message(text = "Sensor split ready",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 6. Training loop (with dB-aware weights)
# ==============================================================================

pipeline_message(text = "Starting dB-aware training loop",
                 level = 1, progress = "start", process = "learn")

models_list <- list()
results_summary <- data.frame()
base_test_predictions <- list()

for (model_name in names(all_configs)) {
  
  config <- all_configs[[model_name]]
  
  pipeline_message(
    text = sprintf("[dB-aware] Training step [%d/%d] - %s",
                   which(names(all_configs) == model_name),
                   length(all_configs), model_name),
    level = 1, progress = "start", process = "wait")
  
  # --- Filter data for this period ---
  pipeline_message(text = "Period selection and target filtering",
                   level = 2, progress = "start", process = "configure")
  
  training_data_over_period <- training_data %>% filter(period == config$period)
  training_data_target <- training_data_over_period[[config$target]]
  valid_idx <- !is.na(training_data_target) & training_data_target >= config$min_valid
  
  min_obs_threshold <- ifelse(grepl("truck", config$target), 20, 50)
  if (sum(valid_idx) < min_obs_threshold) {
    pipeline_message(text = sprintf("Too few valid observations (< %d). Skipping!",
                                    min_obs_threshold), process = "warning")
    next
  }
  
  clean_data <- training_data_over_period[valid_idx, ]
  clean_target <- training_data_target[valid_idx]
  
  quality_col <- get_quality_indicator_column(config$target, names(clean_data))
  quality_indicator <- if (!is.na(quality_col)) {
    as.numeric(clean_data[[quality_col]])
  } else {
    rep(NA_real_, nrow(clean_data))
  }
  
  pipeline_message(text = sprintf("Valid observations: %d", nrow(clean_data)),
                   process = "info")
  pipeline_message(text = "Period selected and target filtered",
                   level = 2, progress = "end", process = "valid")
  
  # --- Sparse feature matrix ---
  pipeline_message(text = "Building sparse feature matrix",
                   level = 2, progress = "start", process = "calc")
  
  sparse_data_matrix <- safe_sparse_model_matrix(
    formula_obj = road_feature_formula,
    data_df = clean_data)
  
  # Re-align if rows were dropped
  if (nrow(sparse_data_matrix) != nrow(clean_data)) {
    kept_rows <- as.integer(rownames(sparse_data_matrix))
    clean_target <- clean_target[kept_rows]
    clean_data <- clean_data[kept_rows, ]
    quality_indicator <- quality_indicator[kept_rows]
  }
  
  pipeline_message(text = "Sparse feature matrix built",
                   level = 2, progress = "end", process = "valid")
  
  # --- Transform target ---
  if (!is.null(config$transform) && config$transform == "log10") {
    transformed_target <- log10(pmax(clean_target, config$min_valid))
  } else {
    transformed_target <- clean_target
  }
  
  if (nrow(sparse_data_matrix) != length(transformed_target)) next
  
  # --- Train/test split (same logic as standard) ---
  pipeline_message(text = "Train/test split",
                   level = 2, progress = "start", process = "calc")
  
  set.seed(123)
  n_final <- nrow(sparse_data_matrix)
  use_grouped_split <- isTRUE(CONFIG$USE_GROUPED_SENSOR_SPLIT)
  is_base_model <- model_name %in% c("flow_D", "truck_pct_D", "speed_D")
  
  if (use_grouped_split && "count_point_id" %in% names(clean_data)) {
    sensor_ids <- clean_data$count_point_id
    unique_sensors <- unique(sensor_ids)
    if (is_base_model && !is.null(shared_base_test_sensors)) {
      train_idx <- which(!sensor_ids %in% shared_base_test_sensors)
      test_idx <- which(sensor_ids %in% shared_base_test_sensors)
      if (length(train_idx) == 0 || length(test_idx) == 0) {
        train_idx <- sample(seq_len(n_final), floor(0.8 * n_final))
        test_idx <- setdiff(seq_len(n_final), train_idx)
      }
    } else if (length(unique_sensors) >= 5) {
      n_train_sensors <- max(1, floor(0.8 * length(unique_sensors)))
      train_sensors <- sample(unique_sensors, size = n_train_sensors)
      train_idx <- which(sensor_ids %in% train_sensors)
      test_idx <- which(!sensor_ids %in% train_sensors)
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
  
  X_train <- sparse_data_matrix[train_idx, ]
  X_test <- sparse_data_matrix[test_idx, ]
  y_train <- transformed_target[train_idx]
  y_test <- transformed_target[test_idx]
  quality_train <- quality_indicator[train_idx]
  test_meta <- clean_data[test_idx, c("osm_id", "count_point_id",
                                       "period", "highway", "speed")]
  
  # Clean invalid labels
  invalid_train <- is.na(y_train) | is.infinite(y_train) | is.nan(y_train)
  invalid_test <- is.na(y_test) | is.infinite(y_test) | is.nan(y_test)
  highway_test <- as.character(clean_data$highway[test_idx])
  
  if (any(invalid_train)) {
    v <- !invalid_train
    X_train <- X_train[v, ]; y_train <- y_train[v]
    quality_train <- quality_train[v]
  }
  if (any(invalid_test)) {
    v <- !invalid_test
    X_test <- X_test[v, ]; y_test <- y_test[v]
    highway_test <- highway_test[v]
    test_meta <- test_meta[v, , drop = FALSE]
  }
  
  if (length(y_train) < 10 || length(y_test) < 5) next
  
  pipeline_message(text = sprintf("Train: %d | Test: %d", length(y_train), length(y_test)),
                   process = "info")
  pipeline_message(text = "Train/test split done",
                   level = 2, progress = "end", process = "valid")
  
  # ==========================================================================
  # KEY DIFFERENCE: Compute combined dB-aware + quality weights
  # ==========================================================================
  pipeline_message(text = "Computing dB-aware sample weights",
                   level = 2, progress = "start", process = "calc")
  
  # Determine which sensitivity column to use
  sens_col <- get_cnossos_weight_for_target(config$target)
  
  # Get sensor IDs for train samples
  train_sensor_ids <- clean_data$count_point_id[train_idx]
  if (any(invalid_train)) train_sensor_ids <- train_sensor_ids[!invalid_train]
  
  # Look up CNOSSOS sensitivity for each training sample
  sens_match_idx <- match(train_sensor_ids, sens_lookup$count_point_id)
  
  if (!is.na(sens_col) && any(!is.na(sens_match_idx))) {
    # Get the raw sensitivity |∂Lw/∂target| for each sample
    raw_sensitivity <- sens_lookup[[sens_col]][sens_match_idx]
    
    # For samples without a match (sensor not in D data), use median
    med_sens <- median(raw_sensitivity, na.rm = TRUE)
    raw_sensitivity[is.na(raw_sensitivity)] <- med_sens
    
    # For the flow model with log10 transform, the target is log10(Q), so
    # we need to adjust: ∂Lw/∂log10(Q) = ∂Lw/∂Q × Q × ln(10)
    # But the sensitivity was computed w.r.t. Q directly.
    # Since XGBoost predicts log10(Q), the prediction error δlog10(Q) maps to
    # δQ = Q × ln(10) × δlog10(Q), so:
    # δLw ≈ (∂Lw/∂Q) × Q × ln(10) × δlog10(Q)
    # → effective weight = (∂Lw/∂Q × Q × ln(10))²
    if (!is.null(config$transform) && config$transform == "log10" &&
        config$target == "aggregate_flow") {
      # Get the actual flow values for train samples to apply chain rule
      train_flow <- clean_target[train_idx]
      if (any(invalid_train)) train_flow <- train_flow[!invalid_train]
      train_flow <- pmax(train_flow, 1)
      raw_sensitivity <- raw_sensitivity * train_flow * log(10)
    }
    
    # Square the sensitivity: w_i = |∂Lw/∂target|²
    # This makes weighted MSE ≈ Σ (ΔLw)²
    db_weight <- raw_sensitivity^2
    
    # Normalize to mean = 1 (preserves effective learning rate)
    db_weight <- db_weight / mean(db_weight, na.rm = TRUE)
    
    # Clamp extreme weights to avoid instability
    # Cap at 10× median to prevent a few high-sensitivity samples from
    # dominating the loss
    weight_cap <- 10 * median(db_weight, na.rm = TRUE)
    weight_floor <- 0.05
    db_weight <- pmin(db_weight, weight_cap)
    db_weight <- pmax(db_weight, weight_floor)
    
    # Re-normalize after clamping
    db_weight <- db_weight / mean(db_weight, na.rm = TRUE)
    
    pipeline_message(text = sprintf(
      "dB weight stats: min=%.3f, median=%.3f, mean=%.3f, max=%.3f",
      min(db_weight), median(db_weight), mean(db_weight), max(db_weight)),
      process = "info")
    
    n_matched <- sum(!is.na(sens_match_idx))
    pipeline_message(text = sprintf(
      "dB sensitivity matched: %d/%d samples (%.1f%%)",
      n_matched, length(train_sensor_ids),
      100 * n_matched / length(train_sensor_ids)),
      process = "info")
    
  } else {
    # Fallback: uniform dB weights (no CNOSSOS sensitivity available)
    db_weight <- rep(1.0, length(y_train))
    pipeline_message(text = "No CNOSSOS sensitivity available → uniform dB weights",
                     process = "warning")
  }
  
  # Combine with AVATAR quality weights (if enabled)
  min_weight <- if (!is.null(CONFIG$MIN_AVATAR_SAMPLE_WEIGHT)) {
    CONFIG$MIN_AVATAR_SAMPLE_WEIGHT
  } else {
    0.20
  }
  
  if (isTRUE(CONFIG$USE_AVATAR_QUALITY_WEIGHTS) && any(!is.na(quality_train))) {
    quality_weight <- 1 - pmin(pmax(quality_train, 0), 100) / 100
    quality_weight[is.na(quality_weight)] <- 1
    quality_weight <- pmax(quality_weight, min_weight)
    
    # Multiplicative combination: dB-aware × quality
    combined_weight <- db_weight * quality_weight
  } else {
    combined_weight <- db_weight
  }
  
  # Final normalization
  combined_weight <- combined_weight / mean(combined_weight, na.rm = TRUE)
  combined_weight <- pmax(combined_weight, 0.01)  # Absolute floor
  
  pipeline_message(text = sprintf(
    "Combined weights: min=%.3f, median=%.3f, mean=%.3f, max=%.3f",
    min(combined_weight), median(combined_weight),
    mean(combined_weight), max(combined_weight)),
    process = "info")
  
  # Create xgb.DMatrix with combined weights
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train,
                                  weight = combined_weight)
  dtest <- xgboost::xgb.DMatrix(data = X_test, label = y_test)
  
  pipeline_message(text = "dB-aware weights computed and applied",
                   level = 2, progress = "end", process = "valid")
  
  # ==========================================================================
  # XGBoost training (same hyperparameters as standard)
  # ==========================================================================
  pipeline_message(text = "Training XGBoost model",
                   level = 2, progress = "start", process = "learn")
  
  start_timer()
  
  if (grepl("truck", config$target)) {
    params <- CONFIG$TRUCK_PARAMS
    if (length(y_train) < 200) {
      nfold <- min(5, length(y_train) %/% 10)
      if (nfold >= 2) {
        cv_result <- xgboost::xgb.cv(
          params = params, data = dtrain, nrounds = CONFIG$NROUNDS,
          nfold = nfold, early_stopping_rounds = 30,
          verbose = 0, showsd = FALSE)
        best_rounds <- cv_result$best_iteration
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
      cv_result <- xgboost::xgb.cv(
        params = params, data = dtrain, nrounds = CONFIG$NROUNDS,
        nfold = nfold, early_stopping_rounds = 50,
        verbose = 0, showsd = FALSE)
      best_rounds <- cv_result$best_iteration
      pipeline_message(text = sprintf("CV(%d-fold) selected %d rounds",
                                      nfold, best_rounds), process = "info")
    } else {
      best_rounds <- CONFIG$NROUNDS
    }
  }
  
  # Acceptable limits
  min_train_xgb <- 150
  min_test_xgb  <- 50
  if (config$period == "D" && config$target == "ratio_speed_to_osm") {
    min_train_xgb <- 10; min_test_xgb <- 5
  }
  if (grepl("truck", config$target)) {
    min_train_xgb <- 10; min_test_xgb <- 5
  }
  if (length(y_train) < min_train_xgb || length(y_test) < min_test_xgb) {
    pipeline_message(text = sprintf("Sample too small (train=%d, test=%d). Skipped!",
                                    length(y_train), length(y_test)),
                     process = "warning")
    next
  }
  
  # Adaptive watchlist
  use_watchlist <- length(y_train) >= 100 && length(y_test) >= 30
  
  xgb_model <- xgboost::xgb.train(
    params = params, data = dtrain, nrounds = best_rounds,
    watchlist = if (use_watchlist) list(train = dtrain, test = dtest) else NULL,
    early_stopping_rounds = if (use_watchlist) 50 else NULL,
    maximize = FALSE, verbose = 0)
  
  elapsed <- stop_timer()
  
  pipeline_message(text = sprintf("Model trained in %.2f min", elapsed),
                   level = 2, progress = "end", process = "valid")
  
  # ==========================================================================
  # Evaluation (same as standard)
  # ==========================================================================
  pipeline_message(text = "Evaluating model",
                   level = 2, progress = "start", process = "search")
  
  pred_test <- predict(xgb_model, X_test)
  
  if (!is.null(config$transform) && config$transform == "log10") {
    pred_original <- 10^pred_test
    actual_original <- 10^y_test
  } else {
    pred_original <- pred_test
    actual_original <- y_test
  }
  
  if (model_name == "speed_D" && config$target == "ratio_speed_to_osm") {
    speed_osm_test <- suppressWarnings(as.numeric(test_meta$speed))
    speed_osm_test[is.na(speed_osm_test) | speed_osm_test <= 0] <- CONFIG$DEFAULT_VEHICLE_SPEED
    pred_eval <- pmax(0, pred_original * speed_osm_test)
    actual_eval <- pmax(0, actual_original * speed_osm_test)
  } else {
    pred_eval <- pred_original
    actual_eval <- actual_original
  }
  
  model_metrics <- compute_model_metrics(y_true = actual_eval, y_pred = pred_eval)
  mae <- model_metrics$mae
  rmse <- model_metrics$rmse
  r2 <- model_metrics$r2
  mape <- model_metrics$mape
  
  mape_values <- abs((pred_eval - actual_eval) / pmax(actual_eval, 0.01)) * 100
  medape <- median(mape_values[is.finite(mape_values)], na.rm = TRUE)
  
  pipeline_message(text = sprintf("R²=%.3f | MAE=%.2f | RMSE=%.2f | MAPE=%.1f%%",
                                  r2, mae, rmse, mape), process = "info")
  
  # Feature importance
  importance <- xgboost::xgb.importance(model = xgb_model)
  top_features <- head(importance, 5)
  
  pipeline_message(text = "Model evaluated",
                   level = 2, progress = "end", process = "valid")
  
  # ==========================================================================
  # Store model
  # ==========================================================================
  
  # Per-highway diagnostics
  highway_metrics <- data.table::data.table(
    highway = highway_test, actual = actual_eval, pred = pred_eval)
  highway_metrics <- highway_metrics[!is.na(highway)]
  if (nrow(highway_metrics) > 0) {
    highway_metrics_summary <- highway_metrics[, {
      m <- compute_model_metrics(y_true = actual, y_pred = pred)
      list(n = .N, r2 = m$r2, mae = m$mae, rmse = m$rmse, mape = m$mape)
    }, by = highway][order(-n)]
  } else {
    highway_metrics_summary <- data.table::data.table()
  }
  
  models_list[[model_name]] <- list(
    model = xgb_model,
    config = config,
    metrics = list(mae = mae, rmse = rmse, r2 = r2, mape = mape, medape = medape),
    feature_names = colnames(X_train),
    feature_importance = importance,
    top_features = top_features,
    highway_metrics = as.data.frame(highway_metrics_summary),
    n_train = length(y_train),
    n_test = length(y_test),
    training_time = elapsed,
    weighting = "db_aware"  # Flag to identify dB-aware models
  )
  
  # Keep base-model test predictions for emission analysis
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
  
  results_summary <- rbind(results_summary,
    data.frame(Model = model_name, Target = config$name,
               N_train = length(y_train), N_test = length(y_test),
               R2 = round(r2, 3), MAE = round(mae, 2),
               RMSE = round(rmse, 2), MAPE = round(mape, 1),
               Time_min = round(elapsed, 2)))
  
  pipeline_message(text = sprintf("[dB-aware] %s complete", model_name),
                   level = 1, progress = "end", process = "valid")
}

# ==============================================================================
# 7. Save models
# ==============================================================================

pipeline_message(text = sprintf("Saving dB-aware models to %s",
                                rel_path(DB_AWARE_MODELS_FILEPATH)),
                 level = 1, progress = "start", process = "save")

saveRDS(object = models_list, file = DB_AWARE_MODELS_FILEPATH)
saveRDS(object = list(road_feature_formula = road_feature_formula,
                      all_periods = all_periods),
        file = DB_AWARE_FEATURE_INFO_FILEPATH)

pipeline_message(text = "Models saved",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 8. Summary comparison
# ==============================================================================

pipeline_message(text = "Training summary",
                 level = 1, progress = "start", process = "plot")

cat("\n")
pipeline_message(text = sprintf("Total models trained: %d / %d",
                                length(models_list), length(all_configs)),
                 process = "info")

if (nrow(results_summary) > 0) {
  # Base models
  base_results <- results_summary[grepl("_D$", results_summary$Model), ]
  ratio_results <- results_summary[grepl("^ratio_", results_summary$Model), ]
  
  if (nrow(base_results) > 0) {
    pipeline_message(text = sprintf(
      "Base models (D): R²=%.3f | MAE=%.2f | RMSE=%.2f | MAPE=%.1f%%",
      mean(base_results$R2, na.rm = TRUE), mean(base_results$MAE, na.rm = TRUE),
      mean(base_results$RMSE, na.rm = TRUE), mean(base_results$MAPE, na.rm = TRUE)),
      process = "info")
  }
  if (nrow(ratio_results) > 0) {
    pipeline_message(text = sprintf(
      "Ratio models: R²=%.3f | MAE=%.2f | RMSE=%.2f | MAPE=%.1f%%",
      mean(ratio_results$R2, na.rm = TRUE), mean(ratio_results$MAE, na.rm = TRUE),
      mean(ratio_results$RMSE, na.rm = TRUE), mean(ratio_results$MAPE, na.rm = TRUE)),
      process = "info")
  }
  
  # Print full summary
  pipeline_message(text = sprintf("Full results:\n%s",
                                  paste0(capture.output(results_summary), collapse = "\n\t\t")),
                   process = "info")
}

pipeline_message(text = "Summary complete",
                 level = 1, progress = "end", process = "valid")

# ==============================================================================
# 9. Quick emission dB comparison (base models only)
# ==============================================================================

pipeline_message(text = "Quick emission dB analysis on test set",
                 level = 1, progress = "start", process = "plot")

if (all(c("flow_D", "truck_pct_D", "speed_D") %in% names(base_test_predictions))) {
  
  # Merge test predictions from 3 base models
  merged <- merge(
    base_test_predictions$flow_D[, c("osm_id", "count_point_id", "highway",
                                      "pred", "actual")],
    base_test_predictions$truck_pct_D[, c("osm_id", "count_point_id",
                                           "pred", "actual")],
    by = c("osm_id", "count_point_id"),
    suffixes = c("_flow", "_truck"))
  
  merged <- merge(
    merged,
    base_test_predictions$speed_D[, c("osm_id", "count_point_id",
                                       "pred", "actual")],
    by = c("osm_id", "count_point_id"))
  
  names(merged)[names(merged) == "pred"] <- "pred_speed"
  names(merged)[names(merged) == "actual"] <- "actual_speed"
  
  if (nrow(merged) >= 10) {
    pipeline_message(text = sprintf("Merged test samples for dB analysis: %d",
                                    nrow(merged)), process = "info")
    
    # Compute CNOSSOS emission at predicted and actual values
    Lw_pred <- compute_emission_cnossos(
      flow = pmax(merged$pred_flow, 1),
      truck_pct = pmin(100, pmax(merged$pred_truck, 0)),
      speed = pmax(merged$pred_speed, 5))
    
    Lw_actual <- compute_emission_cnossos(
      flow = pmax(merged$actual_flow, 1),
      truck_pct = pmin(100, pmax(merged$actual_truck, 0)),
      speed = pmax(merged$actual_speed, 5))
    
    valid <- !is.na(Lw_pred) & !is.na(Lw_actual) &
      is.finite(Lw_pred) & is.finite(Lw_actual)
    
    if (sum(valid) >= 10) {
      dB_error <- Lw_pred[valid] - Lw_actual[valid]
      
      cat("\n")
      pipeline_message(text = "========== dB-AWARE MODEL: EMISSION ANALYSIS ==========",
                       process = "info")
      pipeline_message(text = sprintf("N test samples: %d", sum(valid)),
                       process = "info")
      pipeline_message(text = sprintf("Bias:  %+.2f dB", mean(dB_error)),
                       process = "info")
      pipeline_message(text = sprintf("MAE:   %.2f dB", mean(abs(dB_error))),
                       process = "info")
      pipeline_message(text = sprintf("RMSE:  %.2f dB", sqrt(mean(dB_error^2))),
                       process = "info")
      pipeline_message(text = sprintf("Q05-Q95: [%+.2f, %+.2f] dB",
                                      quantile(dB_error, 0.05),
                                      quantile(dB_error, 0.95)),
                       process = "info")
      pipeline_message(text = "========================================================",
                       process = "info")
      
      # Compare with standard models if available
      std_models_path <- CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH
      if (file.exists(std_models_path)) {
        pipeline_message(text = "Loading standard models for comparison...",
                         process = "load")
        # We'll do the comparison in a separate run of run_emission_analysis.R
        # to keep this script focused on training
        pipeline_message(text = sprintf(
          "Run 'Rscript run_emission_analysis.R' with both model files to compare"),
          process = "info")
      }
    } else {
      pipeline_message(text = "Too few valid CNOSSOS results for dB analysis",
                       process = "warning")
    }
  } else {
    pipeline_message(text = sprintf("Too few merged test samples (%d) for dB analysis",
                                    nrow(merged)), process = "warning")
  }
} else {
  pipeline_message(text = "Not all 3 base models available for dB analysis",
                   process = "warning")
}

pipeline_message(text = "Emission dB analysis complete",
                 level = 1, progress = "end", process = "valid")

pipeline_message(text = "dB-aware XGBoost training complete",
                 level = 0, progress = "end", process = "valid")
