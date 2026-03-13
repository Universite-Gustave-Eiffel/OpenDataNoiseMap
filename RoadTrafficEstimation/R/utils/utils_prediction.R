# ==============================================================================
# UTILITIES: PREDICTION PHASE
# ==============================================================================
# Module d'utilitaires spécifiques à la phase de prédiction.
# Contient les fonctions pour charger la couche France engineered, appliquer
# les modèles XGBoost, et formater les sorties.
# ==============================================================================

# Note: utils_io.R and utils_sf.R are already loaded by bootstrap.R


#' Generate prediction file paths based on spatial extent and mode
#'
#' This function creates a standardized naming scheme for prediction outputs,
#' consistent with the training pipeline. All output files are tagged with the
#' current MODE (e.g., "nantes", "paris", "pemb", "france") to avoid collisions
#' when running multiple regions.
#'
#' For regional predictions (Nantes, Paris, PEMB, sensors):
#'   - Single output file: 07_predictions_{mode}.gpkg
#'
#' For France-wide tiled predictions:
#'   - Geometry layer: 07_predictions_{mode}_network.gpkg
#'   - Temporal chunks: 07_predictions_{mode}_traffic_{CHUNK}.{ext}
#'       where {CHUNK} is DEN, hourly, hourly_wd, or hourly_we
#'       and {ext} is 'gpkg' for geometry, 'csv' for traffic data
#'
#' @param extent Character. Spatial extent: "sensors", "nantes", "paris", "pemb",
#'   or "france".
#' @param mode Character. Pipeline mode (typically from MODE variable: "nantes",
#'   "paris", "pemb", "france", "sensors", "all", or "test").  Default: "all".
#' @return List of file paths for the given extent.
build_prediction_filepaths <- function(extent, mode = NULL) {
  if (is.null(mode) || !nzchar(mode)) {
    mode <- if (exists("MODE") && nzchar(MODE)) MODE else "all"
  }

  switch(extent,
    sensors = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    nantes = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    paris = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    pemb = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    france = list(
      output_dir = file.path(PREDICTION_DIR, mode),
      geom = file.path(PREDICTION_DIR, mode,
                       sprintf("07_predictions_%s_network.gpkg", mode)),
      den = file.path(PREDICTION_DIR, mode,
                      sprintf("07_predictions_%s_traffic_DEN.csv.gz", mode)),
      hourly = file.path(PREDICTION_DIR, mode,
                         sprintf("07_predictions_%s_traffic_hourly.csv.gz", mode)),
      hourly_wd = file.path(PREDICTION_DIR, mode,
                            sprintf("07_predictions_%s_traffic_hourly_wd.csv.gz", mode)),
      hourly_we = file.path(PREDICTION_DIR, mode,
                            sprintf("07_predictions_%s_traffic_hourly_we.csv.gz", mode))
    ),
    stop("Unknown extent: ", extent, ". Valid: sensors, nantes, paris, pemb, france")
  )
}

# ------------------------------------------------------------------------------
# Fonctions de chargement et filtrage spatial
# ------------------------------------------------------------------------------

#' Load and crop France engineered network to bounding box
#'
#' @param bbox numeric vector c(xmin, ymin, xmax, ymax) in target CRS
#' @param cfg Configuration list
#' @return sf data.frame with cropped network
load_network_for_prediction <- function(bbox, cfg) {
  
  # Configuration parameters
  target_crs <- cfg$TARGET_CRS
  osm_roads_path <- cfg$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH
  default_vehicle_speed <- cfg$DEFAULT_VEHICLE_SPEED
  xgb_models_path <- cfg$XGB_MODELS_WITH_RATIOS_FILEPATH
  xgb_feature_path <- cfg$XGB_RATIO_FEATURE_INFO_FILEPATH

  pipeline_message(sprintf("Loading OSM France engineered network from %s", 
                           rel_path(osm_roads_path)), 
                   level = 1, progress = "start", process = "load")
  
  # Memory check before loading large GPKG
  check_memory_available(
    operation_name = "Load France engineered network (GPKG)",
    min_gb = 2, warn_gb = 4)
  
  # Use spatial filter at read time (wkt_filter) to avoid loading entire France
  # This is MUCH more memory-efficient than load-all-then-filter
  if (!is.null(bbox) && length(bbox) == 4) {
    xmin <- as.numeric(bbox[1])
    ymin <- as.numeric(bbox[2])
    xmax <- as.numeric(bbox[3])
    ymax <- as.numeric(bbox[4])
    if (anyNA(c(xmin, ymin, xmax, ymax))) {
      stop("Invalid bbox values for prediction crop: ",
           paste(bbox, collapse = ", "))
    }
    
    # Build WKT polygon for spatial filter at GDAL level
    wkt_bbox <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
                        xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin)
    
    osm_network <- sf::st_read(
      dsn = osm_roads_path,
      wkt_filter = wkt_bbox,
      quiet = TRUE)
    
    pipeline_message(sprintf("Network loaded with spatial filter: %s roads", 
                             fmt(nrow(osm_network))), 
                     process = "info")
  } else {
    # No bbox: load full network (warning: memory-intensive)
    pipeline_message(
      "No bbox provided — loading entire France network (memory-intensive)", 
      process = "warning")
    check_memory_available(
      operation_name = "Load entire France network (no bbox)",
      min_gb = 8, warn_gb = 12)
    osm_network <- sf::st_read(
      dsn = osm_roads_path,
      quiet = TRUE)
  }
  
  # Ensure correct CRS
  if (sf::st_crs(osm_network) != target_crs) {
    osm_network <- osm_network %>% 
      st_transform(crs = target_crs)
  }

  if (nrow(osm_network) == 0) {
    stop("No roads found for prediction after cropping. Check bbox/CRS.")
  }
  
  pipeline_message(sprintf("Network loaded: %s roads", fmt(nrow(osm_network))), 
                   level = 1, progress = "end", process = "valid")
  
  return(osm_network)
}

#' Load and filter network by point buffers
#'
#' @param points sf data.frame with point geometries
#' @param buffer_radius numeric buffer radius in meters
#' @param config CONFIG list with file paths
#' @return sf data.frame with filtered network
load_network_around_points <- function(points, buffer_radius, config) {
  pipeline_message(sprintf("Loading network within %sm of %s points", 
                           buffer_radius, nrow(points)), 
                   level = 1, progress = "start", process = "load")
  
  # Memory check
  check_memory_available(
    operation_name = "Load network around sensor points",
    min_gb = 2, warn_gb = 4)
  
  # Ensure points CRS
  if (sf::st_crs(points) != target_crs) {
    points <- points %>% 
      st_transform(crs = target_crs)
  }
  
  # Compute bounding box of all points + buffer for efficient GPKG read
  pts_bbox <- sf::st_bbox(points)
  wkt_bbox <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
                      pts_bbox["xmin"] - buffer_radius,
                      pts_bbox["ymin"] - buffer_radius,
                      pts_bbox["xmax"] + buffer_radius,
                      pts_bbox["ymin"] - buffer_radius,
                      pts_bbox["xmax"] + buffer_radius,
                      pts_bbox["ymax"] + buffer_radius,
                      pts_bbox["xmin"] - buffer_radius,
                      pts_bbox["ymax"] + buffer_radius,
                      pts_bbox["xmin"] - buffer_radius,
                      pts_bbox["ymin"] - buffer_radius)
  
  # Read only the bbox region from GPKG (much faster + less memory)
  osm_network <- sf::st_read(
    dsn = osm_roads_path,
    wkt_filter = wkt_bbox,
    quiet = TRUE)
  
  # Ensure correct CRS
  if (sf::st_crs(osm_network) != target_crs) {
    osm_network <- osm_network %>% 
      st_transform(crs = target_crs)
  }
  
  # Create buffers and filter precisely
  buffers <- sf::st_buffer(points, dist = buffer_radius)
  combined_buffer <- sf::st_union(buffers)
  osm_network <- sf::st_filter(osm_network, combined_buffer)
  
  pipeline_message(sprintf("Network filtered: %s roads within buffers", 
                           fmt(nrow(osm_network))), 
                   level = 1, progress = "end", process = "valid")
  
  return(osm_network)
}

# ------------------------------------------------------------------------------
# Fonctions de prédiction
# ------------------------------------------------------------------------------

#' Apply XGBoost models to network data
#'
#' @param network_data data.frame with engineered features
#' @param models_list list of trained XGBoost models
#' @param feature_info list with feature formula and periods
#' @return data.frame with predictions for all periods
apply_xgboost_predictions <- function(network_data, models_list, feature_info, default_vehicle_speed = 50) {
  pipeline_message("Applying XGBoost models to network", 
                   level = 1, progress = "start", process = "calc")
  
  # Memory check: predictions will create ~n_roads × n_periods × 3 columns
  n_roads <- nrow(network_data)
  n_periods <- length(feature_info$all_periods)
  est_mb <- round(n_roads * n_periods * 3 * 8 / 1024^2)  # 8 bytes per double
  check_memory_available(
    operation_name = sprintf("XGBoost prediction (%s roads × %d periods, ~%d MB result)",
                             fmt(n_roads), n_periods, est_mb),
    min_gb = 1, warn_gb = 3)

  # lane_number is an AVATAR-derived directional lane feature used in training.
  # For region-wide prediction (no AVATAR), derive a proxy from OSM lanes.
  if (!"lane_number" %in% names(network_data)) {
    if ("lanes_directional" %in% names(network_data)) {
      network_data$lane_number <- suppressWarnings(as.numeric(network_data$lanes_directional))
    } else if ("lanes_osm" %in% names(network_data)) {
      lanes_raw <- suppressWarnings(as.numeric(network_data$lanes_osm))
      network_data$lane_number <- pmax(1, round(lanes_raw / 2))
    } else {
      network_data$lane_number <- 1
    }
    network_data$lane_number[is.na(network_data$lane_number) | !is.finite(network_data$lane_number)] <- 1
    pipeline_message("lane_number missing in prediction input; derived proxy from OSM lane attributes", 
                     process = "info")
  }

  pipeline_message(sprintf("Constructing feature matrix for %s roads", fmt(n_roads)), 
                   level = 2, progress = "start", process = "calc")

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
  
  # Prepare feature matrix (same encoding as training: sparse.model.matrix)
  rownames(network_data) <- seq_len(nrow(network_data))
  feature_matrix_part <- safe_sparse_model_matrix(
    formula_obj = feature_info$road_feature_formula,
    data_df = network_data)
  rows_used <- as.integer(rownames(feature_matrix_part))
  if (length(rows_used) == 0 || anyNA(rows_used)) {
    rows_used <- seq_len(nrow(feature_matrix_part))
  }
  rows_used <- rows_used[rows_used >= 1 & rows_used <= nrow(network_data)]
  feature_matrix <- matrix(
    NA_real_,
    nrow = nrow(network_data),
    ncol = ncol(feature_matrix_part),
    dimnames = list(rownames(network_data), colnames(feature_matrix_part))
  )
  if (length(rows_used) != nrow(feature_matrix_part)) {
    # Fallback alignment when sparse.model.matrix rownames are unavailable
    n_common <- min(length(rows_used), nrow(feature_matrix_part))
    rows_used <- rows_used[seq_len(n_common)]
    feature_matrix[rows_used, ] <- as.matrix(feature_matrix_part[seq_len(n_common), , drop = FALSE])
  } else {
    feature_matrix[rows_used, ] <- as.matrix(feature_matrix_part)
  }
  feature_matrix <- as.data.frame(feature_matrix, stringsAsFactors = FALSE)
  
  pipeline_message(sprintf("Feature matrix constructed with %d rows and %d features", 
                           nrow(feature_matrix), ncol(feature_matrix)), 
                   level = 2, progress = "end", process = "valid")

  # Helper: align feature matrix to a model's expected features and predict
  # Uses the EXACT column names from training stored in feature_info
  # Handles factor level mismatches between training and prediction data
  predict_with_alignment <- function(model_entry, feature_matrix_base, feature_info) {

    # "model_entry" may be either the raw xgb.Booster object (old behaviour)
    # or the list entry stored in models_list (which contains both the booster
    # and the saved feature_names).  This wrapper normalises input.
    if (is.list(model_entry) && !is.null(model_entry$model)) {
      model_obj <- model_entry$model
      model_features <- model_entry$feature_names
    } else {
      model_obj <- model_entry
      # Booster objects do not carry feature_names, so leave NULL.
      model_features <- NULL
    }
    training_features <- feature_info$feature_names_from_training
    
    # Start with base matrix (remove Intercept if present)
    fm <- feature_matrix_base
    if ("(Intercept)" %in% colnames(fm)) {
      fm <- fm[, colnames(fm) != "(Intercept)", drop = FALSE]
    }
    
    # Prefer the per-model feature list if available; this ensures each
    # XGBoost booster receives exactly the columns it was trained with.
    target_features <- NULL
    target_source <- NULL
    if (!is.null(model_features) && length(model_features) > 0) {
      target_features <- model_features
      target_source <- "model"
    } else if (!is.null(training_features) && length(training_features) > 0) {
      target_features <- training_features
      target_source <- "feature_info"
    }
    
    # Warn if both sources are present but differ substantially
    if (!is.null(model_features) && !is.null(training_features) &&
        length(model_features) != length(training_features)) {
      pipeline_message(
        sprintf("Note: model has %d features but feature_info lists %d features; using model list", 
                length(model_features), length(training_features)),
        level = 2, process = "info")
    }
    
    if (!is.null(target_features) && length(target_features) > 0) {
      fm_cols <- colnames(fm)
      
      # Step 1: Add missing columns
      missing_cols <- setdiff(target_features, fm_cols)
      if (length(missing_cols) > 0) {
        for (mc in missing_cols) {
          fm[[mc]] <- 0
        }
        pipeline_message(
          sprintf("⚠️ Added %d missing feature columns with zero values", 
                  length(missing_cols)),
          level = 2, process = "info")
      }
      
      # Step 2: Identify and handle EXTRA columns
      extra_cols <- setdiff(fm_cols, target_features)
      if (length(extra_cols) > 0) {
        # Report extra columns (could indicate factor level mismatch)
        extra_summary <- paste(head(extra_cols, 5), collapse = ", ")
        if (length(extra_cols) > 5) {
          extra_summary <- paste0(extra_summary, " ... (", length(extra_cols) - 5, " more)")
        }
        pipeline_message(
          sprintf("⚠️ Removing %d extra feature columns (factor level mismatch?): %s", 
                  length(extra_cols), extra_summary),
          level = 2, process = "warning")
      }
      
      # Step 3: Select and reorder columns to match target exactly
      fm <- fm[, target_features, drop = FALSE]
      
      pipeline_message(
        sprintf("Feature matrix aligned (%s): %d columns (base %d, aligned %d)",
                target_source, ncol(fm), ncol(feature_matrix_base), 
                length(target_features)),
        level = 2, process = "info")
    } else {
      pipeline_message(
        "⚠️ No training feature names available; using raw feature matrix (may cause dimension errors)",
        level = 2, process = "warning")
    }
    
    # Create DMatrix and predict
    dmat <- xgboost::xgb.DMatrix(data = as.matrix(fm))
    predict(model_obj, dmat)
  }
  
  # Predict base models (period D)
  if (is.null(models_list$flow_D$model)) {
    stop("Missing base model for period D: flow_D")
  }
  flow_D <- predict_with_alignment(models_list$flow_D, feature_matrix, feature_info)
  truck_pct_D <- if (is.null(models_list$truck_pct_D$model)) {
    pipeline_message("Missing base model truck_pct_D: outputs will be NA", 
                     process = "warning")
    rep(NA_real_, length(flow_D))
  } else {
    predict_with_alignment(models_list$truck_pct_D, feature_matrix, feature_info)
  }
  speed_model_target <- NA_character_
  if (!is.null(models_list$speed_D$config) &&
      !is.null(models_list$speed_D$config$target)) {
    speed_model_target <- as.character(models_list$speed_D$config$target)
  }

  speed_D_raw <- if (is.null(models_list$speed_D$model)) {
    pipeline_message("Missing base model speed_D: outputs will be NA", 
                     process = "warning")
    rep(NA_real_, length(flow_D))
  } else {
    predict_with_alignment(models_list$speed_D, feature_matrix, feature_info)
  }

  speed_osm_raw <- suppressWarnings(as.numeric(network_data$speed))
  speed_osm_missing <- is.na(speed_osm_raw) | speed_osm_raw <= 0

  if (identical(speed_model_target, "ratio_speed_to_osm")) {
    # New model: speed_D predicts a ratio to OSM speed.
    speed_osm_base <- speed_osm_raw
    speed_osm_base[speed_osm_missing] <- default_vehicle_speed
    speed_D <- if (all(is.na(speed_D_raw))) {
      rep(NA_real_, length(speed_D_raw))
    } else {
      pmax(5, speed_D_raw * speed_osm_base)
    }
  } else {
    # Legacy model: speed_D already predicts absolute speed (km/h).
    speed_D <- speed_D_raw
  }

  # Keep OSM speed as separate attribute for downstream model choice.
  # If OSM speed is missing/invalid, impute with reconstructed speed_D.
  speed_osm <- speed_osm_raw
  n_speed_osm_missing <- sum(speed_osm_missing)
  if (n_speed_osm_missing > 0) {
    pipeline_message(
      sprintf("Prediction-time OSM speed imputation: %s missing values imputed with %s", 
              fmt(n_speed_osm_missing), 
              ifelse(test = all(is.na(speed_D)), 
                     yes = "DEFAULT_VEHICLE_SPEED", 
                     no = "speed_D XGBoost predictions")),
      process = "warning"
    )
    if (all(is.na(speed_D))) {
      speed_osm[speed_osm_missing] <- default_vehicle_speed
    } else {
      speed_osm[speed_osm_missing] <- pmax(5, speed_D[speed_osm_missing])
    }
  }
  
  # Initialize results data.frame
  results <- data.frame(
    osm_id = network_data$osm_id,
    highway = network_data$highway,
    osm_speed = speed_osm,
    osm_speed_imputed = as.integer(speed_osm_missing)
  )
  
  # Predict all periods
  for (period in feature_info$all_periods) {
    if (period == "D") {
      results[[paste0("flow_", period)]] <- 10^flow_D  # Inverse log10
      results[[paste0("truck_pct_", period)]] <- truck_pct_D
      results[[paste0("speed_", period)]] <- speed_D
    } else {
      # Predict ratios
      ratio_flow_model <- models_list[[paste0("ratio_flow_", period)]]$model
      ratio_truck_model <- models_list[[paste0("ratio_truck_pct_", period)]]$model
      ratio_speed_model <- models_list[[paste0("ratio_speed_", period)]]$model
      
      ratio_flow <- if (is.null(ratio_flow_model)) {
        rep(NA_real_, length(flow_D))
      } else {
        predict_with_alignment(models_list[[paste0("ratio_flow_", period)]], feature_matrix, feature_info)
      }
      ratio_truck_pct <- if (all(is.na(truck_pct_D))) {
        rep(NA_real_, length(truck_pct_D))
      } else if (is.null(ratio_truck_model)) {
        rep(NA_real_, length(truck_pct_D))
      } else {
        predict_with_alignment(models_list[[paste0("ratio_truck_pct_", period)]], feature_matrix, feature_info)
      }
      ratio_speed <- if (all(is.na(speed_D))) {
        rep(NA_real_, length(speed_D))
      } else if (is.null(ratio_speed_model)) {
        rep(NA_real_, length(speed_D))
      } else {
        predict_with_alignment(models_list[[paste0("ratio_speed_", period)]], feature_matrix, feature_info)
      }
      
      # Apply ratios to base predictions
      results[[paste0("flow_", period)]] <- (10^flow_D) * ratio_flow
      results[[paste0("truck_pct_", period)]] <- truck_pct_D * ratio_truck_pct
      results[[paste0("speed_", period)]] <- speed_D * ratio_speed
    }
  }

  # Clamp predictions to sensible ranges
  highway_chr <- tolower(as.character(results$highway))
  speed_min_by_hw <- c(
    motorway = 30, trunk = 20, primary = 15, secondary = 15, tertiary = 12,
    residential = 10, unclassified = 10, service = 5, living_street = 5
  )
  speed_max_by_hw <- c(
    motorway = 130, trunk = 110, primary = 90, secondary = 80, tertiary = 70,
    residential = 50, unclassified = 60, service = 40, living_street = 30
  )
  flow_max_by_hw <- c(
    motorway = 12000, trunk = 9000, primary = 7000, secondary = 5000,
    tertiary = 3000, residential = 1200, unclassified = 1500,
    service = 600, living_street = 300
  )
  speed_min_vec <- as.numeric(speed_min_by_hw[highway_chr])
  speed_max_vec <- as.numeric(speed_max_by_hw[highway_chr])
  flow_max_vec <- as.numeric(flow_max_by_hw[highway_chr])
  speed_min_vec[is.na(speed_min_vec)] <- 5
  speed_max_vec[is.na(speed_max_vec)] <- 130
  flow_max_vec[is.na(flow_max_vec)] <- 15000

  flow_cols <- grep("^flow_", names(results), value = TRUE)
  if (length(flow_cols) > 0) {
    results[flow_cols] <- lapply(
      results[flow_cols],
      function(x) pmin(flow_max_vec, pmax(0, x))
    )
  }
  truck_cols <- grep("^truck_pct_", names(results), value = TRUE)
  if (length(truck_cols) > 0) {
    results[truck_cols] <- lapply(results[truck_cols], function(x) pmin(100, pmax(0, x)))
  }
  speed_cols <- grep("^speed_", names(results), value = TRUE)
  if (length(speed_cols) > 0) {
    results[speed_cols] <- lapply(
      results[speed_cols],
      function(x) pmin(speed_max_vec, pmax(speed_min_vec, x))
    )
  }
  
  # Guard NaN/Inf: impute with median by highway type + emit warning
  pred_cols <- c(flow_cols, truck_cols, speed_cols)
  total_nan <- 0L
  for (col in pred_cols) {
    bad_idx <- which(is.nan(results[[col]]) | is.infinite(results[[col]]))
    if (length(bad_idx) > 0) {
      total_nan <- total_nan + length(bad_idx)
      # Impute each NaN with the median of same highway type
      global_med <- median(results[[col]][is.finite(results[[col]])], na.rm = TRUE)
      for (hw in unique(results$highway[bad_idx])) {
        hw_rows <- which(results$highway == hw)
        hw_good <- results[[col]][setdiff(hw_rows, bad_idx)]
        med_val <- if (length(hw_good) > 0) median(hw_good, na.rm = TRUE) else NA_real_
        if (!is.finite(med_val)) med_val <- global_med
        results[[col]][intersect(bad_idx, hw_rows)] <- med_val
      }
    }
  }
  if (total_nan > 0) {
    pipeline_message(
      sprintf("NaN/Inf detected in predictions: %d values imputed with median by highway type",
              total_nan),
      process = "warning")
  }
  
  pipeline_message(sprintf("Predictions completed for %s roads × %s periods", 
                           fmt(nrow(results)), length(feature_info$all_periods)), 
                   process = "valid")
  
  return(results)
}

#' Validate prediction outputs
#'
#' @param predictions data.frame with predicted values
#' @return list with validation results
validate_predictions <- function(predictions) {
  issues <- list()

  is_long_format <- all(c("period", "speed") %in% names(predictions))
  has_tv <- "TV" %in% names(predictions)
  has_truck <- "truck_pct" %in% names(predictions)
  
  if (is_long_format) {
    if (has_tv) {
      n_negative_tv <- sum(predictions$TV < 0, na.rm = TRUE)
      if (n_negative_tv > 0) {
        issues[["TV_negative"]] <- n_negative_tv
      }
    }

    if (has_truck) {
      n_exceed_truck <- sum(predictions$truck_pct > 100 | predictions$truck_pct < 0, na.rm = TRUE)
      if (n_exceed_truck > 0) {
        issues[["truck_pct_out_of_range"]] <- n_exceed_truck
      }
    }

    n_unrealistic_speed <- sum(predictions$speed < 5 | predictions$speed > 200, na.rm = TRUE)
    if (n_unrealistic_speed > 0) {
      issues[["speed_unrealistic"]] <- n_unrealistic_speed
    }

    # Temporal coherence checks (when D and N are available)
    if (has_tv && "osm_id" %in% names(predictions)) {
      d_rows <- predictions[as.character(predictions$period) == "D", c("osm_id", "TV", "speed")]
      n_rows <- predictions[as.character(predictions$period) == "N", c("osm_id", "TV", "speed")]
      names(d_rows) <- c("osm_id", "TV_D", "speed_D")
      names(n_rows) <- c("osm_id", "TV_N", "speed_N")
      dn <- merge(d_rows, n_rows, by = "osm_id", all = FALSE)
      if (nrow(dn) > 0) {
        n_flow_n_gt_d <- sum(dn$TV_N > (dn$TV_D * 1.05), na.rm = TRUE)
        if (n_flow_n_gt_d > 0) {
          issues[["coherence_flow_N_gt_D"]] <- n_flow_n_gt_d
        }
        n_speed_n_lt_d <- sum(dn$speed_N + 0.5 < dn$speed_D, na.rm = TRUE)
        if (n_speed_n_lt_d > 0) {
          issues[["coherence_speed_N_lt_D"]] <- n_speed_n_lt_d
        }
      }
    }

  } else {
    # Wide format checks
    flow_cols <- grep("^flow_", names(predictions), value = TRUE)
    for (col in flow_cols) {
      n_negative <- sum(predictions[[col]] < 0, na.rm = TRUE)
      if (n_negative > 0) {
        issues[[paste0(col, "_negative")]] <- n_negative
      }
    }

    truck_cols <- grep("^truck_pct_", names(predictions), value = TRUE)
    for (col in truck_cols) {
      n_exceed <- sum(predictions[[col]] > 100 | predictions[[col]] < 0, na.rm = TRUE)
      if (n_exceed > 0) {
        issues[[paste0(col, "_out_of_range")]] <- n_exceed
      }
    }

    speed_cols <- grep("^speed_", names(predictions), value = TRUE)
    for (col in speed_cols) {
      n_unrealistic <- sum(predictions[[col]] < 5 | predictions[[col]] > 200,
                           na.rm = TRUE)
      if (n_unrealistic > 0) {
        issues[[paste0(col, "_unrealistic")]] <- n_unrealistic
      }
    }

    # Temporal coherence for wide format
    if (all(c("flow_D", "flow_N") %in% names(predictions))) {
      n_flow_n_gt_d <- sum(predictions$flow_N > (predictions$flow_D * 1.05), na.rm = TRUE)
      if (n_flow_n_gt_d > 0) {
        issues[["coherence_flow_N_gt_D"]] <- n_flow_n_gt_d
      }
    }
    if (all(c("speed_D", "speed_N") %in% names(predictions))) {
      n_speed_n_lt_d <- sum(predictions$speed_N + 0.5 < predictions$speed_D, na.rm = TRUE)
      if (n_speed_n_lt_d > 0) {
        issues[["coherence_speed_N_lt_D"]] <- n_speed_n_lt_d
      }
    }
  }
  
  list(
    is_valid = length(issues) == 0,
    issues = issues,
    n_rows = nrow(predictions)
  )
}

#' Add QGIS-friendly datetime columns from period labels
#'
#' Mapping rules:
#' - D/E/N -> day 01 of base year (D=06-18h, E=18-22h, N=22-06h)
#' - h0_wd..h23_wd -> day 02 of base year (weekdays)
#' - h0_we..h23_we -> day 03 of base year (weekends)
#' - h0..h23 -> day 04 of base year (generic hourly)
#' 
#' Base year is extracted from the first measure_datetime in AVATAR CSV data.
#'
#' @param predictions_long data.frame with a `period` column
#' @param cfg Configuration list (optional, for AVATAR_CSV_DIR)
#' @return data.frame with added `datetimestart` and `datetimeend` POSIXct columns
add_period_datetime_columns <- function(predictions_long, cfg = NULL) {
  if (!"period" %in% names(predictions_long)) {
    return(predictions_long)
  }

  period_chr <- as.character(predictions_long$period)
  n <- length(period_chr)

  datetimestart <- as.POSIXct(rep(NA_character_, n), tz = "UTC")
  datetimeend <- as.POSIXct(rep(NA_character_, n), tz = "UTC")

  # Get base year from AVATAR data
  avatar_dir <- if (!is.null(cfg) && !is.null(cfg$AVATAR_CSV_DIR)) cfg$AVATAR_CSV_DIR else file.path("data", "avatar", "csv")
  base_year <- 2023  # fallback
  if (dir.exists(avatar_dir)) {
    csv_files <- list.files(avatar_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) > 0) {
      first_csv <- csv_files[1]
      # Read first data line (skip header)
      con <- file(first_csv, "r")
      header <- readLines(con, n = 1)
      first_data_line <- readLines(con, n = 1)
      close(con)
      if (length(first_data_line) > 0) {
        # Split by comma, find measure_datetime column
        cols <- strsplit(header, ",")[[1]]
        data_cols <- strsplit(first_data_line, ",")[[1]]
        measure_idx <- which(cols == "measure_datetime")
        if (length(measure_idx) == 1) {
          measure_datetime <- data_cols[measure_idx]
          # Extract date part: 2023-01-01T00:00:00+01:00 -> 2023-01-01
          date_str <- substr(measure_datetime, 1, 10)
          base_year <- as.integer(format(as.Date(date_str), "%Y"))
        }
      }
    }
  }

  # D / E / N reference periods (day 01)
  idx_D <- which(period_chr == "D")
  if (length(idx_D) > 0) {
    datetimestart[idx_D] <- as.POSIXct(sprintf("%d-01-01 06:00:00", base_year), tz = "UTC")
    datetimeend[idx_D] <- as.POSIXct(sprintf("%d-01-01 18:00:00", base_year), tz = "UTC")
  }

  idx_E <- which(period_chr == "E")
  if (length(idx_E) > 0) {
    datetimestart[idx_E] <- as.POSIXct(sprintf("%d-01-01 18:00:00", base_year), tz = "UTC")
    datetimeend[idx_E] <- as.POSIXct(sprintf("%d-01-01 22:00:00", base_year), tz = "UTC")
  }

  idx_N <- which(period_chr == "N")
  if (length(idx_N) > 0) {
    datetimestart[idx_N] <- as.POSIXct(sprintf("%d-01-01 22:00:00", base_year), tz = "UTC")
    datetimeend[idx_N] <- as.POSIXct(sprintf("%d-01-02 06:00:00", base_year), tz = "UTC")
  }

  # Generic hourly periods h0..h23 (day 04)
  m_h <- regexec("^h([0-9]{1,2})$", period_chr)
  g_h <- regmatches(period_chr, m_h)
  idx_h <- which(lengths(g_h) == 2)
  if (length(idx_h) > 0) {
    h_vals <- as.integer(vapply(g_h[idx_h], function(x) x[2], character(1)))
    start_str <- sprintf("%d-01-04 %02d:00:00", base_year, h_vals)
    datetimestart[idx_h] <- as.POSIXct(start_str, tz = "UTC")
    datetimeend[idx_h] <- datetimestart[idx_h] + 3600
  }

  # Weekday hourly periods h0_wd..h23_wd (day 02)
  m_wd <- regexec("^h([0-9]{1,2})_wd$", period_chr)
  g_wd <- regmatches(period_chr, m_wd)
  idx_wd <- which(lengths(g_wd) == 2)
  if (length(idx_wd) > 0) {
    h_vals <- as.integer(vapply(g_wd[idx_wd], function(x) x[2], character(1)))
    start_str <- sprintf("%d-01-02 %02d:00:00", base_year, h_vals)
    datetimestart[idx_wd] <- as.POSIXct(start_str, tz = "UTC")
    datetimeend[idx_wd] <- datetimestart[idx_wd] + 3600
  }

  # Weekend hourly periods h0_we..h23_we (day 03)
  m_we <- regexec("^h([0-9]{1,2})_we$", period_chr)
  g_we <- regmatches(period_chr, m_we)
  idx_we <- which(lengths(g_we) == 2)
  if (length(idx_we) > 0) {
    h_vals <- as.integer(vapply(g_we[idx_we], function(x) x[2], character(1)))
    start_str <- sprintf("%d-01-03 %02d:00:00", base_year, h_vals)
    datetimestart[idx_we] <- as.POSIXct(start_str, tz = "UTC")
    datetimeend[idx_we] <- datetimestart[idx_we] + 3600
  }

  predictions_long$datetimestart <- datetimestart
  predictions_long$datetimeend <- datetimeend
  predictions_long
}

# ==============================================================================
# GENERIC TRAFFIC PREDICTION (regional and tiled)
# ==============================================================================

#' Run traffic prediction for a geographic region with optional spatial tiling
#'
#' This function provides a unified interface for traffic prediction that
#' automatically handles both small regions (simple single-file export) and
#' large regions like France (memory-efficient tiling with temporal chunking).
#'
#' For small regions (bbox-based), the function:
#'   - Loads the engineered network cropped to the bounding box
#'   - Applies XGBoost models to predict flow, truck percentage, and speed
#'   - Exports all periods to a single GeoPackage file
#'
#' For large regions (tiled method), the function:
#'   - Partitions the domain into a regular grid of square tiles
#'   - Processes each tile independently using spatial filters (memory-friendly)
#'   - Optionally splits traffic attributes into separate GPKG files by
#'     temporal chunk (e.g., separate files for D/E/N, hourly, weekday,
#'     weekend periods)
#'   - Writes geometry once, appending traffic data incrementally
#'
#' Temporal chunks (relevant for tiled method only):
#'   - "DEN": D, E, N periods (day/evening/night)
#'   - "hourly": h0..h23 (all days combined)
#'   - "hourly_wd": h0_wd..h23_wd (weekdays only)
#'   - "hourly_we": h0_we..h23_we (weekend only)
#'
#' @param region_name Character. Human-readable region name for log messages.
#' @param cfg Configuration list with model paths and settings.
#' @param bbox Named numeric vector c(xmin, ymin, xmax, ymax) in EPSG:2154,
#'   or NULL. If NULL, triggers tiled method; if provided, triggers
#'   simple region method.
#' @param output_config List with output file paths. Structure depends on method:
#'   - Simple region: list(filepath = "path/to/output.gpkg")
#'   - Tiled: list(geom = "path/to/geom.gpkg",
#'              den = "path/to/traffic_DEN.gpkg",
#'              hourly = "path/..._hourly.gpkg",
#'              hourly_wd = "path/..._hourly_wd.gpkg",
#'              hourly_we = "path/..._hourly_we.gpkg")
#' @param method Character. Prediction approach: "region" (simple), "tiled"
#'   (spatial tiles), or "auto" (auto-detect based on bbox presence).
#'   Default: "auto".
#' @param chunks Character vector. Temporal chunks to export (tiled method only).
#'   Valid values: "DEN", "hourly", "hourly_wd", "hourly_we".
#'   Default: c("DEN") for memory efficiency.
#'   For full export use: c("DEN", "hourly", "hourly_wd", "hourly_we").
#' @param tile_size_m Numeric. Tile side length in meters for tiled method.
#'   Default: 200000 (200 km).
#'
#' @return Invisible NULL. Side effects: writes GPKG file(s) to disk.
#'
#' @examples
#' \dontrun{
#' # Simple region prediction (e.g., PEMB)
#' predict_traffic(
#'   region_name = "PEMB",
#'   cfg = CFG,
#'   bbox = c(xmin = 654892, ymin = 6852748, xmax = 671006, ymax = 6862393),
#'   output_config = list(filepath = "data/prediction/pemb.gpkg"),
#'   method = "region"
#' )
#'
#' # Tiled prediction for France (all temporal chunks)
#' predict_traffic(
#'   region_name = "France",
#'   cfg = CFG,
#'   bbox = NULL,
#'   output_config = list(
#'     geom = "data/prediction/france/geometry.gpkg",
#'     den = "data/prediction/france/traffic_DEN.gpkg",
#'     hourly = "data/prediction/france/traffic_hourly.gpkg",
#'     hourly_wd = "data/prediction/france/traffic_hourly_wd.gpkg",
#'     hourly_we = "data/prediction/france/traffic_hourly_we.gpkg"
#'   ),
#'   method = "tiled",
#'   chunks = c("DEN", "hourly", "hourly_wd", "hourly_we"),
#'   tile_size_m = 200000
#' )
#' }
predict_traffic <- function(region_name, cfg, bbox = NULL,
                           output_config = NULL,
                           method = "auto",
                           chunks = c("DEN"),
                           tile_size_m = 200000) {

  # --- Auto-detect method ---
  if (method == "auto") {
    method <- if (is.null(bbox)) "tiled" else "region"
  }

  if (!(method %in% c("region", "tiled"))) {
    stop("method must be one of: 'region', 'tiled', 'auto'")
  }

  # --- Validate output_config ---
  if (is.null(output_config) || !is.list(output_config)) {
    stop("output_config must be a non-empty list")
  }

  # --- Route to appropriate method ---
  if (method == "region") {
    if (is.null(bbox)) {
      stop("bbox must be provided for method='region'")
    }
    if (is.null(output_config$filepath)) {
      stop("output_config must contain 'filepath' for method='region'")
    }
    .predict_region_impl(
      region_name = region_name,
      bbox = bbox,
      output_filepath = output_config$filepath,
      cfg = cfg
    )
  } else if (method == "tiled") {
    if (!is.null(bbox)) {
      pipeline_message(
        "Warning: bbox is ignored when method='tiled'; use method='region' for bbox-based prediction",
        process = "warning")
    }
    .predict_france_tiled_impl(
      cfg = cfg,
      region_name = region_name,
      output_config = output_config,
      tile_size_m = tile_size_m,
      chunks = chunks
    )
  }

  invisible(NULL)
}

# ==============================================================================
# GENERIC REGION PREDICTION (INTERNAL IMPLEMENTATION)
# ==============================================================================

#' Internal: Simple region prediction
#'
#' @keywords internal
.predict_region_impl <- function(region_name, bbox, output_filepath, cfg) {

  pipeline_message(sprintf("%s traffic prediction", region_name), level = 0, 
                   progress = "start", process = "calc")
  
  # Configuration parameters
  target_crs <- cfg$TARGET_CRS
  xgb_models_path <- cfg$XGB_MODELS_WITH_RATIOS_FILEPATH
  xgb_feature_path <- cfg$XGB_RATIO_FEATURE_INFO_FILEPATH

  # --- Load models ---
  pipeline_message("Loading trained XGBoost models", level = 1, 
                   progress = "start", process = "load")

  if (!file.exists(xgb_models_path)) {
    pipeline_message(sprintf("Models not found: %s", 
                             rel_path(xgb_models_path)), 
                     process = "stop")
  }

  models_list <- readRDS(xgb_models_path)
  feature_info <- readRDS(xgb_feature_path)

  pipeline_message(sprintf("Models loaded: %s models for %s periods", 
                           length(models_list), 
                           length(feature_info$all_periods)), 
                   level = 1, progress = "end", process = "valid")

  # --- Bbox ---
  pipeline_message(sprintf("Bbox: [%s, %s, %s, %s]", 
                           bbox[1], bbox[2], bbox[3], bbox[4]), 
                   process = "info")

  # --- Load network ---
  osm_region <- load_network_for_prediction(bbox = bbox, cfg = cfg)

  pipeline_message(sprintf("Network loaded: %s roads in %s", 
                           fmt(nrow(osm_region)), region_name), 
                   process = "info")

  # --- Apply predictions ---
  pipeline_message(sprintf("Applying XGBoost models to %s network", region_name), 
                   level = 1, progress = "start", process = "calc")

  osm_region_dt <- as.data.frame(sf::st_drop_geometry(osm_region))

  predictions_wide <- apply_xgboost_predictions(
    network_data = osm_region_dt,
    models_list = models_list,
    feature_info = feature_info,
    default_vehicle_speed = cfg$DEFAULT_VEHICLE_SPEED)

  pipeline_message(sprintf("Predictions completed: %s roads x %s periods", 
                           fmt(nrow(predictions_wide)), 
                           length(feature_info$all_periods)), 
                   level = 1, progress = "end", process = "valid")

  all_periods <- feature_info$all_periods
  rm(models_list, feature_info, osm_region_dt)
  gc(verbose = FALSE)

  # --- Long format ---
  pipeline_message("Converting to long format", level = 1, 
                   progress = "start", process = "calc")

  check_memory_available(
    operation_name = sprintf("Pivot to long format (%s roads)",
                             fmt(nrow(predictions_wide))),
    min_gb = 2, warn_gb = 4)

  dt <- data.table::as.data.table(predictions_wide)

  flow_cols <- grep("^flow_", names(dt), value = TRUE)
  truck_cols <- grep("^truck_pct_", names(dt), value = TRUE)
  speed_cols <- grep("^speed_", names(dt), value = TRUE)

  flow_long <- data.table::melt(
    dt,
    id.vars = c("osm_id","highway","osm_speed","osm_speed_imputed"),
    measure.vars = flow_cols,
    variable.name = "period",
    value.name = "flow")
  
  flow_long[, period := sub(pattern = "^flow_", 
                            replacement = "", 
                            x = period)]

  truck_long <- data.table::melt(
    dt,
    id.vars = c("osm_id"),
    measure.vars = truck_cols,
    variable.name = "period",
    value.name = "truck_pct")

  truck_long[, period := sub(pattern = "^truck_pct_", 
                             replacement = "", 
                             x = period)]

  speed_long <- data.table::melt(
    dt,
    id.vars = c("osm_id"),
    measure.vars = speed_cols,
    variable.name = "period",
    value.name = "speed")

  speed_long[, period := sub(pattern = "^speed_", 
                            replacement = "", 
                            x =  period)]

  predictions_long <- flow_long[
    truck_long, on = c("osm_id","period")
  ][
    speed_long, on = c("osm_id","period")
  ]

  predictions_long[, `:=`(
    HGV = flow * (truck_pct/100),
    LV  = flow - HGV,
    TV  = flow
  )]

  predictions_long[, period := factor(period, levels = all_periods)]

  predictions_long <- predictions_long[
    , .(osm_id, highway, period, TV, HGV, LV, speed,
        osm_speed, osm_speed_imputed, truck_pct)
  ]

  predictions_long <- add_period_datetime_columns(predictions_long, cfg)

  rm(predictions_wide)
  gc(verbose = FALSE)

  pipeline_message(sprintf("Long format: %s rows (roads x periods)", 
                           fmt(nrow(predictions_long))), 
                   level = 1, progress = "end", process = "valid")

  # --- Validate ---
  validation <- validate_predictions(predictions_long)
  if (!validation$is_valid) {
    pipeline_message(sprintf("Validation warnings: %s issues detected", 
                             length(validation$issues)), 
                             process = "warning")
    for (issue_name in names(validation$issues)) {
      pipeline_message(sprintf("  - %s: %s cases", 
                               issue_name, validation$issues[[issue_name]]), 
                       process = "warning")
    }
  }

  # --- Export ---
  pipeline_message("Exporting predictions with geometry", level = 1, 
                   progress = "start", process = "save")

  check_memory_available(
    operation_name = sprintf("Geometry merge (%s rows)",
                             fmt(nrow(predictions_long))),
    min_gb = 2, warn_gb = 4)

  predictions_sf <- merge(
    predictions_long,
    osm_region[, c("osm_id", "name", "geom")],
    by = "osm_id", all.x = TRUE)

  predictions_sf <- sf::st_as_sf(predictions_sf)

  if (sf::st_crs(predictions_sf) != target_crs) {
    predictions_sf <- sf::st_transform(predictions_sf, target_crs)
  }

  predictions_sf <- add_period_datetime_columns(predictions_sf, cfg)

  # add explicit logging around the write so duration is recorded
  pipeline_message("Writing GeoPackage file", level = 1,
                   progress = "start", process = "save")
  sf::st_write(
    obj = predictions_sf,
    dsn = output_filepath,
    delete_dsn = TRUE,
    quiet = FALSE)
  pipeline_message(sprintf("Written GeoPackage to %s",
                           rel_path(output_filepath)),
                   level = 1, progress = "end", process = "save")

  pipeline_message(sprintf("Predictions exported to %s",
                           rel_path(output_filepath)), 
                   level = 1, progress = "info", process = "save")

  # --- Summary ---
  pipeline_message(sprintf("Prediction summary for %s:", region_name),
                   process = "info")
  pipeline_message(sprintf("  - Roads: %s", 
                           fmt(length(unique(predictions_long$osm_id)))), 
                   process = "info")
  pipeline_message(sprintf("  - Periods: %s", length(all_periods)), 
                   process = "info")
  pipeline_message(sprintf("  - Total predictions: %s", 
                           fmt(nrow(predictions_long))), 
                   process = "info")

  period_stats <- predictions_long %>%
    group_by(period) %>%
    summarise(
      avg_TV = round(mean(TV, na.rm = TRUE)),
      avg_speed = round(mean(speed, na.rm = TRUE), 1),
      avg_truck_pct = round(mean(truck_pct, na.rm = TRUE), 1),
      .groups = "drop"
    )

  for (p in c("D", "E", "N", "h7", "h12", "h18")) {
    if (p %in% period_stats$period) {
      stats <- period_stats[period_stats$period == p, ]
      pipeline_message(
        sprintf("  - Period %s: %d veh/h avg, %.1f km/h, %.1f%% trucks",
                       p, stats$avg_TV, stats$avg_speed, stats$avg_truck_pct),
                process = "info")
    }
  }

  pipeline_message(sprintf("%s prediction completed", region_name),
                   level = 0, progress = "end", process = "valid")

  invisible(NULL)
}

# ==============================================================================
# FRANCE-WIDE TILED PREDICTION (GEOMETRY-SEPARATED)
# ==============================================================================
#
# Architecture:
#   - Geometry layer:  07_france_network.gpkg  (osm_id + road attributes + geom)
#   - Traffic data:    07_france_traffic_DEN.gpkg        (D, E, N — 3 periods)
#                      07_france_traffic_hourly.gpkg     (h0..h23 — 24 periods)
#                      07_france_traffic_hourly_wd.gpkg  (h0_wd..h23_wd — 24 periods)
#                      07_france_traffic_hourly_we.gpkg  (h0_we..h23_we — 24 periods)
#
# Geometry is written ONCE; traffic data files are attribute-only tables keyed
# by osm_id. Users join in QGIS / PostGIS / R as needed.
#
# Spatial tiling avoids loading all 4M+ roads into RAM at once.
# ==============================================================================

#' Define temporal chunk groups
#' @return Named list of character vectors (period names per chunk)
get_temporal_chunks <- function() {
  list(
    DEN        = c("D", "E", "N"),
    hourly     = paste0("h", 0:23),
    hourly_wd  = paste0("h", 0:23, "_wd"),
    hourly_we  = paste0("h", 0:23, "_we")
  )
}

#' Build spatial tile grid covering France extent
#'
#' @param tile_size_m Tile side length in meters (Lambert-93)
#' @return data.frame with columns: tile_id, xmin, ymin, xmax, ymax
build_france_tiles <- function(tile_size_m = 200000) {
  # France extent in EPSG:2154 (Lambert-93) — rounded outward
  france_xmin <- 100000
  france_ymin <- 6050000
  france_xmax <- 1250000
  france_ymax <- 7150000

  xs <- seq(france_xmin, france_xmax, by = tile_size_m)
  ys <- seq(france_ymin, france_ymax, by = tile_size_m)

  tiles <- expand.grid(x = xs, y = ys, stringsAsFactors = FALSE)
  tiles$tile_id <- seq_len(nrow(tiles))
  tiles$xmin <- tiles$x
  tiles$ymin <- tiles$y
  tiles$xmax <- tiles$x + tile_size_m
  tiles$ymax <- tiles$y + tile_size_m
  tiles[, c("tile_id", "xmin", "ymin", "xmax", "ymax")]
}


#' (Internal) Run France-wide prediction with spatial tiling
#'
#' @keywords internal
#'
#' This variant is designed to avoid the enormous memory spike encountered
#' when loading the complete national network and pivoting all 75 periods at
#' once.  The function breaks the domain into a regular grid of square tiles,
#' reads each tile independently using a GDAL spatial filter, and immediately
#' drops geometry and writes out results before moving to the next tile.  The
#' long-format conversion is performed once per tile (using the same working
#' code), then split by temporal chunk; intermediate objects are freed
#' explicitly with `rm()` + `gc()` to keep the footprint low.
#' Geometry for the whole country is appended tile-by-tile, and traffic
#' attributes are written into separate tables identified by chunk name.
#'
#' Outputs:
#'   1. Geometry layer (GPKG with spatial index)
#'   2. One GPKG per temporal chunk (attribute-only, keyed by osm_id)
#'
#' @param cfg Configuration list
#' @param region_name Character. Human-readable region name for log messages.
#' @param output_config List with output paths: geom, den, hourly, hourly_wd, hourly_we.
#' @param tile_size_m Tile side in meters (default 200 km)
#' @param chunks Character vector of temporal chunks to export.
#'   Valid values: "DEN", "hourly", "hourly_wd", "hourly_we".
#'   Default: c("DEN") for memory efficiency.
#'   For full export use: c("DEN", "hourly", "hourly_wd", "hourly_we").
#'   Disk estimate per chunk: DEN ~1 GB, hourly/wd/we ~8.5 GB each.
#' @return Invisible NULL
#' @keywords internal
.predict_france_tiled_impl <- function(cfg, region_name = "France",
                                       output_config,
                                       tile_size_m = 200000,
                                       chunks = c("DEN")) {
  
  # Configuration parameters from cfg
  osm_roads_path <- cfg$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH
  xgb_models_path <- cfg$XGB_MODELS_WITH_RATIOS_FILEPATH
  xgb_feature_path <- cfg$XGB_RATIO_FEATURE_INFO_FILEPATH
  
  # Output paths from output_config
  chunk_paths_all <- list(
    DEN       = output_config$den,
    hourly    = output_config$hourly,
    hourly_wd = output_config$hourly_wd,
    hourly_we = output_config$hourly_we
  )
  geom_path <- output_config$geom
  
  pipeline_message(sprintf("%s tiled prediction", region_name), level = 0, 
                   progress = "start", process = "calc")

  # --- Load models (once for all tiles) ---
  pipeline_message("Loading trained XGBoost models", level = 1, 
                   progress = "start", process = "load")

  if (!file.exists(xgb_models_path)) {
    pipeline_message(sprintf("Models not found: %s", xgb_models_path), 
    process = "stop")
  }
  models_list  <- readRDS(xgb_models_path)
  feature_info <- readRDS(xgb_feature_path)
  all_periods  <- feature_info$all_periods

  pipeline_message(
    sprintf("Models loaded: %d models for %d periods",
            length(models_list), length(all_periods)),
    level = 1, progress = "end", process = "valid")

  # --- Define temporal chunks ---
  temporal_chunks <- get_temporal_chunks()
  temporal_chunks <- temporal_chunks[intersect(chunks, names(temporal_chunks))]

  if (length(temporal_chunks) == 0) {
    pipeline_message(
      "No valid temporal chunks requested. Valid: DEN, hourly, hourly_wd, hourly_we", process = "stop")
  }

  pipeline_message(sprintf("Temporal chunks to export: %s",
                           paste(names(temporal_chunks), collapse = ", ")),
    process = "info")

  # Verify all periods are covered
  covered <- unlist(temporal_chunks, use.names = FALSE)
  missing_periods <- setdiff(all_periods, covered)
  if (length(missing_periods) > 0) {
    pipeline_message(sprintf("Warning: %d periods not in any temporal chunk: %s",
                             length(missing_periods),
                             paste(head(missing_periods, 10), collapse = ", ")),
      process = "warning")
  }

  # --- Output paths (use provided output_config) ---
  geom_dir <- dirname(geom_path)
  if (!dir.exists(geom_dir)) dir.create(geom_dir, recursive = TRUE)

  chunk_paths <- chunk_paths_all[names(temporal_chunks)]

  # --- Build spatial tiles ---
  tiles <- build_france_tiles(tile_size_m = tile_size_m)
  pipeline_message(sprintf("Tile grid: %d tiles of %d km each",
                          nrow(tiles), tile_size_m / 1000),
                   process = "info")

  # --- Clean output files (overwrite mode) ---
  for (fp in c(geom_path, unlist(chunk_paths))) {
    if (file.exists(fp)) file.remove(fp)
  }

  # --- Process tiles ---
  total_roads <- 0L
  total_tiles_with_data <- 0L
  tile_times <- numeric(0)

  for (i in seq_len(nrow(tiles))) {
    tile <- tiles[i, ]
    t0 <- proc.time()["elapsed"]

    # Load tile from GPKG with spatial filter
    wkt_bbox <- sprintf(
      "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
      tile$xmin, tile$ymin,
      tile$xmax, tile$ymin,
      tile$xmax, tile$ymax,
      tile$xmin, tile$ymax,
      tile$xmin, tile$ymin)

    tile_sf <- tryCatch(
      sf::st_read(
        dsn   = osm_roads_path,
        wkt_filter = wkt_bbox,
        quiet = TRUE),
      error = function(e) NULL)

    if (is.null(tile_sf) || nrow(tile_sf) == 0) next

    n_tile <- nrow(tile_sf)
    total_roads <- total_roads + n_tile
    total_tiles_with_data <- total_tiles_with_data + 1L

    # --- Write geometry (append mode) ---
    check_memory_available(
      operation_name = sprintf("Geometry write tile %s", tile$tile_id),
      min_gb = 0.5, warn_gb = 1)
    geom_layer <- tile_sf[, c("osm_id", "name", "highway", "speed",
                              "lanes_osm", "oneway_osm", "DEGRE")]
    geom_layer <- add_period_datetime_columns(geom_layer, cfg)  # safe no-op if no period col

    pipeline_message(sprintf("Writing geometry for tile %s", tile$tile_id),
                     level = 1, progress = "start", process = "save")
    sf::st_write(
      obj        = geom_layer,
      dsn        = geom_path,
      layer      = "france_network",
      append     = TRUE,
      quiet      = TRUE)
    pipeline_message(sprintf("Geometry written for tile %s", tile$tile_id),
                     level = 1, progress = "end", process = "save")

    # --- Predict ---
    tile_dt <- as.data.frame(sf::st_drop_geometry(tile_sf))
    rm(tile_sf, geom_layer)

    predictions_wide <- apply_xgboost_predictions(
      network_data = tile_dt,
      models_list  = models_list,
      feature_info = feature_info,
      default_vehicle_speed = cfg$DEFAULT_VEHICLE_SPEED)

    rm(tile_dt)

    # convert to long format exactly as in predict_region (this code has been
    # proven to work and includes memory checks + validation)
    check_memory_available(
      operation_name = sprintf("Pivot tile %s (%s roads)", tile$tile_id, fmt(n_tile)),
      min_gb = 1, warn_gb = 2)

    predictions_long <- predictions_wide %>%
      tidyr::pivot_longer(
        cols = matches("^(flow|truck_pct|speed)_"),
        names_to = c(".value", "period"),
        names_pattern = "^(flow|truck_pct|speed)_(.+)$"
      ) %>%
      mutate(
        HGV = flow * (truck_pct / 100),
        LV  = flow - HGV,
        TV  = flow,
        period = factor(period, levels = all_periods)
      ) %>%
      select(osm_id, highway, period, TV, HGV, LV, speed,
             osm_speed, osm_speed_imputed, truck_pct)

    predictions_long <- add_period_datetime_columns(predictions_long, cfg)

    # validate predictions for this tile, log warnings if any
    validation <- validate_predictions(predictions_long)
    if (!validation$is_valid) {
      pipeline_message(sprintf("Validation warnings in tile %d: %s issues", 
                               tile$tile_id, length(validation$issues)),
                       process = "warning")
      for (issue_name in names(validation$issues)) {
        pipeline_message(sprintf("  - %s: %s cases", 
                                 issue_name, validation$issues[[issue_name]]),
                         process = "warning")
      }
    }

    # --- Write each temporal chunk (append mode CSV) ---
    for (chunk_name in names(temporal_chunks)) {

      chunk_periods <- temporal_chunks[[chunk_name]]

      chunk_long <- predictions_long %>%
        dplyr::filter(period %in% chunk_periods) %>%
        mutate(period = as.character(period))

      if (nrow(chunk_long) > 0) {

        chunk_long <- add_period_datetime_columns(chunk_long, cfg)

        chunk_file <- chunk_paths[[chunk_name]]

        chunk_data <- sf::st_drop_geometry(chunk_long)

        pipeline_message(
          sprintf("Writing chunk '%s' (%d rows)", chunk_name, nrow(chunk_data)),
          level = 1, progress = "start", process = "save")

        # Convert to data.table for fast write
        chunk_dt <- data.table::as.data.table(chunk_data)

        # Write CSV (append if exists)
        data.table::fwrite(
          chunk_dt,
          file = chunk_file,
          append = file.exists(chunk_file),
          col.names = !file.exists(chunk_file)
        )

        pipeline_message(
          sprintf("Chunk '%s' appended to %s", chunk_name, rel_path(chunk_file)),
          level = 1, progress = "end", process = "save")
      }

      rm(chunk_long, chunk_data, chunk_dt)
    }

    rm(predictions_wide, predictions_long)
    gc(verbose = FALSE)

    dt <- proc.time()["elapsed"] - t0
    tile_times <- c(tile_times, dt)
    avg_time <- mean(tile_times)
    remaining <- (nrow(tiles) - i) * avg_time

    pipeline_message(
      sprintf("Tile %d/%d: %s roads (%.1f s) | Total: %s roads | ETA: %s", 
              i, nrow(tiles), fmt(n_tile), dt, 
              fmt(total_roads), 
              format_duration(remaining)), 
      level = 2, process = "calc")
  }

  rm(models_list, feature_info)
  gc(verbose = FALSE)

  # --- Summary ---
  pipeline_message("France-wide prediction summary:", process = "info")
  pipeline_message(sprintf("  - Roads predicted: %s across %d tiles", 
                           fmt(total_roads), total_tiles_with_data), 
                   process = "info")
  pipeline_message(sprintf("  - Geometry layer: %s", rel_path(geom_path)), 
                   process = "info")
  for (cn in names(chunk_paths)) {
    if (file.exists(chunk_paths[[cn]])) {
      sz <- round(file.info(chunk_paths[[cn]])$size / 1024^2, 1)
      pipeline_message(sprintf("  - Traffic [%s]: %s (%.1f MB)", 
                               cn, rel_path(chunk_paths[[cn]]), sz), 
                   process = "info")
    }
  }

  pipeline_message(sprintf("%s prediction completed", region_name), level = 0, 
                   progress = "end", process = "valid")

  invisible(NULL)
}

# ==============================================================================
# COMPATIBILITY WRAPPERS (for legacy code)
# ==============================================================================

#' (Deprecated) Simple region prediction
#'
#' Use [predict_traffic] instead. This wrapper is provided for backward
#' compatibility with existing code.
#'
#' @param region_name Character. Human-readable region name.
#' @param bbox Named numeric vector c(xmin, ymin, xmax, ymax) in EPSG:2154.
#' @param output_filepath Character. Output .gpkg file path.
#' @param cfg Configuration list.
#'
#' @return Invisible NULL.
#' @export
predict_region <- function(region_name, bbox, output_filepath, cfg) {
  .Deprecated(new = "predict_traffic",
    old = "predict_region",
    msg = "predict_region is deprecated; use predict_traffic(method='region') instead")

  predict_traffic(
    region_name = region_name,
    cfg = cfg,
    bbox = bbox,
    output_config = list(filepath = output_filepath),
    method = "region"
  )
}

#' (Deprecated) France-wide tiled prediction
#'
#' Use [predict_traffic] instead. This wrapper is provided for backward
#' compatibility with existing code.
#'
#' @param cfg Configuration list.
#' @param tile_size_m Tile side in meters. Default: 200000.
#' @param chunks Temporal chunks. Default: c("DEN").
#'
#' @return Invisible NULL.
#' @export
predict_france_tiled <- function(cfg, tile_size_m = 200000,
                                  chunks = c("DEN")) {
  .Deprecated(new = "predict_traffic",
    old = "predict_france_tiled",
    msg = "predict_france_tiled is deprecated; use predict_traffic(method='tiled') instead")

  predict_traffic(
    region_name = "France",
    cfg = cfg,
    bbox = NULL,
    output_config = list(
      geom = cfg$FRANCE_GEOMETRY_FILEPATH,
      den = cfg$FRANCE_TRAFFIC_DEN_FILEPATH,
      hourly = cfg$FRANCE_TRAFFIC_HOURLY_FILEPATH,
      hourly_wd = cfg$FRANCE_TRAFFIC_HOURLY_WD_FILEPATH,
      hourly_we = cfg$FRANCE_TRAFFIC_HOURLY_WE_FILEPATH
    ),
    method = "tiled",
    tile_size_m = tile_size_m,
    chunks = chunks
  )
  h <- floor(seconds / 3600)
  m <- floor((seconds %% 3600) / 60)
  s <- round(seconds %% 60)
  if (h > 0) {
    sprintf("%dh%02dm%02ds", h, m, s)
  } else if (m > 0) {
    sprintf("%dm%02ds", m, s)
  } else {
    sprintf("%ds", s)
  }
}
