# ==============================================================================
# UTILITIES: PREDICTION PHASE
# ==============================================================================
# Module d'utilitaires spécifiques à la phase de prédiction.
# Contient les fonctions pour charger la couche France engineered, appliquer
# les modèles XGBoost, et formater les sorties.
# ==============================================================================

# Note: utils_io.R and utils_sf.R are already loaded by bootstrap.R

# ------------------------------------------------------------------------------
# Fonctions de chargement et filtrage spatial
# ------------------------------------------------------------------------------

#' Load and crop France engineered network to bounding box
#'
#' @param bbox numeric vector c(xmin, ymin, xmax, ymax) in target CRS
#' @param config CONFIG list with file paths
#' @return sf data.frame with cropped network
load_network_for_prediction <- function(bbox, config) {
  pipeline_message(
    text = sprintf("Loading OSM France engineered network from %s", 
                   rel_path(config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)),
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
      dsn = config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
      wkt_filter = wkt_bbox,
      quiet = TRUE)
    
    pipeline_message(
      text = sprintf("Network loaded with spatial filter: %s roads",
                     fmt(nrow(osm_network))),
      process = "info")
  } else {
    # No bbox: load full network (warning: memory-intensive)
    pipeline_message(
      text = "No bbox provided — loading entire France network (memory-intensive)",
      process = "warning")
    check_memory_available(
      operation_name = "Load entire France network (no bbox)",
      min_gb = 8, warn_gb = 12)
    osm_network <- sf::st_read(
      dsn = config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
      quiet = TRUE)
  }
  
  # Ensure correct CRS
  if (sf::st_crs(osm_network) != config$TARGET_CRS) {
    osm_network <- osm_network %>% 
      st_transform(crs = config$TARGET_CRS)
  }

  if (nrow(osm_network) == 0) {
    stop("No roads found for prediction after cropping. Check bbox/CRS.")
  }
  
  pipeline_message(
    text = sprintf("Network loaded: %s roads", fmt(nrow(osm_network))),
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
  pipeline_message(
    text = sprintf("Loading network within %sm of %s points", 
                   buffer_radius, nrow(points)),
    level = 1, progress = "start", process = "load")
  
  # Memory check
  check_memory_available(
    operation_name = "Load network around sensor points",
    min_gb = 2, warn_gb = 4)
  
  # Ensure points CRS
  if (sf::st_crs(points) != config$TARGET_CRS) {
    points <- points %>% 
      st_transform(crs = config$TARGET_CRS)
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
    dsn = config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
    wkt_filter = wkt_bbox,
    quiet = TRUE)
  
  # Ensure correct CRS
  if (sf::st_crs(osm_network) != config$TARGET_CRS) {
    osm_network <- osm_network %>% 
      st_transform(crs = config$TARGET_CRS)
  }
  
  # Create buffers and filter precisely
  buffers <- sf::st_buffer(points, dist = buffer_radius)
  combined_buffer <- sf::st_union(buffers)
  osm_network <- sf::st_filter(osm_network, combined_buffer)
  
  pipeline_message(
    text = sprintf("Network filtered: %s roads within buffers", 
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
apply_xgboost_predictions <- function(network_data, models_list, feature_info) {
  pipeline_message(
    text = "Applying XGBoost models to network",
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
    pipeline_message(
      text = "lane_number missing in prediction input; derived proxy from OSM lane attributes",
      process = "info")
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
  
  # Helper: align feature matrix to a model's expected features and predict
  predict_with_alignment <- function(model, feature_matrix_base) {
    model_features <- model$feature_names
    if (!is.null(model_features)) {
      fm <- feature_matrix_base
      missing_cols <- setdiff(model_features, colnames(fm))
      if (length(missing_cols) > 0) {
        for (mc in missing_cols) fm[[mc]] <- 0
      }
      fm <- fm[, model_features, drop = FALSE]
      dmat <- xgboost::xgb.DMatrix(data = as.matrix(fm))
    } else {
      dmat <- xgboost::xgb.DMatrix(data = as.matrix(feature_matrix_base))
    }
    predict(model, dmat)
  }
  
  # Predict base models (period D)
  if (is.null(models_list$flow_D$model)) {
    stop("Missing base model for period D: flow_D")
  }
  flow_D <- predict_with_alignment(models_list$flow_D$model, feature_matrix)
  truck_pct_D <- if (is.null(models_list$truck_pct_D$model)) {
    pipeline_message(
      text = "Missing base model truck_pct_D: outputs will be NA",
      process = "warning")
    rep(NA_real_, length(flow_D))
  } else {
    predict_with_alignment(models_list$truck_pct_D$model, feature_matrix)
  }
  speed_model_target <- NA_character_
  if (!is.null(models_list$speed_D$config) &&
      !is.null(models_list$speed_D$config$target)) {
    speed_model_target <- as.character(models_list$speed_D$config$target)
  }

  speed_D_raw <- if (is.null(models_list$speed_D$model)) {
    pipeline_message(
      text = "Missing base model speed_D: outputs will be NA",
      process = "warning")
    rep(NA_real_, length(flow_D))
  } else {
    predict_with_alignment(models_list$speed_D$model, feature_matrix)
  }

  speed_osm_raw <- suppressWarnings(as.numeric(network_data$speed))
  speed_osm_missing <- is.na(speed_osm_raw) | speed_osm_raw <= 0

  if (identical(speed_model_target, "ratio_speed_to_osm")) {
    # New model: speed_D predicts a ratio to OSM speed.
    speed_osm_base <- speed_osm_raw
    speed_osm_base[speed_osm_missing] <- CONFIG$DEFAULT_VEHICLE_SPEED
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
      text = sprintf(
        "Prediction-time OSM speed imputation: %s missing values imputed with %s",
        fmt(n_speed_osm_missing),
        ifelse(all(is.na(speed_D)), "DEFAULT_VEHICLE_SPEED", "speed_D XGBoost predictions")
      ),
      process = "warning"
    )
    if (all(is.na(speed_D))) {
      speed_osm[speed_osm_missing] <- CONFIG$DEFAULT_VEHICLE_SPEED
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
        predict_with_alignment(ratio_flow_model, feature_matrix)
      }
      ratio_truck_pct <- if (all(is.na(truck_pct_D))) {
        rep(NA_real_, length(truck_pct_D))
      } else if (is.null(ratio_truck_model)) {
        rep(NA_real_, length(truck_pct_D))
      } else {
        predict_with_alignment(ratio_truck_model, feature_matrix)
      }
      ratio_speed <- if (all(is.na(speed_D))) {
        rep(NA_real_, length(speed_D))
      } else if (is.null(ratio_speed_model)) {
        rep(NA_real_, length(speed_D))
      } else {
        predict_with_alignment(ratio_speed_model, feature_matrix)
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
      text = sprintf("NaN/Inf detected in predictions: %d values imputed with median by highway type",
                     total_nan),
      process = "warning")
  }
  
  pipeline_message(
    text = sprintf("Predictions completed for %s roads × %s periods", 
                   fmt(nrow(results)), length(feature_info$all_periods)),
    level = 1, progress = "end", process = "valid")
  
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
#' - D/E/N -> reference day in 1970 (D=06-18h, E=18-22h, N=22-06h)
#' - h0_wd..h23_wd -> weekdays in 1971 (reference day: 1971-01-01)
#' - h0_we..h23_we -> weekend in 1972 (reference Saturday: 1972-01-01)
#' - h0..h23 -> generic hourly periods in 1973 (reference day: 1973-01-01)
#'
#' @param predictions_long data.frame with a `period` column
#' @return data.frame with added `datetimestart` and `datetimeend` POSIXct columns
add_period_datetime_columns <- function(predictions_long) {
  if (!"period" %in% names(predictions_long)) {
    return(predictions_long)
  }

  period_chr <- as.character(predictions_long$period)
  n <- length(period_chr)

  datetimestart <- as.POSIXct(rep(NA_character_, n), tz = "UTC")
  datetimeend <- as.POSIXct(rep(NA_character_, n), tz = "UTC")

  # D / E / N reference periods (1970)
  idx_D <- which(period_chr == "D")
  if (length(idx_D) > 0) {
    datetimestart[idx_D] <- as.POSIXct("1970-01-01 06:00:00", tz = "UTC")
    datetimeend[idx_D] <- as.POSIXct("1970-01-01 18:00:00", tz = "UTC")
  }

  idx_E <- which(period_chr == "E")
  if (length(idx_E) > 0) {
    datetimestart[idx_E] <- as.POSIXct("1970-01-01 18:00:00", tz = "UTC")
    datetimeend[idx_E] <- as.POSIXct("1970-01-01 22:00:00", tz = "UTC")
  }

  idx_N <- which(period_chr == "N")
  if (length(idx_N) > 0) {
    datetimestart[idx_N] <- as.POSIXct("1970-01-01 22:00:00", tz = "UTC")
    datetimeend[idx_N] <- as.POSIXct("1970-01-02 06:00:00", tz = "UTC")
  }

  # Generic hourly periods h0..h23 (1973)
  m_h <- regexec("^h([0-9]{1,2})$", period_chr)
  g_h <- regmatches(period_chr, m_h)
  idx_h <- which(lengths(g_h) == 2)
  if (length(idx_h) > 0) {
    h_vals <- as.integer(vapply(g_h[idx_h], function(x) x[2], character(1)))
    start_str <- sprintf("1973-01-01 %02d:00:00", h_vals)
    datetimestart[idx_h] <- as.POSIXct(start_str, tz = "UTC")
    datetimeend[idx_h] <- datetimestart[idx_h] + 3600
  }

  # Weekday hourly periods h0_wd..h23_wd (1971)
  m_wd <- regexec("^h([0-9]{1,2})_wd$", period_chr)
  g_wd <- regmatches(period_chr, m_wd)
  idx_wd <- which(lengths(g_wd) == 2)
  if (length(idx_wd) > 0) {
    h_vals <- as.integer(vapply(g_wd[idx_wd], function(x) x[2], character(1)))
    start_str <- sprintf("1971-01-01 %02d:00:00", h_vals)
    datetimestart[idx_wd] <- as.POSIXct(start_str, tz = "UTC")
    datetimeend[idx_wd] <- datetimestart[idx_wd] + 3600
  }

  # Weekend hourly periods h0_we..h23_we (1972)
  m_we <- regexec("^h([0-9]{1,2})_we$", period_chr)
  g_we <- regmatches(period_chr, m_we)
  idx_we <- which(lengths(g_we) == 2)
  if (length(idx_we) > 0) {
    h_vals <- as.integer(vapply(g_we[idx_we], function(x) x[2], character(1)))
    start_str <- sprintf("1972-01-01 %02d:00:00", h_vals)
    datetimestart[idx_we] <- as.POSIXct(start_str, tz = "UTC")
    datetimeend[idx_we] <- datetimestart[idx_we] + 3600
  }

  predictions_long$datetimestart <- datetimestart
  predictions_long$datetimeend <- datetimeend
  predictions_long
}

# ==============================================================================
# GENERIC REGION PREDICTION
# ==============================================================================

#' Run traffic prediction for a geographic region
#'
#' Loads the France engineered network (cropped to bbox), applies XGBoost
#' models, converts to long format, validates, and exports to GeoPackage.
#'
#' @param region_name Character. Human-readable region name for log messages.
#' @param bbox Named numeric vector c(xmin, ymin, xmax, ymax) in EPSG:2154.
#' @param output_filepath Character. Full path to the output .gpkg file.
#' @param config CONFIG list with file paths and parameters.
#' @return Invisible NULL. Side effect: writes GPKG to disk.
predict_region <- function(region_name, bbox, output_filepath, config = CONFIG) {

  pipeline_message(text = sprintf("%s traffic prediction", region_name),
                   level = 0, progress = "start", process = "calc")

  # --- Load models ---
  pipeline_message(text = "Loading trained XGBoost models",
                   level = 1, progress = "start", process = "load")

  if (!file.exists(config$XGB_MODELS_WITH_RATIOS_FILEPATH)) {
    pipeline_message(
      text = sprintf("Models not found: %s",
                     rel_path(config$XGB_MODELS_WITH_RATIOS_FILEPATH)),
      process = "stop")
  }

  models_list <- readRDS(config$XGB_MODELS_WITH_RATIOS_FILEPATH)
  feature_info <- readRDS(config$XGB_RATIO_FEATURE_INFO_FILEPATH)

  pipeline_message(
    text = sprintf("Models loaded: %s models for %s periods",
                   length(models_list),
                   length(feature_info$all_periods)),
    level = 1, progress = "end", process = "valid")

  # --- Bbox ---
  pipeline_message(
    text = sprintf("Bbox: [%s, %s, %s, %s]",
                   bbox[1], bbox[2], bbox[3], bbox[4]),
    process = "info")

  # --- Load network ---
  osm_region <- load_network_for_prediction(bbox = bbox, config = config)

  pipeline_message(
    text = sprintf("Network loaded: %s roads in %s",
                   fmt(nrow(osm_region)), region_name),
    process = "info")

  # --- Apply predictions ---
  pipeline_message(text = sprintf("Applying XGBoost models to %s network", region_name),
                   level = 1, progress = "start", process = "calc")

  osm_region_dt <- as.data.frame(sf::st_drop_geometry(osm_region))

  predictions_wide <- apply_xgboost_predictions(
    network_data = osm_region_dt,
    models_list = models_list,
    feature_info = feature_info)

  pipeline_message(
    text = sprintf("Predictions completed: %s roads x %s periods",
                   fmt(nrow(predictions_wide)),
                   length(feature_info$all_periods)),
    level = 1, progress = "end", process = "valid")

  all_periods <- feature_info$all_periods
  rm(models_list, feature_info, osm_region_dt)
  gc(verbose = FALSE)

  # --- Long format ---
  pipeline_message(text = "Converting to long format",
                   level = 1, progress = "start", process = "calc")

  check_memory_available(
    operation_name = sprintf("Pivot to long format (%s roads)",
                             fmt(nrow(predictions_wide))),
    min_gb = 2, warn_gb = 4)

  predictions_long <- predictions_wide %>%
    tidyr::pivot_longer(
      cols = matches("^(flow|truck_pct|speed)_"),
      names_to = c(".value", "period"),
      names_pattern = "^(flow|truck_pct|speed)_(.+)$"
    ) %>%
    mutate(
      HGV = flow * (truck_pct / 100),
      LV = flow - HGV,
      TV = flow,
      period = factor(period, levels = all_periods)
    ) %>%
    select(osm_id, highway, period, TV, HGV, LV, speed,
           osm_speed, osm_speed_imputed, truck_pct)

  predictions_long <- add_period_datetime_columns(predictions_long)

  rm(predictions_wide)
  gc(verbose = FALSE)

  pipeline_message(
    text = sprintf("Long format: %s rows (roads x periods)",
                   fmt(nrow(predictions_long))),
    level = 1, progress = "end", process = "valid")

  # --- Validate ---
  validation <- validate_predictions(predictions_long)
  if (!validation$is_valid) {
    pipeline_message(
      text = sprintf("Validation warnings: %s issues detected",
                     length(validation$issues)),
      process = "warning")
    for (issue_name in names(validation$issues)) {
      pipeline_message(
        text = sprintf("  - %s: %s cases",
                       issue_name, validation$issues[[issue_name]]),
        process = "warning")
    }
  }

  # --- Export ---
  pipeline_message(text = "Exporting predictions with geometry",
                   level = 1, progress = "start", process = "save")

  check_memory_available(
    operation_name = sprintf("Geometry merge (%s rows)",
                             fmt(nrow(predictions_long))),
    min_gb = 2, warn_gb = 4)

  predictions_sf <- merge(
    predictions_long,
    osm_region[, c("osm_id", "name", "geom")],
    by = "osm_id", all.x = TRUE)

  predictions_sf <- sf::st_as_sf(predictions_sf)

  if (sf::st_crs(predictions_sf) != config$TARGET_CRS) {
    predictions_sf <- sf::st_transform(predictions_sf, config$TARGET_CRS)
  }

  predictions_sf <- add_period_datetime_columns(predictions_sf)

  sf::st_write(
    obj = predictions_sf,
    dsn = output_filepath,
    delete_dsn = TRUE,
    quiet = FALSE)

  pipeline_message(
    text = sprintf("Predictions exported to %s",
                   rel_path(output_filepath)),
    level = 1, progress = "end", process = "save")

  # --- Summary ---
  pipeline_message(
    text = sprintf("Prediction summary for %s:", region_name),
    level = 1, process = "info")
  pipeline_message(
    text = sprintf("  - Roads: %s", fmt(length(unique(predictions_long$osm_id)))),
    level = 1, process = "info")
  pipeline_message(
    text = sprintf("  - Periods: %s", length(all_periods)),
    level = 1, process = "info")
  pipeline_message(
    text = sprintf("  - Total predictions: %s", fmt(nrow(predictions_long))),
    level = 1, process = "info")

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
        text = sprintf("  - Period %s: %d veh/h avg, %.1f km/h, %.1f%% trucks",
                       p, stats$avg_TV, stats$avg_speed, stats$avg_truck_pct),
        level = 1, process = "info")
    }
  }

  pipeline_message(text = sprintf("%s prediction completed", region_name),
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

#' Pivot wide predictions to long for a subset of periods
#'
#' @param predictions_wide data.frame from apply_xgboost_predictions()
#' @param periods character vector of period names to include
#' @param all_periods full ordered period vector (for factor levels)
#' @return data.frame in long format (no geometry)
pivot_to_long_chunk <- function(predictions_wide, periods, all_periods) {
  # Build regex to match only requested periods
  period_pattern <- paste0("^(flow|truck_pct|speed)_(", paste(periods, collapse = "|"), ")$")
  cols_to_pivot <- grep(period_pattern, names(predictions_wide), value = TRUE)

  if (length(cols_to_pivot) == 0) return(data.frame())

  predictions_wide %>%
    dplyr::select(osm_id, dplyr::all_of(cols_to_pivot)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols_to_pivot),
      names_to = c(".value", "period"),
      names_pattern = "^(flow|truck_pct|speed)_(.+)$"
    ) %>%
    dplyr::mutate(
      HGV = flow * (truck_pct / 100),
      LV  = flow - HGV,
      TV  = flow,
      period = factor(period, levels = all_periods)
    ) %>%
    dplyr::select(osm_id, period, TV, HGV, LV, speed, truck_pct)
}

#' Run France-wide prediction with spatial tiling and temporal chunking
#'
#' Outputs:
#'   1. Geometry layer (GPKG with spatial index)
#'   2. One GPKG per temporal chunk (attribute-only, keyed by osm_id)
#'
#' @param config CONFIG list
#' @param tile_size_m Tile side in meters (default 200 km)
#' @param chunks Character vector of temporal chunks to export.
#'   Valid values: "DEN", "hourly", "hourly_wd", "hourly_we".
#'   Default: all chunks. Use c("DEN") for noise mapping (smallest output).
#'   Disk estimate per chunk: DEN ~1 GB, hourly/wd/we ~8.5 GB each.
#' @return Invisible NULL
predict_france_tiled <- function(config = CONFIG, tile_size_m = 200000,
                                 chunks = c("DEN", "hourly", "hourly_wd", "hourly_we")) {

  pipeline_message(text = "FRANCE-WIDE tiled prediction",
                   level = 0, progress = "start", process = "calc")

  # --- Load models (once for all tiles) ---
  pipeline_message(text = "Loading trained XGBoost models",
                   level = 1, progress = "start", process = "load")

  if (!file.exists(config$XGB_MODELS_WITH_RATIOS_FILEPATH)) {
    stop("Models not found: ", config$XGB_MODELS_WITH_RATIOS_FILEPATH)
  }
  models_list  <- readRDS(config$XGB_MODELS_WITH_RATIOS_FILEPATH)
  feature_info <- readRDS(config$XGB_RATIO_FEATURE_INFO_FILEPATH)
  all_periods  <- feature_info$all_periods

  pipeline_message(
    text = sprintf("Models loaded: %d models for %d periods",
                   length(models_list), length(all_periods)),
    level = 1, progress = "end", process = "valid")

  # --- Define temporal chunks ---
  temporal_chunks <- get_temporal_chunks()
  temporal_chunks <- temporal_chunks[intersect(chunks, names(temporal_chunks))]

  if (length(temporal_chunks) == 0) {
    stop("No valid temporal chunks requested. Valid: DEN, hourly, hourly_wd, hourly_we")
  }

  pipeline_message(
    text = sprintf("Temporal chunks to export: %s",
                   paste(names(temporal_chunks), collapse = ", ")),
    process = "info")

  # Verify all periods are covered
  covered <- unlist(temporal_chunks, use.names = FALSE)
  missing_periods <- setdiff(all_periods, covered)
  if (length(missing_periods) > 0) {
    pipeline_message(
      text = sprintf("Warning: %d periods not in any temporal chunk: %s",
                     length(missing_periods),
                     paste(head(missing_periods, 10), collapse = ", ")),
      process = "warning")
  }

  # --- Output paths ---
  output_dir <- config$FRANCE_OUTPUT_DIR
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  geom_path  <- config$FRANCE_GEOMETRY_FILEPATH
  chunk_paths <- list(
    DEN       = config$FRANCE_TRAFFIC_DEN_FILEPATH,
    hourly    = config$FRANCE_TRAFFIC_HOURLY_FILEPATH,
    hourly_wd = config$FRANCE_TRAFFIC_HOURLY_WD_FILEPATH,
    hourly_we = config$FRANCE_TRAFFIC_HOURLY_WE_FILEPATH
  )
  chunk_paths <- chunk_paths[names(temporal_chunks)]

  # --- Build spatial tiles ---
  tiles <- build_france_tiles(tile_size_m = tile_size_m)
  pipeline_message(
    text = sprintf("Tile grid: %d tiles of %d km each",
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
        dsn   = config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
        wkt_filter = wkt_bbox,
        quiet = TRUE),
      error = function(e) NULL)

    if (is.null(tile_sf) || nrow(tile_sf) == 0) next

    n_tile <- nrow(tile_sf)
    total_roads <- total_roads + n_tile
    total_tiles_with_data <- total_tiles_with_data + 1L

    # --- Write geometry (append mode) ---
    geom_layer <- tile_sf[, c("osm_id", "name", "highway", "speed",
                              "lanes_osm", "oneway_osm", "DEGRE")]
    geom_layer <- add_period_datetime_columns(geom_layer)  # safe no-op if no period col
    sf::st_write(
      obj        = geom_layer,
      dsn        = geom_path,
      layer      = "france_network",
      append     = TRUE,
      quiet      = TRUE)

    # --- Predict ---
    tile_dt <- as.data.frame(sf::st_drop_geometry(tile_sf))
    rm(tile_sf, geom_layer)

    predictions_wide <- apply_xgboost_predictions(
      network_data = tile_dt,
      models_list  = models_list,
      feature_info = feature_info)

    rm(tile_dt)

    # --- Write each temporal chunk (append mode, no geometry) ---
    for (chunk_name in names(temporal_chunks)) {
      chunk_periods <- temporal_chunks[[chunk_name]]
      chunk_long <- pivot_to_long_chunk(predictions_wide, chunk_periods, all_periods)

      if (nrow(chunk_long) > 0) {
        # Convert factor columns to character for GPKG compatibility
        chunk_long$period <- as.character(chunk_long$period)
        chunk_long <- add_period_datetime_columns(chunk_long)
        # Write as plain table (no geometry) in GPKG
        sf::st_write(
          obj    = chunk_long,
          dsn    = chunk_paths[[chunk_name]],
          layer  = paste0("traffic_", chunk_name),
          append = TRUE,
          quiet  = TRUE)
      }
      rm(chunk_long)
    }

    rm(predictions_wide)
    gc(verbose = FALSE)

    dt <- proc.time()["elapsed"] - t0
    tile_times <- c(tile_times, dt)
    avg_time <- mean(tile_times)
    remaining <- (nrow(tiles) - i) * avg_time

    pipeline_message(
      text = sprintf("Tile %d/%d: %s roads (%.1f s) | Total: %s roads | ETA: %s",
                     i, nrow(tiles), fmt(n_tile), dt,
                     fmt(total_roads),
                     format_duration(remaining)),
      level = 2, process = "calc")
  }

  rm(models_list, feature_info)
  gc(verbose = FALSE)

  # --- Summary ---
  pipeline_message(text = "France-wide prediction summary:",
                   level = 1, process = "info")
  pipeline_message(
    text = sprintf("  Roads predicted: %s across %d tiles",
                   fmt(total_roads), total_tiles_with_data),
    level = 1, process = "info")
  pipeline_message(
    text = sprintf("  Geometry layer: %s", rel_path(geom_path)),
    level = 1, process = "info")
  for (cn in names(chunk_paths)) {
    if (file.exists(chunk_paths[[cn]])) {
      sz <- round(file.info(chunk_paths[[cn]])$size / 1024^2, 1)
      pipeline_message(
        text = sprintf("  Traffic [%s]: %s (%.1f MB)",
                       cn, rel_path(chunk_paths[[cn]]), sz),
        level = 1, process = "info")
    }
  }

  pipeline_message(text = "FRANCE-WIDE prediction completed",
                   level = 0, progress = "end", process = "valid")

  invisible(NULL)
}

#' Format seconds into human-readable duration
#' @param seconds numeric
#' @return character
format_duration <- function(seconds) {
  if (!is.finite(seconds) || seconds < 0) return("??")
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
