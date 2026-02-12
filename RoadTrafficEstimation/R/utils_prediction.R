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
  
  # Prepare feature matrix (same encoding as training: sparse.model.matrix)
  rownames(network_data) <- seq_len(nrow(network_data))
  feature_matrix_part <- Matrix::sparse.model.matrix(
    object = feature_info$road_feature_formula,
    data = network_data)
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
  speed_D <- if (is.null(models_list$speed_D$model)) {
    pipeline_message(
      text = "Missing base model speed_D: outputs will be NA",
      process = "warning")
    rep(NA_real_, length(flow_D))
  } else {
    predict_with_alignment(models_list$speed_D$model, feature_matrix)
  }
  
  # Initialize results data.frame
  results <- data.frame(
    osm_id = network_data$osm_id,
    highway = network_data$highway
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
  flow_cols <- grep("^flow_", names(results), value = TRUE)
  if (length(flow_cols) > 0) {
    results[flow_cols] <- lapply(results[flow_cols], function(x) pmax(0, x))
  }
  truck_cols <- grep("^truck_pct_", names(results), value = TRUE)
  if (length(truck_cols) > 0) {
    results[truck_cols] <- lapply(results[truck_cols], function(x) pmin(100, pmax(0, x)))
  }
  speed_cols <- grep("^speed_", names(results), value = TRUE)
  if (length(speed_cols) > 0) {
    results[speed_cols] <- lapply(results[speed_cols], function(x) pmax(0, x))
  }
  
  # Guard NaN/Inf: impute with median by highway type + emit warning
  pred_cols <- c(flow_cols, truck_cols, speed_cols)
  total_nan <- 0L
  for (col in pred_cols) {
    bad_idx <- which(is.nan(results[[col]]) | is.infinite(results[[col]]))
    if (length(bad_idx) > 0) {
      total_nan <- total_nan + length(bad_idx)
      # Impute each NaN with the median of same highway type
      for (hw in unique(results$highway[bad_idx])) {
        hw_rows <- which(results$highway == hw)
        hw_good <- results[[col]][setdiff(hw_rows, bad_idx)]
        med_val <- if (length(hw_good) > 0) median(hw_good, na.rm = TRUE) else NA_real_
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
  
  # Check for negative flows
  flow_cols <- grep("^flow_", names(predictions), value = TRUE)
  for (col in flow_cols) {
    n_negative <- sum(predictions[[col]] < 0, na.rm = TRUE)
    if (n_negative > 0) {
      issues[[paste0(col, "_negative")]] <- n_negative
    }
  }
  
  # Check for truck_pct > 100
  truck_cols <- grep("^truck_pct_", names(predictions), value = TRUE)
  for (col in truck_cols) {
    n_exceed <- sum(predictions[[col]] > 100, na.rm = TRUE)
    if (n_exceed > 0) {
      issues[[paste0(col, "_exceed_100")]] <- n_exceed
    }
  }
  
  # Check for unrealistic speeds
  speed_cols <- grep("^speed_", names(predictions), value = TRUE)
  for (col in speed_cols) {
    n_unrealistic <- sum(predictions[[col]] < 0 | predictions[[col]] > 200, 
                         na.rm = TRUE)
    if (n_unrealistic > 0) {
      issues[[paste0(col, "_unrealistic")]] <- n_unrealistic
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
