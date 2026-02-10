# ==============================================================================
# UTILITIES: PREDICTION PHASE
# ==============================================================================
# Module d'utilitaires spécifiques à la phase de prédiction.
# Contient les fonctions pour charger la couche France engineered, appliquer
# les modèles XGBoost, et formater les sorties.
# ==============================================================================

# Source des utilitaires génériques nécessaires
source("R/utils_io.R", encoding = "UTF-8")
source("R/utils_sf.R", encoding = "UTF-8")

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
  
  # Load full network
  osm_network <- sf::st_read(
    dsn = config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
    quiet = TRUE)
  
  # Ensure correct CRS
  if (sf::st_crs(osm_network) != config$TARGET_CRS) {
    osm_network <- osm_network %>% 
      st_transform(crs = config$TARGET_CRS)
  }
  
  # Crop to bbox if provided
  if (!is.null(bbox) && length(bbox) == 4) {
    xmin <- as.numeric(bbox[1])
    ymin <- as.numeric(bbox[2])
    xmax <- as.numeric(bbox[3])
    ymax <- as.numeric(bbox[4])
    if (anyNA(c(xmin, ymin, xmax, ymax))) {
      stop("Invalid bbox values for prediction crop: ",
           paste(bbox, collapse = ", "))
    }
    bbox_polygon <- sf::st_bbox(
      c(xmin = xmin, ymin = ymin, 
        xmax = xmax, ymax = ymax),
      crs = config$TARGET_CRS) %>%
      sf::st_as_sfc()
    
    osm_network <- sf::st_filter(osm_network, bbox_polygon)
    
    pipeline_message(
      text = sprintf("Network cropped to bbox: %s roads", 
                     fmt(nrow(osm_network))),
      process = "info")
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
  
  # Load full network
  osm_network <- sf::st_read(
    dsn = config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
    quiet = TRUE)
  
  # Ensure correct CRS
  if (sf::st_crs(osm_network) != config$TARGET_CRS) {
    osm_network <- osm_network %>% 
      st_transform(crs = config$TARGET_CRS)
  }
  
  if (sf::st_crs(points) != config$TARGET_CRS) {
    points <- points %>% 
      st_transform(crs = config$TARGET_CRS)
  }
  
  # Create buffers
  buffers <- sf::st_buffer(points, dist = buffer_radius)
  combined_buffer <- sf::st_union(buffers)
  
  # Filter roads within buffers
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
  
  # Prepare feature matrix (same as training)
  rownames(network_data) <- seq_len(nrow(network_data))
  feature_matrix_part <- model.matrix(
    object = feature_info$road_feature_formula,
    data = network_data,
    na.action = na.pass)
  rows_used <- as.integer(rownames(feature_matrix_part))
  feature_matrix <- matrix(
    NA_real_,
    nrow = nrow(network_data),
    ncol = ncol(feature_matrix_part),
    dimnames = list(rownames(network_data), colnames(feature_matrix_part))
  )
  feature_matrix[rows_used, ] <- feature_matrix_part
  feature_matrix <- as.data.frame(feature_matrix, stringsAsFactors = FALSE)
  
  # Align feature columns with training model
  model_features <- models_list$flow_D$model$feature_names
  if (!is.null(model_features)) {
    missing_cols <- setdiff(model_features, colnames(feature_matrix))
    if (length(missing_cols) > 0) {
      for (mc in missing_cols) {
        feature_matrix[[mc]] <- 0
      }
    }
    extra_cols <- setdiff(colnames(feature_matrix), model_features)
    if (length(extra_cols) > 0) {
      feature_matrix <- feature_matrix[, setdiff(colnames(feature_matrix), extra_cols), drop = FALSE]
    }
    feature_matrix <- feature_matrix[, model_features, drop = FALSE]
  }
  
  # Convert to xgb.DMatrix
  dmatrix <- xgboost::xgb.DMatrix(data = as.matrix(feature_matrix))
  
  # Predict base models (period D)
  if (is.null(models_list$flow_D$model)) {
    stop("Missing base model for period D: flow_D")
  }
  flow_D <- predict(models_list$flow_D$model, dmatrix)
  truck_pct_D <- if (is.null(models_list$truck_pct_D$model)) {
    pipeline_message(
      text = "Missing base model truck_pct_D: outputs will be NA",
      process = "warning")
    rep(NA_real_, length(flow_D))
  } else {
    predict(models_list$truck_pct_D$model, dmatrix)
  }
  speed_D <- if (is.null(models_list$speed_D$model)) {
    pipeline_message(
      text = "Missing base model speed_D: outputs will be NA",
      process = "warning")
    rep(NA_real_, length(flow_D))
  } else {
    predict(models_list$speed_D$model, dmatrix)
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
        predict(ratio_flow_model, dmatrix)
      }
      ratio_truck_pct <- if (all(is.na(truck_pct_D))) {
        rep(NA_real_, length(truck_pct_D))
      } else if (is.null(ratio_truck_model)) {
        rep(NA_real_, length(truck_pct_D))
      } else {
        predict(ratio_truck_model, dmatrix)
      }
      ratio_speed <- if (all(is.na(speed_D))) {
        rep(NA_real_, length(speed_D))
      } else if (is.null(ratio_speed_model)) {
        rep(NA_real_, length(speed_D))
      } else {
        predict(ratio_speed_model, dmatrix)
      }
      
      # Apply ratios to base predictions
      results[[paste0("flow_", period)]] <- (10^flow_D) * ratio_flow
      results[[paste0("truck_pct_", period)]] <- truck_pct_D * ratio_truck_pct
      results[[paste0("speed_", period)]] <- speed_D * ratio_speed
    }
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
