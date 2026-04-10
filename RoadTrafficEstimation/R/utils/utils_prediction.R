# ==============================================================================
# PREDICTION PHASE UTILITIES
# ==============================================================================
#' 
# -------------------------------------------------------------------------------
# Generate Prediction File Paths
# -------------------------------------------------------------------------------
#' @title Generate prediction file paths based on spatial extent and mode
#' @description This function creates a standardized naming scheme for prediction 
#' @description This function creates a standardized naming scheme for prediction 
#'              outputs, consistent with the training pipeline. All output files 
#'              are tagged with the current MODE (e.g., "nantes", "paris", "pemb", 
#'               "sensors", "france") to avoid collisions when running multiple 
#'              regions.
#'              For regional predictions (Nantes, Paris, PEMB, sensors):
#'              \itemize{
#'                \item{Single output file: 07_predictions_{mode}.gpkg}
#'              }
#'              For France-wide tiled predictions:
#'              \itemize{
#'                \item{Output directory: data/prediction/{mode}/}
#'                \item{Temporal chunks (with geometry): 
#'                      07_predictions_{mode}_traffic_{CHUNK}.gpkg} where {CHUNK} 
#'                      is DEN, hourly, hourly_wd, or hourly_we.
#'              }
#' @param extent Character. Spatial extent: "sensors", "nantes", "paris", "pemb", 
#'               or "france".
#' @param mode Character. Pipeline mode (typically from MODE variable: "nantes", 
#'              "paris", "pemb", "france", "sensors", "all", or "test").  Default: "all".
#' @return List of file paths for the given extent.
#' @export
build_prediction_filepaths <- function(extent, mode = NULL) {
  # Default mode for prediction is "all"
  if (is.null(x = mode) || !nzchar(x = mode)) {
    mode <- ifelse(test = exists("MODE") && nzchar(x = MODE), 
                   yes  = MODE, 
                   no   = "all")
  }

  # Generate file paths
  switch(extent,
    sensors = list(
      filepath   = file.path(PREDICTION_DIR, mode,
                              sprintf("07_predictions_%s.gpkg", mode)) ),
    nantes  = list(
      filepath   = file.path(PREDICTION_DIR, mode,
                              sprintf("07_predictions_%s.gpkg", mode)) ),
    paris   = list(
      filepath   = file.path(PREDICTION_DIR, mode,
                              sprintf("07_predictions_%s.gpkg", mode)) ),
    pemb    = list(
      filepath   = file.path(PREDICTION_DIR, mode,
                              sprintf("07_predictions_%s.gpkg", mode)) ),
    france  = list(
      output_dir = file.path(PREDICTION_DIR, mode),
      den        = file.path(PREDICTION_DIR, mode,
                             sprintf("07_predictions_%s_traffic_DEN.gpkg", 
                                     mode)),
      hourly     = file.path(PREDICTION_DIR, mode,
                             sprintf("07_predictions_%s_traffic_hourly.gpkg", 
                                     mode)),
      hourly_wd  = file.path(PREDICTION_DIR, mode,
                             sprintf("07_predictions_%s_traffic_hourly_wd.gpkg", 
                                     mode)),
      hourly_we  = file.path(PREDICTION_DIR, mode,
                             sprintf("07_predictions_%s_traffic_hourly_we.gpkg", 
                                     mode))
    ),
    pipeline_message(sprintf(paste("Unknown extent: ", 
                             "%s. Valid: sensors, nantes, paris, pemb, france"), 
                             extent),
                     process = "stop")
  )
}
#' 
#' @title Compare sf CRS objects safely
#' @description Compare two sf CRS objects by EPSG or WKT when available.
#' @param crs_a First CRS object or numeric EPSG code to compare (reference).
#' @param crs_b Second CRS object or numeric EPSG code to compare (target).
#' @return Logical scalar. TRUE if the CRS definitions are equivalent.
sf_crs_matches <- function(crs_a, crs_b) {
  crs_a <- sf::st_crs(crs_a)
  crs_b <- sf::st_crs(crs_b)
  if (is.na(x = crs_a) || is.na(x = crs_b)) {
    return(FALSE)
  }
  if (!is.null(x = crs_a$epsg) && !is.null(x = crs_b$epsg) &&
      !is.na(x = crs_a$epsg) && !is.na(x = crs_b$epsg)) {
    return(crs_a$epsg == crs_b$epsg)
  }
  if (!is.null(x = crs_a$wkt) && !is.null(x = crs_b$wkt) &&
      nzchar(x = crs_a$wkt) && nzchar(x = crs_b$wkt)) {
    return(crs_a$wkt == crs_b$wkt)
  }
  if (!is.null(x = crs_a$proj4string) && !is.null(x = crs_b$proj4string) &&
      nzchar(x = crs_a$proj4string) && nzchar(x = crs_b$proj4string)) {
    return(crs_a$proj4string == crs_b$proj4string)
  }
  return(identical(crs_a, crs_b))
}
#' 
#' @title Ensure an sf object uses the target CRS
#' @description Assigns the target CRS if missing, or transforms the object if
#'              the current CRS is different.
#' @param sf_obj An sf object to transform if necessary.
#' @param target_crs Target CRS (numeric EPSG code or crs object).
#' @return sf object with the target CRS.
ensure_target_crs <- function(sf_obj, target_crs) {
  if (!inherits(x = sf_obj, what = "sf")) {
    return(sf_obj)
  }
  target_crs <- sf::st_crs(x = target_crs)
  current_crs <- sf::st_crs(x = sf_obj)
  if (is.na(x = current_crs)) {
    sf::st_crs(x = sf_obj) <- target_crs
    return(sf_obj)
  }
  if (!sf_crs_matches(x = current_crs, y = target_crs)) {
    sf_obj <- sf::st_transform(x   = sf_obj, 
                               crs = target_crs)
  }
  return(sf_obj)
}

#' @title Validate tile chunk GeoPackage files
#' @description Check that all expected tile chunk files exist and use the target CRS.
#' @param tile_files Character vector of GeoPackage file paths.
#' @param target_crs Target CRS (numeric EPSG code or crs object).
#' @return Logical scalar. TRUE if all files exist and match the target CRS.
tile_chunk_files_valid <- function(tile_files, target_crs) {
  if (length(x = tile_files) == 0L) {
    return(FALSE)
  }
  if (!all(file.exists(tile_files))) {
    return(FALSE)
  }
  crs_ok <- vapply(X = tile_files, FUN = function(tile_fp) {
    sf_obj <- tryCatch(
      sf::st_read(dsn = tile_fp, quiet = TRUE, n_max = 0),
      error = function(e) NULL)
    if (is.null(x = sf_obj)) {
      return(FALSE)
    }
    sf_crs_matches(x = sf::st_crs(x = sf_obj), y = target_crs)
  }, FUN.VALUE = logical(1))
  all(crs_ok)
}
#'
# ------------------------------------------------------------------------------
# Load and crop France engineered network
# ------------------------------------------------------------------------------
#' @title Load and crop France engineered network to bounding box
#' @description Load and crop France engineered network to bounding box for 
#'              prediction.
#' @param bbox numeric vector c(xmin, ymin, xmax, ymax) in target CRS
#' @param cfg Configuration list
#' @return sf data.frame with cropped network
load_network_for_prediction <- function(bbox, cfg) {
  
  # Configuration parameters
  target_crs            <- cfg$TARGET_CRS
  osm_roads_path        <- cfg$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH
  default_vehicle_speed <- cfg$DEFAULT_VEHICLE_SPEED
  xgb_models_path       <- cfg$XGB_MODELS_WITH_RATIOS_FILEPATH
  xgb_feature_path      <- cfg$XGB_RATIO_FEATURE_INFO_FILEPATH

  pipeline_message(sprintf("Loading OSM France engineered network from %s", 
                           rel_path(osm_roads_path)), 
                   level = 1, progress = "start", process = "load")
  
  # Memory check before loading large GPKG
  check_memory_available(
    operation_name = "Load France engineered network (GPKG)",
    min_gb         = 2, 
    warn_gb        = 4)
  
  # Use spatial filter at read time (wkt_filter) to avoid loading entire France
  # This is MUCH more memory-efficient than load-all-then-filter
  if (!is.null(x = bbox) && length(x = bbox) == 4) {
    xmin <- as.numeric(x = bbox[1])
    ymin <- as.numeric(x = bbox[2])
    xmax <- as.numeric(x = bbox[3])
    ymax <- as.numeric(x = bbox[4])
    if (anyNA(c(xmin, ymin, xmax, ymax))) {
      pipeline_message(sprintf("Invalid bbox values for prediction crop: %s", 
                               paste(bbox, collapse = ", ")), 
                       process = "stop")
    }
    
    # Build WKT polygon for spatial filter at GDAL level
    wkt_bbox <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
                        xmin, ymin, 
                        xmax, ymin, 
                        xmax, ymax, 
                        xmin, ymax, 
                        xmin, ymin)
    
    osm_network <- sf::st_read(
      dsn        = osm_roads_path,
      wkt_filter = wkt_bbox,
      quiet      = TRUE)
    
    pipeline_message(sprintf("Network loaded with spatial filter: %s roads", 
                             fmt(nrow(x = osm_network))), 
                     process = "info")
  } else {
    # No bbox: load full network (warning: memory-intensive)
    pipeline_message(
      "No bbox provided — loading entire France network (memory-intensive)", 
      process = "warning")
    check_memory_available(
      operation_name = "Load entire France network (no bbox)",
      min_gb         = 8, 
      warn_gb        = 12)
    osm_network <- sf::st_read(
      dsn   = osm_roads_path,
      quiet = TRUE)
  }
  
  # Ensure correct CRS
  osm_network <- ensure_target_crs(sf_obj = osm_network, 
                                  target_crs = target_crs)

  if (nrow(x = osm_network) == 0) {
    pipeline_message("No roads found for prediction after cropping. Check bbox/CRS.", 
                     process = "stop")
  }
  
  pipeline_message(sprintf("Network loaded: %s roads", fmt(nrow(x = osm_network))), 
                   level = 1, progress = "end", process = "valid")
  
  return(osm_network)
}
#' 
# ------------------------------------------------------------------------------
# Load and filter network by point buffers
# ------------------------------------------------------------------------------
#' @title Load and filter network by point buffers
#' @description Load and filter network by point buffers for prediction based on 
#'              sensor points.
#' @param points sf data.frame with point geometries
#' @param buffer_radius numeric buffer radius in meters
#' @param config CONFIG list with file paths
#' @return sf data.frame with filtered network by point buffers 
load_network_around_points <- function(points, buffer_radius, config) {
  target_crs     <- config$TARGET_CRS
  osm_roads_path <- config$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH

  pipeline_message(sprintf("Loading network within %sm of %s points", 
                           buffer_radius, nrow(x = points)), 
                   level = 1, progress = "start", process = "load")
  
  # Memory check
  check_memory_available(
    operation_name = "Load network around sensor points",
    min_gb         = 2, 
    warn_gb        = 4)
  
  # Ensure points CRS
  if (!sf_crs_matches(sf::st_crs(x = points), target_crs)) {
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
    dsn        = osm_roads_path,
    wkt_filter = wkt_bbox,
    quiet      = TRUE)
  
  # Ensure correct CRS
  osm_network <- ensure_target_crs(sf_obj = osm_network, 
                                  target_crs = target_crs)
  
  # Create buffers and filter precisely
  buffers         <- sf::st_buffer(x = points, dist = buffer_radius)
  combined_buffer <- sf::st_union(x = buffers)
  osm_network     <- sf::st_filter(x = osm_network, y = combined_buffer)
  
  pipeline_message(sprintf("Network filtered: %s roads within buffers", 
                           fmt(nrow(x = osm_network))), 
                   level = 1, progress = "end", process = "valid")
  
  return(osm_network)
}
#' 
# ------------------------------------------------------------------------------
# Safe sparse model matrix
# ------------------------------------------------------------------------------
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
      return(Matrix::sparse.model.matrix(object = formula_obj, data = data_df))
    }
    mm_data <- data_df
    for (v in vars_in_formula) {
      if (is.factor(x = mm_data[[v]])) {
        mm_data[[v]] <- as.character(x = mm_data[[v]])
      }
    }
    mm_subset <- mm_data[, vars_in_formula, drop = FALSE]
    cc_idx    <- complete.cases(mm_subset)
    cc_data   <- mm_data[cc_idx, , drop = FALSE]
    for (v in vars_in_formula) {
      if (is.character(x = mm_data[[v]])) {
        lv <- unique(x = cc_data[[v]])
        lv <- lv[!is.na(x = lv) & nzchar(x = lv)]
        if (length(x = lv) <= 1) {
          mm_data[[v]] <- 0
        }
      }
    }
    Matrix::sparse.model.matrix(object = formula_obj, data = mm_data)
  }
#' 
# ------------------------------------------------------------------------------
# Apply XGBoost models to network
# ------------------------------------------------------------------------------
#' @title Apply XGBoost models to network data
#' @description Apply XGBoost models to network data. Returns predictions for 
#'              all periods. 
#' @param network_data data.frame with engineered features
#' @param models_list list of trained XGBoost models
#' @param feature_info list with feature formula and periods
#' @return data.frame with predictions for all periods
apply_xgboost_predictions <- function(network_data, 
                                      models_list, 
                                      feature_info, 
                                      default_vehicle_speed = 50) {
  pipeline_message("Applying XGBoost models to network", 
                   level = 1, progress = "start", process = "calc")
  
  # Memory check: predictions will create ~n_roads x n_periods x 3 columns
  n_roads   <- nrow(x = network_data)
  n_periods <- length(x = feature_info$all_periods)
  est_mb    <- round(x = n_roads * n_periods * 3 * 8 / 1024^2)  # 8 bytes / double
  check_memory_available(
    operation_name = sprintf("XGBoost prediction (%s roads x %d periods, ~%d MB result)",
              fmt(n_roads), n_periods, est_mb),
    min_gb        = 1, 
    warn_gb       = 3)

  # lane_number is an AVATAR-derived directional lane feature used in training.
  # For region-wide prediction (no AVATAR), derive a proxy from OSM lanes.
  if (!"lane_number" %in% names(x = network_data)) {
    if ("lanes_directional" %in% names(x = network_data)) {
      network_data$lane_number <- suppressWarnings(
        expr = as.numeric(x = network_data$lanes_directional))
    } else if ("lanes_osm" %in% names(x = network_data)) {
      lanes_raw <- suppressWarnings(
                      expr = as.numeric(x = network_data$lanes_osm))
      network_data$lane_number <- pmax(1, round(x = lanes_raw / 2))
    } else {
      network_data$lane_number <- 1
    }
    network_data$lane_number[is.na(x = network_data$lane_number) | 
                             !is.finite(x = network_data$lane_number)] <- 1
    pipeline_message(paste("lane_number missing in prediction input;", 
                           "derived proxy from OSM lane attributes"), 
                     process = "info")
  }

  pipeline_message(sprintf("Constructing feature matrix for %s roads", 
                           fmt(n_roads)), 
                   level = 2, progress = "start", process = "calc")
  
  # Prepare feature matrix (same encoding as training: sparse.model.matrix)
  rownames(x = network_data) <- seq_len(to = nrow(x = network_data))
  feature_matrix_part <- safe_sparse_model_matrix(
    formula_obj = feature_info$road_feature_formula,
    data_df     = network_data)
  rows_used <- as.integer(x = rownames(x = feature_matrix_part))
  if (length(x = rows_used) == 0 || anyNA(rows_used)) {
    rows_used <- seq_len(to = nrow(x = feature_matrix_part))
  }
  rows_used <- rows_used[rows_used >= 1 & rows_used <= nrow(x = network_data)]
  feature_matrix <- matrix(
    NA_real_,
    nrow     = nrow(x = network_data),
    ncol     = ncol(x = feature_matrix_part),
    dimnames = list(rownames(x = network_data), 
                    colnames(x = feature_matrix_part))
  )
  if (length(x = rows_used) != nrow(x = feature_matrix_part)) {
    # Fallback alignment when sparse.model.matrix rownames are unavailable
    n_common  <- min(length(x = rows_used), nrow(x = feature_matrix_part))
    rows_used <- rows_used[seq_len(to = n_common)]
    feature_matrix[rows_used, ] <- as.matrix(
          x = feature_matrix_part[seq_len(to = n_common), , drop = FALSE])
  } else {
    feature_matrix[rows_used, ] <- as.matrix(x = feature_matrix_part)
  }
  feature_matrix <- as.data.frame(x = feature_matrix, stringsAsFactors = FALSE)
  
  pipeline_message(
    sprintf("Feature matrix constructed with %d rows and %d features", 
            nrow(x = feature_matrix), ncol(x = feature_matrix)), 
            level = 2, progress = "end", process = "valid")
#' 
# ------------------------------------------------------------------------------
# Apply XGBoost models to network data
# ------------------------------------------------------------------------------
#' @title Apply XGBoost models to network data
#' @description Apply XGBoost models to network data. Returns predictions for 
#'              all periods. Align feature matrix to a model's expected features 
#'              before training and predict with the model. Uses the EXACT 
#'              column names from training stored in feature_info. Handles 
#'              factor level mismatches between training and prediction data.
#' @param network_data data.frame with engineered features
#' @param models_list list of trained XGBoost models
#' @param feature_info list of feature information
#' @return data.frame with predictions for all periods
#' @export
  predict_with_alignment <- function(model_entry, 
                                     feature_matrix_base, 
                                     feature_info) {

    # "model_entry" may be either the raw xgb.Booster object (old behaviour)
    # or the list entry stored in models_list (which contains both the booster
    # and the saved feature_names).  This wrapper normalises input.
    if (is.list(x = model_entry) && !is.null(x = model_entry$model)) {
      model_obj       <- model_entry$model
      model_features  <- model_entry$feature_names
    } else {
      model_obj       <- model_entry
      model_features  <- NULL       # Booster objects do not carry feature_names
    }
    training_features <- feature_info$feature_names_from_training
    
    # Start with base matrix (remove Intercept if present)
    fm <- feature_matrix_base
    if ("(Intercept)" %in% colnames(x = fm)) {
      fm <- fm[, colnames(x = fm) != "(Intercept)", drop = FALSE]
    }
    
    # Prefer the per-model feature list if available; this ensures each
    # XGBoost booster receives exactly the columns it was trained with.
    target_features <- NULL
    target_source <- NULL
    if (!is.null(x = model_features) && length(x = model_features) > 0) {
      target_features <- model_features
      target_source   <- "model"
    } else if (!is.null(x = training_features) && 
               length(x = training_features) > 0) {
      target_features <- training_features
      target_source   <- "feature_info"
    }
    
    # Warn if both sources are present but differ substantially
    if (!is.null(x = model_features) && !is.null(x = training_features) && 
        length(x = model_features) != length(x = training_features)) {
      pipeline_message(
        sprintf(paste("Note: model has %d features", 
                      "but feature_info lists %d features; using model list"), 
                length(x = model_features), length(x = training_features)),
        process = "warning")
    }
    
    if (!is.null(x = target_features) && length(x = target_features) > 0) {
      fm_cols <- colnames(x = fm)
      
      # Step 1: Add missing columns
      missing_cols <- setdiff(x = target_features, y = fm_cols)
      if (length(x = missing_cols) > 0) {
        for (mc in missing_cols) {
          fm[[mc]] <- 0
        }
        pipeline_message(
          sprintf("Added %d missing feature columns with zero values", 
                  length(x = missing_cols)),
          process = "warning")
      }
      
      # Step 2: Identify and handle EXTRA columns
      extra_cols <- setdiff(x = fm_cols, y = target_features)
      if (length(x = extra_cols) > 0) {
        # Report extra columns (could indicate factor level mismatch)
        extra_summary   <- paste(head(x = extra_cols, n = 5), collapse = ", ")
        if (length(x = extra_cols) > 5) {
          extra_summary <- paste0(extra_summary, 
                                  " ... (", length(x = extra_cols) - 5, 
                                  " more)")
        }
        pipeline_message(
          sprintf(paste("Removing %d extra feature columns", 
                       "(factor level mismatch?): %s"),
                  length(x = extra_cols), extra_summary),
          process = "warning")
      }
      
      # Step 3: Select and reorder columns to match target exactly
      fm <- fm[, target_features, drop = FALSE]
      
      pipeline_message(
        sprintf("Feature matrix aligned (%s): %d columns (base %d, aligned %d)", 
                target_source, ncol(x = fm), ncol(x = feature_matrix_base), 
                length(x = target_features)),
        process = "info")
    } else {
      pipeline_message(
        paste("No training feature names available;", 
              "using raw feature matrix (may cause dimension errors)"),
        process = "warning")
    }
    
    # Create DMatrix and predict
    dmat <- xgboost::xgb.DMatrix(data = as.matrix(x = fm))
    predict(object = model_obj, newdata = dmat)
  }
  
  # Predict base models (period D)
  if (is.null(x = models_list$flow_D$model)) {
    pipeline_message("Missing base model for period D: flow_D", 
                     process = "stop")
  }
  flow_D <- predict_with_alignment(models_list$flow_D, 
                                   feature_matrix, 
                                   feature_info)
  truck_pct_D <- if (is.null(x = models_list$truck_pct_D$model)) {
    pipeline_message("Missing base model truck_pct_D: outputs will be NA", 
                     process = "warning")
    rep(x = NA_real_, length(x = flow_D))
  } else {
    predict_with_alignment(models_list$truck_pct_D, 
                           feature_matrix, 
                           feature_info)
  }
  speed_model_target <- NA_character_
  if (!is.null(x = models_list$speed_D$config) &&
      !is.null(x = models_list$speed_D$config$target)) {
    speed_model_target <- as.character(x = models_list$speed_D$config$target)
  }

  speed_D_raw <- if (is.null(x = models_list$speed_D$model)) {
    pipeline_message("Missing base model speed_D: outputs will be NA", 
                     process = "warning")
    rep(x = NA_real_, length(x = flow_D))
  } else {
    predict_with_alignment(models_list$speed_D, feature_matrix, feature_info)
  }

  speed_osm_raw <- suppressWarnings(expr = as.numeric(x = network_data$speed))
  speed_osm_missing <- is.na(x = speed_osm_raw) | speed_osm_raw <= 0

  if (identical(speed_model_target, "ratio_speed_to_osm")) {
    # New model: speed_D predicts a ratio to OSM speed.
    speed_osm_base <- speed_osm_raw
    speed_osm_base[speed_osm_missing] <- default_vehicle_speed
    speed_D <- ifelse(test = all(is.na(x = speed_D_raw)), 
                      yes   = rep(x = NA_real_, length(x = speed_D_raw)), 
                      no    = pmax(5, speed_D_raw * speed_osm_base))
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
      sprintf(paste("Prediction-time OSM speed imputation:" , 
                    "%s missing values imputed with %s"), 
              fmt(n_speed_osm_missing), 
              ifelse(test = all(is.na(x = speed_D)), 
                     yes = "DEFAULT_VEHICLE_SPEED", 
                     no = "speed_D XGBoost predictions")),
      process = "warning"
    )
    if (all(is.na(x = speed_D))) {
      speed_osm[speed_osm_missing] <- default_vehicle_speed
    } else {
      speed_osm[speed_osm_missing] <- pmax(5, speed_D[speed_osm_missing])
    }
  }
  
  # Initialize results data.frame
  results <- data.frame(
    osm_id            = network_data$osm_id,
    highway           = network_data$highway,
    osm_speed         = speed_osm,
    osm_speed_imputed = as.integer(x = speed_osm_missing)
  )
  
  # Predict all periods
  for (period in feature_info$all_periods) {
    if (period == "D") {
      results[[paste0("flow_", period)]]      <- 10^flow_D  # Inverse log10
      results[[paste0("truck_pct_", period)]] <- truck_pct_D
      results[[paste0("speed_", period)]]     <- speed_D
    } else {
      # Predict ratios
      ratio_flow_model <- models_list[[paste0("ratio_flow_", 
                                              period)]]$model
      ratio_truck_model <- models_list[[paste0("ratio_truck_pct_", 
                                               period)]]$model
      ratio_speed_model <- models_list[[paste0("ratio_speed_", 
                                               period)]]$model
      
      ratio_flow <- if (is.null(x = ratio_flow_model)) {
        rep(x = NA_real_, length(x = flow_D))
      } else {
        predict_with_alignment(models_list[[paste0("ratio_flow_", 
                                                   period)]], 
                               feature_matrix, 
                               feature_info)
      }
      ratio_truck_pct <- if (all(is.na(x = truck_pct_D))) {
        rep(x = NA_real_, length(x = truck_pct_D))
      } else if (is.null(x = ratio_truck_model)) {
        rep(x = NA_real_, length(x = truck_pct_D))
      } else {
        predict_with_alignment(models_list[[paste0("ratio_truck_pct_", 
                                                   period)]], 
                               feature_matrix, 
                               feature_info)
      }
      ratio_speed <- if (all(is.na(x = speed_D))) {
        rep(x = NA_real_, length(x = speed_D))
      } else if (is.null(x = ratio_speed_model)) {
        rep(x = NA_real_, length(x = speed_D))
      } else {
        predict_with_alignment(models_list[[paste0("ratio_speed_", 
                                            period)]], 
                               feature_matrix, 
                               feature_info)
      }
      
      # Apply ratios to base predictions
      results[[paste0("flow_", period)]]      <- (10^flow_D) * ratio_flow
      results[[paste0("truck_pct_", period)]] <- truck_pct_D * ratio_truck_pct
      results[[paste0("speed_", period)]]     <- speed_D * ratio_speed
    }
  }

  # Clamp predictions to sensible ranges
  highway_chr     <- tolower(x = as.character(x = results$highway))
  speed_min_by_hw <- c(
    motorway = 30, trunk = 20, primary = 15, secondary = 15, tertiary = 12,
    residential = 10, unclassified = 10, service = 5, living_street = 5
  )
  speed_max_by_hw <- c(
    motorway = 130, trunk = 110, primary = 90, secondary = 80, tertiary = 70,
    residential = 50, unclassified = 60, service = 40, living_street = 30
  )
  flow_max_by_hw  <- c(
    motorway = 12000, trunk = 9000, primary = 7000, secondary = 5000,
    tertiary = 3000, residential = 1200, unclassified = 1500,
    service = 600, living_street = 300
  )
  speed_min_vec <- as.numeric(x = speed_min_by_hw[highway_chr])
  speed_max_vec <- as.numeric(x = speed_max_by_hw[highway_chr])
  flow_max_vec  <- as.numeric(x = flow_max_by_hw[highway_chr])
  speed_min_vec[is.na(x = speed_min_vec)] <- 5
  speed_max_vec[is.na(x = speed_max_vec)] <- 130
  flow_max_vec[is.na(x = flow_max_vec)]   <- 15000

  flow_cols <- grep(pattern = "^flow_", x = names(x = results), value = TRUE)
  if (length(x = flow_cols) > 0) {
    results[flow_cols] <- lapply(X   = results[flow_cols], 
                                 FUN = function(x) pmin(flow_max_vec, pmax(0, x))
    )
  }
  truck_cols <- grep(pattern = "^truck_pct_", 
                     x = names(x = results), 
                     value = TRUE)
  if (length(x = truck_cols) > 0) {
    results[truck_cols] <- lapply(X   = results[truck_cols], 
                                  FUN = function(x) pmin(100, pmax(0, x)))
  }
  speed_cols <- grep(pattern = "^speed_", x = names(x = results), value = TRUE)
  if (length(x = speed_cols) > 0) {
    results[speed_cols] <- lapply(
      X   = results[speed_cols], 
      FUN = function(x) pmin(speed_max_vec, pmax(speed_min_vec, x))
    )
  }
  
  # Guard NaN/Inf: impute with median by highway type + emit warning
  pred_cols <- c(flow_cols, truck_cols, speed_cols)
  total_nan <- 0L
  for (col in pred_cols) {
    bad_idx <- which(x = is.nan(x = results[[col]]) | 
                     is.infinite(x = results[[col]]))
    if (length(x = bad_idx) > 0) {
      total_nan <- total_nan + length(x = bad_idx)
      # Impute each NaN with the median of same highway type
      global_med <- median(x = results[[col]][is.finite(x = results[[col]])], 
                                                        na.rm = TRUE)
      for (hw in unique(x = results$highway[bad_idx])) {
        hw_rows <- which(x = results$highway == hw)
        hw_good <- results[[col]][setdiff(x = hw_rows, y = bad_idx)]
        med_val <- ifelse(test = length(x = hw_good) > 0, 
                          yes  = median(x = hw_good, na.rm = TRUE), 
                          no   = NA_real_)
        if (!is.finite(x = med_val)) med_val <- global_med
        results[[col]][intersect(x = bad_idx, y = hw_rows)] <- med_val
      }
    }
  }
  if (total_nan > 0) {
    pipeline_message(
      sprintf(paste("NaN/Inf detected in predictions:", 
                    "%d values imputed with median by highway type"),
              total_nan),
      process = "warning")
  }
  
  pipeline_message(sprintf("Predictions completed for %s roads x %s periods", 
                           fmt(nrow(x = results)), 
                           fmt(length(x = feature_info$all_periods))), 
                   process = "valid")
  
  return(results)
}
#' 
# ------------------------------------------------------------------------------
# Validate prediction outputs
# ------------------------------------------------------------------------------
#' @title Validate prediction outputs for quality control
#' @description Validate prediction outputs for quality control. Check for 
#'              negative TV, speed out of range, and truck pct out of range.
#' @param predictions data.frame with predicted values for all periods and 
#'                    features
#' @return list with validation results
#' @export
validate_predictions <- function(predictions) {
  issues <- list()

  is_long_format <- all(c("period", "speed") %in% names(x = predictions))
  has_tv         <- "TV" %in% names(x = predictions)
  has_truck      <- "truck_pct" %in% names(x = predictions)
  
  if (is_long_format) {
    if (has_tv) {
      n_negative_tv <- sum(predictions$TV < 0, na.rm = TRUE)
      if (n_negative_tv > 0) {
        issues[["TV_negative"]] <- n_negative_tv
      }
    }

    if (has_truck) {
      n_exceed_truck <- sum(predictions$truck_pct > 100 | 
                            predictions$truck_pct < 0, na.rm = TRUE)
      if (n_exceed_truck > 0) {
        issues[["truck_pct_out_of_range"]] <- n_exceed_truck
      }
    }

    n_unrealistic_speed <- sum(predictions$speed < 5 | 
                               predictions$speed > 200, na.rm = TRUE)
    if (n_unrealistic_speed > 0) {
      issues[["speed_unrealistic"]] <- n_unrealistic_speed
    }

    # Temporal coherence checks (when D and N are available)
    if (has_tv && "osm_id" %in% names(x = predictions)) {
      d_rows <- predictions[as.character(x = predictions$period) == "D", 
                            c("osm_id", "TV", "speed")]
      n_rows <- predictions[as.character(x = predictions$period) == "N", 
                            c("osm_id", "TV", "speed")]
      names(x = d_rows) <- c("osm_id", "TV_D", "speed_D")
      names(x = n_rows) <- c("osm_id", "TV_N", "speed_N")
      dn <- merge(x = d_rows, y = n_rows, by = "osm_id", all = FALSE)
      if (nrow(x = dn) > 0) {
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
    flow_cols <- grep(pattern = "^flow_", x = names(predictions), value = TRUE)
    for (col in flow_cols) {
      n_negative <- sum(predictions[[col]] < 0, na.rm = TRUE)
      if (n_negative > 0) {
        issues[[paste0(col, "_negative")]] <- n_negative
      }
    }

    truck_cols <- grep(pattern = "^truck_pct_", 
                       x = names(predictions), 
                       value = TRUE)
    for (col in truck_cols) {
      n_exceed <- sum(predictions[[col]] > 100 | predictions[[col]] < 0, 
                      na.rm = TRUE)
      if (n_exceed > 0) {
        issues[[paste0(col, "_out_of_range")]] <- n_exceed
      }
    }

    speed_cols <- grep(pattern = "^speed_", x = names(predictions), value = TRUE)
    for (col in speed_cols) {
      n_unrealistic <- sum(predictions[[col]] < 5 | predictions[[col]] > 200,
                           na.rm = TRUE)
      if (n_unrealistic > 0) {
        issues[[paste0(col, "_unrealistic")]] <- n_unrealistic
      }
    }

    # Temporal coherence for wide format
    if (all(c("flow_D", "flow_N") %in% names(predictions))) {
      n_flow_n_gt_d <- sum(predictions$flow_N > (predictions$flow_D * 1.05), 
                           na.rm = TRUE)
      if (n_flow_n_gt_d > 0) {
        issues[["coherence_flow_N_gt_D"]] <- n_flow_n_gt_d
      }
    }
    if (all(c("speed_D", "speed_N") %in% names(predictions))) {
      n_speed_n_lt_d <- sum(predictions$speed_N + 0.5 < predictions$speed_D, 
                            na.rm = TRUE)
      if (n_speed_n_lt_d > 0) {
        issues[["coherence_speed_N_lt_D"]] <- n_speed_n_lt_d
      }
    }
  }
  
  return(list(is_valid = length(x = issues) == 0,
              issues   = issues,
              n_rows   = nrow(x = predictions)))
}
#' 
# ------------------------------------------------------------------------------
#' Add QGIS-friendly datetime columns from period labels
# ------------------------------------------------------------------------------
#' @title Add QGIS-friendly datetime columns from period labels
#' @description Add QGIS-friendly datetime columns from period labels. Mapping 
#'              rules are:
#'              \itemize{
#'                \item{D/E/N -> day 01 of base year (D=06-18h, E=18-22h, 
#'                      N=22-06h)}
#'                \item{h0_wd..h23_wd -> day 02 of base year (weekdays)}
#'                \item{h0_we..h23_we -> day 03 of base year (weekends)}
#'                \item{h0..h23 -> day 04 of base year (generic hourly)}
#'              }
#'              Base year is extracted from the first measure_datetime in AVATAR 
#'              CSV data.
#' @param predictions_long data.frame with a `period` column
#' @param cfg Configuration list (optional, for AVATAR_CSV_DIR)
#' @return data.frame with added `datetimestart` and `datetimeend` POSIXct 
#'         columns (UTC timezone). These are properly recognized by QGis for 
#'         temporal animation when written to GeoPackage via sf::st_write().
#' @examples 
#' \dontrun{
#' add_period_datetime_columns(predictions_long)
#' }
#' @export 
add_period_datetime_columns <- function(predictions_long, cfg = NULL) {
  if (!"period" %in% names(predictions_long)) {
    return(predictions_long)
  }

  period_chr <- as.character(x = predictions_long$period)
  n          <- length(x = period_chr)

  # Store as POSIXct (native datetime type) so sf/GDAL writes to GeoPackage correctly
  # QGis recognizes POSIXct-backed datetime columns for temporal animation
  datetimestart <- as.POSIXct(rep(NA_real_, n), tz = "UTC")
  datetimeend   <- as.POSIXct(rep(NA_real_, n), tz = "UTC")

  # Get base year from AVATAR data
  avatar_dir <- ifelse(test = !is.null(x = cfg) && !is.null(x = cfg$AVATAR_CSV_DIR), 
                       yes  = cfg$AVATAR_CSV_DIR, 
                       no   = file.path("data", "avatar", "csv"))
  base_year <- 2023  # fallback
  if (dir.exists(paths = avatar_dir)) {
    csv_files <- list.files(path       = avatar_dir, 
                            pattern    = "\\.csv$", 
                            full.names = TRUE)
    if (length(x = csv_files) > 0) {
      first_csv <- csv_files[1]
      # Read first data line (skip header)
      con             <- file(description = first_csv, open = "r")
      header          <- readLines(con = con, n = 1)
      first_data_line <- readLines(con = con, n = 1)
      close(con)
      if (length(x = first_data_line) > 0) {
        # Split by comma, find measure_datetime column
        cols        <- strsplit(x = header, ",")[[1]]
        data_cols   <- strsplit(x = first_data_line, ",")[[1]]
        measure_idx <- which(x = cols == "measure_datetime")
        if (length(x = measure_idx) == 1) {
          measure_datetime <- data_cols[measure_idx]
          # Extract date part: 2023-01-01T00:00:00+01:00 -> 2023-01-01
          date_str  <- substr(x = measure_datetime, start = 1, stop = 10)
          base_year <- as.integer(x = format(as.Date(x = date_str), "%Y"))
        }
      }
    }
  }

  # D / E / N reference periods (day 01)
  idx_D <- which(x = period_chr == "D")
  if (length(x = idx_D) > 0) {
    datetimestart[idx_D] <- as.POSIXct(sprintf("%d-01-01 06:00:00", base_year), tz = "UTC")
    datetimeend[idx_D]   <- as.POSIXct(sprintf("%d-01-01 18:00:00", base_year), tz = "UTC")
  }

  idx_E <- which(x = period_chr == "E")
  if (length(x = idx_E) > 0) {
    datetimestart[idx_E] <- as.POSIXct(sprintf("%d-01-01 18:00:00", base_year), tz = "UTC")
    datetimeend[idx_E]   <- as.POSIXct(sprintf("%d-01-01 22:00:00", base_year), tz = "UTC")
  }

  idx_N <- which(x = period_chr == "N")
  if (length(x = idx_N) > 0) {
    datetimestart[idx_N] <- as.POSIXct(sprintf("%d-01-01 22:00:00", base_year), tz = "UTC")
    datetimeend[idx_N]   <- as.POSIXct(sprintf("%d-01-02 06:00:00", base_year), tz = "UTC")
  }

  # Generic hourly periods h0..h23 (day 04)
  m_h <- regexec(pattern = "^h([0-9]{1,2})$", 
                 text    = period_chr)
  g_h <- regmatches(x = period_chr, 
                    m = m_h)
  idx_h <- which(x = lengths(g_h) == 2)
  if (length(x = idx_h) > 0) {
    h_vals <- as.integer(x = vapply(X   = g_h[idx_h], 
                                    FUN = function(x) x[2], character(1)))
    start_str <- sprintf("%d-01-04 %02d:00:00", base_year, h_vals)
    datetimestart[idx_h] <- as.POSIXct(x = start_str, tz = "UTC")
    datetimeend[idx_h]   <- datetimestart[idx_h] + 3600
  }

  # Weekday hourly periods h0_wd..h23_wd (day 02)
  m_wd   <- regexec(pattern = "^h([0-9]{1,2})_wd$", 
                  text    = period_chr)
  g_wd   <- regmatches(x = period_chr, 
                     m = m_wd)
  idx_wd <- which(x = lengths(g_wd) == 2)
  if (length(x = idx_wd) > 0) {
    h_vals    <- as.integer(x   = vapply(X   = g_wd[idx_wd], 
                                         FUN = function(x) x[2], character(1)))
    start_str <- sprintf("%d-01-02 %02d:00:00", base_year, h_vals)
    datetimestart[idx_wd] <- as.POSIXct(x = start_str, tz = "UTC")
    datetimeend[idx_wd]   <- datetimestart[idx_wd] + 3600
  }

  # Weekend hourly periods h0_we..h23_we (day 03)
  m_we <- regexec(pattern = "^h([0-9]{1,2})_we$", 
                  text    = period_chr)
  g_we <- regmatches(x = period_chr, 
                     m = m_we)
  idx_we <- which(x = lengths(g_we) == 2)
  if (length(x = idx_we) > 0) {
    h_vals    <- as.integer(x   = vapply(X   = g_we[idx_we], 
                                         FUN = function(x) x[2], character(1)))
    start_str <- sprintf("%d-01-03 %02d:00:00", base_year, h_vals)
    datetimestart[idx_we] <- as.POSIXct(x  = start_str, 
                                        tz = "UTC")
    datetimeend[idx_we]   <- datetimestart[idx_we] + 3600
  }

  predictions_long$datetimestart <- datetimestart
  predictions_long$datetimeend   <- datetimeend

  return(predictions_long)
}
#' 
# ------------------------------------------------------------------------------
# Generic traffic prediction (regional and tiled)
# ------------------------------------------------------------------------------
#' @title Generic traffic prediction
#' @description Run traffic prediction for a geographic region with optional 
#'              spatial tiling. This function provides a unified interface for 
#'              running both regional and tiled traffic prediction that 
#'              automatically handles both small regions (simple single-file 
#'              export) and more large regions like France (memory-efficient 
#'              spatial tiling with temporal chunking).
#' @details For small regions (bbox-based), the function:
#'          \itemize{
#'            \item{Loads the engineered network cropped to the bounding box},
#'            \item{Applies XGBoost models to predict flow, truck percentage, 
#'                  and speed},
#'            \item{Exports all periods to a single GeoPackage file}.
#'          }
#'          For large regions (tiled method), the function:
#'          \itemize{
#'            \item{Partitions the domain into a regular grid of square tiles},
#'            \item{Processes each tile independently using spatial filters 
#'                  (memory-friendly)},
#'            \item{Optionally splits traffic attributes into separate GPKG 
#'                  files by temporal chunk (e.g., separate files for D/E/N, 
#'                  hourly, weekday, and weekend periods)},
#'            \item{Writes geometry once, appending traffic data incrementally}.
#'          }
#'          Temporal chunks (relevant for tiled method only):
#'          \itemize{
#'            \item{"DEN": D, E, N periods (day/evening/night)}
#'            \item{"hourly": h0..h23 (all days combined)}
#'            \item{"hourly_wd": h0_wd..h23_wd (weekdays only)}
#'            \item{"hourly_we": h0_we..h23_we (weekend only)}
#'          }
#' @param region_name Character. Human-readable region name for log messages.
#' @param cfg Configuration list with model paths and settings.
#' @param bbox Named numeric vector c(xmin, ymin, xmax, ymax) in EPSG:2154,
#'             or NULL. If NULL, triggers tiled method; if provided, triggers
#'   simple region method.
#' @param output_config List with output file paths. Structure depends on 
#'                      method:
#'                      \itemize{
#'                        \item{Simple region: list(filepath = 
#'                              "path/to/output.gpkg")}
#'                        \item{Tiled: list(den = "path/to/traffic_DEN.gpkg",
#'                                       hourly = "path/..._hourly.gpkg",
#'                                       hourly_wd = "path/..._hourly_wd.gpkg",
#'                                       hourly_we = "path/..._hourly_we.gpkg")}
#'                      }
#'                      Note: Both methods now include geometry in output GPKG files.
#' @param method Character. Prediction approach: "region" (simple), "tiled" 
#'               (spatial tiles), or "auto" (auto-detect based on bbox 
#'               presence). Default: "auto".
#' @param chunks Character vector. Temporal chunks to export (tiled method 
#'               only). 
#'               Valid values: "DEN", "hourly", "hourly_wd", "hourly_we". 
#'               Default: c("DEN") for memory efficiency.
#'               For full export use: c("DEN", "hourly", "hourly_wd", 
#'               "hourly_we").
#' @param tile_size_m Numeric. Tile side length in meters for tiled method.
#'                    Default: 200000 (200 km).
#' @return Invisible NULL. Side effects: writes GPKG file(s) to disk.
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
#' # Tiled prediction for France (all temporal chunks)
#' predict_traffic(
#'   region_name = "France",
#'   cfg = CFG,
#'   bbox = NULL,
#'   output_config = list(
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
#' @export
predict_traffic <- function(region_name, 
                            cfg, 
                            bbox = NULL,
                            output_config = NULL,
                            method = "auto",
                            mode = NULL,
                            chunks = c("DEN"),
                            tile_size_m = 200000) {

  # --- Auto-detect method ---
  if (method == "auto") {
    method <- if (is.null(x = bbox)) "tiled" else "region"
  }

  if (!(method %in% c("region", "tiled"))) {
    pipeline_message("method must be one of: 'region', 'tiled', 'auto'", 
                     process = "stop")
  }

  # --- Validate output_config ---
  if (is.null(x = output_config) || !is.list(x = output_config)) {
    pipeline_message("output_config must be a non-empty list", 
                     process = "stop")
  }

  # --- Route to appropriate method ---
  if (method == "region") {
    if (is.null(x = bbox)) {
      pipeline_message("bbox must be provided for method='region'", 
                       process = "stop")
    }
    if (is.null(x = output_config$filepath)) {
      pipeline_message("bbox must be provided for method='region'", 
                       process = "stop")
    }
    .predict_region_impl(
      region_name     = region_name,
      bbox            = bbox,
      output_filepath = output_config$filepath,
      cfg             = cfg
    )
  } else if (method == "tiled") {
    if (!is.null(x = bbox)) {
      pipeline_message(paste("bbox is ignored when method='tiled';", 
                             "use method='region' for bbox-based prediction"), 
                       process = "warning")
    }
    .predict_france_tiled_impl(
      cfg           = cfg,
      region_name   = region_name,
      output_config = output_config,
      mode          = mode,
      tile_size_m   = tile_size_m,
      chunks        = chunks
    )
  }

  invisible(NULL)
}
#' 
# ------------------------------------------------------------------------------
# Internal function for region prediction
# ------------------------------------------------------------------------------
#' @title Internal function for region prediction
#' @description Internal function for region prediction. 
#'              This function is not intended to be called directly by users.
#' @param region_name Character. Human-readable region name for log messages.
#' @param bbox Numeric vector. Bounding box coordinates (xmin, ymin, xmax, ymax).
#' @param output_filepath Character. File path for the output GPKG file.
#' @param cfg List. Configuration parameters.
#' @return Invisible NULL. Side effects: writes GPKG file to disk.
#' @examples
#' \dontrun{
#' .predict_region_impl(
#'   region_name = "PEMB",
#'   bbox = c(xmin = 654892, ymin = 6852748, xmax = 671006, ymax = 6862393),
#'   output_filepath = "data/prediction/pemb.gpkg",
#'   cfg = CFG
#' )
#' }
#' @keywords internal
#' @export
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

  models_list <- readRDS(file = xgb_models_path)
  feature_info <- readRDS(file = xgb_feature_path)

  pipeline_message(sprintf("Models loaded: %s models for %s periods", 
                           length(x = models_list), 
                           length(x = feature_info$all_periods)), 
                   level = 1, progress = "end", process = "valid")

  # --- Bbox ---
  pipeline_message(sprintf("Bbox: [%s, %s, %s, %s]", 
                           bbox[1], bbox[2], bbox[3], bbox[4]), 
                   process = "info")

  # --- Load network ---
  osm_region <- load_network_for_prediction(bbox = bbox, cfg = cfg)

  pipeline_message(sprintf("Network loaded: %s roads in %s", 
                           fmt(nrow(x = osm_region)), region_name), 
                   process = "info")

  # --- Apply predictions ---
  pipeline_message(sprintf("Applying XGBoost models to %s network", 
                           region_name), 
                   level = 1, progress = "start", process = "calc")

  osm_region_dt <- as.data.frame(x = sf::st_drop_geometry(x = osm_region))

  predictions_wide <- apply_xgboost_predictions(
    network_data = osm_region_dt,
    models_list = models_list,
    feature_info = feature_info,
    default_vehicle_speed = cfg$DEFAULT_VEHICLE_SPEED)

  pipeline_message(sprintf("Predictions completed: %s roads x %s periods", 
                           fmt(nrow(x = predictions_wide)), 
                           length(x = feature_info$all_periods)), 
                   level = 1, progress = "end", process = "valid")

  all_periods <- feature_info$all_periods
  rm(models_list, feature_info, osm_region_dt)
  gc(verbose = FALSE)

  # --- Long format ---
  pipeline_message("Converting to long format", level = 1, 
                   progress = "start", process = "calc")

  check_memory_available(
    operation_name = sprintf("Pivot to long format (%s roads)",
                             fmt(nrow(x = predictions_wide))),
    min_gb         = 2, 
    warn_gb        = 4)

  dt <- data.table::as.data.table(predictions_wide)

  flow_cols  <- grep(pattern = "^flow_", x = names(dt), value = TRUE)
  truck_cols <- grep(pattern = "^truck_pct_", x = names(dt), value = TRUE)
  speed_cols <- grep(pattern = "^speed_", x = names(dt), value = TRUE)

  flow_long <- data.table::melt(
    data          = dt,
    id.vars       = c("osm_id","highway","osm_speed","osm_speed_imputed"),
    measure.vars  = flow_cols,
    variable.name = "period",
    value.name    = "flow")
  
  flow_long[, period := sub(pattern     = "^flow_", 
                            replacement = "", 
                            x           = period)]

  truck_long <- data.table::melt(
    data          = dt,
    id.vars       = c("osm_id"),
    measure.vars  = truck_cols,
    variable.name = "period",
    value.name    = "truck_pct")

  truck_long[, period := sub(pattern     = "^truck_pct_", 
                             replacement = "", 
                             x           = period)]

  speed_long <- data.table::melt(
    data          = dt,
    id.vars       = c("osm_id"),
    measure.vars  = speed_cols,
    variable.name = "period",
    value.name    = "speed")

  speed_long[, period := sub(pattern     = "^speed_", 
                             replacement = "", 
                             x           =  period)]

  predictions_long <- flow_long[
    truck_long, on = c("osm_id","period")
  ][
    speed_long, on = c("osm_id","period")
  ]

  predictions_long[, HGV := flow * (truck_pct / 100)]
  predictions_long[, LV := flow - HGV]
  predictions_long[, TV := flow]

  predictions_long[, period := factor(x      = period, 
                                      levels = all_periods)]

  predictions_long <- predictions_long[, .(osm_id, highway, period, TV, HGV, 
                                           LV, speed, osm_speed, 
                                           osm_speed_imputed, truck_pct)]

  predictions_long <- add_period_datetime_columns(predictions_long, cfg)

  rm(predictions_wide)
  gc(verbose = FALSE)

  pipeline_message(sprintf("Long format: %s rows (roads x periods)", 
                           fmt(nrow(x = predictions_long))), 
                   level = 1, progress = "end", process = "valid")

  # --- Validate ---
  validation <- validate_predictions(predictions_long)
  if (!validation$is_valid) {
    pipeline_message(sprintf("Validation warnings: %s issues detected", 
                             length(x = validation$issues)), 
                     process = "warning")
    for (issue_name in names(validation$issues)) {
      pipeline_message(sprintf("\t- %s: %s cases", 
                               issue_name, validation$issues[[issue_name]]), 
                       process = "warning")
    }
  }

  # --- Export ---
  pipeline_message("Exporting predictions with geometry", level = 1, 
                   progress = "start", process = "save")

  check_memory_available(
    operation_name = sprintf("Geometry merge (%s rows)",
                             fmt(nrow(x = predictions_long))),
    min_gb         = 2, 
    warn_gb        = 4)

  # Create output directory if needed
  output_dir <- dirname(path = output_filepath)
  if (!dir.exists(paths = output_dir)) {
    dir.create(path      = output_dir, 
               recursive = TRUE)
  }

  predictions_sf <- merge(
    x  = predictions_long,
    y  = osm_region[, c("osm_id", "name", "geom")],
    by = "osm_id", all.x = TRUE)

  predictions_sf <- sf::st_as_sf(x = predictions_sf)
  predictions_sf <- ensure_target_crs(sf_obj = predictions_sf, 
                                      target_crs = target_crs)

  predictions_sf <- add_period_datetime_columns(predictions_sf, cfg)

  # add explicit logging around the write so duration is recorded
  pipeline_message("Writing GeoPackage file", level = 1,
                   progress = "start", process = "save")
  sf::st_write(
    obj        = predictions_sf,
    dsn        = output_filepath,
    delete_dsn = TRUE,
    quiet      = FALSE)
  pipeline_message(sprintf("Written GeoPackage to %s",
                           rel_path(output_filepath)),
                   level = 1, progress = "end", process = "save")

  pipeline_message(sprintf("Predictions exported to %s",
                           rel_path(output_filepath)), 
                   level = 1, progress = "end", process = "save")

  # --- Summary ---
  pipeline_message(sprintf("Prediction summary for %s:", region_name),
                   process = "info")
  pipeline_message(sprintf("\t- Roads: %s", 
                           fmt(length(x = 
                                  unique(x = predictions_long$osm_id)))), 
                   process = "info")
  pipeline_message(sprintf("\t- Periods: %s", length(x = all_periods)), 
                   process = "info")
  pipeline_message(sprintf("\t- Total predictions: %s", 
                           fmt(nrow(x = predictions_long))), 
                   process = "info")

  period_stats <- predictions_long %>%
    group_by(period) %>%
    summarise(
      avg_TV        = round(x = mean(x = TV, na.rm = TRUE)),
      avg_speed     = round(x = mean(x = speed, na.rm = TRUE), 1),
      avg_truck_pct = round(x = mean(x = truck_pct, na.rm = TRUE), 1),
      .groups       = "drop"
    )

  for (p in c("D", "E", "N", "h7", "h12", "h18")) {
    if (p %in% period_stats$period) {
      stats <- period_stats[period_stats$period == p, ]
      pipeline_message(
        sprintf("\t- Period %s: %d veh/h avg, %.1f km/h, %.1f%% trucks", 
                p, stats$avg_TV, stats$avg_speed, stats$avg_truck_pct), 
                process = "info")
    }
  }

  pipeline_message(sprintf("%s prediction completed", region_name), 
                   level = 0, progress = "end", process = "valid")

  invisible(NULL)
}
#' 
# ------------------------------------------------------------------------------
# Define temporal chunk groups
# ------------------------------------------------------------------------------
#' @title Define temporal chunk groups
#' @description Define temporal chunk groups. Used to group periods into
#'              temporal chunks for prediction. Used in `predict_traffic()`.
#' @return Named list of character vectors (period names per chunk)
#' @examples
#' \dontrun{
#' temporal_chunks <- get_temporal_chunks()
#' }
#' @export
get_temporal_chunks <- function() {
  list(
    DEN        = c("D", "E", "N"),
    hourly     = paste0("h", 0:23),
    hourly_wd  = paste0("h", 0:23, "_wd"),
    hourly_we  = paste0("h", 0:23, "_we")
  )
}
#' 
# ------------------------------------------------------------------------------
# Build spatial tile grid
# ------------------------------------------------------------------------------
#' @title Build spatial tile grid
#' @description Build spatial tile grid covering France extent. Used in
#'              `predict_traffic()`.
#' @param tile_size_m Tile side length in meters (Lambert-93)
#' @return data.frame with columns: tile_id, xmin, ymin, xmax, ymax
#' @examples
#' \dontrun{
#' france_tiles <- build_france_tiles()
#' }
#' @export
build_france_tiles <- function(tile_size_m = 200000) {
  # France extent in EPSG:2154 (Lambert-93) — rounded outward
  france_xmin <- 100000
  france_ymin <- 6050000
  france_xmax <- 1250000
  france_ymax <- 7150000

  xs <- seq(from = france_xmin, to = france_xmax, by = tile_size_m)
  ys <- seq(from = france_ymin, to = france_ymax, by = tile_size_m)

  tiles         <- expand.grid(x = xs, y = ys, stringsAsFactors = FALSE)
  tiles$tile_id <- seq_len(to = nrow(x = tiles))
  tiles$xmin    <- tiles$x
  tiles$ymin    <- tiles$y
  tiles$xmax    <- tiles$x + tile_size_m
  tiles$ymax    <- tiles$y + tile_size_m
  tiles[, c("tile_id", "xmin", "ymin", "xmax", "ymax")]
}
#' 
# ------------------------------------------------------------------------------
# Internal function for tiled prediction
# ------------------------------------------------------------------------------
#' @title Internal function for tiled prediction
#' @description Internal function for running France-wide prediction with 
#'              spatial tiling and temporal chunking. This function is not 
#'              intended to be called directly by users.
#' @details This variant is designed to avoid the enormous memory spike 
#'          encountered when loading the complete national network and pivoting 
#'          all 75 periods at once. The function breaks the domain into a 
#'          regular grid of square tiles, and reads each tile independently 
#'          using a GDAL spatial filter.
#'          
#'          **Optimization (batch write)**: Previously, each tile was written
#'          immediately using slow incremental GPKG appends. Now, tiles are 
#'          accumulated in memory by temporal chunk and written once per chunk
#'          using fast GDAL bulk writes (~3-4x speedup). Geometry is included 
#'          in each chunk's output GPKG file (not stored separately).
#'          
#'          Intermediate objects are freed explicitly with `rm()` + `gc()` to 
#'          keep the memory footprint low.
#' @param cfg Configuration list
#' @param region_name Character. Human-readable region name for log messages.
#' @param output_config List with output paths for temporal chunks, all with 
#'                      geometries:
#'                      \itemize{
#'                        \item{den: Path to DEN chunk GPKG}
#'                        \item{hourly: Path to hourly chunk GPKG}
#'                        \item{hourly_wd: Path to hourly_wd chunk GPKG}
#'                        \item{hourly_we: Path to hourly_we chunk GPKG}
#'                      }
#'                      Note: `output_config$geom` is no longer used (geometry 
#'                      is integrated into each chunk file).
#' @param tile_size_m Tile side in meters (default 200 km)
#' @param chunks Character vector of temporal chunks to export. 
#'               Valid values: "DEN", "hourly", "hourly_wd", "hourly_we". 
#'               Default: c("DEN") for memory efficiency.   
#'               For full export use: c("DEN", "hourly", "hourly_wd", 
#'               "hourly_we").
#' @examples
#' \dontrun{
#' .predict_france_tiled_impl(
#'   cfg = CFG,
#'   region_name = "France",
#'   output_config = list(
#'     den = "data/prediction/france/traffic_DEN.gpkg",
#'     hourly = "data/prediction/france/traffic_hourly.gpkg",
#'     hourly_wd = "data/prediction/france/traffic_hourly_wd.gpkg",
#'     hourly_we = "data/prediction/france/traffic_hourly_we.gpkg"
#'   ),
#'   tile_size_m = 200000,
#'   chunks = c("DEN", "hourly", "hourly_wd", "hourly_we")
#' )
#' }
#' @return Invisible NULL (side effects: writes GPKG file(s) to disk)
#' @export
#' @keywords internal
.predict_france_tiled_impl <- function(cfg, 
                                       region_name = "France",
                                       output_config,
                                       mode = NULL,
                                       tile_size_m = 200000,
                                       chunks = c("DEN")) {
  
  # Ensure mode is a valid character string (not NULL or empty)
  if (is.null(x = mode) || length(x = mode) == 0L || !is.character(x = mode)) {
    mode <- "france"
  }
  if (!nzchar(x = mode)) {
    mode <- "france"
  }
  
  # Configuration parameters from cfg
  osm_roads_path   <- cfg$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH
  xgb_models_path  <- cfg$XGB_MODELS_WITH_RATIOS_FILEPATH
  xgb_feature_path <- cfg$XGB_RATIO_FEATURE_INFO_FILEPATH
  
  # Output paths from output_config
  output_dir <- output_config$output_dir  # Base directory for tile subdirs
  chunk_paths_all <- list(
    DEN       = output_config$den,
    hourly    = output_config$hourly,
    hourly_wd = output_config$hourly_wd,
    hourly_we = output_config$hourly_we
  )
  
  pipeline_message(sprintf("%s tiled prediction", region_name), level = 0, 
                   progress = "start", process = "calc")

  # --- Load models (once for all tiles) ---
  pipeline_message("Loading trained XGBoost models", level = 1, 
                   progress = "start", process = "load")

  if (!file.exists(xgb_models_path)) {
    pipeline_message(sprintf("Models not found: %s", xgb_models_path), 
                     process = "stop")
  }
  models_list  <- readRDS(file = xgb_models_path)
  feature_info <- readRDS(file = xgb_feature_path)
  all_periods  <- feature_info$all_periods

  pipeline_message(
    sprintf("Models loaded: %d models for %d periods",
            length(x = models_list), length(x = all_periods)),
    level = 1, progress = "end", process = "valid")

  # --- Define temporal chunks ---
  temporal_chunks <- get_temporal_chunks()
  temporal_chunks <- temporal_chunks[intersect(x = chunks, 
                                               y = names(temporal_chunks))]

  if (length(x = temporal_chunks) == 0) {
    pipeline_message(paste("No valid temporal chunks requested.", 
                           "Valid: DEN, hourly, hourly_wd, hourly_we"), 
                     process = "stop")
  }

  pipeline_message(sprintf("Temporal chunks to export: %s",
                           paste(names(x = temporal_chunks), 
                                 collapse = ", ")),
    process = "info")

  # Verify all periods are covered
  covered         <- unlist(x = temporal_chunks, use.names = FALSE)
  missing_periods <- setdiff(x = all_periods, y = covered)
  if (length(x = missing_periods) > 0) {
    pipeline_message(
      sprintf("Warning: %d periods not in any temporal chunk: %s",
              length(x = missing_periods),
              paste(head(x = missing_periods, n = 10), 
                    collapse = ", ")),
      process = "warning")
  }

  chunk_paths <- chunk_paths_all[names(x = temporal_chunks)]

  # --- Create output directories if needed ---
  for (fp in unlist(x = chunk_paths)) {
    output_dir <- dirname(path = fp)
    if (!dir.exists(paths = output_dir)) {
      dir.create(path = output_dir, recursive = TRUE)
    }
  }

  # --- Build spatial tiles ---
  tiles <- build_france_tiles(tile_size_m = tile_size_m)
  pipeline_message(sprintf("Tile grid: %d tiles of %d km each",
                          nrow(x = tiles), tile_size_m / 1000),
                   process = "info")

  # Calculate number of digits for tile numbering
  n_tiles <- nrow(x = tiles)
  n_digits <- floor(log10(n_tiles)) + 1L

  # --- Process tiles ---
  total_roads           <- 0L
  total_tiles_with_data <- 0L
  tile_times            <- numeric(length = 0)

  force_reprocess <- isTRUE(cfg$FORCE_REPROCESS_ALL_TILES)

  for (i in seq_len(to = n_tiles)) {
    tile <- tiles[i, ]
    tile_id_str <- sprintf("%0*d", n_digits, i)
    tile_dir <- file.path(output_dir, sprintf("tile_%s", tile_id_str))

    if (!force_reprocess) {
      expected_files <- file.path(
        tile_dir,
        sprintf("07_predictions_%s_traffic_%s_tile_%s.gpkg", 
                mode, names(x = temporal_chunks), tile_id_str)
      )
      if (all(file.exists(expected_files))) {
        if (tile_chunk_files_valid(expected_files, cfg$TARGET_CRS)) {
          pipeline_message(
            sprintf("Skipping tile %s: all %d chunk files already exist", 
                    tile_id_str, length(x = expected_files)),
            level = 2, process = "info")
          next
        }
        pipeline_message(
          sprintf("Reprocessing tile %s: existing chunk files present but not valid", 
                  tile_id_str),
          level = 2, process = "info")
      }
    }

    dir.create(path = tile_dir, recursive = TRUE, showWarnings = FALSE)
    
    t0   <- proc.time()["elapsed"]

    # Load tile from GPKG with spatial filter
    wkt_bbox <- sprintf(
      "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
      tile$xmin, tile$ymin,
      tile$xmax, tile$ymin,
      tile$xmax, tile$ymax,
      tile$xmin, tile$ymax,
      tile$xmin, tile$ymin)

    tile_sf <- tryCatch(
                  expr = sf::st_read(dsn        = osm_roads_path,
                                     wkt_filter = wkt_bbox,
                                     quiet      = TRUE),
                  error = function(e) NULL)

    if (is.null(x = tile_sf) || nrow(x = tile_sf) == 0) next

    # Ensure correct CRS
    tile_sf <- ensure_target_crs(sf_obj = tile_sf, 
                                  target_crs = cfg$TARGET_CRS)

    n_tile                <- nrow(x = tile_sf)
    total_roads           <- total_roads + n_tile
    total_tiles_with_data <- total_tiles_with_data + 1L

    # Keep geometry for merging with traffic data (only once per tile)
    geom_for_merge <- tile_sf[, c("osm_id", "geom")]
    tile_dt        <- as.data.frame(x = sf::st_drop_geometry(x = tile_sf))
    rm(tile_sf)

    predictions_wide <- apply_xgboost_predictions(
      network_data          = tile_dt,
      models_list           = models_list,
      feature_info          = feature_info,
      default_vehicle_speed = cfg$DEFAULT_VEHICLE_SPEED)

    rm(tile_dt)

    # OPTIMIZATION: Convert to long format ONCE per tile, not per chunk
    check_memory_available(
      operation_name = sprintf("Pivot tile %s (%s roads)", 
                               tile$tile_id, fmt(n_tile)),
      min_gb         = 1, 
      warn_gb        = 2)

    predictions_long <- predictions_wide %>%
      tidyr::pivot_longer(
        cols          = matches("^(flow|truck_pct|speed)_"),
        names_to      = c(".value", "period"),
        names_pattern = "^(flow|truck_pct|speed)_(.+)$"
      ) %>%
      mutate(
        HGV    = flow * (truck_pct / 100),
        LV     = flow - HGV,
        TV     = flow,
        period = factor(x = period, levels = all_periods)
      ) %>%
      select(osm_id, highway, period, TV, HGV, LV, speed,
             osm_speed, osm_speed_imputed, truck_pct)

    # Add period datetime columns once per tile
    predictions_long <- add_period_datetime_columns(predictions_long, cfg)

    # validate predictions for this tile
    validation <- validate_predictions(predictions_long)
    if (!validation$is_valid) {
      pipeline_message(sprintf("Validation warnings in tile %d: %s issues", 
                               tile$tile_id, length(x = validation$issues)),
                       process = "warning")
    }

    # Write each tile per chunk immediately
    for (chunk_name in names(x = temporal_chunks)) {
      chunk_periods <- temporal_chunks[[chunk_name]]
      chunk_long    <- predictions_long %>%
        dplyr::filter(period %in% chunk_periods) %>%
        mutate(period = as.character(x = period))

      # Debug logging for tile 1
      if (i == 1L) {
        pipeline_message(
          sprintf("DEBUG Tile %d chunk '%s': %d rows after filter (periods: %s)",
                  i, chunk_name, nrow(x = chunk_long),
                  paste(chunk_periods, collapse = ", ")),
          level = 2, process = "info")
      }

      if (nrow(x = chunk_long) > 0) {
        # Join attributes with geometry
        tile_chunk_sf <- dplyr::left_join(
          x  = chunk_long,
          y  = geom_for_merge,
          by = "osm_id"
        )
        
        # Debug: check for NA geometries after join
        if (i == 1L) {
          n_na_geom <- sum(is.na(sf::st_geometry(tile_chunk_sf)))
          pipeline_message(
            sprintf("DEBUG Tile %d chunk '%s': %d NA geometries after join",
                    i, chunk_name, n_na_geom),
            level = 2, process = "info")
        }
        
        tile_chunk_sf <- sf::st_as_sf(
          x              = tile_chunk_sf,
          sf_column_name = attr(geom_for_merge, "sf_column")
        )

        # Ensure CRS is explicit and consistent
        tile_chunk_sf <- ensure_target_crs(sf_obj     = tile_chunk_sf, 
                                           target_crs = cfg$TARGET_CRS)

        # Write tile file
        tile_file <- file.path(tile_dir, 
                               sprintf("07_predictions_%s_traffic_%s_tile_%s.gpkg", 
                                       mode, chunk_name, tile_id_str))
        
        # Debug logging before write
        if (i == 1L) {
          pipeline_message(
            sprintf("DEBUG Tile %d: writing to %s", i, basename(path = tile_file)),
            level = 2, process = "info")
        }
        
        sf::st_write(
          obj        = tile_chunk_sf,
          dsn        = tile_file,
          delete_dsn = TRUE,
          quiet      = TRUE
        )
        
        # Debug logging after write
        if (i == 1L) {
          file_exists_after <- file.exists(tile_file)
          pipeline_message(
            sprintf("DEBUG Tile %d: file exists after write = %s",
                    i, file_exists_after),
            level = 2, process = "info")
        }
      } else if (i == 1L) {
        # Debug: no rows to write
        pipeline_message(
          sprintf("DEBUG Tile %d chunk '%s': SKIPPED (0 rows)",
                  i, chunk_name),
          level = 2, process = "info")
      }
    }

    rm(predictions_wide, predictions_long, geom_for_merge)
    gc(verbose = FALSE)

    dt         <- proc.time()["elapsed"] - t0
    tile_times <- c(tile_times, dt)
    avg_time   <- mean(x = tile_times)
    remaining  <- (n_tiles - i) * avg_time

    pipeline_message(
      sprintf("Tile %d/%d: %s roads (%.1f s) | Total: %s roads | ETA: %s", 
              i, n_tiles, fmt(n_tile), dt, 
              fmt(total_roads), 
              format_duration(remaining)), 
      level = 2, process = "calc")
  }

  rm(models_list, feature_info)
  gc(verbose = FALSE)

  # Merge tiles per chunk
  pipeline_message("Merging tiles into final chunk files", 
                   level = 1, progress = "start", process = "save")

  for (chunk_name in names(x = temporal_chunks)) {
    chunk_file <- chunk_paths[[chunk_name]]
    
    # Collect all tile files for this chunk
    tile_files <- list()
    for (i in seq_len(to = n_tiles)) {
      tile_id_str <- sprintf("%0*d", n_digits, i)
      tile_dir    <- file.path(output_dir, sprintf("tile_%s", tile_id_str))
      tile_file   <- file.path(tile_dir, 
                          sprintf("07_predictions_%s_traffic_%s_tile_%s.gpkg", 
                                  mode, chunk_name, tile_id_str))
      if (file.exists(tile_file)) {
        tile_files <- c(tile_files, tile_file)
      }
    }
    
    if (length(tile_files) == 0) {
      pipeline_message(sprintf("No tile files found for chunk '%s'", 
                               chunk_name),
                       process = "warning")
      next
    }

    # Read and combine all tile sf objects
    check_memory_available(
      operation_name = sprintf("Merge %d tiles for chunk '%s'", 
                               length(tile_files), chunk_name),
      min_gb         = 4, 
      warn_gb        = 8)

    tile_sf_list <- lapply(tile_files, function(tf) {
      sf_obj <- sf::st_read(dsn   = tf, 
                            quiet = TRUE)
      ensure_target_crs(sf_obj     = sf_obj, 
                        target_crs = cfg$TARGET_CRS)
    })

    # Combine all tiles
    combined_sf <- do.call(
      what = rbind,
      args = c(tile_sf_list, 
               list(make.row.names = FALSE))
    )

    # Write final chunk file
    pipeline_message(
      sprintf("Writing merged chunk '%s': %d rows with geometry", 
              chunk_name, nrow(x = combined_sf)),
      level = 2, progress = "start", process = "save")

    sf::st_write(
      obj        = combined_sf,
      dsn        = chunk_file,
      delete_dsn = TRUE,
      quiet      = FALSE
    )

    pipeline_message(
      sprintf("Chunk '%s' written to %s", 
              chunk_name, rel_path(chunk_file)),
      level = 2, progress = "end", process = "save")

    rm(tile_sf_list, combined_sf)
    gc(verbose = FALSE)
  }

  pipeline_message("Tile merging phase completed", 
                   level = 1, progress = "end", process = "save")

  rm(models_list, feature_info)
  gc(verbose = FALSE)

  # --- Summary ---
  pipeline_message("France-wide prediction summary:", 
                   process = "info")
  pipeline_message(sprintf("\t- Roads predicted: %s across %d tiles", 
                           fmt(total_roads), total_tiles_with_data), 
                   process = "info")
  pipeline_message(sprintf("\t- Note: Geometry included in each temporal chunk file"), 
                   process = "info")
  for (cn in names(x = chunk_paths)) {
    if (file.exists(chunk_paths[[cn]])) {
      sz <- round(x = file.info(chunk_paths[[cn]])$size / 1024^2, 1)
      pipeline_message(sprintf("\t- Traffic [%s]: %s (%.1f MB)", 
                               cn, rel_path(chunk_paths[[cn]]), sz), 
                   process = "info")
    }
  }

  pipeline_message(sprintf("%s prediction completed", region_name), level = 0, 
                   progress = "end", process = "valid")

  invisible(NULL)
}