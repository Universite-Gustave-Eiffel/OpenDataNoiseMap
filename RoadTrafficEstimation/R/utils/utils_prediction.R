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
# -------------------------------------------------------------------------------
# CRS objects utilities for tile validation and alignment
# -------------------------------------------------------------------------------
#' @title Compare sf CRS objects safely
#' @description Compare two sf CRS objects by EPSG or WKT when available.
#' @param crs_a First CRS object or numeric EPSG code to compare (reference).
#' @param crs_b Second CRS object or numeric EPSG code to compare (target).
#' @return Logical scalar. TRUE if the CRS definitions are equivalent.
sf_crs_matches <- function(x, y) {
  x <- sf::st_crs(x = x)
  y <- sf::st_crs(x = y)
  if (is.na(x = x) || is.na(x = y)) {
    return(FALSE)
  }
  if (!is.null(x = x$epsg) && !is.null(x = y$epsg) &&
      !is.na(x = x$epsg) && !is.na(x = y$epsg)) {
    return(x$epsg == y$epsg)
  }
  if (!is.null(x = x$wkt) && !is.null(x = y$wkt) &&
      nzchar(x = x$wkt) && nzchar(x = y$wkt)) {
    return(x$wkt == y$wkt)
  }
  if (!is.null(x = x$proj4string) && !is.null(x = y$proj4string) &&
      nzchar(x = x$proj4string) && nzchar(x = y$proj4string)) {
    return(x$proj4string == y$proj4string)
  }
  return(identical(x, y))
}
#' 
# -------------------------------------------------------------------------------
# CRS objects conversion
# -------------------------------------------------------------------------------
#' @title Convert sf CRS object to a stable string identifier
#' @description Returns an EPSG-based string when available, or falls back to
#'              WKT/proj4. This is used to compare CRS across tile files.
#' @param crs CRS object or object accepted by sf::st_crs.
#' @return Character scalar.
sf_crs_to_string <- function(crs) {
  crs <- sf::st_crs(x = crs)
  if (is.na(x = crs)) {
    return(NA_character_)
  }
  if (!is.null(x = crs$epsg) && !is.na(x = crs$epsg)) {
    return(sprintf("EPSG:%s", crs$epsg))
  }
  if (!is.null(x = crs$wkt) && nzchar(x = crs$wkt)) {
    return(crs$wkt)
  }
  if (!is.null(x = crs$proj4string) && nzchar(x = crs$proj4string)) {
    return(crs$proj4string)
  }
  return(as.character(x = crs)[1])
}
#' 
# -------------------------------------------------------------------------------
# Check sf objects for expected CRS and report issues
# -------------------------------------------------------------------------------
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
  target_crs  <- sf::st_crs(x = target_crs)
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

#' 
# -------------------------------------------------------------------------------
# Write sf object to GeoPackage using a temporary file first
# -------------------------------------------------------------------------------
#' @title Write sf GeoPackage atomically
#' @description Write an sf object to a temporary GeoPackage and rename it to the
#'              final destination to avoid leaving partially written files.
#' @param sf_obj An sf object to write.
#' @param dsn Destination GeoPackage path.
#' @return TRUE if writing and rename succeeded, otherwise throws.
write_sf_gpkg_atomic <- function(sf_obj, dsn) {
  dest_dir <- dirname(dsn)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  temp_fp <- tempfile(pattern = "tmp_tile_", fileext = ".gpkg", tmpdir = dest_dir)

  sf::st_write(obj        = sf_obj,
               dsn        = temp_fp,
               delete_dsn = TRUE,
               quiet      = TRUE)

  validation_sf <- tryCatch(
    sf::st_read(dsn   = temp_fp,
                quiet = TRUE,
                n_max = 0),
    error = function(e) e)

  if (inherits(validation_sf, "error") || !inherits(validation_sf, "sf")) {
    unlink(temp_fp)
    stop(sprintf("GeoPackage validation failed for temporary file %s: %s",
                 temp_fp,
                 if (inherits(validation_sf, "error")) validation_sf$message else "output is not an sf object"))
  }

  if (!file.rename(from = temp_fp, to = dsn)) {
    if (!file.copy(from = temp_fp, to = dsn, overwrite = TRUE)) {
      unlink(temp_fp)
      stop(sprintf("Cannot move temporary GeoPackage to %s", dsn))
    }
    unlink(temp_fp)
  }
  invisible(TRUE)
}
#' 
# -------------------------------------------------------------------------------
# Tile chunk GeoPackage files validation
# -------------------------------------------------------------------------------
#' @title Validate tile chunk GeoPackage files
#' @description Check that all expected tile chunk files exist and use the target CRS.
#' @param tile_files Character vector of GeoPackage file paths.
#' @param target_crs Target CRS (numeric EPSG code or crs object).
#' @return Logical scalar. TRUE if all files exist and match the target CRS.
tile_chunk_files_valid <- function(tile_files, target_crs) {
  validate_tile_chunk_files(tile_files = tile_files, 
                            target_crs = target_crs)$is_valid
}
#' 
# -------------------------------------------------------------------------------
# Tile chunk GeoPackage files validation with detailed issue reporting
# -------------------------------------------------------------------------------
#' @title Validate tile chunk GeoPackage files with detailed issue reporting
#' @description Check that all expected tile chunk files exist and use the 
#'              target CRS. Returns a list with overall validity and detailed issues
#'              issues for any problems found.
#' @param tile_files Character vector of GeoPackage file paths.
#' @param target_crs Target CRS (numeric EPSG code or crs object).
#' @return List with is_valid (logical scalar) and issues (named list).
#' @export
validate_tile_chunk_files <- function(tile_files, target_crs) {
  issues <- list()
  if (length(x = tile_files) == 0L) {
    issues[["no_files"]] <- "No expected tile chunk files were provided."
    return(list(is_valid = FALSE, 
                issues   = issues))
  }
  # Check for missing files first (avoid reading files if some are missing)
  missing_files <- tile_files[!file.exists(tile_files)]
  if (length(x = missing_files) > 0L) {
    issues[["missing_files"]] <- sprintf(
      "Missing %d file(s): %s",
      length(x = missing_files),
      paste(basename(path = missing_files), collapse = ", "))
    return(list(is_valid = FALSE, issues = issues))
  }
  # Check CRS of each file
  for (tile_fp in tile_files) {
    sf_obj <- tryCatch(
      sf::st_read(dsn   = tile_fp,
                  quiet = TRUE,
                  n_max = 0),
      error = function(e) e)
    # If st_read fails (e.g., file is corrupted), report as an issue
    if (inherits(x = sf_obj, "error")) {
      issues[[basename(tile_fp)]] <- sprintf(
        "cannot read (%s)", 
        sf_obj$message)
      next
    }
    # Otherwise, check CRS
    if (!sf_crs_matches(x = sf::st_crs(x = sf_obj), 
                        y = target_crs)) {
      issues[[basename(tile_fp)]] <- sprintf(
        "CRS mismatch (%s)",
        as.character(x = sf::st_crs(x = sf_obj))
      )
    }
  }
  return(list(is_valid = length(x = issues) == 0L, 
              issues   = issues))
}

#' 
# -------------------------------------------------------------------------------
# Repair tile chunk GeoPackage files with bad CRS metadata
# -------------------------------------------------------------------------------
#' @title Repair tile chunk GeoPackage files with bad CRS metadata
#' @description Reads each file, assigns/transforms the target CRS when needed,
#'              and overwrites the original GeoPackage.
#' @param tile_files Character vector of GeoPackage file paths.
#' @param target_crs Target CRS (numeric EPSG code or crs object).
#' @return List containing repaired and failed file names.
repair_tile_chunk_files <- function(tile_files, target_crs) {
  repaired <- character(0)
  failed   <- list()

  for (tile_fp in tile_files) {
    if (!file.exists(tile_fp)) {
      failed[[basename(tile_fp)]] <- "file does not exist"
      next
    }

    sf_obj <- tryCatch(
      sf::st_read(dsn   = tile_fp,
                  quiet = TRUE),
      error = function(e) e)
    if (inherits(x = sf_obj, "error")) {
      failed[[basename(tile_fp)]] <- sf_obj$message
      next
    }

    if (sf_crs_matches(x = sf::st_crs(x = sf_obj),
                       y = target_crs)) {
      next
    }

    sf_obj <- ensure_target_crs(sf_obj     = sf_obj,
                                target_crs = target_crs)
    if (is.na(x = sf::st_crs(x = sf_obj))) {
      failed[[basename(tile_fp)]] <- "unable to assign target CRS"
      next
    }

    write_err <- tryCatch({
      write_sf_gpkg_atomic(sf_obj = sf_obj,
                           dsn    = tile_fp)
      NULL
    }, error = function(e) e)

    if (inherits(x = write_err, "error")) {
      failed[[basename(tile_fp)]] <- write_err$message
      next
    }
    repaired <- c(repaired, basename(tile_fp))
  }

  return(list(repaired = repaired, failed = failed))
}
#' 
#' ------------------------------------------------------------------------------
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
    
    # Detect layer name for explicit read (prevents auto-detection failure in GDAL 2.4)
    layers_info <- sf::st_layers(osm_roads_path)
    layer_name  <- layers_info$name[1]

    osm_network <- sf::st_read(
      dsn        = osm_roads_path,
      layer      = layer_name,
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
  pts_bbox <- sf::st_bbox(obj = points)
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
  
  # Detect layer name
  layers_info <- sf::st_layers(osm_roads_path)
  layer_name  <- layers_info$name[1]

  # Read only the bbox region from GPKG (much faster + less memory)
  osm_network <- sf::st_read(dsn        = osm_roads_path, 
                             layer      = layer_name, 
                             wkt_filter = wkt_bbox, 
                             quiet      = TRUE)
  
  # Ensure correct CRS
  osm_network <- ensure_target_crs(sf_obj     = osm_network, 
                                   target_crs = target_crs)
  
  # Create buffers and filter precisely
  buffers         <- sf::st_buffer(x    = points, 
                                   dist = buffer_radius)
  combined_buffer <- sf::st_union(x = buffers)
  osm_network     <- sf::st_filter(x = osm_network, 
                                   y = combined_buffer)
  
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
      return(Matrix::sparse.model.matrix(object = formula_obj, 
                                         data   = data_df))
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
    return(Matrix::sparse.model.matrix(object = formula_obj, 
                                       data = mm_data))
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
                   level = 1, process = "wait")
  
  # Memory check: predictions will create ~n_roads x n_periods x 3 columns
  n_roads   <- nrow(x = network_data)
  n_periods <- length(x = feature_info$all_periods)
  est_mb    <- round(x = n_roads * n_periods * 3 * 8 / 1024^2)  # 8 bytes / double
  check_memory_available(
    operation_name = sprintf("XGBoost prediction (%s roads x %d periods, ~%d MB result)",
                             fmt(n_roads), n_periods, est_mb),
    min_gb         = 1, 
    warn_gb        = 3)

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
  rownames(x = network_data) <- seq_len(nrow(x = network_data))
  feature_matrix_part <- safe_sparse_model_matrix(
    formula_obj = feature_info$road_feature_formula,
    data_df     = network_data)
  rows_used <- as.integer(x = rownames(x = feature_matrix_part))
  if (length(x = rows_used) == 0 || anyNA(rows_used)) {
    rows_used <- seq_len(nrow(x = feature_matrix_part))
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
    rows_used <- rows_used[seq_len(n_common)]
    feature_matrix[rows_used, ] <- as.matrix(
          x = feature_matrix_part[seq_len(n_common), , drop = FALSE])
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
                      yes  = rep(x = NA_real_, length(x = speed_D_raw)), 
                      no   = pmax(5, speed_D_raw * speed_osm_base))
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
                     yes  = "DEFAULT_VEHICLE_SPEED", 
                     no   = "speed_D XGBoost predictions")),
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
    flow_cols <- grep(pattern = "^flow_", 
                      x       = names(x = predictions), 
                      value   = TRUE)
    for (col in flow_cols) {
      n_negative <- sum(predictions[[col]] < 0, na.rm = TRUE)
      if (n_negative > 0) {
        issues[[paste0(col, "_negative")]] <- n_negative
      }
    }

    truck_cols <- grep(pattern = "^truck_pct_", 
                       x       = names(x = predictions), 
                       value   = TRUE)
    for (col in truck_cols) {
      n_exceed <- sum(predictions[[col]] > 100 | predictions[[col]] < 0, 
                      na.rm = TRUE)
      if (n_exceed > 0) {
        issues[[paste0(col, "_out_of_range")]] <- n_exceed
      }
    }

    speed_cols <- grep(pattern = "^speed_", x = names(x = predictions), value = TRUE)
    for (col in speed_cols) {
      n_unrealistic <- sum(predictions[[col]] < 5 | predictions[[col]] > 200,
                           na.rm = TRUE)
      if (n_unrealistic > 0) {
        issues[[paste0(col, "_unrealistic")]] <- n_unrealistic
      }
    }

    # Temporal coherence for wide format
    if (all(c("flow_D", "flow_N") %in% names(x = predictions))) {
      n_flow_n_gt_d <- sum(predictions$flow_N > (predictions$flow_D * 1.05), 
                           na.rm = TRUE)
      if (n_flow_n_gt_d > 0) {
        issues[["coherence_flow_N_gt_D"]] <- n_flow_n_gt_d
      }
    }
    if (all(c("speed_D", "speed_N") %in% names(x = predictions))) {
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
  if (!"period" %in% names(x = predictions_long)) {
    return(predictions_long)
  }

  period_chr <- as.character(x = predictions_long$period)
  n          <- length(x = period_chr)

  # Store as POSIXct (native datetime type) so sf/GDAL writes to GeoPackage correctly
  # QGis recognizes POSIXct-backed datetime columns for temporal animation
  datetimestart <- as.POSIXct(x = rep(NA_real_, n), tz = "UTC")
  datetimeend   <- as.POSIXct(x = rep(NA_real_, n), tz = "UTC")

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
    datetimestart[idx_D] <- as.POSIXct(x  = sprintf("%d-01-01 06:00:00", base_year), 
                                       tz = "UTC")
    datetimeend[idx_D]   <- as.POSIXct(x  = sprintf("%d-01-01 18:00:00", base_year), 
                                       tz = "UTC")
  }

  idx_E <- which(x = period_chr == "E")
  if (length(x = idx_E) > 0) {
    datetimestart[idx_E] <- as.POSIXct(x  = sprintf("%d-01-01 18:00:00", base_year), 
                                       tz = "UTC")
    datetimeend[idx_E]   <- as.POSIXct(x  = sprintf("%d-01-01 22:00:00", base_year), 
                                       tz = "UTC")
  }

  idx_N <- which(x = period_chr == "N")
  if (length(x = idx_N) > 0) {
    datetimestart[idx_N] <- as.POSIXct(x  = sprintf("%d-01-01 22:00:00", base_year), 
                                       tz = "UTC")
    datetimeend[idx_N]   <- as.POSIXct(x  = sprintf("%d-01-02 06:00:00", base_year), 
                                       tz = "UTC")
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
    h_vals    <- as.integer(x = vapply(X   = g_we[idx_we], 
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
    network_data          = osm_region_dt,
    models_list           = models_list,
    feature_info          = feature_info,
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

  flow_cols  <- grep(pattern = "^flow_", 
                     x       = names(x = dt), 
                     value   = TRUE)
  truck_cols <- grep(pattern = "^truck_pct_", 
                     x       = names(x = dt), 
                     value   = TRUE)
  speed_cols <- grep(pattern = "^speed_", 
                     x       = names(x = dt), 
                     value   = TRUE)

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
    for (issue_name in names(x = validation$issues)) {
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
  tiles$tile_id <- seq_len(nrow(x = tiles))
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
  if (is.null(x = mode) || length(x = mode) == 0L || 
      !is.character(x = mode)) {
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

  # --- Validate source OSM network ---
  pipeline_message("Validating source OSM network file", level = 1, 
                   progress = "start", process = "search")
  
  if (!file.exists(osm_roads_path)) {
    pipeline_message(sprintf("Source OSM network file not found: %s", 
                             osm_roads_path), 
                     process = "stop")
  }
  
  # Metadata check using st_layers (much faster and avoids geometry detection bugs with n_max)
  layers_info <- try(expr = sf::st_layers(dsn = osm_roads_path), silent = TRUE)
  
  if (inherits(layers_info, "try-error")) {
    pipeline_message(sprintf("Source OSM network file is unreadable by GDAL: %s", 
                             attr(layers_info, "condition")$message), 
                     process = "stop")
  }
  if (length(layers_info$name) == 0) {
    pipeline_message("Source OSM network file contains no layers", process = "stop")
  }

  layer_name <- layers_info$name[1]
  pipeline_message(sprintf("GPKG Layer detected: '%s' (%s features)", 
                           layer_name, fmt(layers_info$features[1])), 
                   process = "info")

  # Safe column name check without loading data (using quoted identifier for SQLite)
  cols <- try(names(sf::st_read(osm_roads_path, 
                               query = sprintf('SELECT * FROM "%s" LIMIT 0', layer_name), 
                               quiet = TRUE)), silent = TRUE)
  if (!inherits(cols, "try-error")) {
    pipeline_message(sprintf("Columns in layer: %s", paste(cols, collapse = ", ")), 
                     process = "info")
  }

  # Validation check: read first few rows using explicit layer name
  osm_validation <- tryCatch(
    sf::st_read(dsn   = osm_roads_path,
                layer = layer_name,
                quiet = TRUE,
                n_max = 10),
    error = function(e) e)
  
  if (inherits(osm_validation, "error")) {
    pipeline_message(sprintf("Source OSM network file validation failed: %s", 
                             osm_validation$message), 
                     process = "stop")
  }
  
  if (!inherits(osm_validation, "sf")) {
    pipeline_message("Source OSM network layer does not contain a valid simple features geometry column", 
                     process = "stop")
  }

  # Repair geometry if needed (safe because checked as sf above)
  osm_validation <- sf::st_make_valid(x = osm_validation)
  
  geom_col <- attr(osm_validation, "sf_column")
  if (!geom_col %in% names(osm_validation)) {
    pipeline_message(sprintf("Source OSM network file missing geometry column '%s'", 
                             geom_col), 
                     process = "stop")
  }
  
  # Check CRS
  if (is.na(sf::st_crs(osm_validation))) {
    pipeline_message("Source OSM network file has missing CRS", 
                     process = "warning")
  } else if (!sf_crs_matches(sf::st_crs(osm_validation), cfg$TARGET_CRS)) {
    pipeline_message(sprintf("Source OSM network file CRS mismatch: expected %s, got %s",
                             sf_crs_to_string(cfg$TARGET_CRS),
                             sf_crs_to_string(sf::st_crs(osm_validation))), 
                     process = "warning")
  }
  
  # Check that all rows have geometry
  pipeline_message(sprintf("Source OSM network validated: %s rows with geometry", 
                          fmt(nrow(osm_validation))), 
                  level = 1, progress = "end", process = "valid")

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
  temporal_chunks <- temporal_chunks[
                        intersect(
                          x = chunks, 
                          y = names(x = temporal_chunks))]

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
  n_tiles  <- nrow(x = tiles)
  n_digits <- floor(log10(n_tiles)) + 1L

  # --- Process tiles ---
  force_reprocess <- isTRUE(cfg$FORCE_REPROCESS_ALL_TILES)
  tile_jobs        <- list()
  tile_results     <- list()
  tile_seq_counter <- 0L

  if (force_reprocess && dir.exists(output_dir)) {
    old_tile_dirs <- list.dirs(path = output_dir, recursive = FALSE, full.names = TRUE)
    old_tile_dirs <- old_tile_dirs[grepl(pattern = "^tile_[0-9]+$", x = basename(old_tile_dirs))]
    if (length(x = old_tile_dirs) > 0L) {
      unlink(old_tile_dirs, recursive = TRUE, force = TRUE)
      pipeline_message(sprintf("Removed %d existing tile directories due to force reprocess", 
                               length(x = old_tile_dirs)),
                       level = 2, process = "warning")
    }
  }

  tile_progress_log <- if (exists("PROJECT_ROOT", envir = .GlobalEnv)) {
    file.path(PROJECT_ROOT, "logs", "pipeline_prediction_tiles.log")
  } else {
    file.path("logs", "pipeline_prediction_tiles.log")
  }
  dir.create(dirname(tile_progress_log), recursive = TRUE, showWarnings = FALSE)
  append_tile_progress <- function(msg) {
    cat(sprintf("%s %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg),
        file = tile_progress_log,
        append = TRUE)
  }

  for (i in seq_len(n_tiles)) {
    tile           <- tiles[i, ]
    tile_id_str    <- sprintf("%0*d", n_digits, i)

    tile_jobs[[length(tile_jobs) + 1L]] <- list(
      tile_index       = i,
      tile             = tile,
      grid_tile_id_str = tile_id_str
    )
  }

  if (length(x = tile_jobs) == 0) {
    pipeline_message("All tiles already exist and are valid; no tile processing needed.",
                     level = 1, progress = "end", process = "info")
    total_roads           <- 0L
    total_tiles_with_data <- 0L
  } else {
    process_tile <- function(job) {
      i                 <- job$tile_index
      tile              <- job$tile
      grid_tile_id_str  <- job$grid_tile_id_str

      t0 <- proc.time()["elapsed"]

      tryCatch({
        # Read data for tile using spatial filter (WKT bbox)
        wkt_bbox <- sprintf(
          "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
          tile$xmin, tile$ymin,
          tile$xmax, tile$ymin,
          tile$xmax, tile$ymax,
          tile$xmin, tile$ymax,
          tile$xmin, tile$ymin)

        tile_sf <- sf::st_read(dsn        = osm_roads_path,
                               layer      = layer_name,
                               wkt_filter = wkt_bbox,
                               quiet      = TRUE)

        if (nrow(x = tile_sf) == 0) {
          elapsed <- proc.time()["elapsed"] - t0
          pipeline_message(
            sprintf("Tile %s has no roads; skipping", grid_tile_id_str),
            process = "warning")
          return(list(tile_roads       = 0L,
                      with_data        = FALSE,
                      elapsed          = elapsed,
                      grid_tile_id_str = grid_tile_id_str,
                      tile_dir_id      = NA_character_))
        }

        tile_sf <- ensure_target_crs(sf_obj = tile_sf, 
                                     target_crs = cfg$TARGET_CRS)

        n_tile <- nrow(x = tile_sf)
        pipeline_message(
          sprintf("Tile %s: %s roads", grid_tile_id_str, fmt(n_tile)),
          level = 2, process = "calc")

        tile_seq_counter <<- tile_seq_counter + 1L
        tile_dir_id <- sprintf("%0*d", n_digits, tile_seq_counter)
        tile_dir <- file.path(output_dir, sprintf("tile_%s", tile_dir_id))
        if (dir.exists(path = tile_dir)) {
          unlink(x = tile_dir, recursive = TRUE, force = TRUE)
        }
        dir.create(path = tile_dir, recursive = TRUE, showWarnings = FALSE)

        # Keep geometry and osm_id for later merge
        geom_col_name <- attr(tile_sf, "sf_column")
        geom_for_merge <- tile_sf[, c("osm_id", geom_col_name), drop = FALSE]
        geom_for_merge$osm_id <- as.character(x = geom_for_merge$osm_id)
        if (!inherits(geom_for_merge, "sf") || is.null(sf::st_geometry(geom_for_merge))) {
          stop(sprintf("Tile %s failed to preserve geometry from source OSM road layer", 
                       grid_tile_id_str))
        }
        tile_dt <- as.data.frame(x = sf::st_drop_geometry(x = tile_sf))
        rm(tile_sf)

        predictions_wide <- apply_xgboost_predictions(
          network_data          = tile_dt,
          models_list           = models_list,
          feature_info          = feature_info,
          default_vehicle_speed = cfg$DEFAULT_VEHICLE_SPEED)

        rm(tile_dt)

        check_memory_available(
          operation_name = sprintf("Pivot tile %s (%s roads)",
                                   grid_tile_id_str, fmt(n_tile)),
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

        predictions_long <- add_period_datetime_columns(predictions_long, cfg)

        tile_geom_file <- file.path(tile_dir,
                                   sprintf("07_predictions_%s_geometry_tile_%s.gpkg",
                                           mode, tile_dir_id))
        write_sf_gpkg_atomic(sf_obj = geom_for_merge,
                             dsn    = tile_geom_file)
        pipeline_message(
          sprintf("Tile %s geometry file written: %s",
                  grid_tile_id_str, rel_path(tile_geom_file)),
          level = 2, process = "save")

        tile_csv_file <- file.path(tile_dir,
                                   sprintf("07_predictions_%s_traffic_tile_%s.csv",
                                           mode, tile_dir_id))
        write.csv(
          x = predictions_long %>%
              dplyr::mutate(period = as.character(x = period)),
          file     = tile_csv_file,
          row.names = FALSE)
        pipeline_message(
          sprintf("Tile %s traffic CSV written: %s",
                  grid_tile_id_str, rel_path(tile_csv_file)),
          level = 2, process = "save")

        validation       <- validate_predictions(predictions_long)
        if (!validation$is_valid) {
          pipeline_message(sprintf("Validation warnings in tile %s: %s issues",
                                   grid_tile_id_str, length(x = validation$issues)),
                           process = "warning")
        }

        for (chunk_name in names(x = temporal_chunks)) {
          chunk_periods <- temporal_chunks[[chunk_name]]
          chunk_long    <- predictions_long %>%
            dplyr::filter(period %in% chunk_periods) %>%
            mutate(period = as.character(x = period))

          if (nrow(x = chunk_long) > 0) {
            chunk_long_df <- as.data.frame(chunk_long)
            chunk_long_df$osm_id <- as.character(x = chunk_long_df$osm_id)

            geom_for_merge$osm_id <- as.character(x = geom_for_merge$osm_id)
            geom_col_name <- attr(geom_for_merge, "sf_column")

            tile_chunk_sf <- dplyr::left_join(
              x  = geom_for_merge,
              y  = chunk_long_df,
              by = "osm_id"
            )

            if (!inherits(tile_chunk_sf, "sf")) {
              tile_chunk_sf <- sf::st_as_sf(
                x              = tile_chunk_sf,
                sf_column_name = geom_col_name)
            }

            if (!inherits(tile_chunk_sf, "sf") || is.null(sf::st_geometry(tile_chunk_sf))) {
              stop(sprintf("Tile %s chunk '%s' failed to build a valid sf object", 
                           grid_tile_id_str, chunk_name))
            }

            tile_chunk_sf <- ensure_target_crs(sf_obj     = tile_chunk_sf,
                                               target_crs = cfg$TARGET_CRS)

            tile_file <- file.path(tile_dir,
                                   sprintf("07_predictions_%s_traffic_%s_tile_%s.gpkg",
                                           mode, chunk_name, tile_dir_id))
            write_sf_gpkg_atomic(sf_obj = tile_chunk_sf,
                                 dsn    = tile_file)
            pipeline_message(
              sprintf("Tile %s chunk '%s' file written: %s",
                      grid_tile_id_str, chunk_name, rel_path(tile_file)),
              level = 2, process = "info")
          }
        }

        rm(predictions_wide, predictions_long, geom_for_merge)
        gc(verbose = FALSE)

        elapsed <- proc.time()["elapsed"] - t0
        pipeline_message(
          sprintf("Tile %s completed: %s roads in %.1f s", 
                  grid_tile_id_str, fmt(n_tile), elapsed),
          process = "info")

        list(tile_roads        = n_tile,
             with_data        = TRUE,
             elapsed          = elapsed,
             grid_tile_id_str = grid_tile_id_str,
             tile_dir_id      = tile_dir_id)
      }, error = function(e) {
        elapsed <- proc.time()["elapsed"] - t0
        pipeline_message(
          sprintf("Tile %s failed: %s", grid_tile_id_str, conditionMessage(e)),
          level = 1, process = "error")
        list(tile_roads     = NA_integer_,
           with_data        = FALSE,
           elapsed          = elapsed,
           grid_tile_id_str = grid_tile_id_str,
           tile_dir_id      = NA_character_,
           error            = conditionMessage(e))
      })
    }

    pipeline_message(
      sprintf("Processing %d tiles sequentially", length(x = tile_jobs)),
      level = 1, progress = "start", process = "calc")
    append_tile_progress(sprintf("Tile processing sequential"))
    tile_results <- list()
    for (job in tile_jobs) {
      append_tile_progress(sprintf("Tile %s start", job$grid_tile_id_str))
      res <- process_tile(job)
      tile_results[[length(tile_results) + 1L]] <- res
      append_tile_progress(sprintf("Tile %s end (roads=%s, elapsed=%s s)",
                                 job$grid_tile_id_str,
                                 ifelse(test = is.na(x = res$tile_roads),
                                        yes  = "unknown",
                                        no   = fmt(res$tile_roads)),
                                 ifelse(test = is.na(x = res$elapsed),
                                        yes  = "unknown",
                                        no   = sprintf("%.1f", res$elapsed))))
    }

    total_roads           <- sum(vapply(X   = tile_results,
                                        FUN = function(x) {
                                          if (is.list(x) && "tile_roads" %in% names(x)) {
                                            as.integer(x$tile_roads)
                                          } else {
                                            NA_integer_
                                          }
                                        },
                                        integer(1)), na.rm = TRUE)
    total_tiles_with_data <- sum(vapply(X   = tile_results,
                                        FUN = function(x) {
                                          if (is.list(x) && "with_data" %in% names(x)) {
                                            as.integer(x$with_data)
                                          } else {
                                            0L
                                          }
                                        },
                                        integer(1)), na.rm = TRUE)
    tile_times            <- vapply(X   = tile_results,
                                    FUN = function(x) {
                                      if (is.list(x) && "elapsed" %in% names(x)) {
                                        as.numeric(x$elapsed)
                                      } else {
                                        NA_real_
                                      }
                                    },
                                    numeric(1))

    if (length(x = tile_times) > 0) {
      avg_time <- mean(x = tile_times, na.rm = TRUE)
      pipeline_message(
        sprintf("France tile prediction completed: %d tiles with data, avg %.1f s per tile",
                total_tiles_with_data, avg_time),
        process = "info")
    }
    pipeline_message(
      sprintf("Tile processing loop finished for %d tiles", length(x = tile_jobs)),
      level = 1, progress = "end", process = "info")

    tile_grid_fp <- file.path(output_dir,
                               sprintf("07_predictions_%s_tile_grid.gpkg", mode))

    tile_metadata <- tiles
    tile_metadata$tile_id_str <- sprintf("%0*d", n_digits, tile_metadata$tile_id)
    tile_metadata$tile_roads  <- NA_integer_
    tile_metadata$with_data  <- FALSE
    tile_metadata$tile_status <- "skipped"

    if (length(x = tile_results) > 0) {
      for (res in tile_results) {
        tidx <- which(tile_metadata$tile_id_str == as.character(x = res$grid_tile_id_str))
        if (length(x = tidx) == 1L) {
          tile_metadata$tile_roads[tidx] <- as.integer(x = res$tile_roads)
          tile_metadata$with_data[tidx] <- isTRUE(x = res$with_data)
          tile_metadata$tile_status[tidx] <- if ("error" %in% names(x = res)) {
            "error"
          } else if (isTRUE(x = res$with_data)) {
            "with_data"
          } else {
            "no_data"
          }
        }
      }
    }

    tile_polys <- lapply(seq_len(nrow(x = tile_metadata)), FUN = function(i) {
      sf::st_polygon(x =list(matrix(
        data  = c(tile_metadata$xmin[i], tile_metadata$ymin[i], 
                  tile_metadata$xmax[i], tile_metadata$ymin[i], 
                  tile_metadata$xmax[i], tile_metadata$ymax[i], 
                  tile_metadata$xmin[i], tile_metadata$ymax[i], 
                  tile_metadata$xmin[i], tile_metadata$ymin[i]), 
        ncol  = 2, 
        byrow = TRUE)))})

    tile_grid_sf <- sf::st_sf(
      tile_metadata[, c("tile_id_str", "tile_roads", "with_data", "tile_status")],
      geometry = sf::st_sfc(tile_polys, crs = cfg$TARGET_CRS))

    pipeline_message(sprintf("Writing France tile grid overview: %s", rel_path(tile_grid_fp)),
                     level = 1, progress = "start", process = "save")
    sf::st_write(
      obj        = tile_grid_sf,
      dsn        = tile_grid_fp,
      delete_dsn = TRUE,
      quiet      = TRUE)
    pipeline_message(sprintf("France tile grid overview written: %s", rel_path(tile_grid_fp)),
                     level = 1, progress = "end", process = "save")
  }

  rm(models_list, feature_info)
  gc(verbose = FALSE)

  # Merge tiles per chunk
  pipeline_message("Merging tiles into final chunk files", 
                   level = 1, progress = "start", process = "save")

  for (chunk_name in names(x = temporal_chunks)) {
    chunk_file <- chunk_paths[[chunk_name]]
    
    all_tile_files <- list.files(path       = output_dir,
                                 pattern    = "^07_predictions_.*_tile_.*\\.gpkg$",
                                 recursive  = TRUE,
                                 full.names = TRUE)
    tile_files <- all_tile_files[grepl(pattern = paste0("_", chunk_name, "_tile_"), 
                                       x       = all_tile_files, 
                                       fixed   = TRUE)]
    tile_files <- sort(tile_files)

    if (length(tile_files) == 0) {
      pipeline_message(sprintf("No tile files found for chunk '%s'", 
                               chunk_name),
                       process = "warning")
      next
    }

    pipeline_message(
      sprintf("Merging chunk '%s' with %d tile files", chunk_name, 
              length(X = tile_files)),
      level = 1, process = "join")

    # Read and combine all tile sf objects
    check_memory_available(
      operation_name = sprintf("Merge %d tiles for chunk '%s'", 
                               length(tile_files), chunk_name),
      min_gb         = 4, 
      warn_gb        = 8)

    tile_sf_list <- lapply(X = tile_files, FUN= function(tf) {
      pipeline_message(
        sprintf("Reading tile file %s for chunk '%s'", rel_path(tf), chunk_name),
        level = 2, process = "search")
      sf_obj <- sf::st_read(dsn   = tf, 
                            quiet = TRUE)
      sf_obj <- ensure_target_crs(sf_obj     = sf_obj, 
                                  target_crs = cfg$TARGET_CRS)
      if (is.na(x = sf::st_crs(sf_obj))) {
        pipeline_message(
          sprintf("Tile chunk file %s has missing CRS after read", rel_path(tf)),
          level = 2, process = "fail")
      } else {
        pipeline_message(
          sprintf("Tile file %s loaded with %d rows", rel_path(tf), nrow(sf_obj)),
          level = 2, process = "valid")
      }
      sf_obj
    })

    # Ensure all tiles share the same target CRS before bind
    tile_crs <- vapply(X = tile_sf_list, FUN = function(x) {
      sf_crs_to_string(crs = sf::st_crs(x))
    }, character(1))
    if (length(x = unique(x = tile_crs)) > 1) {
      pipeline_message(
        sprintf("CRS mismatch detected across tile chunk files: %s",
                paste(unique(tile_crs), collapse = " | ")),
        process = "warning")
      tile_sf_list <- lapply(X = tile_sf_list, FUN = function(sf_obj) {
        ensure_target_crs(sf_obj     = sf_obj,
                          target_crs = cfg$TARGET_CRS)
      })
    }

    # Combine all tiles
    combined_sf <- do.call(
      what = rbind,
      args = c(tile_sf_list, 
               list(make.row.names = FALSE))
    )

    if (any(duplicated(x = combined_sf$osm_id))) {
      pipeline_message(
        sprintf("Removing %d duplicated osm_id rows from merged chunk '%s'",
                sum(duplicated(x = combined_sf$osm_id)), chunk_name),
        process = "warning")
      combined_sf <- combined_sf[!duplicated(x = combined_sf$osm_id), ]
    }

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
  if (exists("geometry_output_file", inherits = FALSE) && 
      file.exists(geometry_output_file)) {
    sz <- round(x = file.info(geometry_output_file)$size / 1024^2, 1)
    pipeline_message(sprintf("\t- Geometry: %s (%.1f MB)", 
                             rel_path(geometry_output_file), sz), 
                     process = "info")
  }
}