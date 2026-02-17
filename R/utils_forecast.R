# ==============================================================================
# UTILITIES FOR FORECASTING PROCESSES
# ==============================================================================
# 
# ------------------------------------------------------------------------------
# Load trained XGBoost models and feature metadata
# ------------------------------------------------------------------------------
#' @title Load trained XGBoost traffic forecasting models
#' @description Loads the trained XGBoost models used for traffic forecasting, 
#'              including the base daily traffic model and the associated ratio 
#'              models (hourly, vehicle categories, etc.), as well as the 
#'              feature metadata required to align prediction matrices with 
#'              training.
#'              By default, file paths are read from the global configuration 
#'              object \code{CONFIG}. Custom paths can be provided explicitly if 
#'              needed.
#'              The function stops the pipeline if model files are missing.
#' @param models_path Character string giving the path to the RDS file 
#'                    containing the trained XGBoost models. Defaults to 
#'                    \code{CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH}.
#' @param features_path Character string giving the path to the RDS file 
#'                      containing the feature metadata used during training. 
#'                      Defaults to 
#'                      \code{CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH}.
#' @return A list with the following elements:
#'         \describe{
#'            \item{models_list}{Named list of trained XGBoost models.}
#'            \item{feature_info}{List containing feature names, periods, and 
#'                                metadata used during training.}
#'         }
#' @seealso \code{\link{xgboost}}
#' @export
load_forecast_models <- function(
    models_path = CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH, 
    features_path = CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH) {
  if (!file.exists(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)) {
    pipeline_message(
      sprintf("Models not found: %s", CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH),
      process = "stop")
  }
  if (!file.exists(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)) {
    pipeline_message(
      sprintf("Features not found: %s", CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH),
      process = "stop")
  }
  list(
    models_list = readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH),
    feature_info = readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)
  )
}
# ------------------------------------------------------------------------------
# Spatial filtering dispatch table for forecasting modes
# ------------------------------------------------------------------------------
FORECAST_FILTERS <- list(
  nantes = function(roads_sf) {
    boundary <- sf::st_read(dsn   = CONFIG$NANTES_BOUNDARY_GPKG, 
                            layer = CONFIG$NANTES_BOUNDARY_LAYER, 
                            quiet = TRUE)
    sf::st_intersection(x = roads_sf, y = boundary)
  },
  paris = function(roads_sf) {
    boundary <- sf::st_read(dsn   = CONFIG$PARIS_BOUNDARY_GPKG, 
                            layer = CONFIG$PARIS_BOUNDARY_LAYER, 
                            quiet = TRUE)
    sf::st_intersection(x = roads_sf, y = boundary)
  },
  sensors = function(roads_sf) {
    sensors_sf <- sf::st_read(dsn   = CONFIG$SENSORS_BUFFER_GPKG, 
                              layer = CONFIG$SENSORS_BUFFER_LAYER, 
                              quiet = TRUE)
    sf::st_join(x = roads_sf, 
                y = sensors_sf, 
                join = sf::st_intersects, 
                left = FALSE)
  }
)
# ------------------------------------------------------------------------------
# Load target OSM road network for forecasting
# ------------------------------------------------------------------------------
#' @title Load target OSM road network for traffic forecasting
#' @description Loads the OpenStreetMap road network corresponding to a given 
#'              forecasting mode (e.g. city-level area, sensor buffer, 
#'              regional subset). The function returns a spatial object 
#'              containing road geometries and attributes required for traffic 
#'              prediction.
#'              The function is intentionally limited to **data access and 
#'              spatial filtering** only.#'
#' @param mode Character string defining the forecasting context.
#'             Allowed values are:
#'             \itemize{
#'               \item \code{"nantes"}: Nantes metropolitan area, 
#'               \item \code{"paris"}: Paris region, 
#'               \item \code{"sensors"}: roads intersecting acoustic sensor 
#'                                       buffers.
#'             }
#' @return An \code{sf} object containing OSM road geometries and attributes,
#'         ready for feature preparation and prediction.
#' @details The function relies on configuration values defined in 
#'          \code{config_forecast.R}, including file paths and spatial layers 
#'          used for filtering (e.g. administrative boundaries or sensor 
#'          buffers).
#'          The returned object must contain at least the following fields:
#'          \itemize{
#'            \item \code{osm_id}, 
#'            \item \code{highway}, 
#'            \item \code{name}, 
#'            \item network indicators (connectivity, betweenness, etc.).
#'          }
#' @examples
#' \dontrun{
#' roads_nantes  <- load_target_roads("nantes")
#' roads_paris   <- load_target_roads("paris")
#' roads_sensors <- load_target_roads("sensors")
#' }
#' @export
load_target_roads <- function(mode) {
  stopifnot(is.character(mode), length(mode) == 1)
  mode <- tolower(mode)
  # Load full OSM road network
  if (!file.exists(CONFIG$OSM_ROADS_GPKG)) {
    pipeline_message(sprintf("OSM roads file not found: %s", 
                             CONFIG$OSM_ROADS_GPKG), 
                     process = "stop")
  }
  roads_sf <- sf::st_read(dsn = CONFIG$OSM_ROADS_GPKG, 
                          layer = CONFIG$OSM_ROADS_LAYER, 
                          quiet = TRUE)
  # Mode-specific spatial filtering
  roads_sf <- FORECAST_FILTERS[[mode]](roads_sf)
  
  # Final checks
  if (nrow(roads_sf) == 0) {
    pipeline_message(sprintf("No roads selected for mode '%s'", mode), 
                     process = "stop")
  }
  pipeline_message(sprintf("Loaded %d road segments for mode '%s'", 
                           nrow(roads_sf), mode), 
                   process = "info")
  return(roads_sf)
}
# ------------------------------------------------------------------------------
# Prepare OSM road features for traffic estimation
# ------------------------------------------------------------------------------
#' @title Prepare OSM road features for traffic prediction
#' @description Applies the exact same preprocessing steps used during model 
#'              training to OpenStreetMap road features prior to traffic 
#'              prediction.
#'              This includes:
#'              \itemize{
#'                \item transformation of categorical variables (e.g. highway), 
#'                \item derivation of textual features (e.g. ref letter, first 
#'                      word), 
#'                \item imputation of missing values for structural variables 
#'                      (lanes, speed), 
#'                \item normalization of network-based indicators, 
#'                \item replacement of missing network metrics by zeros.
#'              }
#'              The function is designed to ensure strict feature compatibility 
#'              between training and prediction datasets.
#' @param roads_dt A \code{data.table} containing OSM road attributes without 
#'                 geometry.
#' @param imputation_rules A \code{data.table} defining imputation rules derived 
#'                         from training data.
#' @return A \code{data.table} with engineered and fully prepared features, 
#'         ready for model matrix construction.
#' @details The input table is modified by reference for performance reasons.
#'
#' @export
prepare_osm_features <- function(roads_dt, imputation_rules) {
  prepare_osm_features <- function(roads_dt, imputation_rules) {
    stopifnot(inherits(roads_dt, "data.table"))
    # 1. Highway (ordered factor – must match training)
    highway_levels <- c("residential", "tertiary", "secondary", "primary", 
                        "trunk", "motorway", "tertiary_link", "secondary_link", 
                        "primary_link", "trunk_link", "motorway_link", 
                        "unclassified")
    roads_dt[, highway := as.character(x = highway)]
    roads_dt[is.na(highway) | highway == "", highway := "unclassified"]
    roads_dt[, highway := factor(x = highway, 
                                 levels = c(highway_levels, "missing"), 
                                 ordered = TRUE)]
    # 2. Degre (DEGRE)
    roads_dt[, DEGRE := as.numeric(x = as.character(x = DEGRE))]
    roads_dt[is.na(DEGRE), DEGRE := 1]
    # 3. Reference letter (ref_letter)
    ref_col <- intersect(x = c("ref", "ref_osm"), 
                         y = names(roads_dt))[1]
    if (!is.na(ref_col)) {
      roads_dt[, ref_letter := ifelse(
        test = !is.na(get(ref_col)) & get(ref_col) != "", 
        yes = substr(x = gsub(pattern = "^([A-Za-z]).*", 
                              replacement = "\\1", 
                              x = get(ref_col)), 
                     start = 1, stop = 1), 
        no = "missing")]
      roads_dt[!grepl(pattern = "^[A-Za-z]$", x= ref_letter), 
               ref_letter := "missing"]
    } else {
      roads_dt[, ref_letter := "missing"]
    }
    roads_dt[, ref_letter := factor(x = ref_letter)]
    # 4. First word  (first_word)
    if ("name" %in% names(roads_dt)) {
      roads_dt[, first_word := ifelse(
        test = !is.na(name) & name != "", 
        yes = tolower(x = trimws(x = gsub(pattern = "\\s+.*", 
                                          replacement = "", 
                                          x = name))), 
        no = "missing")]
      rare_words <- names(which(table(roads_dt$first_word) < 5))
      roads_dt[first_word %in% rare_words, first_word := "missing"]
    } else {
      roads_dt[, first_word := "missing"]
    }
    roads_dt[, first_word := factor(x = first_word)]
    # 5. Oneway roads (oneway_osm)
    oneway_col <- intersect(x = c("oneway", "oneway_osm"), 
                            y = names(roads_dt))[1]
    
    if (!is.na(oneway_col)) {
      val <- as.character(x = roads_dt[[oneway_col]])
      val[is.na(val) | val == ""] <- "missing"
      val[val == "-1"] <- "yes"
      val[val %in% c("true", "1")] <- "yes"
      val[val %in% c("false", "0")] <- "no"
      val[!val %in% c("yes", "no", "missing")] <- "yes"
      roads_dt[, oneway_osm := val]
    } else {
      roads_dt[, oneway_osm := "missing"]
    }
    roads_dt[, oneway_osm := factor(x = oneway_osm, 
                                    levels = c("yes", "no", "missing"))]
    # 6. Road lanes (lanes_osm - imputed)
    lanes_col <- intersect(x = c("lanes", "lanes_osm"), 
                           y = names(roads_dt))[1]
    roads_dt[, lanes_osm := if (!is.na(lanes_col)){
                              as.numeric(x = get(lanes_col))
                            } else {NA_real_}]
    roads_dt <- merge(x = roads_dt, 
                      y = imputation_rules[, .(highway, median_lanes)], 
                      by = "highway", all.x = TRUE)
    roads_dt[is.na(lanes_osm), lanes_osm := median_lanes]
    roads_dt[is.na(lanes_osm), lanes_osm := 2]
    roads_dt[, median_lanes := NULL]
    # 7. Vehicle speed (speed - imputed)
    speed_col <- intersect(x = c("maxspeed", "maxspeed_osm", "speed"), 
                           y = names(roads_dt))[1]
    roads_dt[, speed := if (!is.na(speed_col)){
                          as.numeric(x = get(speed_col))
                        } else {NA_real_}]
    roads_dt <- merge( x = roads_dt, 
                       y = imputation_rules[, .(highway, median_speed)], 
                       by = "highway", all.x = TRUE)
    roads_dt[is.na(speed), speed := median_speed]
    roads_dt[is.na(speed), speed := 50]
    roads_dt[, median_speed := NULL]
    # 8. Network features NA → 0
    network_features <- c("connectivity", "betweenness", 
                          "closeness", "pagerank")
    for (f in network_features) {
      if (!f %in% names(roads_dt)) {
        pipeline_message(sprintf("Missing network feature: %s", f), 
                         process = "stop")
      }
      roads_dt[is.na(get(f)), (f) := 0]
    }
    invisible(roads_dt)
  }
}
# ------------------------------------------------------------------------------
# Build forecasting design matrix aligned with training
# ------------------------------------------------------------------------------
#' @title Build forecasting design matrix
#' @description Constructs a sparse design matrix for traffic prediction using 
#'              the same feature formula and encoding strategy as during model 
#'              training.
#'              Polynomial contrasts are applied to the \code{highway} variable 
#'              to ensure consistency with the trained XGBoost models.
#' @param dt A \code{data.table} containing engineered features.
#' @param feature_vars A character vector listing the feature variables to use.
#' @return A sparse \code{Matrix} suitable for XGBoost prediction.
#' @export
build_prediction_matrix <- function(dt, feature_vars) {
  feature_formula <- as.formula(object = paste("~", paste(feature_vars, 
                                                          collapse = " + ")))
  dt[, highway := factor(x = highway, ordered = TRUE)]
  contrasts(dt$highway) <- contr.poly(n = length(levels(x = dt$highway)))
  sparse.model.matrix(object = feature_formula, data = dt)
}
# ------------------------------------------------------------------------------
# Align prediction matrix with training features
# ------------------------------------------------------------------------------
#' @title Align prediction matrix with training features
#' @description Ensures that the prediction design matrix has exactly the same 
#'              feature columns, in the same order, as the matrix used during 
#'              training.
#'              Missing features are added as zero-filled columns, and extra 
#'              features are discarded.
#' @param X A sparse \code{Matrix} used for prediction.
#' @param training_features A character vector of feature names used during 
#'                          training.
#' @return A sparse \code{Matrix} aligned with the training feature space.
#' @export
align_features <- function(X, training_features) {
  X_aligned <- X[, intersect(x = training_features, y = colnames(X)), 
                 drop = FALSE]
  missing <- setdiff(x = training_features, y = colnames(X_aligned))
  if (length(missing) > 0) {
    X_aligned <- cbind(
      X_aligned,
      Matrix::Matrix(data = 0, 
                     nrow = nrow(X), 
                     ncol = length(missing), 
                     dimnames = list(NULL, missing)))
  }
  X_aligned[, training_features, drop = FALSE]
}
# ------------------------------------------------------------------------------
# Run traffic predictions for all periods
# ------------------------------------------------------------------------------
#' @title Run traffic predictions for all periods
#' @description Applies trained XGBoost models to a prepared design matrix in 
#'              order to estimate traffic indicators for all temporal periods 
#'              and vehicle categories.
#'              The function computes:
#'              \itemize{
#'                \item base daily traffic predictions, 
#'                \item temporal ratio-based disaggregation, 
#'                \item vehicle-type specific traffic estimates (TV, HGV, LV), 
#'                \item long-format output suitable for spatial or tabular 
#'                      export.
#'              }
#' @param X A sparse \code{Matrix} aligned with training features.
#' @param base_dt A \code{data.frame} containing at least \code{osm_id}, 
#'                \code{highway}, and \code{name}.
#' @param models_list A named list of trained XGBoost models.
#' @param all_periods Character vector of temporal periods to predict.
#' @return A \code{data.frame} in long format with predicted traffic values.
#' @export
run_predictions <- function(X, base_dt, models_list, all_periods) {
  # Base predictions (D)
  base_preds <- base_dt[, c("osm_id", "highway", "name")]
  base_targets <- c("flow_D", "truck_pct_D", "speed_D")
  for (target in base_targets) {
    model_obj <- models_list[[target]]
    if (is.null(model_obj)){ next }
    training_features <- model_obj$feature_names
    X_aligned <- align_features(X = X, training_features = training_features)
    preds <- predict(object = model_obj$model, X_aligned)
    if (!is.null(model_obj$config$transform) && 
        model_obj$config$transform == "log10") {
      preds <- 10^preds
    }
    base_preds[[gsub(pattern = "_D$", 
                     replacement = "", 
                     x = target)]] <- round(preds, 2)
  }
  base_preds$period <- "D"
  base_preds$TV <- base_preds$flow
  base_preds$HGV <- round(x = base_preds$flow * base_preds$truck_pct / 100)
  base_preds$LV <- base_preds$TV - base_preds$HGV
  results <- list(D = base_preds[, c("osm_id", "highway", "name", "period", 
                                     "TV", "HGV", "LV")])
  # Ratio predictions
  for (p in setdiff(x = all_periods, y = "D")) {
    p_dt <- base_preds[, c("osm_id", "highway", "name")]
    p_dt$period <- p
    for (var in c("flow", "truck_pct", "speed")) {
      key <- paste0("ratio_", var, "_", p)
      if (!key %in% names(models_list)) {
        p_dt[[var]] <- base_preds[[var]]
        next
      }
      model_obj <- models_list[[key]]
      training_features <- model_obj$feature_names
      
      X_aligned <- align_features(X = X, training_features = training_features)
      ratio <- predict(object = model_obj$model, X_aligned)
      p_dt[[var]] <- round(x = base_preds[[var]] * ratio, 2)
    }
    p_dt$TV <- p_dt$flow
    p_dt$HGV <- round(x = p_dt$flow * p_dt$truck_pct / 100)
    p_dt$LV <- p_dt$TV - p_dt$HGV
    results[[p]] <- p_dt[, c("osm_id", "highway", "name", "period", "TV", 
                             "HGV", "LV")]
  }
  do.call(rbind, results)
}
# ------------------------------------------------------------------------------
# Convert period labels to milliseconds
# ------------------------------------------------------------------------------
#' @title Convert traffic period labels to milliseconds
#' @description Converts traffic period identifiers (e.g. \code{"h7"}, 
#'              \code{"D"}, \code{"E"}, \code{"N"}) into millisecond offsets 
#'              suitable for time-based representations.
#' @param period Character vector of period labels.
#' @return Integer vector of time offsets in milliseconds.
#' @export
period_to_ms <- function(period) {
  ifelse(test = grepl(pattern = "^h", x = period),
         as.integer(sub(pattern = "^h", replacement = "", x = period)) * 3600000L,
         match(x = period, table = c("D", "E", "N")) * 3600000L + 24 * 3600000L)
}