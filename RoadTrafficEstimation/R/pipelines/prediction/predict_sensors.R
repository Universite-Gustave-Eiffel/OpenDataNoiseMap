# ==============================================================================
# PREDICTION: NOISE SENSORS (800M RADIUS AROUND ALL SENSORS)
# ==============================================================================
# Predicts traffic on roads within an 800-meter radius of noise sensors 
# (Bruitparif, Acoucité, Child project). Uses the "France engineered" layer.
#
# Input:
#   - 02_osm_network_france_engineered.gpkg : France layer with features
#   - 06_xgboost_trained_models.rds : XGBoost models
#   - 06_xgboost_feature_info.rds : XGBoost feature info (formula and periods)
#   - data/POINT_NOISE_*.shp : acoustic sensors
#   - data/CHILD_*.shp : Child project sensors
# Output:
#   - 07_predictions_sensors_all.gpkg : all combined predictions
#   - 07_predictions_sensors_{SOURCE}.gpkg : predictions for each source
# ==============================================================================

# Extract local variables from configuration
SENSORS_DATA_DIR <- CFG$SENSORS_DATA_DIR
PREDICTION_DIR <- CFG$PREDICTION_DIR

pipeline_message("Noise sensors traffic prediction", level = 0, 
                 progress = "start", process = "calc")

# Buffer radius for sensors
SENSOR_BUFFER_RADIUS <- 800  # meters

# ------------------------------------------------------------------------------
# Load trained models
# ------------------------------------------------------------------------------

pipeline_message("Loading trained XGBoost models", level = 1, 
                 progress = "start", process = "load")

if (!file.exists(CFG$XGB_MODELS_WITH_RATIOS_FILEPATH)) {
  pipeline_message(sprintf("Models not found: %s\n\t\tPlease run 
                           R/model_training/train_xgboost_models.R first", 
                           rel_path(CFG$XGB_MODELS_WITH_RATIOS_FILEPATH)), 
                   process = "stop")
}

# Load models and feature info
models_list  <- readRDS(file = CFG$XGB_MODELS_WITH_RATIOS_FILEPATH)
feature_info <- readRDS(file = CFG$XGB_RATIO_FEATURE_INFO_FILEPATH)

pipeline_message(sprintf("Models loaded: %s models for %s periods", 
                         length(x = models_list), length(x = feature_info$all_periods)), 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Load noise sensors from all sources
# ------------------------------------------------------------------------------

pipeline_message("Loading noise sensors", level = 1, 
                 progress = "start", process = "load")

sensors_list <- list()

# Bruitparif sensors
bruitparif_data <- load_sensor_source(
  base_dir   = SENSORS_DATA_DIR,
  dir_name   = "POINT_NOISE_BRUITPARIF_COMPARE",
  file_root  = "POINT_NOISE_BRUITPARIF_COMPARE",
  target_crs = CFG$TARGET_CRS)
if (!is.null(bruitparif_data)) {
  sensors_list[["BRUITPARIF"]] <- bruitparif_data
  pipeline_message(sprintf("Bruitparif sensors loaded: %s sensors", 
                           nrow(bruitparif_data)), 
                   process = "info")
}

# Acoucité sensors
acoucite_data <- load_sensor_source(
  base_dir   = SENSORS_DATA_DIR,
  dir_name   = "POINT_NOISE_ACOUCITE",
  file_root  = "POINT_NOISE_ACOUCITE_2154",
  target_crs = CFG$TARGET_CRS)
if (!is.null(acoucite_data)) {
  sensors_list[["ACOUCITE"]] <- acoucite_data
  pipeline_message(sprintf("Acoucité sensors loaded: %s sensors", 
                           nrow(acoucite_data)), 
                   process = "info")
}

# Child project sensors (11 sources)
child_sources <- list(
  CHILD_HOME_BORDEAUX        = list(dir = "CHILD_HOME_BORDEAUXrfhome", 
                                    root = "CHILD_HOME_BORDEAUX_CBS"),
  CHILD_HOME_BREST           = list(dir = "CHILD_HOME_BRESTrfhome", 
                                    root = "CHILD_HOME_BREST_CBS"),
  CHILD_HOME_LYON            = list(dir = "CHILD_HOME_LYONrfhome", 
                                    root = "CHILD_HOME_LYON_CBS"),
  CHILD_HOME_STRASBOURG_GEO  = list(dir = "CHILD_HOME_STRASBOURGgeoclimateHome", 
                                    root = "CHILD_HOME_STRASBOURG_CBS"),
  CHILD_HOME_STRASBOURG_RF   = list(dir = "CHILD_HOME_STRASBOURGrfhome", 
                                    root = "CHILD_HOME_STRASBOURG_CBS"),
  CHILD_RANDOM_BORDEAUX_GEO  = list(dir = "CHILD_RANDOM_BORDEAUXgeoclimate", 
                                    root = "CHILD_RANDOM_BORDEAUX_CBS"),
  CHILD_RANDOM_BORDEAUX_RF   = list(dir = "CHILD_RANDOM_BORDEAUXrf", 
                                    root = "CHILD_RANDOM_BORDEAUX_CBS"),
  CHILD_RANDOM_BREST         = list(dir = "CHILD_RANDOM_BRESTrf", 
                                    root = "CHILD_RANDOM_BREST_CBS"),
  CHILD_RANDOM_LYON          = list(dir = "CHILD_RANDOM_LYONrf", 
                                    root = "CHILD_RANDOM_LYON_CBS"),
  CHILD_RANDOM_STRASBOURG_RF = list(dir = "CHILD_RANDOM_STRASBOURGrf", 
                                    root = "CHILD_RANDOM_STRASBOURG_CBS"),
  CHILD_STRASBOURG_GEO       = list(dir = "CHILD_STRASBOURGgeoclimate", 
                                    root = "CHILD_RANDOM_STRASBOURG_CBS" )
)

for (source_name in names(x = child_sources)) {
  child_info <- child_sources[[source_name]]
  sensor_path <- find_sensor_file(
    base_dir  = SENSORS_DATA_DIR,
    dir_name  = child_info$dir,
    file_root = child_info$root)

  if (!is.null(sensor_path)) {
    sensors_list[[source_name]] <- load_sensor_source(
      base_dir   = SENSORS_DATA_DIR,
      dir_name   = child_info$dir,
      file_root  = child_info$root,
      target_crs = CFG$TARGET_CRS)
    
    pipeline_message(sprintf("%s sensors loaded: %s sensors", 
                             source_name, 
                             nrow(sensors_list[[source_name]])), 
                     process = "info")
  }
}

# Remove any NULL entries so only successfully loaded sources remain.
sensors_list <- sensors_list[!vapply(sensors_list, is.null, logical(1))]

pipeline_message(sprintf("Sensors loaded: %d sources", length(x = sensors_list)), 
                 level = 1, progress = "end", process = "valid")

for (source_name in names(x = sensors_list)) {
  pipeline_message(sprintf("- %s: %s sensors", 
                           source_name, nrow(sensors_list[[source_name]])), 
                   process = "info")
}

# ------------------------------------------------------------------------------
# Combine sensors and load relevant network
# ------------------------------------------------------------------------------

if (length(x = sensors_list) == 0) {
  pipeline_message("No sensor files found - skipping sensor predictions", 
                   process = "warning")
} else {

  # Combine all sensors
  # Ensure all sensor sources use the target CRS and merge rows even when
  # source attribute schemas differ.
  sensors_list <- lapply(X   = sensors_list, 
                         FUN = function(x) {
                                ensure_target_crs(sf_obj = x, 
                                                  target_crs = CFG$TARGET_CRS)})
  all_sensors  <- dplyr::bind_rows(sensors_list)

  # Load network around sensors
  osm_sensors <- load_network_around_points(
    points        = all_sensors,
    buffer_radius = SENSOR_BUFFER_RADIUS,
    target_crs    = CFG$TARGET_CRS, 
    osm_roads_path= CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)

  # Apply predictions
  pipeline_message("Applying XGBoost models", level = 1, 
                   progress = "start", process = "calc")

  osm_sensors_dt <- as.data.frame(
    x = sf::st_drop_geometry(x = osm_sensors))

  predictions_wide <- apply_xgboost_predictions(
    network_data = osm_sensors_dt,
    models_list  = models_list,
    feature_info = feature_info)

  pipeline_message(sprintf("Predictions completed: %s roads x %s periods", 
                           fmt(nrow(predictions_wide)), 
                           length(x = feature_info$all_periods)), 
                   level = 1, progress = "end", process = "valid")

  # Save periods list before freeing models
  all_periods <- feature_info$all_periods

  # Free model data no longer needed
  rm(models_list, feature_info, osm_sensors_dt)
  gc(verbose = FALSE)

  # Convert to long format
  predictions_long <- predictions_wide %>%
    pivot_longer(
      cols          = tidyr::matches(match = "^(flow|truck_pct|speed)_"),
      names_to      = c(".value", "period"),
      names_pattern = "^(flow|truck_pct|speed)_(.+)$"
    )

  predictions_long <- predictions_long %>%
    mutate(
      HGV    = flow * (truck_pct / 100),
      LV     = flow - HGV,
      TV     = flow,
      period = factor(x      = period, 
                      levels = all_periods)
    ) %>%
    select(osm_id, highway, period, TV, HGV, LV, speed, 
           osm_speed, osm_speed_imputed, truck_pct)

  # Add QGIS-friendly datetime fields for each period
  predictions_long <- add_period_datetime_columns(predictions_long)

  # Add geometry
  predictions_sf <- merge(x     = predictions_long, 
                          y     = osm_sensors[, c("osm_id", "name", "geom")], 
                          by    = "osm_id", 
                          all.x = TRUE)

  predictions_sf <- sf::st_as_sf(x = predictions_sf)

  if (sf::st_crs(x = predictions_sf) != CFG$TARGET_CRS) {
    predictions_sf <- sf::st_transform(x   = predictions_sf, 
                                      crs = CFG$TARGET_CRS)
  }

  # Enforce datetime fields on exported layers
  predictions_sf <- add_period_datetime_columns(predictions_sf)

  # Validate predictions
  validation <- validate_predictions(predictions_long)

  if (!validation$is_valid) {
    pipeline_message(sprintf("Validation warnings: %s issues detected", 
                             length(x = validation$issues)), 
                     process = "warning")
  }

  # Export combined predictions (all sensors)
  pipeline_message("Exporting combined predictions", level = 1, 
                   progress = "start", process = "save")

  sf::st_write(obj        = predictions_sf, 
               dsn        = CFG$SENSORS_PREDICTION_FILEPATH, 
               delete_dsn = TRUE, 
               quiet      = FALSE)

  pipeline_message(sprintf("Combined export: %s", 
                           rel_path(CFG$SENSORS_PREDICTION_FILEPATH)), 
                   level = 1, progress = "end", process = "save")

  # Export predictions for each sensor source
  pipeline_message("Exporting per-source predictions", level = 1, 
                   progress = "start", process = "save")

  for (source_name in names(x = sensors_list)) {
    pipeline_message(sprintf("%s: %s sensors", source_name, 
                             nrow(sensors_list[[source_name]])), 
                     process = "info")
    # Create buffer union for this source
    source_buffer <- sf::st_buffer(x    = sensors_list[[source_name]], 
                                   dist = SENSOR_BUFFER_RADIUS)
    source_union  <- sf::st_union(x = source_buffer)
    pipeline_message(sprintf("%s: buffer union area %.0f m²", source_name, 
                             sf::st_area(source_union)), 
                     process = "info")
    
    # Filter roads in this source's buffer
    roads_in_source <- sf::st_filter(x = osm_sensors, 
                                     y = source_union)
    
    pipeline_message(sprintf("%s: checking %s roads in buffer", source_name, 
                             nrow(roads_in_source)), 
                     process = "info")
    
    if (nrow(roads_in_source) > 0) {
      # Filter predictions
      predictions_source <- predictions_sf[
                              predictions_sf$osm_id %in% roads_in_source$osm_id, ]
      predictions_source <- add_period_datetime_columns(predictions_source)
      
      # Export
      output_file <- file.path(
        CFG$SENSORS_OUTPUT_DIR,
        sprintf("07_predictions_sensors_%s.gpkg", source_name))
      
      sf::st_write(obj        = predictions_source, 
                   dsn        = output_file, 
                   delete_dsn = TRUE, 
                   quiet      = FALSE)
      
      pipeline_message(sprintf("- %s: %s roads", source_name, 
                               length(x = unique(x = predictions_source$osm_id))), 
                       process = "info")
    } else {
      pipeline_message(sprintf("- %s: no roads within %sm buffer", source_name, 
                               SENSOR_BUFFER_RADIUS), 
                       process = "warning")
    }
  }

  pipeline_message("Per-source exports completed", level = 1, 
                  progress = "end", process = "save")

  # Summary statistics
  pipeline_message("Prediction summary for noise sensors:", level = 1, 
                  progress = "start", process = "search")

pipeline_message(sprintf("- Total sensors: %s (%s sources)", 
                          fmt(nrow(all_sensors)), length(x = sensors_list)), 
                  process = "info")

  pipeline_message(sprintf("- Roads predicted: %s", 
                          fmt(length(x = unique(x = predictions_long$osm_id)))), 
                  process = "info")

  pipeline_message(sprintf("- Total predictions: %s", 
                          fmt(nrow(x = predictions_long))), 
                  process = "info")

  pipeline_message("End of prediction summary for noise sensors:", level = 1, 
                  progress = "end", process = "valid")

  pipeline_message("Noise sensors traffic prediction completed", level = 0, 
                  progress = "end", process = "valid")

}
