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
if (file.exists(file.path(SENSORS_DATA_DIR, 
                          "POINT_NOISE_BRUITPARIF_COMPARE",
                          "POINT_NOISE_BRUITPARIF_COMPARE.shp"))) {
  sensors_list[["BRUITPARIF"]] <- sf::st_read(
    dsn   = file.path(SENSORS_DATA_DIR, 
                      "POINT_NOISE_BRUITPARIF_COMPARE",
                      "POINT_NOISE_BRUITPARIF_COMPARE.shp"), 
    quiet = TRUE) %>% 
    st_transform(CFG$TARGET_CRS)
  
  pipeline_message(sprintf("Bruitparif sensors loaded: %s sensors", 
                           nrow(sensors_list[["BRUITPARIF"]])), 
                   process = "info")
}

# Acoucité sensors
if (file.exists(file.path(SENSORS_DATA_DIR, 
                          "POINT_NOISE_ACOUCITE",
                          "POINT_NOISE_ACOUCITE_COMPARE.shp"))) {
  sensors_list[["ACOUCITE"]] <- sf::st_read(
    dsn   = file.path(SENSORS_DATA_DIR, 
                      "POINT_NOISE_ACOUCITE",
                      "POINT_NOISE_ACOUCITE_COMPARE.shp"), 
    quiet = TRUE) %>% 
    st_transform(CFG$TARGET_CRS)
  
  pipeline_message(sprintf("Acoucité sensors loaded: %s sensors", 
                           nrow(sensors_list[["ACOUCITE"]])), 
                   process = "info")
}

# Child project sensors (11 sources)
child_files <- list(
  CHILD_HOME_BORDEAUX        = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_HOME_BORDEAUXrfhome/CHILD_HOME_BORDEAUX_CBS.shp"),
  CHILD_HOME_BREST           = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_HOME_BRESTrfhome/CHILD_HOME_BREST_CBS.shp"),
  CHILD_HOME_LYON            = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_HOME_LYONrfhome/CHILD_HOME_LYON_CBS.shp"),
  CHILD_HOME_STRASBOURG_GEO  = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_HOME_STRASBOURGgeoclimateHome/CHILD_HOME_STRASBOURG_CBS.shp"),
  CHILD_HOME_STRASBOURG_RF   = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_HOME_STRASBOURGrfhome/CHILD_HOME_STRASBOURG_CBS.shp"),
  CHILD_RANDOM_BORDEAUX_GEO  = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_RANDOM_BORDEAUXgeoclimate/CHILD_RANDOM_BORDEAUX_CBS.shp"),
  CHILD_RANDOM_BORDEAUX_RF   = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_RANDOM_BORDEAUXrf/CHILD_RANDOM_BORDEAUX_CBS.shp"),
  CHILD_RANDOM_BREST         = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_RANDOM_BRESTrf/CHILD_RANDOM_BREST_CBS.shp"),
  CHILD_RANDOM_LYON          = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_RANDOM_LYONrf/CHILD_RANDOM_LYON_CBS.shp"),
  CHILD_RANDOM_STRASBOURG_RF = file.path(SENSORS_DATA_DIR, 
                                         "CHILD_RANDOM_STRASBOURGrf/CHILD_RANDOM_STRASBOURG_CBS.shp"),
  CHILD_STRASBOURG_GEO       = file.path(SENSORS_DATA_DIR,
                                         "CHILD_STRASBOURGgeoclimate/CHILD_RANDOM_STRASBOURG_CBS.shp")
)

for (source_name in names(x = child_files)) {
  if (file.exists(child_files[[source_name]])) {
    sensors_list[[source_name]] <- sf::st_read(
        dsn   = child_files[[source_name]], 
        quiet = TRUE) %>% 
      st_transform(CFG$TARGET_CRS)
    
    pipeline_message(sprintf("%s sensors loaded: %s sensors", 
                             source_name, nrow(sensors_list[[source_name]])), 
                     process = "info")
  }
}

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
  # Ensure all sensor sources have the exact same CRS representation to avoid 'different crs' error
  sensors_list <- lapply(X   = sensors_list, 
                         FUN = function(x) {
                                sf::st_set_crs(x, CFG$TARGET_CRS)})
  all_sensors  <- do.call(what = rbind, 
                          args = sensors_list)

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
    # Create buffer union for this source
    source_buffer <- sf::st_buffer(x    = sensors_list[[source_name]], 
                                   dist = SENSOR_BUFFER_RADIUS)
    source_union  <- sf::st_union(x = source_buffer)
    
    # Filter roads in this source's buffer
    roads_in_source <- sf::st_filter(x = osm_sensors, 
                                     y = source_union)
    
    if (nrow(roads_in_source) > 0) {
      # Filter predictions
      predictions_source <- predictions_sf[
                              predictions_sf$osm_id %in% roads_in_source$osm_id, ]
      predictions_source <- add_period_datetime_columns(predictions_source)
      
      # Export
      output_file <- file.path(
        CFG$SENSORS_PREDICTION_FILEPATH,
        sprintf("07_predictions_sensors_%s.gpkg", source_name))
      
      sf::st_write(obj        = predictions_source, 
                   dsn        = output_file, 
                   delete_dsn = TRUE, 
                   quiet      = FALSE)
      
      pipeline_message(sprintf("- %s: %s roads", source_name, 
                               length(x = unique(x = predictions_source$osm_id))), 
                       process = "info")
    }
  }

  pipeline_message("Per-source exports completed", level = 1, 
                  progress = "end", process = "save")

  # Summary statistics
  pipeline_message("Prediction summary for noise sensors:", level = 1, 
                  progress = "start", process = "search")

  pipeline_message(sprintf("- Total sensors: %s (13 sources)", 
                          fmt(nrow(all_sensors))), 
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
