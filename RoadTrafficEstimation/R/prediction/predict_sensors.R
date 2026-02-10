# ==============================================================================
# PREDICTION: NOISE SENSORS (800M RADIUS AROUND ALL SENSORS)
# ==============================================================================
# Prédit le trafic pour les routes dans un rayon de 800m autour de capteurs de
# bruit (BRUITPARIF, ACOUCITE, CHILD). Utilise la couche France engineered.
#
# Entrées:
#   - 02_osm_network_france_engineered.gpkg (couche France avec features)
#   - 06_xgboost_trained_models.rds (modèles XGBoost)
#   - 06_xgboost_feature_info.rds (formule et périodes)
#   - data/POINT_NOISE_*.shp (capteurs bruit)
#   - data/CHILD_*.shp (capteurs CHILD)
# Sorties:
#   - 07_predictions_sensors_all.gpkg (toutes les prédictions combinées)
#   - 07_predictions_sensors_{SOURCE}.gpkg (x13 fichiers par source)
# ==============================================================================

pipeline_message(text = "Noise sensors traffic prediction", 
                 level = 0, progress = "start", process = "calc")

# Source utilities
source("R/utils_prediction.R", encoding = "UTF-8")

# Buffer radius for sensors
SENSOR_BUFFER_RADIUS <- 800  # meters

# ------------------------------------------------------------------------------
# Load trained models
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Loading trained XGBoost models",
  level = 1, progress = "start", process = "load")

if (!file.exists(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)) {
  pipeline_message(
    text = sprintf("Models not found: %s", 
                   rel_path(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)), 
    process = "error")
  stop("Please run R/model_training/train_xgboost_models.R first")
}

models_list <- readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
feature_info <- readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)

pipeline_message(
  text = sprintf("Models loaded: %s models for %s periods", 
                 length(models_list), 
                 length(feature_info$all_periods)),
  level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Load noise sensors from all sources
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Loading noise sensors",
  level = 1, progress = "start", process = "load")

sensors_list <- list()

# BRUITPARIF sensors
if (file.exists("data/POINT_NOISE_BRUITPARIF_COMPARE.shp")) {
  sensors_list[["BRUITPARIF"]] <- sf::st_read(
    "data/POINT_NOISE_BRUITPARIF_COMPARE.shp", 
    quiet = TRUE) %>% 
    st_transform(CONFIG$TARGET_CRS)
}

# ACOUCITE sensors
if (file.exists("data/POINT_NOISE_ACOUCITE_COMPARE.shp")) {
  sensors_list[["ACOUCITE"]] <- sf::st_read(
    "data/POINT_NOISE_ACOUCITE_COMPARE.shp", 
    quiet = TRUE) %>% 
    st_transform(CONFIG$TARGET_CRS)
}

# CHILD sensors (11 sources)
child_files <- list(
  CHILD_HOME_BORDEAUX = "data/CHILD_HOME_BORDEAUXrfhome/CHILD_HOME_BORDEAUX_CBS.shp",
  CHILD_HOME_BREST = "data/CHILD_HOME_BRESTrfhome/CHILD_HOME_BREST_CBS.shp",
  CHILD_HOME_LYON = "data/CHILD_HOME_LYONrfhome/CHILD_HOME_LYON_CBS.shp",
  CHILD_HOME_STRASBOURG_GEO = "data/CHILD_HOME_STRASBOURGgeoclimateHome/CHILD_HOME_STRASBOURG_CBS.shp",
  CHILD_HOME_STRASBOURG_RF = "data/CHILD_HOME_STRASBOURGrfhome/CHILD_HOME_STRASBOURG_CBS.shp",
  CHILD_RANDOM_BORDEAUX_GEO = "data/CHILD_RANDOM_BORDEAUXgeoclimate/CHILD_RANDOM_BORDEAUX_CBS.shp",
  CHILD_RANDOM_BORDEAUX_RF = "data/CHILD_RANDOM_BORDEAUXrf/CHILD_RANDOM_BORDEAUX_CBS.shp",
  CHILD_RANDOM_BREST = "data/CHILD_RANDOM_BRESTrf/CHILD_RANDOM_BREST_CBS.shp",
  CHILD_RANDOM_LYON = "data/CHILD_RANDOM_LYONrf/CHILD_RANDOM_LYON_CBS.shp",
  CHILD_RANDOM_STRASBOURG_RF = "data/CHILD_RANDOM_STRASBOURGrf/CHILD_RANDOM_STRASBOURG_CBS.shp",
  CHILD_STRASBOURG_GEO = "data/CHILD_STRASBOURGgeoclimate/CHILD_RANDOM_STRASBOURG_CBS.shp"
)

for (source_name in names(child_files)) {
  if (file.exists(child_files[[source_name]])) {
    sensors_list[[source_name]] <- sf::st_read(
      child_files[[source_name]], 
      quiet = TRUE) %>% 
      st_transform(CONFIG$TARGET_CRS)
  }
}

pipeline_message(
  text = sprintf("Sensors loaded: %d sources", length(sensors_list)),
  level = 1, progress = "end", process = "valid")

for (source_name in names(sensors_list)) {
  pipeline_message(
    text = sprintf("  - %s: %s sensors", source_name, nrow(sensors_list[[source_name]])),
    process = "info")
}

# ------------------------------------------------------------------------------
# Load France engineered network
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Loading France engineered network",
  level = 1, progress = "start", process = "load")

osm_france <- sf::st_read(
  CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
  quiet = TRUE)

if (sf::st_crs(osm_france) != CONFIG$TARGET_CRS) {
  osm_france <- sf::st_transform(osm_france, CONFIG$TARGET_CRS)
}

pipeline_message(
  text = sprintf("Network loaded: %s roads", fmt(nrow(osm_france))),
  level = 1, progress = "end", process = "valid")

# Combine all sensors
all_sensors <- do.call(rbind, sensors_list)

# Load network around sensors
osm_sensors <- load_network_around_points(
  points = all_sensors,
  buffer_radius = SENSOR_BUFFER_RADIUS,
  config = CONFIG)

# Apply predictions
pipeline_message(
  text = "Applying XGBoost models",
  level = 1, progress = "start", process = "calc")

osm_sensors_dt <- as.data.frame(sf::st_drop_geometry(osm_sensors))

predictions_wide <- apply_xgboost_predictions(
  network_data = osm_sensors_dt,
  models_list = models_list,
  feature_info = feature_info)

pipeline_message(
  text = sprintf("Predictions completed: %s roads × %s periods", 
                 fmt(nrow(predictions_wide)), 
                 length(feature_info$all_periods)),
  level = 1, progress = "end", process = "valid")

# Convert to long format
library(tidyr)
predictions_long <- predictions_wide %>%
  pivot_longer(
    cols = matches("^(flow|truck_pct|speed)_"),
    names_to = c(".value", "period"),
    names_pattern = "(.+)_(.+)"
  )

predictions_long <- predictions_long %>%
  mutate(
    HGV = flow * (truck_pct / 100),
    LV = flow - HGV,
    TV = flow,
    period = factor(period, levels = feature_info$all_periods)
  ) %>%
  select(osm_id, highway, period, TV, HGV, LV, speed, truck_pct)

# Add geometry
predictions_sf <- merge(
  predictions_long,
  osm_sensors[, c("osm_id", "name", "geom")],
  by = "osm_id",
  all.x = TRUE)

predictions_sf <- sf::st_as_sf(predictions_sf)

if (sf::st_crs(predictions_sf) != CONFIG$TARGET_CRS) {
  predictions_sf <- sf::st_transform(predictions_sf, CONFIG$TARGET_CRS)
}

# Validate predictions
validation <- validate_predictions(predictions_long)

if (!validation$is_valid) {
  pipeline_message(
    text = sprintf("Validation warnings: %s issues detected", 
                   length(validation$issues)),
    process = "warn")
}

# Export combined predictions (all sensors)
pipeline_message(
  text = "Exporting combined predictions",
  level = 1, progress = "start", process = "save")

sf::st_write(
  obj = predictions_sf,
  dsn = CONFIG$SENSORS_ALL_PREDICTION_FILEPATH,
  delete_dsn = TRUE,
  quiet = FALSE)

pipeline_message(
  text = sprintf("Combined export: %s", 
                 rel_path(CONFIG$SENSORS_ALL_PREDICTION_FILEPATH)),
  level = 1, progress = "end", process = "save")

# Export predictions for each sensor source
pipeline_message(
  text = "Exporting per-source predictions",
  level = 1, progress = "start", process = "save")

for (source_name in names(sensors_list)) {
  # Create buffer union for this source
  source_buffer <- sf::st_buffer(sensors_list[[source_name]], SENSOR_BUFFER_RADIUS)
  source_union <- sf::st_union(source_buffer)
  
  # Filter roads in this source's buffer
  roads_in_source <- sf::st_filter(osm_sensors, source_union)
  
  if (nrow(roads_in_source) > 0) {
    # Filter predictions
    predictions_source <- predictions_sf[
      predictions_sf$osm_id %in% roads_in_source$osm_id, ]
    
    # Export
    output_file <- file.path(
      CONFIG$FORECAST_DATA_DIR,
      sprintf("07_predictions_sensors_%s.gpkg", source_name))
    
    sf::st_write(
      obj = predictions_source,
      dsn = output_file,
      delete_dsn = TRUE,
      quiet = FALSE)
    
    pipeline_message(
      text = sprintf("  - %s: %s roads", 
                     source_name, 
                     length(unique(predictions_source$osm_id))),
      process = "info")
  }
}

pipeline_message(
  text = "Per-source exports completed",
  level = 1, progress = "end", process = "save")

# Summary statistics
pipeline_message(
  text = "Prediction summary for noise sensors:",
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - Total sensors: %s (13 sources)", fmt(nrow(all_sensors))),
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - Roads predicted: %s", fmt(length(unique(predictions_long$osm_id)))),
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - Total predictions: %s", fmt(nrow(predictions_long))),
  level = 1, process = "info")

pipeline_message(text = "Noise sensors traffic prediction completed", 
                 level = 0, progress = "end", process = "valid")
