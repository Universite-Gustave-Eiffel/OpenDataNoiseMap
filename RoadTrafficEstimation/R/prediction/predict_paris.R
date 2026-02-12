# ==============================================================================
# PREDICTION: PARIS METROPOLITAN AREA
# ==============================================================================
# Prédit le trafic pour la région métropolitaine de Paris en utilisant la couche
# France engineered pré-calculée.
#
# Entrées:
#   - 02_osm_network_france_engineered.gpkg (couche France avec features)
#   - 06_xgboost_trained_models.rds (modèles XGBoost)
#   - 06_xgboost_feature_info.rds (formule et périodes)
# Sorties:
#   - 07_predictions_paris.gpkg (prédictions format long)
# ==============================================================================

pipeline_message(text = "Paris Metropolitan Area traffic prediction", 
                 level = 0, progress = "start", process = "calc")

# Source utilities
source("R/utils_prediction.R", encoding = "UTF-8")

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
# Define Paris Metropolitan Area bounding box
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Defining Paris Metropolitan Area",
  level = 1, progress = "start", process = "calc")

# Paris Metropolitan (Île-de-France) bounds in Lambert93 (EPSG:2154)
# Center: ~655000, 6862000
# Extend ~30km radius for greater Paris
paris_bbox <- c(
  xmin = 625000,  # West
  ymin = 6832000, # South
  xmax = 685000,  # East
  ymax = 6892000  # North
)

pipeline_message(
  text = sprintf("Bbox defined: [%s, %s, %s, %s]", 
                 paris_bbox[1], paris_bbox[2], 
                 paris_bbox[3], paris_bbox[4]),
  level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Load and crop France engineered network
# ------------------------------------------------------------------------------

osm_paris <- load_network_for_prediction(
  bbox = paris_bbox, 
  config = CONFIG)

pipeline_message(
  text = sprintf("Network loaded: %s roads in Paris Metropolitan Area", 
                 fmt(nrow(osm_paris))),
  process = "info")

# ------------------------------------------------------------------------------
# Apply XGBoost predictions
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Applying XGBoost models to Paris network",
  level = 1, progress = "start", process = "calc")

# Convert to data.frame for model application
osm_paris_dt <- as.data.frame(sf::st_drop_geometry(osm_paris))

# Apply predictions (returns wide format with all periods)
predictions_wide <- apply_xgboost_predictions(
  network_data = osm_paris_dt,
  models_list = models_list,
  feature_info = feature_info)

pipeline_message(
  text = sprintf("Predictions completed: %s roads × %s periods", 
                 fmt(nrow(predictions_wide)), 
                 length(feature_info$all_periods)),
  level = 1, progress = "end", process = "valid")

# Save periods list before freeing models
all_periods <- feature_info$all_periods

# Free model data no longer needed
rm(models_list, feature_info, osm_paris_dt)
gc(verbose = FALSE)

# ------------------------------------------------------------------------------
# Convert to long format with period column
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Converting to long format",
  level = 1, progress = "start", process = "calc")

# Memory check before pivot (multiplies rows by n_periods)
check_memory_available(
  operation_name = sprintf("Pivot to long format (%s roads)",
                           fmt(nrow(predictions_wide))),
  min_gb = 2, warn_gb = 4)

# Pivot to long format
library(tidyr)
predictions_long <- predictions_wide %>%
  pivot_longer(
    cols = matches("^(flow|truck_pct|speed)_"),
    names_to = c(".value", "period"),
    names_pattern = "^(flow|truck_pct|speed)_(.+)$"
  )

# Calculate derived metrics
predictions_long <- predictions_long %>%
  mutate(
    HGV = flow * (truck_pct / 100),          # Heavy goods vehicles
    LV = flow - HGV,                          # Light vehicles
    TV = flow,                                # Total vehicles
    period = factor(period, levels = all_periods)
  )

# Select final columns
predictions_long <- predictions_long %>%
  select(osm_id, highway, period, TV, HGV, LV, speed, truck_pct)

# Add QGIS-friendly datetime fields for each period
predictions_long <- add_period_datetime_columns(predictions_long)

# Free wide format no longer needed
rm(predictions_wide)
gc(verbose = FALSE)

pipeline_message(
  text = sprintf("Long format: %s rows (roads × periods)", 
                 fmt(nrow(predictions_long))),
  level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Validate predictions
# ------------------------------------------------------------------------------

validation <- validate_predictions(predictions_long)

if (!validation$is_valid) {
  pipeline_message(
    text = sprintf("Validation warnings: %s issues detected", 
                   length(validation$issues)),
    process = "warn")
  for (issue_name in names(validation$issues)) {
    pipeline_message(
      text = sprintf("  - %s: %s cases", 
                     issue_name, validation$issues[[issue_name]]),
      process = "warn")
  }
}

# ------------------------------------------------------------------------------
# Add geometry and export
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Exporting predictions with geometry",
  level = 1, progress = "start", process = "save")

# Memory check before geometry join (duplicates geometry for each period)
check_memory_available(
  operation_name = sprintf("Geometry merge (%s rows)", fmt(nrow(predictions_long))),
  min_gb = 2, warn_gb = 4)

# Join geometry from original network
predictions_sf <- merge(
  predictions_long,
  osm_paris[, c("osm_id", "name", "geom")],
  by = "osm_id",
  all.x = TRUE)

# Convert to sf
predictions_sf <- sf::st_as_sf(predictions_sf)

# Ensure correct CRS
if (sf::st_crs(predictions_sf) != CONFIG$TARGET_CRS) {
  predictions_sf <- sf::st_transform(predictions_sf, CONFIG$TARGET_CRS)
}

# Enforce datetime fields on exported layer
predictions_sf <- add_period_datetime_columns(predictions_sf)

# Export to GeoPackage
sf::st_write(
  obj = predictions_sf,
  dsn = CONFIG$PARIS_PREDICTION_FILEPATH,
  delete_dsn = TRUE,
  quiet = FALSE)

pipeline_message(
  text = sprintf("Predictions exported to %s", 
                 rel_path(CONFIG$PARIS_PREDICTION_FILEPATH)),
  level = 1, progress = "end", process = "save")

# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Prediction summary for Paris Metropolitan Area:",
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

# Period-wise statistics
period_stats <- predictions_long %>%
  group_by(period) %>%
  summarise(
    avg_TV = round(mean(TV, na.rm = TRUE)),
    avg_speed = round(mean(speed, na.rm = TRUE), 1),
    avg_truck_pct = round(mean(truck_pct, na.rm = TRUE), 1),
    .groups = 'drop'
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

pipeline_message(text = "Paris Metropolitan Area prediction completed", 
                 level = 0, progress = "end", process = "valid")
