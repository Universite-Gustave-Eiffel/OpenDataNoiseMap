# ==============================================================================
# PREDICTION: PEMB (PARIS EST MARNE & BOIS)
# ==============================================================================
# Prédit le trafic pour la zone Paris Est Marne & Bois en utilisant la couche
# France engineered pré-calculée.
#
# Zone couverte: Est parisien — Vincennes, Nogent-sur-Marne, Champigny,
#   Fontenay-sous-Bois, Créteil, Maisons-Alfort, Joinville, Saint-Mandé,
#   Bois de Vincennes, vallée de la Marne.
#
# Entrées:
#   - 02_osm_network_france_engineered.gpkg (couche France avec features)
#   - 06_xgboost_trained_models.rds (modèles XGBoost)
#   - 06_xgboost_feature_info.rds (formule et périodes)
# Sorties:
#   - 07_predictions_pemb.gpkg (prédictions format long)
# ==============================================================================

pipeline_message(text = "PEMB (Paris Est Marne & Bois) traffic prediction", 
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
# Define PEMB bounding box
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Defining PEMB (Paris Est Marne & Bois) area",
  level = 1, progress = "start", process = "calc")

# PEMB bounds in Lambert93 (EPSG:2154)
# Converted from WGS84: (48.8609, 2.3862) → (48.7733, 2.6048)
pemb_bbox <- c(
  xmin = 654892,  # West
  ymin = 6852748, # South
  xmax = 671006,  # East
  ymax = 6862393  # North
)

pipeline_message(
  text = sprintf("Bbox defined: [%s, %s, %s, %s]", 
                 pemb_bbox[1], pemb_bbox[2], 
                 pemb_bbox[3], pemb_bbox[4]),
  level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Load and crop France engineered network
# ------------------------------------------------------------------------------

osm_pemb <- load_network_for_prediction(
  bbox = pemb_bbox, 
  config = CONFIG)

pipeline_message(
  text = sprintf("Network loaded: %s roads in PEMB area", 
                 fmt(nrow(osm_pemb))),
  process = "info")

# ------------------------------------------------------------------------------
# Apply XGBoost predictions
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Applying XGBoost models to PEMB network",
  level = 1, progress = "start", process = "calc")

# Convert to data.frame for model application
osm_pemb_dt <- as.data.frame(sf::st_drop_geometry(osm_pemb))

# Apply predictions (returns wide format with all periods)
predictions_wide <- apply_xgboost_predictions(
  network_data = osm_pemb_dt,
  models_list = models_list,
  feature_info = feature_info)

pipeline_message(
  text = sprintf("Predictions completed: %s roads × %s periods", 
                 fmt(nrow(predictions_wide)), 
                 length(feature_info$all_periods)),
  level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Convert to long format with period column
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Converting to long format",
  level = 1, progress = "start", process = "calc")

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
    period = factor(period, levels = feature_info$all_periods)
  )

# Select final columns
predictions_long <- predictions_long %>%
  select(osm_id, highway, period, TV, HGV, LV, speed, truck_pct)

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

# Join geometry from original network
predictions_sf <- merge(
  predictions_long,
  osm_pemb[, c("osm_id", "name", "geom")],
  by = "osm_id",
  all.x = TRUE)

# Convert to sf
predictions_sf <- sf::st_as_sf(predictions_sf)

# Ensure correct CRS
if (sf::st_crs(predictions_sf) != CONFIG$TARGET_CRS) {
  predictions_sf <- sf::st_transform(predictions_sf, CONFIG$TARGET_CRS)
}

# Export to GeoPackage
sf::st_write(
  obj = predictions_sf,
  dsn = CONFIG$PEMB_PREDICTION_FILEPATH,
  delete_dsn = TRUE,
  quiet = FALSE)

pipeline_message(
  text = sprintf("Predictions exported to %s", 
                 rel_path(CONFIG$PEMB_PREDICTION_FILEPATH)),
  level = 1, progress = "end", process = "save")

# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Prediction summary for PEMB (Paris Est Marne & Bois):",
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - Roads: %s", fmt(length(unique(predictions_long$osm_id)))),
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - Periods: %s", length(feature_info$all_periods)),
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

pipeline_message(text = "PEMB (Paris Est Marne & Bois) prediction completed", 
                 level = 0, progress = "end", process = "valid")
