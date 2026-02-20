# =============================================================================
# GENERIC FORECAST SCRIPT
# Usage:
#   Rscript predict_forecast.R nantes
#   Rscript predict_forecast.R paris
#   Rscript predict_forecast.R sensors
# =============================================================================

MODE <- commandArgs(trailingOnly = TRUE)[1] %||% "sensors"

# ---------------------------------------------------------------------------
# Load models + metadata
# ---------------------------------------------------------------------------
models <- load_forecast_models()
models_list <- models$models_list
feature_info <- models$feature_info

pipeline_message(
  sprintf("Loaded %d trained models", length(models_list)),
  process = "info")
pipeline_message(
  sprintf("Feature formula: %s", deparse(expr = feature_formula)),
  process = "info")
pipeline_message(
  sprintf("Periods: %s", paste(feature_info$all_periods, collapse=", ")),
  process = "info")

# ---------------------------------------------------------------------------
# Load roads according to MODE
# ---------------------------------------------------------------------------
roads_sf <- load_target_roads(mode = MODE)
roads_dt <- data.table(st_drop_geometry(roads_sf))

# ---------------------------------------------------------------------------
# Prepare features
# ---------------------------------------------------------------------------
imputation_rules <- readRDS(file = CONFIG$AVATAR_IMPUTATION_RULES_FILEPATH)
roads_dt <- prepare_osm_features(roads_dt, imputation_rules)

# ---------------------------------------------------------------------------
# Design matrix
# ---------------------------------------------------------------------------
X <- sparse.model.matrix(feature_info$feature_formula, data = roads_dt)

# ---------------------------------------------------------------------------
# Run predictions
# ---------------------------------------------------------------------------
results_long <- run_predictions(X = X, 
                                base_dt = roads_dt, 
                                models_list = models_list, 
                                all_periods = feature_info$all_periods)

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------
results_sf <- merge(x = results_long, y = roads_sf, by = "osm_id")
st_write(obj = results_sf, 
         dsn = file.path("output", paste0("forecast_", MODE, ".gpkg")), 
         delete_dsn = TRUE)
