# ==============================================================================
# STAGE 3: OSM FEATURE ENGINEERING ON FRANCE NETWORK
# ==============================================================================
# This script applies feature engineering to the entire OSM France network
# (including connectivity and DEGRE) to create a pre-calculated layer
# that can be used directly for training and prediction.
# 
# Inputs:
#   - 01_osm_network_augmented.gpkg (OSM + connectivity + DEGRE)
# Outputs:
#   - 02_osm_network_france_engineered.gpkg (France layer with features)
#   - 02_imputation_rules_france.rds (global imputation rules)
#
# In TEST mode: reduces the region to a test bbox for speed
# ==============================================================================

pipeline_message("Road feature engineering processing", level = 0, 
                 progress = "start", process = "calc")

# ------------------------------------------------------------------------------
# Load full OSM France road network with connectivities and communes
# ------------------------------------------------------------------------------

if (!exists('osm_full_network') 
    && isFALSE(is.data.frame(get('osm_full_network')))){
  
  pipeline_message(sprintf("Loading OSM full road network from %s", 
                           rel_path(cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
                   level = 1, progress = "start", process = "load")
  
  osm_full_network <- sf::st_read(
    dsn = cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH, 
    quiet = TRUE)
  
  # Project data into target CRS if needed
  if (sf::st_crs(osm_full_network) != cfg_g$TARGET_CRS){
    osm_full_network <- osm_full_network %>% 
      st_transform(crs = cfg_g$TARGET_CRS)
  }
  
  pipeline_message("OSM France network successfully loaded", level = 1, 
                   progress = "end", process = "valid")
}

# Crop to test region if in TEST mode
if (IS_TEST_MODE) {
  osm_full_network <- crop_to_test_region(osm_full_network)
  pipeline_message(
    sprintf("Cropped to test region: %s roads remain", 
            fmt(nrow(osm_full_network))),
    process = "info"
  )
}

# ------------------------------------------------------------------------------
# Compute global imputation rules from full France network
# ------------------------------------------------------------------------------

pipeline_message(
  "Computing imputation rules from full France network", 
  level = 1, progress = "start", process = "build")

# Coerce OSM data frame to data.table
setDT(osm_full_network)

pipeline_message(describe_df(osm_full_network), process = "info")
pipeline_message(describe_df(full_network_avatar_id), process = "info")

# Clean highway types
osm_full_network$highway <- as.character(x = osm_full_network$highway)
osm_full_network$highway[
  is.na(osm_full_network$highway) | 
    osm_full_network$highway == ""] <- "unclassified"

# Imputation rules
lanes_col <- if ("lanes_osm" %in% names(osm_full_network)){
  "lanes_osm"
} else if ("lanes" %in% names(osm_full_network)) {
  "lanes"
} else {
  NULL
}
maxspeed_col <- if ("maxspeed_osm" %in% names(osm_full_network)){
  "maxspeed_osm"
} else if ("maxspeed" %in% names(osm_full_network)) {
  "maxspeed"
} else {
  NULL
}

# Compute imputation rules by highway type
if (!is.null(lanes_col) && !is.null(maxspeed_col)) {
  imputation_rules <- osm_full_network[, .(
    median_lanes = median(x = as.numeric(get(lanes_col)), na.rm = TRUE),
    median_speed = median(x = as.numeric(get(maxspeed_col)), na.rm = TRUE),
    n_roads = .N), 
    by = highway]
} else {
  # Fallback if columns missing
  imputation_rules <- osm_full_network[, .(
    median_lanes = NA_real_,
    median_speed = NA_real_,
    n_roads = .N), 
    by = highway]
}

# Apply default values where imputation rules are missing
imputation_rules[is.na(median_lanes), 
                 median_lanes := cfg_data$DEFAULT_NUMBER_OF_LANES]
imputation_rules[is.na(median_speed), 
                 median_speed := cfg_data$DEFAULT_VEHICLE_SPEED]

# Add fallback rule for missing highway types
imputation_rules <- rbind(
  imputation_rules, 
  data.table(highway = "missing", 
             median_lanes = cfg_data$DEFAULT_NUMBER_OF_LANES, 
             median_speed = cfg_data$DEFAULT_VEHICLE_SPEED, 
             n_roads = cfg_data$DEFAULT_NUMBER_OF_ROADS))

# Save imputation rules
saveRDS(object = imputation_rules, 
        file = cfg_data$AVATAR_IMPUTATION_RULES_FILEPATH)

pipeline_message(describe_df(imputation_rules), process = "info")

pipeline_message(
  sprintf("Default road traffic imputation rules successfully built and saved ", 
          "into file ", rel_path(cfg_data$AVATAR_IMPUTATION_RULES_FILEPATH)), 
  level = 2, progress = "end", process = "save")

# ------------------------------------------------------------------------------
# Apply feature engineering to full France network
# ------------------------------------------------------------------------------

pipeline_message("Applying feature engineering to France network", 
                 level = 1, progress = "start", process = "calc")

# Apply feature engineering pipeline
# This function from utils_osm.R:
# - Creates ordered highway factor
# - Extracts ref_letter (first letter of ref tag)
# - Extracts first_word from name
# - Normalizes oneway to binary
# - Imputes missing lanes and speed using rules
osm_france_engineered <- process_network_features(
  data = osm_full_network, 
  rules = imputation_rules)

pipeline_message(
  sprintf("Feature engineering applied: %s roads processed", 
          fmt(nrow(osm_france_engineered))), 
  process = "info")

# ------------------------------------------------------------------------------
# Save engineered France network
# ------------------------------------------------------------------------------

pipeline_message("Saving engineered France network", 
                 level = 1, progress = "start", process = "save")

# Convert back to sf object if needed
if (!"sf" %in% class(osm_france_engineered)) {
  osm_france_engineered <- sf::st_as_sf(osm_france_engineered)
}

# Add QGIS-friendly datetime fields when `period` exists
osm_france_engineered <- add_period_datetime_columns(osm_france_engineered)

# Export to GeoPackage
sf::st_write(
  obj = osm_france_engineered, 
  dsn = cfg_data$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH, 
  delete_dsn = TRUE,
  quiet = FALSE)

pipeline_message(
  sprintf("Engineered France network saved to %s", 
          rel_path(cfg_data$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)), 
  level = 1, progress = "end", process = "save")

# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------

pipeline_message("Feature engineering summary:", process = "info")

pipeline_message(
  sprintf("  - Total roads: %s", 
          fmt(nrow(osm_france_engineered))), 
  process = "info")

pipeline_message(
  sprintf("  - Highway types: %s", 
          length(unique(osm_france_engineered$highway))), 
  process = "info")

pipeline_message(
  sprintf("  - DEGRE classes: %s", 
          length(unique(osm_france_engineered$DEGRE))), 
  process = "info")

# Feature completeness check
required_features <- c("highway", "DEGRE", "ref_letter", "first_word", 
                      "oneway_osm", "lanes_osm", "lanes_directional",
                      "speed", "junction_osm",
                      "connectivity", "betweenness", "closeness", "pagerank",
                      "coreness", "dead_end_score", "edge_length_m")

available_features <- intersect(required_features, 
                               names(osm_france_engineered))

pipeline_message(
  sprintf("  - Features available: %s/%s", 
          length(available_features), 
          length(required_features)), 
  process = "info")

if (length(available_features) < length(required_features)) {
  missing_features <- setdiff(required_features, available_features)
  pipeline_message(
    sprintf("  - Missing features: %s", 
            paste(missing_features, collapse = ", ")), 
    process = "warn")
}

pipeline_message("OSM France feature engineering completed", 
                 level = 0, progress = "end", process = "valid")

# Cleanup large objects to free memory for next steps
rm(list = intersect(ls(), c("osm_france_engineered", "osm_full_network",
                            "imputation_rules")))
gc(verbose = FALSE)
