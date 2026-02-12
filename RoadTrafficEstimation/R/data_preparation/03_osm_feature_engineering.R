# ==============================================================================
# STAGE 3: OSM FEATURE ENGINEERING ON FRANCE NETWORK
# ==============================================================================
# Ce script applique le feature engineering sur l'ensemble du réseau OSM France
# (incluant connectivité et DEGRE communes) pour créer une couche pré-calculée
# utilisable directement pour l'entraînement et la prédiction.
# 
# Entrées:
#   - 01_osm_network_augmented.gpkg (OSM + connectivité + DEGRE)
# Sorties:
#   - 02_osm_network_france_engineered.gpkg (couche France avec features)
#   - 02_imputation_rules_france.rds (règles d'imputation globales)
#
# En mode TEST: réduit la région à une bbox de test pour rapidité
# ==============================================================================

pipeline_message(text = "OSM France feature engineering", 
                 level = 0, progress = "start", process = "calc")

# Check if running in TEST mode
IS_TEST_MODE <- exists("TEST_REGION") && !is.null(TEST_REGION)
if (IS_TEST_MODE) {
  pipeline_message(
    text = sprintf("TEST MODE: Cropping to region %s (bbox: %s)", 
                   TEST_REGION$name, 
                   paste(TEST_REGION$bbox, collapse=", ")),
    level = 1, process = "info"
  )
}

# ------------------------------------------------------------------------------
# Load full OSM France road network with connectivities and communes
# ------------------------------------------------------------------------------

if (!exists('osm_full_network') || 
    !is.data.frame(get('osm_full_network'))) {
  
  pipeline_message(
    text = sprintf("Loading OSM France network from %s", 
                   rel_path(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  osm_full_network <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH, 
    quiet = TRUE)
  
  # Ensure correct CRS
  if (sf::st_crs(osm_full_network) != CONFIG$TARGET_CRS) {
    osm_full_network <- osm_full_network %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  pipeline_message(
    text = sprintf("OSM France network loaded: %s roads", 
                   fmt(nrow(osm_full_network))), 
    level = 1, progress = "end", process = "valid")
}

# Crop to test region if in TEST mode
if (IS_TEST_MODE) {
  osm_full_network <- crop_to_test_region(osm_full_network)
  pipeline_message(
    text = sprintf("Cropped to test region: %s roads remain", 
                   fmt(nrow(osm_full_network))),
    level = 1, process = "info"
  )
}

# ------------------------------------------------------------------------------
# Compute global imputation rules from full France network
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Computing imputation rules from full France network", 
  level = 1, progress = "start", process = "build")

# Coerce to data.table for efficient processing
setDT(osm_full_network)

pipeline_message(text = describe_df(osm_full_network), process = "info")

# Clean highway types
osm_full_network$highway <- as.character(x = osm_full_network$highway)
osm_full_network$highway[
  is.na(osm_full_network$highway) | 
    osm_full_network$highway == ""] <- "unclassified"

# Determine column names (handle both naming conventions)
lanes_col <- if ("lanes_osm" %in% names(osm_full_network)) {
  "lanes_osm"
} else if ("lanes" %in% names(osm_full_network)) {
  "lanes"
} else {
  NULL
}

maxspeed_col <- if ("maxspeed_osm" %in% names(osm_full_network)) {
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
                 median_lanes := CONFIG$DEFAULT_NUMBER_OF_LANES]
imputation_rules[is.na(median_speed), 
                 median_speed := CONFIG$DEFAULT_VEHICLE_SPEED]

# Add fallback rule for missing highway types
imputation_rules <- rbind(
  imputation_rules, 
  data.table(
    highway = "missing", 
    median_lanes = CONFIG$DEFAULT_NUMBER_OF_LANES, 
    median_speed = CONFIG$DEFAULT_VEHICLE_SPEED, 
    n_roads = CONFIG$DEFAULT_NUMBER_OF_ROADS))

# Save imputation rules
saveRDS(object = imputation_rules, 
        file = CONFIG$IMPUTATION_RULES_FRANCE_FILEPATH)

pipeline_message(text = describe_df(imputation_rules), process = "info")

pipeline_message(
  text = sprintf("Imputation rules computed and saved to %s", 
                 rel_path(CONFIG$IMPUTATION_RULES_FRANCE_FILEPATH)), 
  level = 1, progress = "end", process = "save")

# ------------------------------------------------------------------------------
# Apply feature engineering to full France network
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Applying feature engineering to France network", 
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
  text = sprintf("Feature engineering applied: %s roads processed", 
                 fmt(nrow(osm_france_engineered))), 
  process = "info")

# ------------------------------------------------------------------------------
# Save engineered France network
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Saving engineered France network", 
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
  dsn = CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH, 
  delete_dsn = TRUE,
  quiet = FALSE)

pipeline_message(
  text = sprintf("Engineered France network saved to %s", 
                 rel_path(CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)), 
  level = 1, progress = "end", process = "save")

# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Feature engineering summary:", 
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - Total roads: %s", 
                 fmt(nrow(osm_france_engineered))), 
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - Highway types: %s", 
                 length(unique(osm_france_engineered$highway))), 
  level = 1, process = "info")

pipeline_message(
  text = sprintf("  - DEGRE classes: %s", 
                 length(unique(osm_france_engineered$DEGRE))), 
  level = 1, process = "info")

# Feature completeness check
required_features <- c("highway", "DEGRE", "ref_letter", "first_word", 
                      "oneway_osm", "lanes_osm", "speed", 
                      "connectivity", "betweenness", "closeness", "pagerank",
                      "coreness", "dead_end_score", "edge_length_m")

available_features <- intersect(required_features, 
                               names(osm_france_engineered))

pipeline_message(
  text = sprintf("  - Features available: %s/%s", 
                 length(available_features), 
                 length(required_features)), 
  level = 1, process = "info")

if (length(available_features) < length(required_features)) {
  missing_features <- setdiff(required_features, available_features)
  pipeline_message(
    text = sprintf("  - Missing features: %s", 
                   paste(missing_features, collapse = ", ")), 
    level = 1, process = "warn")
}

pipeline_message(text = "OSM France feature engineering completed", 
                 level = 0, progress = "end", process = "valid")
