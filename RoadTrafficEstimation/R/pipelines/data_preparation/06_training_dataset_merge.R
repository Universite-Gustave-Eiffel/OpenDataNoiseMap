# ==============================================================================
# STAGE 6: TRAINING DATASET MERGE
# ==============================================================================
# Ce script fusionne les données Avatar nettoyées avec la couche France 
# engineered (déjà pré-calculée avec tous les features) pour créer le dataset
# d'entraînement final.
#
# Entrées:
#   - 02_osm_network_france_engineered.gpkg (couche France avec features)
#   - 03_osm_network_with_avatar_ids.gpkg (réseau OSM filtré capteurs Avatar)
#   - 04_avatar_aggregated_with_ratios.rds (données trafic agrégées)
# Sorties:
#   - 05_training_dataset.rds/.gpkg (dataset final pour entraînement)
#
# En mode TEST: utilise une fraction des données pour rapidité
# ==============================================================================

pipeline_message(text = "Training dataset merge", 
                 level = 0, progress = "start", process = "calc")

# Check if running in TEST mode
IS_TEST_MODE <- exists("TEST_REGION") && !is.null(TEST_REGION)

# ------------------------------------------------------------------------------
# Load OSM France engineered network (with all features pre-computed)
# ------------------------------------------------------------------------------

if (!exists('osm_france_engineered') || 
    !is.data.frame(get('osm_france_engineered'))) {
  
  pipeline_message(
    text = sprintf("Loading OSM France engineered network from %s", 
                   rel_path(CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  osm_france_engineered <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH, 
    quiet = TRUE)
  
  if (sf::st_crs(osm_france_engineered) != CONFIG$TARGET_CRS) {
    osm_france_engineered <- osm_france_engineered %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  pipeline_message(
    text = sprintf("OSM France engineered network loaded: %s roads", 
                   fmt(nrow(osm_france_engineered))), 
    level = 1, progress = "end", process = "valid")
}

# Crop to test region if in TEST mode
if (IS_TEST_MODE) {
  osm_france_engineered <- crop_to_test_region(osm_france_engineered)
}

# ------------------------------------------------------------------------------
# Load OSM network with Avatar IDs (to filter engineered network)
# ------------------------------------------------------------------------------

if (!exists('full_network_avatar_id') || 
    !is.data.frame(get('full_network_avatar_id'))) {
  
  pipeline_message(
    text = sprintf("Loading OSM network with Avatar IDs from %s", 
                   rel_path(CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  full_network_avatar_id <- sf::st_read(
    dsn = CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH, 
    quiet = TRUE)
  
  if (sf::st_crs(full_network_avatar_id) != CONFIG$TARGET_CRS) {
    full_network_avatar_id <- full_network_avatar_id %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  pipeline_message(
    text = sprintf("OSM network with Avatar IDs loaded: %s roads", 
                   fmt(nrow(full_network_avatar_id))), 
    level = 1, progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# Filter engineered network to Avatar roads only
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Filtering France engineered network to Avatar roads", 
  level = 1, progress = "start", process = "join")

# Coerce to data.table
setDT(osm_france_engineered)
setDT(full_network_avatar_id)

pipeline_message(text = describe_df(osm_france_engineered), process = "info")
pipeline_message(text = describe_df(full_network_avatar_id), process = "info")

# Get Avatar road OSM IDs
avatar_osm_ids <- unique(full_network_avatar_id$osm_id)

# Filter engineered network to Avatar roads
network_clean <- osm_france_engineered[osm_id %in% avatar_osm_ids]

# Get Avatar count point ID mapping (osm_id -> id + lane_number)
id_mapping <- full_network_avatar_id[, .(osm_id, id, lane_number)]
setnames(id_mapping, "id", "count_point_id")

# Add count_point_id and lane_number to network_clean
network_clean <- merge(network_clean, id_mapping, by = "osm_id", all.x = TRUE)

# Select network columns
network_cols <- c("count_point_id", "osm_id", "highway", "DEGRE", "ref_letter", 
                  "first_word", "oneway_osm", "lanes_osm", "lanes_directional",
                  "speed", "junction_osm", "lane_number",
                  "connectivity", "betweenness", "closeness", "pagerank",
                  "coreness", "dead_end_score", "edge_length_m")

available_network_cols <- intersect(network_cols, names(network_clean))
network_clean <- network_clean[, ..available_network_cols]

pipeline_message(
  text = sprintf("Network filtered to Avatar roads: %s roads, %s attributes", 
                 fmt(nrow(network_clean)), ncol(network_clean)), 
  level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Avatar data processing
# ------------------------------------------------------------------------------

pipeline_message(text = "Processing Avatar data", 
                 level = 1, progress = "start", process = "calc")

# avatar_data <- aggregated_measures_with_ratios_df
avatar_data <- readRDS(file = CONFIG$AVATAR_AGGREGATED_FILEPATH)

pipeline_message(text = describe_df(avatar_data), process = "info")
  
setDT(avatar_data)

pipeline_message(
  text = sprintf("Initial observations: %s", fmt(nrow(x = avatar_data))), 
  process = "info")
pipeline_message(
  text = sprintf("Unique count_points: %s", 
                 fmt(length(x = unique(avatar_data$count_point_id)))), 
  process = "info")
pipeline_message(
  text = sprintf("Periods: %s", 
                 paste(unique(avatar_data$period), collapse = ", ")), 
  process = "info")

# ************************ #
# Data quality diagnostics #
# ************************ #

n_initial <- nrow(x = avatar_data)

pipeline_message(text = "Applying Avatar data quality rules", 
                 level = 1, progress = "start", process = "valid")

# Apply quality rules to Avatar data
apply_avatar_quality_rules(avatar_data)

pipeline_message(text = "Avatar data quality rules applied", 
                 level = 1, progress = "end", process = "valid")

# Diagnostic: Check problematic cases
n_truck_exceed <- sum(!is.na(avatar_data$truck_pct) 
                      & avatar_data$truck_pct > 100, 
                      na.rm = TRUE)
n_extreme_ratio_trucks <- sum(!is.na(avatar_data$ratio_flow_trucks) 
                              & avatar_data$ratio_flow_trucks > 3, 
                              na.rm = TRUE)
n_extreme_ratio_truck_pct <- sum(!is.na(avatar_data$ratio_truck_pct) 
                                 & avatar_data$ratio_truck_pct > 3, 
                                 na.rm = TRUE)

# *************************** #
# Filter invalid observations #
# *************************** #

pipeline_message(text = "Cleaning Avatar data", 
                 level = 1, progress = "start", process = "calc")

# Filter criteria (STRICT for targets, PERMISSIVE for optional variables)
avatar_clean <- avatar_data[
  flow_D > 0 &                                                                  # Must have valid period D baseline flow
  !is.na(aggregate_flow) &                                                      # Must have flow data
  aggregate_flow > 0 &                                                          # Must be positive
  aggregate_flow < 20000 &                                                      # Remove extreme outliers (>20k veh/h reasonable for highways)
  (is.na(aggregate_speed) | (aggregate_speed > 0 & aggregate_speed < 200))]     # Speed can be missing
n_removed <- n_initial - nrow(x = avatar_clean)

# ***************** #
# Add period factor #
# ***************** #

period_levels <- c("D", "E", "N", paste0("h", 0:23),
                   paste0("h", 0:23, "_wd"), paste0("h", 0:23, "_we"))
avatar_clean$period <- factor(x = avatar_clean$period, 
                              levels = period_levels)

# Check period distribution
period_dist <- avatar_clean[, .N, by = period][order(period)]

# *************************** #
# Select final avatar columns #
# *************************** #

avatar_cols <- c("count_point_id", "period", 
                "aggregate_flow", "aggregate_flow_trucks", 
                "aggregate_speed", "aggregate_occupancy",
                "flow_D", "flow_trucks_D", "speed_D", 
                "occupancy_D", "truck_pct_D",
                "ratio_flow", "ratio_flow_trucks", 
                "ratio_speed", "ratio_occupancy", 
                "truck_pct", "ratio_truck_pct",
                "perc_flow_predicted", "perc_flow_trucks_predicted", 
                "perc_speed_predicted", "perc_occupancy_predicted",
                "n_hours_with_data", 
                "n_total_observations")

available_avatar_cols <- intersect(x = avatar_cols, 
                                   y = names(avatar_clean))
avatar_clean <- avatar_clean[, ..available_avatar_cols]

pipeline_message(
  text = sprintf("Cleaned up Avatar data: %s observations, %s attributes", 
                 fmt(nrow(x = avatar_clean)), ncol(avatar_clean)), 
  process = "info")

# Save processed avatar data
saveRDS(object = avatar_clean, 
        file = CONFIG$AVATAR_AGGREGATED_CLEAN_FILEPATH)
assign(x = "avatar_clean", 
       value = as.data.frame(avatar_clean), 
       envir = .GlobalEnv)

pipeline_message(text = describe_df(avatar_clean), process = "info")

pipeline_message(
  text = sprintf("Avatar data successfully cleaned up and saved into file %s", 
                rel_path(CONFIG$AVATAR_AGGREGATED_CLEAN_FILEPATH)), 
  level = 1, progress = "end", process = "valid")


# ------------------------------------------------------------------------------
# Merge Avatar data with road network
# ------------------------------------------------------------------------------

pipeline_message(text = "Merging Avatar data with OSM road network", 
                 level = 1, progress = "start", process = "join")

pipeline_message(
  text = "Merging cleaned up Avatar data with cleaned up OSM road network", 
  level = 2, progress = "start", process = "join")

# Merge avatar data with network attributes (inner join)
training_data <- merge(
  x = as.data.frame(avatar_clean), 
  y = as.data.frame(network_clean),
  by = "count_point_id",
  all.x = FALSE,  # Inner join - keep only matched observations
  all.y = FALSE)

# Ratio vitesse réelle / vitesse réglementaire OSM (quand disponible)
training_data$ratio_speed_to_osm <- ifelse(
  !is.na(training_data$aggregate_speed) &
    !is.na(training_data$speed) &
    training_data$speed > 0,
  training_data$aggregate_speed / training_data$speed,
  NA_real_
)

# Final column order
final_cols <- c("osm_id", "count_point_id", "period",
               # Targets
               "aggregate_flow", "aggregate_flow_trucks", 
               "aggregate_speed", "aggregate_occupancy",
               # Baseline values
               "flow_D", "flow_trucks_D", "speed_D", 
               "occupancy_D", "truck_pct_D",
               # Ratios
               "ratio_flow", "ratio_flow_trucks", 
               "ratio_speed", "ratio_occupancy", "ratio_speed_to_osm",
               "truck_pct", "ratio_truck_pct",
               # Network features
               "highway", "DEGRE", "ref_letter", "first_word", 
               "oneway_osm", "lanes_osm", "lanes_directional",
               "speed", "junction_osm", "lane_number",
               "connectivity", "betweenness", "closeness", "pagerank",
               "coreness", "dead_end_score", "edge_length_m",
               # Quality indicators
               "perc_flow_predicted", "perc_flow_trucks_predicted",
               "perc_speed_predicted", "perc_occupancy_predicted",
               "n_hours_with_data", 
               "n_total_observations")
available_final_cols <- intersect(x = final_cols, 
                                  y = names(training_data))
training_data <- training_data[, available_final_cols]

# Training data integrity check
n_na  <- sum(is.na(training_data))

pipeline_message(text = sprintf("Training data integrity check: %s NA detected", 
                                fmt(n_na)), process = "info")

# Save training data
saveRDS(object = training_data, 
        file = CONFIG$TRAINING_RDS_DATA_FILEPATH)

pipeline_message(
  text = sprintf("Training dataset successfully created and saved to %s", 
                 rel_path(CONFIG$TRAINING_RDS_DATA_FILEPATH)),
  level = 2, progress = "end", process = "valid")

# Add geometry from France engineered network
training_data_sf <- merge(
  x = training_data, 
  y = osm_france_engineered[, c("osm_id", "geom")],  
  by = "osm_id")

# Convert to sf object
training_data_sf <- sf::st_as_sf(x = training_data_sf)

# Add QGIS-friendly datetime fields when `period` exists
training_data_sf <- add_period_datetime_columns(training_data_sf)

# Export to GeoPackage file
sf::st_write(
  obj = training_data_sf, 
  dsn = CONFIG$TRAINING_GPKG_DATA_FILEPATH, 
  delete_dsn = TRUE)

pipeline_message(
  text = sprintf("Training dataset saved to %s", 
                 rel_path(CONFIG$TRAINING_GPKG_DATA_FILEPATH)), 
  level = 1, progress = "end", process = "valid")

pipeline_message(text = "Training dataset merge completed", 
                 level = 0, progress = "end", process = "valid")