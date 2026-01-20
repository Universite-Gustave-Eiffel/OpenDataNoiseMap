# ==============================================================================
# STAGE 5: ROAD FEATURE ENGINEERING
# ==============================================================================

pipeline_message(text = "Road feature engineering processing", 
                 level = 0, progress = "start", process = "calc")

# ------------------------------------------------------------------------------
# Load full road network with connectivities and towns
# ------------------------------------------------------------------------------

if (!exists('osm_full_network') 
    && isFALSE(is.data.frame(get('osm_full_network')))){
  
  pipeline_message(
    text = paste0("Loading OSM full road network from ", 
                  rel_path(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  osm_full_network <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH, 
    quiet = TRUE)
  elapsed <- stop_timer()
  
  if (sf::st_crs(osm_full_network) != CONFIG$TARGET_CRS){
    osm_full_network <- osm_full_network %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  pipeline_message(text = "OSM full road network successfully loaded", 
                   level = 1, progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# Load full road network with Avatar count point indexes
# ------------------------------------------------------------------------------

if (!exists('full_network_avatar_id') 
    && isFALSE(is.data.frame(get('full_network_avatar_id')))){
  
  pipeline_message(
    text = paste0("Loading full road network with count point data from ", 
                  rel_path(CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  full_network_avatar_id <- sf::st_read(
    dsn = CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH, 
    quiet = TRUE)
  
  if (sf::st_crs(full_network_avatar_id) != CONFIG$TARGET_CRS){
    full_network_avatar_id <- full_network_avatar_id %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  pipeline_message(
    text = "Full road network with count point data successfully loaded", 
    level = 1, progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# Road network processing
# ------------------------------------------------------------------------------

pipeline_message(text = "Assignment of rules to the road network", 
                 level = 1, progress = "start", process = "join")

# ********************************************** #
# Compute imputation rules from the full network #
# ********************************************** #

pipeline_message(
  text = "Building of default allocation rules for roads without information", 
  level = 2, progress = "start", process = "build")

# Coerce OSM and Avatar data frame to data.table
setDT(osm_full_network)
setDT(full_network_avatar_id)

# Clean highway types
osm_full_network$highway <- as.character(x = osm_full_network$highway)
osm_full_network$highway[
  is.na(osm_full_network$highway) | 
    osm_full_network$highway == ""] <- "unclassified"

# Imputation rules
lanes_col <- if ("lanes_osm" %in% names(osm_full_network)){
  "lanes_osm"
} else {
  "lanes"
}
maxspeed_col <- if ("maxspeed_osm" %in% names(osm_full_network)){
  "maxspeed_osm"
} else {
    "maxspeed"
}

imputation_rules <- osm_full_network[, .(
  median_lanes = median(x = as.numeric(lanes_col), 
                        na.rm = TRUE),
  median_speed = median(x = as.numeric(maxspeed_col), 
                        na.rm = TRUE),
  n_roads = .N), 
  by = highway]

# Default values for imputation rules where information is missing
imputation_rules[is.na(median_lanes), 
                 median_lanes := CONFIG$DEFAULT_NUMBER_OF_LANES]
imputation_rules[is.na(median_speed), 
                 median_speed := CONFIG$DEFAULT_VEHICLE_SPEED]
imputation_rules <- rbind(
  imputation_rules, 
  data.table(highway = "missing", 
             median_lanes = CONFIG$DEFAULT_NUMBER_OF_LANES, 
             median_speed = CONFIG$DEFAULT_VEHICLE_SPEED, 
             n_roads = CONFIG$DEFAULT_NUMBER_OF_ROADS))

# Save rules for later use
saveRDS(object = imputation_rules, 
        file = CONFIG$AVATAR_IMPUTATION_RULES_FILEPATH)

pipeline_message(
  text = sprintf("Default road traffic imputation rules successfully built and 
                saved into file ", 
                 rel_path(CONFIG$AVATAR_IMPUTATION_RULES_FILEPATH)), 
  level = 2, progress = "end", process = "save")

# ************************************************************** #
# Clean road network and impute filtered network (Avatar subset) #
# ************************************************************** #

pipeline_message(
  text = "Imputing default road traffic rules to the road network", 
  level = 2, progress = "start", process = "join")

# Apply to filtered network
network_processed <- process_network_features(data = full_network_avatar_id, 
                                              rules = imputation_rules)
# Select network columns
network_cols <- c("id", "osm_id", "highway", "DEGRE", "ref_letter", 
                  "first_word", "oneway_osm", "lanes_osm", "speed", 
                  "connectivity", "betweenness", "closeness", "pagerank")

# Define common columns
available_network_cols <- intersect(x= network_cols, 
                                    y=  names(network_processed))

# Clean road network data
network_clean <- network_processed[, available_network_cols]

cat(sprintf("\t\t ✓ Network data cleaned: %s roads, %s attributes\n\n", 
            fmt(nrow(x = network_clean)), 
            ncol(network_clean)))

# Save processed network
saveRDS(object = network_clean, 
        file = CONFIG$AVATAR_CLEAN_NETWORK_RDS_DATA_FILEPATH)
assign(x = "network_clean", 
       value = network_clean, 
       envir = .GlobalEnv)

pipeline_message(
  text = sprintf("Default road traffic rules assigned to the road network and 
                  saved into file ", 
                 rel_path(CONFIG$AVATAR_CLEAN_NETWORK_RDS_DATA_FILEPATH)), 
  level = 2, progress = "end", process = "save")

pipeline_message(text = "Rules assigned to the road network", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Avatar data processing
# ------------------------------------------------------------------------------

pipeline_message(text = "Processing Avatar data", 
                 level = 1, progress = "start", process = "calc")

# avatar_data <- aggregated_measures_with_ratios_df
avatar_data <- readRDS(file = CONFIG$AVATAR_AGGREGATED_FILEPATH)

pipeline_message(text = describe_df(avatar_data), level = 1, progress = "end")
  
setDT(avatar_data)

pipeline_message(
  text = sprintf("Initial observations: %s\n", fmt(nrow(x = avatar_data))), 
  level = 4, process = "info")
pipeline_message(
  text = sprintf("Unique count_points: %s\n", 
                 fmt(length(x = unique(avatar_data$count_point_id)))), 
  level = 4, process = "info")
pipeline_message(text = sprintf("Periods: %s\n\n", 
                                paste(unique(avatar_data$period), 
                                      collapse = ", ")), 
                 level = 4, process = "info")

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

period_levels <- c("D", "E", "N", paste0("h", 0:23))
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
                "n_directions_measured", "n_hours_with_data", 
                "n_total_observations")

available_avatar_cols <- intersect(x = avatar_cols, 
                                   y = names(avatar_clean))
avatar_clean <- avatar_clean[, ..available_avatar_cols]

pipeline_message(
  text = sprintf("Cleaned up Avatar data: %s observations, %s attributes\n\n", 
                 fmt(nrow(x = avatar_clean)), ncol(avatar_clean)), 
  level = 4, process = "info")

# Save processed avatar data
saveRDS(object = avatar_clean, 
        file = CONFIG$AVATAR_AGGREGATED_CLEAN_FILEPATH)
assign(x = "avatar_clean", 
       value = as.data.frame(avatar_clean), 
       envir = .GlobalEnv)

pipeline_message(
  text = sprintf("Avatar data successfully cleaned up and saved into file ", 
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

# Merge avatar data with network attributes (keep only avatar observations that 
# have matched network data)
training_data <- merge(
  x = as.data.frame(avatar_clean), 
  y = network_clean,
  by.x = "count_point_id", 
  by.y = "id",
  all.x = FALSE,                                                                # Inner join - keep only matched observations
  all.y = FALSE)

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
               "ratio_speed", "ratio_occupancy",
               "truck_pct", "ratio_truck_pct",
               # Network features
               "highway", "DEGRE", "ref_letter", "first_word", 
               "oneway_osm", "lanes_osm", "speed", 
               "connectivity", "betweenness", "closeness", "pagerank",
               # Quality indicators
               "perc_flow_predicted", "perc_flow_trucks_predicted",
               "perc_speed_predicted", "perc_occupancy_predicted",
               "n_directions_measured", "n_hours_with_data", 
               "n_total_observations")
available_final_cols <- intersect(x = final_cols, 
                                  y = names(training_data))
training_data <- training_data[, available_final_cols]

# Save training data
saveRDS(object = training_data, 
        file = CONFIG$TRAINING_RDS_DATA_FILEPATH)

pipeline_message(
  text = sprintf("Cleaned up Avatar data and OSM road network successfully 
                 merged and saved into file ", 
                 rel_path(CONFIG$AVATAR_RDS_DATA_FILEPATH)),
  level = 2, progress = "end", process = "valid")

# Export merged data to GeoPackage files with periods as milliseconds (integers)
# h0 = 0, h1 = 3600000, h2 = 7200000, ..., h23 = 82800000
# D = 90000000, E = 93600000, N = 97200000
training_data_export <- training_data
training_data_export$period_ms <- sapply(X = training_data_export$period, 
                                         FUN = function(p) {
  if (grepl(pattern = "^h", x = p)) {
    # Extract hour number: h0 -> 0, h1 -> 1, etc.
    hour <- as.integer(x = sub(pattern = "^h", 
                               replacement = "", 
                               x = p))
    return(as.integer(x = hour * 3600000))
  } else if (p == "D") {
    return(as.integer(x = 25 * 3600000))
  } else if (p == "E") {
    return(as.integer(x = 26 * 3600000))
  } else if (p == "N") {
    return(as.integer(x = 27 * 3600000))
  } else {
    return(NA_integer_)
  }
})

# Add geometry from network data
training_data_sf <- merge(x = training_data_export, 
                          y = osm_full_network,  
                          by = "osm_id")

message("\t\t ✓ Avatar data merged with OSM road network data successfully \n ")

# Convert to sf object
training_data_sf <- sf::st_as_sf(x = training_data_sf)

# Export to GeoPackage file
sf::st_write(obj = training_data_sf, 
             dsn = CONFIG$TRAINING_GPKG_DATA_FILEPATH, 
             delete_dsn = TRUE)

message("\t\t ✓ Merged Avatar data with OSM road network saved into file ", 
        CONFIG$TRAINING_GPKG_DATA_FILEPATH, "\n")

pipeline_message(text = "Avatar data successfully merged with OSM road network", 
                 level = 1, progress = "end", process = "valid")

pipeline_message(text = "Road feature engineering successfully processed", 
                 level = 0, progress = "end", process = "valid")