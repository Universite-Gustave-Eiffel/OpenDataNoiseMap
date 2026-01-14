# ==============================================================================
# STAGE 5: ROAD FEATURE ENGINEERING
# ==============================================================================

# ------------------------------------------------------------------------------
# Load full road network with connectivities and towns
# ------------------------------------------------------------------------------

if (!exists('osm_full_network') 
    && isFALSE(is.data.frame(get('osm_full_network')))){
  
  message("\t 🔗 Loading OSM full road network \n")
  
  start_timer()
  osm_full_network <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH, 
    quiet = TRUE)
  elapsed <- stop_timer()
  
  if (sf::st_crs(osm_full_network) != CONFIG$TARGET_CRS){
    osm_full_network <- osm_full_network %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  message("\t\t ✓ OSM full road network loaded from ", 
          CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH,
          " in ", elapsed, "\n")
}

if (!exists('full_network_avatar_id') 
    && isFALSE(is.data.frame(get('full_network_avatar_id')))){
  
  message("\t 🔗 Loading full road network with cound point data \n")
  
  start_timer()
  full_network_avatar_id <- sf::st_read(
    dsn = CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH, 
    quiet = TRUE)
  elapsed <- stop_timer()
  
  if (sf::st_crs(full_network_avatar_id) != CONFIG$TARGET_CRS){
    full_network_avatar_id <- full_network_avatar_id %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  message("\t\t ✓ Full road network with cound point data loaded from ", 
          CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH,
          " in ", elapsed, "\n")
}

# ------------------------------------------------------------------------------
# Road network processing
# ------------------------------------------------------------------------------

message("\t 🔗 Rule imputation to the road network \n")

# ********************************************** #
# Compute imputation rules from the full network #
# ********************************************** #

message("\t 🚧 Building default road traffic imputation rules for roads without 
        information \n")

# Coerce OSM and Avatar data frame to data.table
setDT(osm_full_network)
setDT(full_network_avatar_id)

# Clean highway types
osm_full_network$highway <- as.character(x = osm_full_network$highway)
osm_full_network$highway[
  is.na(osm_full_network$highway) | 
    osm_full_network$highway == ""] <- "unclassified"
imputation_rules <- full_network_dt[, .(
  median_lanes = median(x = as.numeric(lanes), 
                        na.rm = TRUE),
  median_speed = median(x = as.numeric(maxspeed), 
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
        file = file.path(AVATAR_RDS_DATA_FILEPATH, 
                         "imputation_rules.rds"))

message("\t\t ✓ Default road traffic imputation rules built and saved into 
        file ", CONFIG$AVATAR_RDS_DATA_FILEPATH, "\n")

# ************************************************************** #
# Clean road network and impute filtered network (Avatar subset) #
# ************************************************************** #

message("\t 🔗 Imputing default road traffic rules to the road network \n")

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

message("\t\t 📥 Default road traffic rules assigned to the road network and 
        saved into file", CONFIG$AVATAR_CLEAN_NETWORK_RDS_DATA_FILEPATH, "\n")

# ------------------------------------------------------------------------------
# Avatar data processing
# ------------------------------------------------------------------------------

message("\t 🔁 Processing Avatar data \n")

avatar_data <- aggregated_measures_with_ratios_df
setDT(avatar_data)
cat(sprintf("\t\t 👀 Initial observations: %s\n", 
            fmt(nrow(x = avatar_data))))
cat(sprintf("\t\t 👀 Unique count_points: %s\n", 
            fmt(length(x = unique(avatar_data$count_point_id)))))
cat(sprintf("\t\t 👀 Periods: %s\n\n", 
            paste(unique(avatar_data$period), 
                  collapse = ", ")))

# ************************ #
# Data quality diagnostics #
# ************************ #

n_initial <- nrow(x = avatar_data)
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

# ************************ #
# Fix truck_pct and ratios #
# ************************ #

# Recalculate truck_pct with safeguards
avatar_data[, truck_pct := ifelse(
  test = (!is.na(aggregate_flow) 
          & !is.na(aggregate_flow_trucks) 
          & aggregate_flow > 0), 
  yes = pmin(100, 100 * aggregate_flow_trucks / aggregate_flow),                # Cap at 100%
  no = NA_real_)]
avatar_data[, truck_pct_D := ifelse(
  test = (!is.na(flow_D) & !is.na(flow_trucks_D) & flow_D > 0),
  yes = pmin(100, 100 * flow_trucks_D / flow_D),  # Cap at 100%
  no = NA_real_)]

# Recalculate ratio_truck_pct with safeguards
avatar_data[, ratio_truck_pct := ifelse(
  test = (!is.na(truck_pct) & !is.na(truck_pct_D) & truck_pct_D > 0.1),         # Avoid division by near-zero
  yes = pmin(5.0, truck_pct / truck_pct_D),                                     # Cap ratio at 5x (reasonable max variation)
  no = NA_real_)]

# Fix ratio_flow_trucks (cap at 5x)
avatar_data[, ratio_flow_trucks := ifelse(
  test = (!is.na(ratio_flow_trucks)),
  yes = pmin(5.0, pmax(0.0, ratio_flow_trucks)),                                # Cap between 0-5x
  no = NA_real_)]
n_capped_truck_pct <- sum(!is.na(avatar_data$truck_pct) 
                          & avatar_data$truck_pct == 100)

# *************************** #
# Filter invalid observations #
# *************************** #

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

cat(sprintf("\t\t ✓ Avatar data cleaned: %s observations, %s attributes\n\n", 
            fmt(nrow(x = avatar_clean)), 
            ncol(avatar_clean)))

# Save processed avatar data
saveRDS(object = avatar_clean, 
        file = CONFIG$AVATAR_CLEAN_RDS_DATA_FILEPATH)
assign(x = "avatar_clean", 
       value = as.data.frame(avatar_clean), 
       envir = .GlobalEnv)

message("\t\t ✓ Clean Avatar data saved into file ", 
        CONFIG$AVATAR_CLEAN_RDS_DATA_FILEPATH, "\n")

# ------------------------------------------------------------------------------
# Merge Avatar data with road network
# ------------------------------------------------------------------------------

message("\t 🔗 Merging Avatar data with OSM road network \n")

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

message("\t\t ✓ Avatar data merged with OSM road network and saved into file ", 
        CONFIG$AVATAR_RDS_DATA_FILEPATH, "\n")

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