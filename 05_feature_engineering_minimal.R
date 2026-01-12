# =============================================================================
# STAGE 5: FEATURE ENGINEERING
# =============================================================================

library(data.table)
library(dplyr)



full_network <- st_read( "data/routes_france_osm_complete_including_connectivity_and_communes_for_R.gpkg")
full_network_2154 <- st_transform(full_network, 2154)

full_network <- full_network_2154
filtered_network <- full_network_2154_avatar_id

# -----------------------------------------------------------------------------
# STEP 1.1: Compute imputation rules from FULL network
# -----------------------------------------------------------------------------

setDT(full_network)

# Clean highway types first
full_network$highway <- as.character(full_network$highway)
full_network$highway[is.na(full_network$highway) | full_network$highway == ""] <- "unclassified"

imputation_rules <- full_network_dt[, .(
  median_lanes = median(as.numeric(lanes), na.rm = TRUE),
  median_speed = median(as.numeric(maxspeed), na.rm = TRUE),
  n_roads = .N
), by = highway]

imputation_rules[is.na(median_lanes), median_lanes := 2]
imputation_rules[is.na(median_speed), median_speed := 50]
imputation_rules <- rbind(imputation_rules, 
                         data.table(highway = "missing", 
                                   median_lanes = 2, 
                                   median_speed = 50, 
                                   n_roads = 0))

# Save rules for later use
saveRDS(imputation_rules, "rdsFiles/imputation_rules.rds")

# -----------------------------------------------------------------------------
# STEP 1.2: Clean and impute FILTERED network (avatar subset)
# -----------------------------------------------------------------------------


# Function to process network features
process_network_features <- function(data, rules) {
  setDT(data)
  
  # Highway factor (ordered levels)
  highway_levels <- c("residential", "tertiary", "secondary", "primary", "trunk", "motorway",
                      "tertiary_link", "secondary_link", "primary_link", "trunk_link", 
                      "motorway_link", "unclassified")
  data$highway <- as.character(data$highway)
  data$highway[is.na(data$highway) | data$highway == ""] <- "unclassified"
  data$highway <- factor(data$highway, levels = c(highway_levels, "missing"), ordered = TRUE)
  
  data$DEGRE <- as.numeric(data$DEGRE)
  n_missing_degre <- sum(is.na(data$DEGRE))
  if (n_missing_degre > 0) {
    cat(sprintf("     Imputing %s missing DEGRE values → 1 (urban default)\n", 
               format(n_missing_degre, big.mark=",")))
  }
  data$DEGRE[is.na(data$DEGRE)] <- 1
  
  ref_col <- if("ref_osm" %in% names(data)) "ref_osm" else "ref"
  data$ref_letter <- ifelse(is.na(data[[ref_col]]) | data[[ref_col]] == "", "missing",
                            substr(gsub("^([A-Za-z]).*", "\\1", data[[ref_col]]), 1, 1))
  data$ref_letter[data$ref_letter == ""] <- "missing"
  data$ref_letter <- factor(data$ref_letter)
  
  # Extract first_word from road name
  data$first_word <- ifelse(is.na(data$name) | data$name == "" | data$name == "NA", "missing", {
    # Split by space and take first word, convert to lowercase
    first_words <- sapply(strsplit(as.character(data$name), "\\s+"), function(x) x[1])
    tolower(trimws(first_words))
  })
  # Handle empty strings and NA results
  data$first_word[is.na(data$first_word) | data$first_word == "" | data$first_word == "na"] <- "missing"
  
  # Filter rare first_words: keep only if appears >= 100 times, else set to "missing"
  first_word_counts <- table(data$first_word)
  rare_first_words <- names(first_word_counts[first_word_counts < 100])
  n_rare <- sum(data$first_word %in% rare_first_words)
  if (n_rare > 0) {
    cat(sprintf("     Setting %s rare first_words (<100 occurrences) to 'missing'\n", 
               format(n_rare, big.mark=",")))
  }
  data$first_word[data$first_word %in% rare_first_words] <- "missing"
  data$first_word <- factor(data$first_word)
  
  # Oneway factor (check for _osm suffix)
  oneway_col <- if("oneway_osm" %in% names(data)) "oneway_osm" else "oneway"
  
  if (oneway_col %in% names(data)) {
    data$oneway_osm <- ifelse(is.na(data[[oneway_col]]) | data[[oneway_col]] == "", "missing", as.character(data[[oneway_col]]))
  } else {
    data$oneway_osm <- "missing"
  }
  
  # Handle oneway values: -1 should become "yes" (reverse oneway), others standardized
  data$oneway_osm[data$oneway_osm == "-1"] <- "yes"  # -1 means oneway in reverse direction
  allowed_oneway_values <- c("yes", "no", "missing")
  data$oneway_osm[!data$oneway_osm %in% allowed_oneway_values] <- "yes"  # Default unknown values to "yes"
  data$oneway_osm <- factor(data$oneway_osm, levels = allowed_oneway_values)
  
  # ============================================================================
  # LANE HANDLING: OSM lanes = BOTH directions (for total segment capacity)
  # ============================================================================
  # We predict TOTAL flow on segment (both directions combined)
  # OSM lanes = lanes in both directions (keep as-is)
  # Training data flows are aggregated by osm_id (sum both directions)
  # ============================================================================
  
  # Use lanes_osm column (with _osm suffix)
  lanes_col <- if("lanes_osm" %in% names(data)) "lanes_osm" else "lanes"
  data$lanes_osm <- as.numeric(data[[lanes_col]])
  missing_lanes_idx <- which(is.na(data$lanes_osm) | data$lanes_osm <= 0)
  n_missing_lanes <- length(missing_lanes_idx)
  
  if (n_missing_lanes > 0) {
    cat(sprintf("     Imputing %s missing lane values using highway-specific medians\n", 
               format(n_missing_lanes, big.mark=",")))
    
    # Vectorized imputation using rules
    lanes_lookup <- setNames(rules$median_lanes, rules$highway)
    missing_highways <- as.character(data$highway[missing_lanes_idx])
    imputed_lanes <- lanes_lookup[missing_highways]
    imputed_lanes[is.na(imputed_lanes)] <- 2  # Default 2 lanes
    data$lanes_osm[missing_lanes_idx] <- as.numeric(imputed_lanes)
  }
  
  # Apply intelligent imputation for maxspeed (check for _osm suffix)
  maxspeed_col <- if("maxspeed_osm" %in% names(data)) "maxspeed_osm" else "maxspeed"
  data$speed <- as.numeric(data[[maxspeed_col]])
  missing_speed_idx <- which(is.na(data$speed) | data$speed <= 0)
  n_missing_speed <- length(missing_speed_idx)
  
  if (n_missing_speed > 0) {
    cat(sprintf("     Imputing %s missing speed values using highway-specific medians\n", 
               format(n_missing_speed, big.mark=",")))
    
    # Vectorized imputation using rules
    speed_lookup <- setNames(rules$median_speed, rules$highway)
    missing_highways <- as.character(data$highway[missing_speed_idx])
    imputed_speeds <- speed_lookup[missing_highways]
    imputed_speeds[is.na(imputed_speeds)] <- 50  # Default 50 km/h
    data$speed[missing_speed_idx] <- as.numeric(imputed_speeds)
  }
  
  return(as.data.frame(data))
}

# Apply to filtered network
network_processed <- process_network_features(filtered_network, imputation_rules)

# Select network columns
network_cols <- c("id", "osm_id", "highway", "DEGRE", "ref_letter", "first_word", 
                 "oneway_osm", "lanes_osm", "speed", "connectivity", "betweenness", 
                 "closeness", "pagerank")
available_network_cols <- intersect(network_cols, names(network_processed))
network_clean <- network_processed[, available_network_cols]

cat(sprintf("✅ Network data cleaned: %s roads, %s attributes\n\n", 
           format(nrow(network_clean), big.mark=","),
           ncol(network_clean)))

# Save processed network
saveRDS(network_clean, "rdsFiles/network_clean.rds")
assign("network_clean", network_clean, envir = .GlobalEnv)

# =============================================================================
# PART 2: AVATAR DATA PROCESSING (Traffic measurements)
# =============================================================================

avatar_data <- aggregated_measures_with_ratios_df
setDT(avatar_data)

cat(sprintf("   Initial observations: %s\n", format(nrow(avatar_data), big.mark=",")))
cat(sprintf("   Unique count_points: %s\n", 
           format(length(unique(avatar_data$count_point_id)), big.mark=",")))
cat(sprintf("   Periods: %s\n\n", 
           paste(unique(avatar_data$period), collapse=", ")))

# -----------------------------------------------------------------------------
# STEP 2.1: Data quality diagnostics
# -----------------------------------------------------------------------------

n_initial <- nrow(avatar_data)

# Diagnostic: Check problematic cases
n_truck_exceed <- sum(!is.na(avatar_data$truck_pct) & avatar_data$truck_pct > 100, na.rm = TRUE)
n_extreme_ratio_trucks <- sum(!is.na(avatar_data$ratio_flow_trucks) & avatar_data$ratio_flow_trucks > 3, na.rm = TRUE)
n_extreme_ratio_truck_pct <- sum(!is.na(avatar_data$ratio_truck_pct) & avatar_data$ratio_truck_pct > 3, na.rm = TRUE)

# -----------------------------------------------------------------------------
# STEP 2.2: Fix truck_pct and ratios
# -----------------------------------------------------------------------------

# Recalculate truck_pct with safeguards
avatar_data[, truck_pct := ifelse(
  !is.na(aggregate_flow) & !is.na(aggregate_flow_trucks) & aggregate_flow > 0,
  pmin(100, 100 * aggregate_flow_trucks / aggregate_flow),  # Cap at 100%
  NA_real_
)]

avatar_data[, truck_pct_D := ifelse(
  !is.na(flow_D) & !is.na(flow_trucks_D) & flow_D > 0,
  pmin(100, 100 * flow_trucks_D / flow_D),  # Cap at 100%
  NA_real_
)]

# Recalculate ratio_truck_pct with safeguards
avatar_data[, ratio_truck_pct := ifelse(
  !is.na(truck_pct) & !is.na(truck_pct_D) & truck_pct_D > 0.1,  # Avoid division by near-zero
  pmin(5.0, truck_pct / truck_pct_D),  # Cap ratio at 5x (reasonable max variation)
  NA_real_
)]

# Fix ratio_flow_trucks (cap at 5x)
avatar_data[, ratio_flow_trucks := ifelse(
  !is.na(ratio_flow_trucks),
  pmin(5.0, pmax(0.0, ratio_flow_trucks)),  # Cap between 0-5x
  NA_real_
)]

n_capped_truck_pct <- sum(!is.na(avatar_data$truck_pct) & avatar_data$truck_pct == 100)

# -----------------------------------------------------------------------------
# STEP 2.3: Filter invalid observations
# -----------------------------------------------------------------------------


# Filter criteria (STRICT for targets, PERMISSIVE for optional variables)
avatar_clean <- avatar_data[
  # REQUIRED: Must have valid period D baseline
  !is.na(flow_D) &                   # Must have baseline flow
  flow_D > 0 &
  
  # REQUIRED: Must have valid current period flow
  !is.na(aggregate_flow) &           # Must have flow data
  aggregate_flow > 0 &               # Must be positive
  aggregate_flow < 20000 &           # Remove extreme outliers (>20k veh/h reasonable for highways)
  
  # OPTIONAL: Speed can be missing (we'll impute later if needed)
  # Don't filter on aggregate_speed here - keep observations even if speed is NA
  (is.na(aggregate_speed) | (aggregate_speed > 0 & aggregate_speed < 200))
]

n_removed <- n_initial - nrow(avatar_clean)

# -----------------------------------------------------------------------------
# STEP 2.4: Add period factor
# -----------------------------------------------------------------------------


period_levels <- c("D", "E", "N", paste0("h", 0:23))
avatar_clean$period <- factor(avatar_clean$period, levels = period_levels)

# Check period distribution
period_dist <- avatar_clean[, .N, by = period][order(period)]

# -----------------------------------------------------------------------------
# STEP 2.5: Select final avatar columns
# -----------------------------------------------------------------------------


avatar_cols <- c("count_point_id", "period", 
                "aggregate_flow", "aggregate_flow_trucks", "aggregate_speed", "aggregate_occupancy",
                "flow_D", "flow_trucks_D", "speed_D", "occupancy_D", "truck_pct_D",
                "ratio_flow", "ratio_flow_trucks", "ratio_speed", "ratio_occupancy", 
                "truck_pct", "ratio_truck_pct",
                "perc_flow_predicted", "perc_flow_trucks_predicted", 
                "perc_speed_predicted", "perc_occupancy_predicted",
                "n_directions_measured", "n_hours_with_data", "n_total_observations")

available_avatar_cols <- intersect(avatar_cols, names(avatar_clean))
avatar_clean <- avatar_clean[, ..available_avatar_cols]

cat(sprintf("✅ Avatar data cleaned: %s observations, %s attributes\n\n", 
           format(nrow(avatar_clean), big.mark=","),
           ncol(avatar_clean)))

# Save processed avatar data
saveRDS(avatar_clean, "rdsFiles/avatar_clean.rds")
assign("avatar_clean", as.data.frame(avatar_clean), envir = .GlobalEnv)

# =============================================================================
# PART 3: MERGE NETWORK + AVATAR DATA
# =============================================================================


# Merge avatar data with network attributes
# Keep only avatar observations that have matching network data
training_data <- merge(
  as.data.frame(avatar_clean), 
  network_clean,
  by.x = "count_point_id", 
  by.y = "id",
  all.x = FALSE,  # Inner join - keep only matched observations
  all.y = FALSE
)

# Final column order
final_cols <- c("osm_id", "count_point_id", "period",
               # Targets
               "aggregate_flow", "aggregate_flow_trucks", "aggregate_speed", "aggregate_occupancy",
               # Baseline values
               "flow_D", "flow_trucks_D", "speed_D", "occupancy_D", "truck_pct_D",
               # Ratios
               "ratio_flow", "ratio_flow_trucks", "ratio_speed", "ratio_occupancy",
               "truck_pct", "ratio_truck_pct",
               # Network features
               "highway", "DEGRE", "ref_letter", "first_word", "oneway_osm",
               "lanes_osm", "speed", "connectivity", "betweenness", "closeness", "pagerank",
               # Quality indicators
               "perc_flow_predicted", "perc_flow_trucks_predicted",
               "perc_speed_predicted", "perc_occupancy_predicted",
               "n_directions_measured", "n_hours_with_data", "n_total_observations")

available_final_cols <- intersect(final_cols, names(training_data))
training_data <- training_data[, available_final_cols]
saveRDS(training_data, "data/training_data.rds")

# =============================================================================
# EXPORT: GeoPackage with period as milliseconds
# =============================================================================


# Convert period to milliseconds (integer)
# h0 = 0, h1 = 3600000, h2 = 7200000, ..., h23 = 82800000
# D = 90000000, E = 93600000, N = 97200000
training_data_export <- training_data

training_data_export$period_ms <- sapply(training_data_export$period, function(p) {
  if (grepl("^h", p)) {
    # Extract hour number: h0 -> 0, h1 -> 1, etc.
    hour <- as.integer(sub("^h", "", p))
    return(as.integer(hour * 3600000))
  } else if (p == "D") {
    return(as.integer(25 * 3600000))
  } else if (p == "E") {
    return(as.integer(26 * 3600000))
  } else if (p == "N") {
    return(as.integer(27 * 3600000))
  } else {
    return(NA_integer_)
  }
})

# Add geometry from network data
training_data_sf <- merge(training_data_export, full_network_2154,  by = "osm_id")

# Convert to sf object
training_data_sf <- st_as_sf(training_data_sf)

# Export to GeoPackage
output_path <- "output/training_data_with_period_ms.gpkg"
st_write(training_data_sf, output_path, delete_dsn = TRUE)


