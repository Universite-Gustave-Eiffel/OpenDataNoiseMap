# =============================================================================
# STAGE 7D: PREDICTIONS AROUND NOISE SENSORS (800M RADIUS)
# =============================================================================
# Predict hourly traffic flow, speed, and truck % for roads within 800m of sensors
# Output: Long format with 'period' column (D, E, N, h0-h23)
# =============================================================================

if (!file.exists(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)) {
  pipeline_message(
    text = sprintf("Models not found! File %s not found. Please run 
                   R/06d_xgboost_training_with_ratios.R first", 
                   CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH), 
    process = "stop")
}

models_list <- readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
feature_info <- readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)

feature_formula <- feature_info$feature_formula
all_periods <- feature_info$all_periods

# Models loaded successfully (81 total: 3 base + 78 ratios)
# Feature formula and periods extracted from training

# =============================================================================
# LOAD NOISE SENSORS AND FILTER ROADS WITHIN 800M RADIUS
# =============================================================================


# Load BRUITPARIF and ACOUCITE sensors
sensors_bruitparif <- st_read("data/POINT_NOISE_BRUITPARIF_COMPARE.shp", quiet = TRUE)
sensors_acoucite <- st_read("data/POINT_NOISE_ACOUCITE_COMPARE.shp", quiet = TRUE)

# Transform to Lambert93
sensors_bruitparif <- st_transform(sensors_bruitparif, 2154)
sensors_acoucite <- st_transform(sensors_acoucite, 2154)

# ===== TEST MODE: ONLY ACOUCITE =====
cat("⚠️  TEST MODE: Using ONLY ACOUCITE sensors\n")
sensors_bruitparif <- sensors_bruitparif[0, ]  # Empty dataframe (skip BRUITPARIF)

# Keep only geometry and source column (simplified structure for all sensors)
sensors_bruitparif_simple <- st_geometry(sensors_bruitparif) %>% st_sf() %>% mutate(source = "BRUITPARIF")
sensors_acoucite_simple <- st_geometry(sensors_acoucite) %>% st_sf() %>% mutate(source = "ACOUCITE")

# Store original sensors for individual exports
sensors_bruitparif_full <- sensors_bruitparif
sensors_acoucite_full <- sensors_acoucite

# Load CHILD sensors from 11 locations
child_sensor_files <- list(
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

# Load and store all CHILD sensors (full versions for individual exports)
# ===== TEST MODE: SKIP CHILD SENSORS =====
child_sensors_list <- list()
# for (source_name in names(child_sensor_files)) {
#   file_path <- child_sensor_files[[source_name]]
#   if (file.exists(file_path)) {
#     sensor_data <- st_read(file_path, quiet = TRUE)
#     sensor_data <- st_transform(sensor_data, 2154)
#     # Store full version
#     child_sensors_list[[source_name]] <- sensor_data
#   } else {
#     warning(sprintf("%s: file not found", source_name))
#   }
# }

# Create simplified versions for combining (only geometry + source)
child_sensors_simple <- list()  # Empty list in test mode
# child_sensors_simple <- lapply(names(child_sensors_list), function(source_name) {
#   st_geometry(child_sensors_list[[source_name]]) %>% st_sf() %>% mutate(source = source_name)
# })

# Combine all sensors (simplified structure: geometry + source only)
# ===== TEST MODE: ONLY ACOUCITE =====
all_sensors <- sensors_acoucite_simple  # Skip BRUITPARIF and CHILD

# Total sensors loaded: BRUITPARIF (31) + ACOUCITE (17) + CHILD (11 sources)

# Create 800m buffer around each sensor
buffer_radius <- 800  # meters
sensors_buffer <- st_buffer(all_sensors, dist = buffer_radius)

# Create union of all buffers (to avoid duplicates)
sensors_union <- st_union(sensors_buffer)

# 1300m buffers created around all sensors

# Load road network
full_network <- st_read("data/routes_france_osm_complete_including_connectivity_and_communes_for_R.gpkg")
full_network_2154 <- st_transform(full_network, 2154)

# Filter roads within sensor buffers
roads_near_sensors <- st_intersection(full_network_2154, sensors_union)

# Roads extracted within 1300m radius of all sensors

# Convert to data.table for processing (similar to step 5)
sensor_roads_dt <- data.table(st_drop_geometry(roads_near_sensors))

# Load imputation rules from training
if (file.exists("rdsFiles/imputation_rules.rds")) {
  imputation_rules <- readRDS("rdsFiles/imputation_rules.rds")
} else {
  # Basic imputation rules
  imputation_rules <- sensor_roads_dt[!is.na(highway), .(
    median_lanes = median(as.numeric(lanes), na.rm = TRUE),
    median_speed = median(as.numeric(maxspeed), na.rm = TRUE),
    n_roads = .N
  ), by = highway]
  imputation_rules[is.na(median_lanes), median_lanes := 2]  # Default
  imputation_rules[is.na(median_speed), median_speed := 50]  # Default
}

# STEP 1: Highway types (ordered factor)
# Must match training levels exactly (including links)
highway_levels <- c("residential", "tertiary", "secondary", "primary", "trunk", "motorway",
                   "tertiary_link", "secondary_link", "primary_link", "trunk_link", 
                   "motorway_link", "unclassified")
sensor_roads_dt[, highway := as.character(highway)]
sensor_roads_dt[is.na(highway) | highway == "", highway := "unclassified"]
sensor_roads_dt[, highway := factor(highway, levels = c(highway_levels, "missing"), ordered = TRUE)]

# Highway factor created with ordered levels (residential → motorway)

sensor_roads_dt[, DEGRE := as.numeric(as.character(DEGRE))]
sensor_roads_dt[is.na(DEGRE), DEGRE := 1]  # Urban default

# STEP 3: ref_letter (first letter of route reference)
# Auto-detect column name (ref, ref_osm, or create missing)
ref_col <- NA
for (col in c("ref", "ref_osm")) {
  if (col %in% names(sensor_roads_dt)) {
    ref_col <- col
    break
  }
}

if (!is.na(ref_col)) {
  # Use regex to extract letter only (match training logic)
  # Training: substr(gsub("^([A-Za-z]).*", "\\1", data[[ref_col]]), 1, 1)
  sensor_roads_dt[, ref_letter := ifelse(
    !is.na(get(ref_col)) & get(ref_col) != "",
    substr(gsub("^([A-Za-z]).*", "\\1", get(ref_col)), 1, 1),
    "missing"
  )]
  # If result is not a letter (e.g. was a number), set to missing
  sensor_roads_dt[!grepl("^[A-Za-z]$", ref_letter), ref_letter := "missing"]
} else {
  sensor_roads_dt[, ref_letter := "missing"]
}
sensor_roads_dt[, ref_letter := factor(ref_letter)]

# STEP 4: first_word (first word of road name)
if ("name" %in% names(sensor_roads_dt)) {
  sensor_roads_dt[, first_word := ifelse(
    !is.na(name) & name != "",
    tolower(trimws(gsub("\\s+.*", "", name))),
    "missing"
  )]
  # Keep only common words, others → "missing" (same logic as training)
  word_counts <- table(sensor_roads_dt$first_word)
  rare_words <- names(word_counts[word_counts < 5])  # Conservative threshold
  sensor_roads_dt[first_word %in% rare_words, first_word := "missing"]
} else {
  sensor_roads_dt[, first_word := "missing"]
}
sensor_roads_dt[, first_word := factor(first_word)]

# STEP 5: oneway_osm (standardized)
oneway_col <- NA
for (col in c("oneway", "oneway_osm")) {
  if (col %in% names(sensor_roads_dt)) {
    oneway_col <- col
    break
  }
}

if (!is.na(oneway_col)) {
  # Match training logic: -1 -> yes, others -> yes if unknown
  # Training: data$oneway_osm[data$oneway_osm == "-1"] <- "yes"
  # Training: data$oneway_osm[!data$oneway_osm %in% allowed_oneway_values] <- "yes"
  
  val <- as.character(sensor_roads_dt[[oneway_col]])
  val[is.na(val) | val == ""] <- "missing"
  val[val == "-1"] <- "yes"
  
  # Standardize
  val[val %in% c("true", "1")] <- "yes"
  val[val %in% c("false", "0")] <- "no"
  
  # Default unknown to "yes" (as in training)
  val[!val %in% c("yes", "no", "missing")] <- "yes"
  
  sensor_roads_dt[, oneway_osm := val]
} else {
  sensor_roads_dt[, oneway_osm := "missing"]
}
sensor_roads_dt[, oneway_osm := factor(oneway_osm, levels = c("yes", "no", "missing"))]

# STEP 6: lanes_osm (imputed by highway type)
lanes_col <- NA
for (col in c("lanes", "lanes_osm")) {
  if (col %in% names(sensor_roads_dt)) {
    lanes_col <- col
    break
  }
}

if (!is.na(lanes_col)) {
  sensor_roads_dt[, lanes_osm := as.numeric(get(lanes_col))]
} else {
  sensor_roads_dt[, lanes_osm := NA_real_]
}

# Impute missing lanes by highway type
sensor_roads_dt <- merge(sensor_roads_dt, imputation_rules[, .(highway, median_lanes)], 
                  by = "highway", all.x = TRUE)
sensor_roads_dt[is.na(lanes_osm), lanes_osm := median_lanes]
sensor_roads_dt[is.na(lanes_osm), lanes_osm := 2]  # Final fallback
sensor_roads_dt[, median_lanes := NULL]

# STEP 7: speed (maxspeed, imputed by highway type)  
speed_col <- NA
for (col in c("maxspeed", "maxspeed_osm", "speed")) {
  if (col %in% names(sensor_roads_dt)) {
    speed_col <- col
    break
  }
}

if (!is.na(speed_col)) {
  sensor_roads_dt[, speed := as.numeric(get(speed_col))]
} else {
  sensor_roads_dt[, speed := NA_real_]
}

# Impute missing speed by highway type
sensor_roads_dt <- merge(sensor_roads_dt, imputation_rules[, .(highway, median_speed)], 
                  by = "highway", all.x = TRUE)
sensor_roads_dt[is.na(speed), speed := median_speed]
sensor_roads_dt[is.na(speed), speed := 50]  # Final fallback
sensor_roads_dt[, median_speed := NULL]


# Verify we have the main columns (without suffixes)
for (col in c("connectivity", "betweenness", "closeness", "pagerank")) {
  if (!col %in% names(sensor_roads_dt)) {
    stop(sprintf("ERROR: %s missing from network features", col))
  }
}

# STEP 9: Impute remaining NAs in network features (isolated road segments)
network_features <- c("connectivity", "betweenness", "closeness", "pagerank")
for (feat in network_features) {
  na_count <- sum(is.na(sensor_roads_dt[[feat]]))
  if (na_count > 0) {
    # Use 0 for isolated segments (they have no network connectivity)
    sensor_roads_dt[is.na(get(feat)), (feat) := 0]
  }
}

# Feature engineering complete



# =============================================================================
# GENERATE PREDICTIONS (LONG FORMAT WITH PERIOD COLUMN)
# =============================================================================


# Identify actual feature columns (handle .x/.y suffixes from joins)
available_cols <- names(sensor_roads_dt)

# Complete feature list with actual column names
feature_vars <- c("highway", "DEGRE", "ref_letter", "first_word", "oneway_osm", 
                  "lanes_osm", "speed", "connectivity", "betweenness", "closeness", "pagerank")

all_features <- feature_vars
feature_formula <- as.formula(paste("~", paste(all_features, collapse = " + ")))

# Highway: Ordered factor (matches training)
sensor_roads_dt[, highway := factor(highway, levels = c(highway_levels, "missing"), ordered = TRUE)]
contrasts(sensor_roads_dt$highway) <- contr.poly(length(levels(sensor_roads_dt$highway)))

# DEGRE: Numeric (matches training)
sensor_roads_dt[, DEGRE := as.numeric(as.character(DEGRE))]

# Other factors: Unordered
sensor_roads_dt[, ref_letter := factor(ref_letter)]
sensor_roads_dt[, first_word := factor(first_word)]
sensor_roads_dt[, oneway_osm := factor(oneway_osm, levels = c("yes", "no", "missing"))]

# 5. Generate matrix on combined data
pred_matrix <- sparse.model.matrix(feature_formula, data = sensor_roads_dt)

# Prediction matrix generated with polynomial contrasts for ordered factors

# Verify dimensions match (should be equal after NA imputation)
if (nrow(pred_matrix) != nrow(sensor_roads_dt)) {
  stop(sprintf("Dimension mismatch: matrix has %d rows but data has %d rows - check for remaining NAs", 
              nrow(pred_matrix), nrow(sensor_roads_dt)))
}

# Data and matrix aligned
saveRDS(sensor_roads_dt, CONFIG$SENSOR_FORECAST_FILEPATH)

# Initialize results list for long format
all_results <- list()

# =============================================================================
# BASE PREDICTIONS (Period D)
# =============================================================================


osm_id_col <- "osm_id"

base_predictions <- data.frame(
  osm_id = sensor_roads_dt[[osm_id_col]],
  highway = as.character(sensor_roads_dt$highway),
  name = ifelse("name" %in% names(sensor_roads_dt), as.character(sensor_roads_dt$name), "")
)

base_targets <- c("flow_D", "truck_pct_D", "speed_D")

for (target in base_targets) {
  if (target %in% names(models_list)) {
    model <- models_list[[target]]$model
    config <- models_list[[target]]$config
    training_features <- models_list[[target]]$feature_names
    
    # ROBUST FEATURE ALIGNMENT
    # Check which features are available
    available_features <- colnames(pred_matrix)
    missing_features <- setdiff(training_features, available_features)
    extra_features <- setdiff(available_features, training_features)
    
    if (length(missing_features) > 0) {
      # Add missing features as zeros
      missing_matrix <- Matrix(0, 
                              nrow = nrow(pred_matrix), 
                              ncol = length(missing_features),
                              dimnames = list(NULL, missing_features))
      pred_matrix_extended <- cbind(pred_matrix, missing_matrix)
    } else {
      pred_matrix_extended <- pred_matrix
    }
    
    # Select and reorder features to match training exactly
    pred_matrix_aligned <- pred_matrix_extended[, training_features, drop = FALSE]
    
    # Verify alignment
    if (ncol(pred_matrix_aligned) != length(training_features)) {
      stop("Feature alignment failed")
    }
    
    # Predict
    predictions <- predict(model, pred_matrix_aligned)
    
    # Back-transform if log10
    if (!is.null(config$transform) && config$transform == "log10") {
      predictions <- 10^predictions
    }
    
    # Store base prediction
    target_clean <- gsub("_D$", "", target)
    base_predictions[[target_clean]] <- round(predictions, 2)
  }
}

# Add D period data with vehicle breakdown
base_predictions$period <- "D"
base_predictions$TV <- base_predictions$flow
base_predictions$HGV <- round(base_predictions$flow * (base_predictions$truck_pct / 100))
base_predictions$LV <- base_predictions$TV - base_predictions$HGV

# Store D period in results
all_results[["D"]] <- base_predictions[, c("osm_id", "highway", "name", "period", 
                                          "TV", "HGV", "LV", "speed")]

# =============================================================================
# RATIO PREDICTIONS (Other Periods)  
# =============================================================================


ratio_periods <- setdiff(all_periods, "D")

for (period in ratio_periods) {
  # Initialize period data structure (using aligned data to match prediction matrix!)
  period_data <- data.frame(
    osm_id = base_predictions$osm_id,     # Use already aligned osm_ids (1888 rows)
    highway = base_predictions$highway,   # Use already aligned highway data
    name = base_predictions$name,         # Use already aligned name data
    period = period
  )
  
  # For each target, predict ratio and calculate final value
  for (target in c("flow", "truck_pct", "speed")) {
    ratio_model_name <- paste0("ratio_", target, "_", period)
    
    if (ratio_model_name %in% names(models_list) && target %in% names(base_predictions)) {
      model <- models_list[[ratio_model_name]]$model
      training_features <- models_list[[ratio_model_name]]$feature_names
      baseline_values <- base_predictions[[target]]
      
      # ROBUST FEATURE ALIGNMENT FOR RATIOS
      available_features <- colnames(pred_matrix)
      missing_features <- setdiff(training_features, available_features)
      
      if (length(missing_features) > 0) {
        # Add missing features as zeros
        missing_matrix <- Matrix(0, 
                                nrow = nrow(pred_matrix), 
                                ncol = length(missing_features),
                                dimnames = list(NULL, missing_features))
        pred_matrix_full <- cbind(pred_matrix, missing_matrix)
      } else {
        pred_matrix_full <- pred_matrix
      }
      
      # Align and predict ratio
      pred_matrix_aligned <- pred_matrix_full[, training_features, drop = FALSE]
      ratio_pred <- predict(model, pred_matrix_aligned)
      
      # Final value = baseline × ratio
      period_data[[target]] <- round(baseline_values * ratio_pred, 2)
    } else {
      # No ratio model, use baseline
      if (target %in% names(base_predictions)) {
        period_data[[target]] <- base_predictions[[target]]
      }
    }
  }
  
  # Calculate vehicle breakdown
  period_data$TV <- period_data$flow
  period_data$HGV <- round(period_data$flow * (period_data$truck_pct / 100))
  period_data$LV <- period_data$TV - period_data$HGV
  
  # Store period results
  all_results[[period]] <- period_data[, c("osm_id", "highway", "name", "period",
                                          "TV", "HGV", "LV", "speed")]
}

# All ratio periods processed

# =============================================================================
# COMBINE INTO LONG FORMAT AND EXPORT
# =============================================================================


# Combine all periods
final_results <- do.call(rbind, all_results)

# Transform to wide format with D/E/N suffixes
# Filter only D, E, N periods
final_results_den <- final_results %>% filter(period %in% c("D", "E", "N"))

# Pivot to wide format (including speed)
library(tidyr)
final_results_wide <- final_results_den %>%
  select(osm_id, highway, name, period, TV, HGV, LV, speed) %>%
  pivot_wider(
    names_from = period,
    values_from = c(LV, HGV, TV, speed),
    names_glue = "{.value}_{period}"
  )

# Combine and add PVMT column
final_results_export <- final_results_wide %>%
  mutate(PVMT = "FR2N") %>%
  select(OSM_ID = osm_id, PVMT, 
         LV_SPD_D = speed_D, LV_SPD_E = speed_E, LV_SPD_N = speed_N,
         HGV_SPD_D = speed_D, HGV_SPD_E = speed_E, HGV_SPD_N = speed_N,
         LV_D, LV_E, LV_N,
         HGV_D, HGV_E, HGV_N)

# Add geometry from network data
final_results_sf <- merge(final_results_export, full_network_2154,  by.x = "OSM_ID", by.y = "osm_id")

# Convert to sf object
final_results_sf <- st_as_sf(final_results_sf)

# Export full dataset to shapefile (all sensors combined)
output_shp <- "output/final_results_ALL_SENSORS_sf_with_period_ms.shp"
st_write(final_results_sf, output_shp, delete_dsn = TRUE, quiet = TRUE)
# All sensors combined shapefile exported

# Now create separate exports for each sensor source (13 total)

# Function to export predictions for a specific sensor source
export_sensor_predictions <- function(sensor_data, source_name, full_network, predictions, buffer_dist) {
  # Create buffer and union
  sensor_buffer <- st_buffer(sensor_data, dist = buffer_dist)
  sensor_union <- st_union(sensor_buffer)
  
  # Get roads within buffer
  roads_in_buffer <- st_intersection(full_network, sensor_union)
  
  # Filter predictions
  predictions_filtered <- predictions[predictions$OSM_ID %in% roads_in_buffer$osm_id, ]
  
  # Export
  output_file <- sprintf("output/final_results_%s_sf_with_period_ms.shp", source_name)
  st_write(predictions_filtered, output_file, delete_dsn = TRUE, quiet = TRUE)
  
  return(output_file)
}

# Export BRUITPARIF and ACOUCITE (use full versions)
export_sensor_predictions(sensors_bruitparif_full, "BRUITPARIF", full_network_2154, final_results_sf, buffer_radius)
export_sensor_predictions(sensors_acoucite_full, "ACOUCITE", full_network_2154, final_results_sf, buffer_radius)

# Export each CHILD sensor source (use full versions)
for (source_name in names(child_sensors_list)) {
  export_sensor_predictions(child_sensors_list[[source_name]], source_name, 
                           full_network_2154, final_results_sf, buffer_radius)
}









# =============================================================================
# SUMMARY STATISTICS
# =============================================================================


cat(sprintf("🗺️  Area: Roads within 1300m of noise sensors\n"))
cat(sprintf("🎤 Sensors: %d total from 13 sources\n", nrow(all_sensors)))
cat(sprintf("   • BRUITPARIF: %d sensors\n", nrow(sensors_bruitparif_full)))
cat(sprintf("   • ACOUCITE: %d sensors\n", nrow(sensors_acoucite_full)))
cat(sprintf("   • CHILD (11 sources): %d sensors\n", sum(sapply(child_sensors_list, nrow))))
cat(sprintf("📍 Roads: %s segments\n", format(nrow(final_results_export), big.mark=",")))
cat(sprintf("⏰ Periods: D, E, N (wide format)\n"))
cat(sprintf("📊 Total output rows: %s\n", format(nrow(final_results_export), big.mark=",")))
cat(sprintf("🎯 Columns: OSM_ID, PVMT, LV_SPD_D/E/N, HGV_SPD_D/E/N, LV_D/E/N, HGV_D/E/N\n\n"))

# Statistics by period (using long format)
library(dplyr)
period_stats <- final_results %>%
  group_by(period) %>%
  summarise(
    roads = n(),
    avg_TV = round(mean(TV, na.rm = TRUE)),
    max_TV = max(TV, na.rm = TRUE),
    avg_speed = round(mean(speed, na.rm = TRUE), 1),
    avg_truck_pct = round(mean(truck_pct, na.rm = TRUE), 1),
    total_trucks = sum(HGV, na.rm = TRUE),
    .groups = 'drop'
  )

# Show key periods
key_periods <- intersect(c("D", "E", "N", "h7", "h12", "h18", "h22"), unique(final_results$period))

for (p in key_periods) {
  period_data <- period_stats[period_stats$period == p, ]
  if (nrow(period_data) > 0) {
    period_name <- switch(p,
                         "D" = "Day (6h-18h)",
                         "E" = "Evening (18h-22h)", 
                         "N" = "Night (22h-6h)",
                         paste0("Hour ", gsub("h", "", p), ":00"))
    
    cat(sprintf("📊 %s - %s:\n", p, period_name))
    cat(sprintf("   • Traffic: %d vehicles/h avg (max: %d)\n", 
               period_data$avg_TV, period_data$max_TV))
    cat(sprintf("   • Speed: %.1f km/h avg\n", period_data$avg_speed))
    cat(sprintf("   • Trucks: %.1f%% avg (%d total)\n", 
               period_data$avg_truck_pct, period_data$total_trucks))
  }
}

