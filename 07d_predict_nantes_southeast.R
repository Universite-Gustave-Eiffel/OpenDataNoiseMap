# =============================================================================
# STAGE 7D: PREDICTIONS FOR NANTES SOUTHEAST QUARTER (LONG FORMAT)
# =============================================================================
# Predict hourly traffic flow, speed, and truck % for southeast Nantes
# Output: Long format with 'period' column (D, E, N, h0-h23)
# =============================================================================

library(xgboost)
library(dplyr)
library(Matrix)
library(sf)
library(data.table)

if (!file.exists("rdsFiles/xgb_models_with_ratios.rds")) {
  stop("❌ Models not found! Please run R/06d_xgboost_training_with_ratios.R first")
}

models_list <- readRDS("rdsFiles/xgb_models_with_ratios.rds")
feature_info <- readRDS("rdsFiles/xgb_ratio_feature_info.rds")

feature_formula <- feature_info$feature_formula
all_periods <- feature_info$all_periods

cat(sprintf("  ✓ Loaded %d trained models\n", length(models_list)))
cat(sprintf("  ✓ Feature formula: %s\n", deparse(feature_formula)))
cat(sprintf("  ✓ Periods: %s\n\n", paste(all_periods, collapse=", ")))

# =============================================================================
# LOAD AND FILTER NANTES SOUTHEAST DATA
# =============================================================================

full_network <- st_read( "data/routes_france_osm_complete_including_connectivity_and_communes_for_R.gpkg")
full_network_2154 <- st_transform(full_network, 2154)
full_network <- full_network_2154

# Define Nantes Métropole bounds (entire metropolitan area)
# Nantes center approximately: lon=-1.553, lat=47.218
# Métropole extends ~15km in each direction
nantes_center <- list(lon = -1.553, lat = 47.218)
metropole_bounds <- list(
  lon_min = nantes_center$lon - 0.15,  # ~15km west
  lon_max = nantes_center$lon + 0.15,  # ~15km east
  lat_min = nantes_center$lat - 0.10,  # ~10km south  
  lat_max = nantes_center$lat + 0.10   # ~10km north
)

# Convert corner points to Lambert93 
corners_wgs84 <- data.frame(
  lon = c(metropole_bounds$lon_min, metropole_bounds$lon_max),
  lat = c(metropole_bounds$lat_min, metropole_bounds$lat_max)
)

corners_sf <- st_as_sf(corners_wgs84, coords = c("lon", "lat"), crs = 4326)
corners_sf <- st_transform(corners_sf,2154)
corners_coords <- st_coordinates(corners_sf)

# Create bounding box in Lambert93 (same CRS as full_network)
bbox_lambert <- st_bbox(c(
  xmin = min(corners_coords[, "X"]), 
  xmax = max(corners_coords[, "X"]),
  ymin = min(corners_coords[, "Y"]), 
  ymax = max(corners_coords[, "Y"])
), crs = st_crs(2154))

cat(sprintf("  📍 Bounding box: [%.0f, %.0f] x [%.0f, %.0f] (Lambert93)\n",
           bbox_lambert["xmin"], bbox_lambert["xmax"],
           bbox_lambert["ymin"], bbox_lambert["ymax"]))

# Try spatial crop
nantes_metropole <- st_crop(full_network, bbox_lambert)

cat(sprintf("  ✓ Extracted %s roads from Nantes Métropole\n", format(nrow(nantes_metropole), big.mark=",")))

# Convert to data.table for processing (similar to step 5)
nantes_dt <- data.table(st_drop_geometry(nantes_metropole))

# Load imputation rules from training
if (file.exists("rdsFiles/imputation_rules.rds")) {
  imputation_rules <- readRDS("rdsFiles/imputation_rules.rds")
} else {
  # Basic imputation rules
  imputation_rules <- nantes_dt[!is.na(highway), .(
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
nantes_dt[, highway := as.character(highway)]
nantes_dt[is.na(highway) | highway == "", highway := "unclassified"]
nantes_dt[, highway := factor(highway, levels = c(highway_levels, "missing"), ordered = TRUE)]

cat(sprintf("  ✅ Highway: ordered=%s, levels=%d, sample=%s\n", 
           is.ordered(nantes_dt$highway), 
           length(levels(nantes_dt$highway)),
           paste(head(as.character(nantes_dt$highway), 3), collapse=", ")))


nantes_dt[, DEGRE := as.numeric(as.character(DEGRE))]
nantes_dt[is.na(DEGRE), DEGRE := 1]  # Urban default

# STEP 3: ref_letter (first letter of route reference)
# Auto-detect column name (ref, ref_osm, or create missing)
ref_col <- NA
for (col in c("ref", "ref_osm")) {
  if (col %in% names(nantes_dt)) {
    ref_col <- col
    break
  }
}

if (!is.na(ref_col)) {
  # Use regex to extract letter only (match training logic)
  # Training: substr(gsub("^([A-Za-z]).*", "\\1", data[[ref_col]]), 1, 1)
  nantes_dt[, ref_letter := ifelse(
    !is.na(get(ref_col)) & get(ref_col) != "",
    substr(gsub("^([A-Za-z]).*", "\\1", get(ref_col)), 1, 1),
    "missing"
  )]
  # If result is not a letter (e.g. was a number), set to missing
  nantes_dt[!grepl("^[A-Za-z]$", ref_letter), ref_letter := "missing"]
} else {
  nantes_dt[, ref_letter := "missing"]
}
nantes_dt[, ref_letter := factor(ref_letter)]

# STEP 4: first_word (first word of road name)
if ("name" %in% names(nantes_dt)) {
  nantes_dt[, first_word := ifelse(
    !is.na(name) & name != "",
    tolower(trimws(gsub("\\s+.*", "", name))),
    "missing"
  )]
  # Keep only common words, others → "missing" (same logic as training)
  word_counts <- table(nantes_dt$first_word)
  rare_words <- names(word_counts[word_counts < 5])  # Conservative threshold
  nantes_dt[first_word %in% rare_words, first_word := "missing"]
} else {
  nantes_dt[, first_word := "missing"]
}
nantes_dt[, first_word := factor(first_word)]

# STEP 5: oneway_osm (standardized)
oneway_col <- NA
for (col in c("oneway", "oneway_osm")) {
  if (col %in% names(nantes_dt)) {
    oneway_col <- col
    break
  }
}

if (!is.na(oneway_col)) {
  # Match training logic: -1 -> yes, others -> yes if unknown
  # Training: data$oneway_osm[data$oneway_osm == "-1"] <- "yes"
  # Training: data$oneway_osm[!data$oneway_osm %in% allowed_oneway_values] <- "yes"
  
  val <- as.character(nantes_dt[[oneway_col]])
  val[is.na(val) | val == ""] <- "missing"
  val[val == "-1"] <- "yes"
  
  # Standardize
  val[val %in% c("true", "1")] <- "yes"
  val[val %in% c("false", "0")] <- "no"
  
  # Default unknown to "yes" (as in training)
  val[!val %in% c("yes", "no", "missing")] <- "yes"
  
  nantes_dt[, oneway_osm := val]
} else {
  nantes_dt[, oneway_osm := "missing"]
}
nantes_dt[, oneway_osm := factor(oneway_osm, levels = c("yes", "no", "missing"))]

# STEP 6: lanes_osm (imputed by highway type)
lanes_col <- NA
for (col in c("lanes", "lanes_osm")) {
  if (col %in% names(nantes_dt)) {
    lanes_col <- col
    break
  }
}

if (!is.na(lanes_col)) {
  nantes_dt[, lanes_osm := as.numeric(get(lanes_col))]
} else {
  nantes_dt[, lanes_osm := NA_real_]
}

# Impute missing lanes by highway type
nantes_dt <- merge(nantes_dt, imputation_rules[, .(highway, median_lanes)], 
                  by = "highway", all.x = TRUE)
nantes_dt[is.na(lanes_osm), lanes_osm := median_lanes]
nantes_dt[is.na(lanes_osm), lanes_osm := 2]  # Final fallback
nantes_dt[, median_lanes := NULL]

# STEP 7: speed (maxspeed, imputed by highway type)  
speed_col <- NA
for (col in c("maxspeed", "maxspeed_osm", "speed")) {
  if (col %in% names(nantes_dt)) {
    speed_col <- col
    break
  }
}

if (!is.na(speed_col)) {
  nantes_dt[, speed := as.numeric(get(speed_col))]
} else {
  nantes_dt[, speed := NA_real_]
}

# Impute missing speed by highway type
nantes_dt <- merge(nantes_dt, imputation_rules[, .(highway, median_speed)], 
                  by = "highway", all.x = TRUE)
nantes_dt[is.na(speed), speed := median_speed]
nantes_dt[is.na(speed), speed := 50]  # Final fallback
nantes_dt[, median_speed := NULL]


# Verify we have the main columns (without suffixes)
for (col in c("connectivity", "betweenness", "closeness", "pagerank")) {
  if (!col %in% names(nantes_dt)) {
    cat(sprintf("     ⚠️  ERROR: %s still missing!\n", col))
  } else {
    cat(sprintf("     ✅ %s: [%.2f, %.2f] mean=%.2f\n", 
               col, 
               min(nantes_dt[[col]], na.rm=TRUE),
               max(nantes_dt[[col]], na.rm=TRUE),
               mean(nantes_dt[[col]], na.rm=TRUE)))
  }
}

# STEP 9: Impute remaining NAs in network features (isolated road segments)
network_features <- c("connectivity", "betweenness", "closeness", "pagerank")
for (feat in network_features) {
  na_count <- sum(is.na(nantes_dt[[feat]]))
  if (na_count > 0) {
    # Use 0 for isolated segments (they have no network connectivity)
    nantes_dt[is.na(get(feat)), (feat) := 0]
    cat(sprintf("     🔧 Imputed %d NA values in %s with 0 (isolated segments)\n", na_count, feat))
  }
}

cat(sprintf("  ✓ Feature engineering complete: %d roads with %d features\n", 
           nrow(nantes_dt), ncol(nantes_dt)))



# =============================================================================
# GENERATE PREDICTIONS (LONG FORMAT WITH PERIOD COLUMN)
# =============================================================================


# Identify actual feature columns (handle .x/.y suffixes from joins)
available_cols <- names(nantes_dt)

# Complete feature list with actual column names
feature_vars <- c("highway", "DEGRE", "ref_letter", "first_word", "oneway_osm", 
                  "lanes_osm", "speed", "connectivity", "betweenness", "closeness", "pagerank")

all_features <- feature_vars
feature_formula <- as.formula(paste("~", paste(all_features, collapse = " + ")))

# Highway: Ordered factor (matches training)
nantes_dt[, highway := factor(highway, levels = c(highway_levels, "missing"), ordered = TRUE)]
contrasts(nantes_dt$highway) <- contr.poly(length(levels(nantes_dt$highway)))

# DEGRE: Numeric (matches training)
nantes_dt[, DEGRE := as.numeric(as.character(DEGRE))]

# Other factors: Unordered
nantes_dt[, ref_letter := factor(ref_letter)]
nantes_dt[, first_word := factor(first_word)]
nantes_dt[, oneway_osm := factor(oneway_osm, levels = c("yes", "no", "missing"))]

# 5. Generate matrix on combined data
pred_matrix <- sparse.model.matrix(feature_formula, data = nantes_dt)

# DEBUG: Check generated columns
cat(sprintf("  🔍 Generated %d columns. Examples: %s\n", 
           ncol(pred_matrix), 
           paste(head(colnames(pred_matrix), 5), collapse=", ")))

# Check if highway.L exists
if (!any(grepl("highway\\.L", colnames(pred_matrix)))) {
  cat("     Highway columns found: ", 
      paste(grep("highway", colnames(pred_matrix), value=TRUE), collapse=", "), "\n")
} else {
}

cat(sprintf("  📏 Final prediction matrix: %d roads × %d features\n", 
           nrow(pred_matrix), ncol(pred_matrix)))

# Verify dimensions match (should be equal after NA imputation)
if (nrow(pred_matrix) != nrow(nantes_dt)) {
  stop(sprintf("❌ Dimension mismatch: matrix has %d rows but data has %d rows!\n   Check for remaining NAs in features.", 
              nrow(pred_matrix), nrow(nantes_dt)))
}

cat(sprintf("  ✅ Data aligned: %d roads\n", nrow(nantes_dt)))

saveRDS(nantes_dt, "data/nantes_dt.rds")

# Initialize results list for long format
all_results <- list()

# =============================================================================
# BASE PREDICTIONS (Period D)
# =============================================================================


osm_id_col <- "osm_id"

base_predictions <- data.frame(
  osm_id = nantes_dt[[osm_id_col]],
  highway = as.character(nantes_dt$highway),
  name = ifelse("name" %in% names(nantes_dt), as.character(nantes_dt$name), "")
)

base_targets <- c("flow_D", "truck_pct_D", "speed_D")

for (target in base_targets) {
  if (target %in% names(models_list)) {
    model <- models_list[[target]]$model
    config <- models_list[[target]]$config
    training_features <- models_list[[target]]$feature_names
    
    
    # DEBUG: Check feature encoding expected by model
    highway_features <- grep("highway", training_features, value = TRUE)
    if (length(highway_features) > 0) {
      cat(sprintf("\n      [DEBUG] Model expects highway features: %s ...\n", 
                 paste(head(highway_features, 3), collapse=", ")))
    }
    
    # ROBUST FEATURE ALIGNMENT
    # Check which features are available
    available_features <- colnames(pred_matrix)
    missing_features <- setdiff(training_features, available_features)
    extra_features <- setdiff(available_features, training_features)
    
    if (length(missing_features) > 0) {
      cat(sprintf(" [adding %d missing features]", length(missing_features)))
      
      # DEBUG: Show which features are missing
      if (length(missing_features) < 5) {
        cat(sprintf(" (%s)", paste(missing_features, collapse=", ")))
      } else {
        cat(sprintf(" (e.g. %s)", paste(head(missing_features, 3), collapse=", ")))
      }
      
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
      cat(sprintf(" ERROR: Feature alignment failed\n"))
      next
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
    
    cat(sprintf(" ✓ [%.1f, %.1f]\n", min(predictions), max(predictions)))
  }
}

# Add D period data with vehicle breakdown
base_predictions$period <- "D"
base_predictions$TV <- base_predictions$flow
base_predictions$HGV <- round(base_predictions$flow * (base_predictions$truck_pct / 100))
base_predictions$LV <- base_predictions$TV - base_predictions$HGV

# Store D period in results
all_results[["D"]] <- base_predictions[, c("osm_id", "highway", "name", "period", 
                                          "TV", "HGV", "LV")]

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
                                          "TV", "HGV", "LV")]
}

cat(sprintf("    ✓ Completed %d periods\n", length(ratio_periods)))

# =============================================================================
# COMBINE INTO LONG FORMAT AND EXPORT
# =============================================================================


# Combine all periods
final_results <- do.call(rbind, all_results)

cat(sprintf("  ✓ Final dataset: %s observations (%d roads × %d periods)\n", 
           format(nrow(final_results), big.mark=","),
           length(unique(final_results$osm_id)), 
           length(unique(final_results$period))))


# Convert period to milliseconds (integer)
# h0 = 0, h1 = 3600000, h2 = 7200000, ..., h23 = 82800000
# D = 90000000, E = 93600000, N = 97200000
final_results_export <- final_results

final_results_export$period_ms <- sapply(final_results_export$period, function(p) {
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
final_results_sf <- merge(final_results_export, full_network_2154,  by = "osm_id")

# Convert to sf object
final_results_sf <- st_as_sf(final_results_sf)

# Export to GeoPackage
output_path <- "output/final_results_sf_with_period_ms.gpkg"
st_write(final_results_sf, output_path, delete_dsn = TRUE)






# Add geometry
final_results_sf <- merge(
  final_results, 
  nantes_metropole[, c("osm_id", "geom")], 
  by.x = "osm_id", by.y = "osm_id",
  all.x = TRUE
) %>%
  st_as_sf()

# Export single GeoPackage

st_write(final_results_sf, "output/nantes_metropole_predictions.gpkg", 
         delete_dsn = TRUE, quiet = TRUE)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================


cat(sprintf("🗺️  Area: Nantes Métropole (30km × 20km)\n"))
cat(sprintf("📍 Roads: %s segments\n", format(length(unique(final_results$osm_id)), big.mark=",")))
cat(sprintf("⏰ Periods: %d (%s)\n", length(unique(final_results$period)), 
           paste(sort(unique(final_results$period)), collapse=", ")))
cat(sprintf("📊 Total rows: %s\n", format(nrow(final_results), big.mark=",")))
cat(sprintf("🎯 Columns: osm_id, highway, name, period, TV, HGV, LV, truck_pct, speed\n\n"))

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

