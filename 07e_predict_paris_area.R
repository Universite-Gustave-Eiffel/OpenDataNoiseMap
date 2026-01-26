# =============================================================================
# STAGE 7E: PREDICTIONS FOR PARIS METROPOLITAN AREA (LONG FORMAT)
# =============================================================================
# Predict hourly traffic flow, speed, and truck % for Greater Paris
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

# =============================================================================
# LOAD AND FILTER PARIS AREA DATA
# =============================================================================

full_network <- st_read(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH)
full_network_2154 <- st_transform(full_network, 2154)
full_network <- full_network_2154

# Define Paris Métropole bounds (Île-de-France inner ring)
# Paris center: lon=2.3522, lat=48.8566
# Metropolitan area extends ~30km in each direction
paris_center <- list(lon = 2.3522, lat = 48.8566)
metropole_bounds <- list(
  lon_min = paris_center$lon - 0.40,  # ~30km west
  lon_max = paris_center$lon + 0.40,  # ~30km east
  lat_min = paris_center$lat - 0.25,  # ~25km south  
  lat_max = paris_center$lat + 0.25   # ~25km north
)

corners_wgs84 <- data.frame(
  lon = c(metropole_bounds$lon_min, metropole_bounds$lon_max),
  lat = c(metropole_bounds$lat_min, metropole_bounds$lat_max)
)

corners_sf <- st_as_sf(corners_wgs84, coords = c("lon", "lat"), crs = 4326)
corners_sf <- st_transform(corners_sf, 2154)
corners_coords <- st_coordinates(corners_sf)

bbox_lambert <- st_bbox(c(
  xmin = min(corners_coords[, "X"]), 
  xmax = max(corners_coords[, "X"]),
  ymin = min(corners_coords[, "Y"]), 
  ymax = max(corners_coords[, "Y"])
), crs = st_crs(2154))

cat(sprintf("  📍 Bounding box: [%.0f, %.0f] x [%.0f, %.0f] (Lambert93)\n",
           bbox_lambert["xmin"], bbox_lambert["xmax"],
           bbox_lambert["ymin"], bbox_lambert["ymax"]))

paris_area <- st_crop(full_network, bbox_lambert)

cat(sprintf("  ✓ Extracted %s roads from Paris Metropolitan Area\n", format(nrow(paris_area), big.mark=",")))

paris_dt <- data.table(st_drop_geometry(paris_area))

if (file.exists(CONFIG$AVATAR_IMPUTATION_RULES_FILEPATH)) {
  imputation_rules <- readRDS(CONFIG$AVATAR_IMPUTATION_RULES_FILEPATH)
} else {
  imputation_rules <- paris_dt[!is.na(highway), .(
    median_lanes = median(as.numeric(lanes_osm), na.rm = TRUE),
    median_speed = median(as.numeric(maxspeed_osm), na.rm = TRUE),
    n_roads = .N
  ), by = highway]
  imputation_rules[is.na(median_lanes), median_lanes := 2]
  imputation_rules[is.na(median_speed), median_speed := 50]
}

pipeline_message(text = describe_df(imputation_rules), process = "info")

highway_levels <- c("residential", "tertiary", "secondary", "primary", "trunk", "motorway",
                   "tertiary_link", "secondary_link", "primary_link", "trunk_link", 
                   "motorway_link", "unclassified")
paris_dt[, highway := as.character(highway)]
paris_dt[is.na(highway) | highway == "", highway := "unclassified"]
paris_dt[, highway := factor(highway, levels = c(highway_levels, "missing"), ordered = TRUE)]

paris_dt[, DEGRE := as.numeric(as.character(DEGRE))]
paris_dt[is.na(DEGRE), DEGRE := 1]

ref_col <- NA
for (col in c("ref", "ref_osm")) {
  if (col %in% names(paris_dt)) {
    ref_col <- col
    break
  }
}

if (!is.na(ref_col)) {
  paris_dt[, ref_letter := ifelse(
    !is.na(get(ref_col)) & get(ref_col) != "",
    substr(gsub("^([A-Za-z]).*", "\\1", get(ref_col)), 1, 1),
    "missing"
  )]
  paris_dt[!grepl("^[A-Za-z]$", ref_letter), ref_letter := "missing"]
} else {
  paris_dt[, ref_letter := "missing"]
}
paris_dt[, ref_letter := factor(ref_letter)]

if ("name" %in% names(paris_dt)) {
  paris_dt[, first_word := ifelse(
    !is.na(name) & name != "",
    tolower(trimws(gsub("\\s+.*", "", name))),
    "missing"
  )]
  word_counts <- table(paris_dt$first_word)
  rare_words <- names(word_counts[word_counts < 5])
  paris_dt[first_word %in% rare_words, first_word := "missing"]
} else {
  paris_dt[, first_word := "missing"]
}
paris_dt[, first_word := factor(first_word)]

oneway_col <- NA
for (col in c("oneway", "oneway_osm")) {
  if (col %in% names(paris_dt)) {
    oneway_col <- col
    break
  }
}

if (!is.na(oneway_col)) {
  val <- as.character(paris_dt[[oneway_col]])
  val[is.na(val) | val == ""] <- "missing"
  val[val == "-1"] <- "yes"
  val[val %in% c("true", "1")] <- "yes"
  val[val %in% c("false", "0")] <- "no"
  val[!val %in% c("yes", "no", "missing")] <- "yes"
  paris_dt[, oneway_osm := val]
} else {
  paris_dt[, oneway_osm := "missing"]
}
paris_dt[, oneway_osm := factor(oneway_osm, levels = c("yes", "no", "missing"))]

lanes_col <- NA
for (col in c("lanes", "lanes_osm")) {
  if (col %in% names(paris_dt)) {
    lanes_col <- col
    break
  }
}

if (!is.na(lanes_col)) {
  paris_dt[, lanes_osm := as.numeric(get(lanes_col))]
} else {
  paris_dt[, lanes_osm := NA_real_]
}

paris_dt <- merge(paris_dt, imputation_rules[, .(highway, median_lanes)], 
                  by = "highway", all.x = TRUE)
paris_dt[is.na(lanes_osm), lanes_osm := median_lanes]
paris_dt[is.na(lanes_osm), lanes_osm := 2]
paris_dt[, median_lanes := NULL]

speed_col <- NA
for (col in c("maxspeed", "maxspeed_osm", "speed")) {
  if (col %in% names(paris_dt)) {
    speed_col <- col
    break
  }
}

if (!is.na(speed_col)) {
  paris_dt[, speed := as.numeric(get(speed_col))]
} else {
  paris_dt[, speed := NA_real_]
}

paris_dt <- merge(paris_dt, imputation_rules[, .(highway, median_speed)], 
                  by = "highway", all.x = TRUE)
paris_dt[is.na(speed), speed := median_speed]
paris_dt[is.na(speed), speed := 50]
paris_dt[, median_speed := NULL]

for (col in c("connectivity", "betweenness", "closeness", "pagerank")) {
  if (!col %in% names(paris_dt)) {
    stop(sprintf("ERROR: %s missing from network features", col))
  }
}

network_features <- c("connectivity", "betweenness", "closeness", "pagerank")
for (feat in network_features) {
  na_count <- sum(is.na(paris_dt[[feat]]))
  if (na_count > 0) {
    paris_dt[is.na(get(feat)), (feat) := 0]
  }
}

# Feature engineering complete

# =============================================================================
# GENERATE PREDICTIONS (LONG FORMAT WITH PERIOD COLUMN)
# =============================================================================

feature_vars <- c("highway", "DEGRE", "ref_letter", "first_word", "oneway_osm", 
                  "lanes_osm", "speed", "connectivity", "betweenness", "closeness", "pagerank")

all_features <- feature_vars
feature_formula <- as.formula(paste("~", paste(all_features, collapse = " + ")))

paris_dt[, highway := factor(highway, levels = c(highway_levels, "missing"), ordered = TRUE)]
contrasts(paris_dt$highway) <- contr.poly(length(levels(paris_dt$highway)))

paris_dt[, DEGRE := as.numeric(as.character(DEGRE))]

paris_dt[, ref_letter := factor(ref_letter)]
paris_dt[, first_word := factor(first_word)]
paris_dt[, oneway_osm := factor(oneway_osm, levels = c("yes", "no", "missing"))]

pred_matrix <- sparse.model.matrix(feature_formula, data = paris_dt)

# Prediction matrix generated with polynomial contrasts for ordered factors

if (nrow(pred_matrix) != nrow(paris_dt)) {
  stop(sprintf("Dimension mismatch: matrix has %d rows but data has %d rows - check for remaining NAs", 
              nrow(pred_matrix), nrow(paris_dt)))
}

# Data and matrix aligned
saveRDS(paris_dt, CONFIG$PARIS_FORECAST_FILEPATH)

all_results <- list()

# =============================================================================
# BASE PREDICTIONS (Period D)
# =============================================================================

osm_id_col <- "osm_id"

base_predictions <- data.frame(
  osm_id = paris_dt[[osm_id_col]],
  highway = as.character(paris_dt$highway),
  name = ifelse("name" %in% names(paris_dt), as.character(paris_dt$name), "")
)

base_targets <- c("flow_D", "truck_pct_D", "speed_D")

for (target in base_targets) {
  if (target %in% names(models_list)) {
    model <- models_list[[target]]$model
    config <- models_list[[target]]$config
    training_features <- models_list[[target]]$feature_names
    
    available_features <- colnames(pred_matrix)
    missing_features <- setdiff(training_features, available_features)
    extra_features <- setdiff(available_features, training_features)
    
    if (length(missing_features) > 0) {
      missing_matrix <- Matrix(0, 
                              nrow = nrow(pred_matrix), 
                              ncol = length(missing_features),
                              dimnames = list(NULL, missing_features))
      pred_matrix_extended <- cbind(pred_matrix, missing_matrix)
    } else {
      pred_matrix_extended <- pred_matrix
    }
    
    pred_matrix_aligned <- pred_matrix_extended[, training_features, drop = FALSE]
    
    if (ncol(pred_matrix_aligned) != length(training_features)) {
      stop("Feature alignment failed")
    }
    
    predictions <- predict(model, pred_matrix_aligned)
    
    if (!is.null(config$transform) && config$transform == "log10") {
      predictions <- 10^predictions
    }
    
    target_clean <- gsub("_D$", "", target)
    base_predictions[[target_clean]] <- round(predictions, 2)
  }
}

base_predictions$period <- "D"
base_predictions$TV <- base_predictions$flow
base_predictions$HGV <- round(base_predictions$flow * (base_predictions$truck_pct / 100))
base_predictions$LV <- base_predictions$TV - base_predictions$HGV

all_results[["D"]] <- base_predictions[, c("osm_id", "highway", "name", "period", 
                                          "TV", "HGV", "LV")]

# =============================================================================
# RATIO PREDICTIONS (Other Periods)  
# =============================================================================

ratio_periods <- setdiff(all_periods, "D")

for (period in ratio_periods) {
  period_data <- data.frame(
    osm_id = base_predictions$osm_id,
    highway = base_predictions$highway,
    name = base_predictions$name,
    period = period
  )
  
  for (target in c("flow", "truck_pct", "speed")) {
    ratio_model_name <- paste0("ratio_", target, "_", period)
    
    if (ratio_model_name %in% names(models_list) && target %in% names(base_predictions)) {
      model <- models_list[[ratio_model_name]]$model
      training_features <- models_list[[ratio_model_name]]$feature_names
      baseline_values <- base_predictions[[target]]
      
      available_features <- colnames(pred_matrix)
      missing_features <- setdiff(training_features, available_features)
      
      if (length(missing_features) > 0) {
        missing_matrix <- Matrix(0, 
                                nrow = nrow(pred_matrix), 
                                ncol = length(missing_features),
                                dimnames = list(NULL, missing_features))
        pred_matrix_full <- cbind(pred_matrix, missing_matrix)
      } else {
        pred_matrix_full <- pred_matrix
      }
      
      pred_matrix_aligned <- pred_matrix_full[, training_features, drop = FALSE]
      ratio_pred <- predict(model, pred_matrix_aligned)
      
      period_data[[target]] <- round(baseline_values * ratio_pred, 2)
    } else {
      if (target %in% names(base_predictions)) {
        period_data[[target]] <- base_predictions[[target]]
      }
    }
  }
  
  period_data$TV <- period_data$flow
  period_data$HGV <- round(period_data$flow * (period_data$truck_pct / 100))
  period_data$LV <- period_data$TV - period_data$HGV
  
  all_results[[period]] <- period_data[, c("osm_id", "highway", "name", "period",
                                          "TV", "HGV", "LV")]
}

# All ratio periods processed

# =============================================================================
# COMBINE INTO LONG FORMAT AND EXPORT
# =============================================================================

final_results <- do.call(rbind, all_results)

# Final dataset combined: roads × periods in long format

final_results_export <- final_results

final_results_export$period_ms <- sapply(final_results_export$period, function(p) {
  if (grepl("^h", p)) {
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

final_results_sf <- merge(final_results_export, full_network_2154, by = "osm_id")

final_results_sf <- st_as_sf(final_results_sf)

output_path <- "output/paris_area_predictions_with_period_ms.gpkg"
st_write(final_results_sf, output_path, delete_dsn = TRUE)

cat(sprintf("  ✓ Exported to: %s\n\n", output_path))

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat(sprintf("🗺️  Area: Paris Metropolitan Area (~60km × 50km)\n"))
cat(sprintf("📍 Roads: %s segments\n", format(length(unique(final_results$osm_id)), big.mark=",")))
cat(sprintf("⏰ Periods: %d (%s)\n", length(unique(final_results$period)), 
           paste(sort(unique(final_results$period)), collapse=", ")))
cat(sprintf("📊 Total rows: %s\n", format(nrow(final_results), big.mark=",")))
cat(sprintf("🎯 Columns: osm_id, highway, name, period, TV, HGV, LV, period_ms\n\n"))

library(dplyr)
period_stats <- final_results %>%
  group_by(period) %>%
  summarise(
    roads = n(),
    avg_TV = round(mean(TV, na.rm = TRUE)),
    max_TV = max(TV, na.rm = TRUE),
    .groups = 'drop'
  )

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
  }
}
