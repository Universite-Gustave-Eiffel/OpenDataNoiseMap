# ==============================================================================
# STAGE 2: OSM FEATURE ENGINEERING ON FRANCE NETWORK
# ==============================================================================
# This stage applies feature engineering to the entire OSM France network 
# (including connectivity and DEGRE) to create a pre-calculated layer that can 
# be used directly for training and prediction.
# 
# Inputs:
#   - OSM_ROADS_CONNECTIVITY_FILEPATH: OSM + connectivity + DEGRE for France
#   - OSM_ROADS_FRANCE_ENGINEERED_FILEPATH (optional): if exists and 
#     FORCE_REENGINEER_OSM_FRANCE = FALSE, will be loaded instead of 
#     re-computing
# Outputs:
#   - OSM_ROADS_FRANCE_ENGINEERED_FILEPATH: GeoPackage with engineered features for 
#     France layer
#   - IMPUTATION_RULES_FRANCE_FILEPATH: RDS file with imputation rules by highway 
#     type
# The engineered France network includes the following features:
#   - highway: OSM highway type
#   - DEGRE: connectivity class
#   - ref_letter: first letter of ref tag
#   - first_word: first word of name tag
#   - oneway_osm: binary oneway flag
#   - lanes_osm: number of lanes from OSM
#   - lanes_directional: number of lanes in the direction of traffic flow
#   - speed: vehicle speed, imputed if missing
#   - junction_osm: binary junction flag from OSM
#   - connectivity: connectivity measure from graph analysis
#   - betweenness: betweenness centrality from graph analysis
#   - closeness: closeness centrality from graph analysis
#   - pagerank: PageRank centrality from graph analysis
#   - coreness: coreness from graph analysis
#   - dead_end_score: custom score for dead-end roads
#   - edge_length_m: length of road segment in meters
#
# In TEST mode: reduces the region to a test bbox for speed
# ==============================================================================

pipeline_message(text = "OSM France feature engineering",
                 level = 0, progress = "start", process = "calc")

# Check if running in TEST mode
IS_TEST_MODE <- exists("TEST_REGION") && !is.null(TEST_REGION)
if (IS_TEST_MODE) {
  pipeline_message(
    sprintf("TEST MODE: Cropping to region %s (bbox: %s)",
            TEST_REGION$name,
            paste(TEST_REGION$bbox, collapse = ", ")),
    level = 1, process = "info")
}

# ------------------------------------------------------------------------------
# Load full OSM France road network with connectivities and communes
# ------------------------------------------------------------------------------

if (!exists('osm_full_network') || !is.data.frame(osm_full_network)) {
  
  # Verify that the GeoPackage file exists
  if (file.exists(CFG$OSM_ROADS_CONNECTIVITY_FILEPATH)) {
    
    pipeline_message(sprintf("Loading OSM full road network from %s", 
                             rel_path(CFG$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
                     level = 1, progress = "start", process = "load")
    
    osm_full_network <- sf::st_read(
      dsn   = CFG$OSM_ROADS_CONNECTIVITY_FILEPATH, 
      quiet = TRUE)
    
    # Project data into target CRS if needed
    if (sf::st_crs(osm_full_network) != CFG$TARGET_CRS){
      osm_full_network <- sf::st_transform(x   = osm_full_network, 
                                           crs = CFG$TARGET_CRS)
    }

    pipeline_message(
      sprintf("GeoDataframe contains %i features with %i fields and geometry type %s", 
              nrow(osm_full_network), ncol(osm_full_network), 
              sf::st_geometry_type(x = osm_full_network)[1]), 
      process = "info")
    
    pipeline_message("OSM France network successfully loaded", level = 1, 
                     progress = "end", process = "valid")
    
  } else {
    pipeline_message(
      sprintf("OSM full network object not found and file %s does not exist!", 
              CFG$OSM_ROADS_CONNECTIVITY_FILEPATH), 
      process = "stop")
  }
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
if (file.exists(CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH) && 
    isFALSE(CFG$FORCE_REENGINEER_OSM_FRANCE)) {
  
  pipeline_message(
    sprintf("Loading already engineered road network data from %s", 
            rel_path(CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  osm_france_engineered <- sf::st_read(
    dsn   = CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH,
    quiet = TRUE)
  
  pipeline_message("Engineered road network data loaded", level = 1, 
                   progress = "end", process = "valid")
  
} else if (!file.exists(CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH) || 
    isTRUE(CFG$FORCE_REENGINEER_OSM_FRANCE)) {
  
  pipeline_message("Computing imputation rules from full France network", 
                  level = 1, progress = "start", process = "build")

  # Coerce OSM data frame to data.table
  setDT(osm_full_network)

  pipeline_message(describe_df(osm_full_network), process = "info")

  # Clean highway types
  osm_full_network$highway <- as.character(x = osm_full_network$highway)
  osm_full_network$highway[is.na(osm_full_network$highway) | 
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
      n_roads      = .N), 
      by = highway]
  } else {
    # Fallback if columns missing
    imputation_rules <- osm_full_network[, .(
      median_lanes = NA_real_,
      median_speed = NA_real_,
      n_roads      = .N), 
      by = highway]
  }

  # Apply default values where imputation rules are missing
  imputation_rules[is.na(median_lanes), 
                   median_lanes := CFG$DEFAULT_NUMBER_OF_LANES]
  imputation_rules[is.na(median_speed), 
                   median_speed := CFG$DEFAULT_VEHICLE_SPEED]

  # Add fallback rule for missing highway types
  imputation_rules <- rbind(
    imputation_rules, 
    data.table(
      highway      = "missing", 
      median_lanes = CFG$DEFAULT_NUMBER_OF_LANES, 
      median_speed = CFG$DEFAULT_VEHICLE_SPEED, 
      n_roads      = CFG$DEFAULT_NUMBER_OF_ROADS))

  # Save imputation rules
  saveRDS(object = imputation_rules, 
          file   = CFG$IMPUTATION_RULES_FRANCE_FILEPATH)


  pipeline_message(describe_df(imputation_rules), process = "info")

  pipeline_message(
    text = sprintf("Imputation rules computed and saved to %s", 
                  rel_path(CFG$IMPUTATION_RULES_FRANCE_FILEPATH)), 
    level = 1, progress = "end", process = "save")

  # ------------------------------------------------------------------------------
  # Apply feature engineering to full France network
  # ------------------------------------------------------------------------------

  pipeline_message("Applying feature engineering to France network", 
                    level = 1, progress = "start", process = "calc")

  # Apply feature engineering pipeline:
  # - creates ordered highway factor
  # - extracts ref_letter (first letter of ref tag)
  # - extracts first_word from name
  # - normalizes oneway to binary
  # - imputes missing lanes and speed using rules
  osm_france_engineered <- process_network_features(
    data                    = osm_full_network, 
    rules                   = imputation_rules, 
    default_degre           = CFG$DEFAULT_DEGRE, 
    default_number_of_lanes = CFG$DEFAULT_NUMBER_OF_LANES, 
    default_vehicle_speed   = CFG$DEFAULT_VEHICLE_SPEED)

  pipeline_message(
    sprintf("Feature engineering applied: %s roads processed", 
            fmt(nrow(osm_france_engineered))), 
    process = "info")

  # ----------------------------------------------------------------------------
  # Save engineered France network
  # ----------------------------------------------------------------------------

  pipeline_message("Saving engineered France network", 
                  level = 1, progress = "start", process = "save")

  # Convert back to sf object if needed
  if (!"sf" %in% class(osm_france_engineered)) {
    osm_france_engineered <- sf::st_as_sf(x = osm_france_engineered)
  }

  # Add QGIS-friendly datetime fields when `period` exists
  osm_france_engineered <- add_period_datetime_columns(osm_france_engineered)

  # Export to GeoPackage
  sf::st_write(
    obj        = osm_france_engineered, 
    dsn        = CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH, 
    delete_dsn = TRUE,
    quiet      = TRUE)

  pipeline_message(
    sprintf("Writing %i features with %i fields and geometry type %s", 
            nrow(osm_france_engineered), ncol(osm_france_engineered), sf::st_geometry_type(osm_france_engineered)[1]), 
    process = "info")

  pipeline_message(
    sprintf("Engineered France network saved to %s", 
            rel_path(CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)), 
    level = 1, progress = "end", process = "save")

  # ----------------------------------------------------------------------------
  # Summary statistics
  # ----------------------------------------------------------------------------

  pipeline_message("Feature engineering summary:", process = "info")

  pipeline_message(
    sprintf("\t- Total roads: %s", 
            fmt(nrow(osm_france_engineered))), 
    process = "info")

  pipeline_message(
    sprintf("\t- Highway types: %s", 
            length(unique(osm_france_engineered$highway))), 
    process = "info")

  pipeline_message(
    sprintf("\t- DEGRE classes: %s", 
            length(unique(osm_france_engineered$DEGRE))), 
    process = "info")

  # Feature completeness check
  required_features <- c("highway", "DEGRE", "ref_letter", "first_word", 
                         "oneway_osm", "lanes_osm", "lanes_directional",
                         "speed", "junction_osm",
                         "connectivity", "betweenness", "closeness", "pagerank",
                         "coreness", "dead_end_score", "edge_length_m")

  available_features <- intersect(x =required_features, 
                                  y = names(osm_france_engineered))

  pipeline_message(
    text = sprintf("\t- Features available: %s/%s", 
                   length(available_features), 
                   length(required_features)), 
    level = 1, process = "info")

  if (length(available_features) < length(required_features)) {
    missing_features <- setdiff(x = required_features, y = available_features)
    pipeline_message(
      sprintf("\t- Missing features: %s", 
              paste(missing_features, collapse = ", ")), 
      process = "warning")
  }

  pipeline_message("OSM France feature engineering completed", 
                  level = 0, progress = "end", process = "valid")

  # Cleanup large objects to free memory for next steps
  rm(list = intersect(x = ls(), 
                      y = c("osm_france_engineered", 
                            "osm_full_network",
                            "imputation_rules")))
  gc(verbose = FALSE)
}
