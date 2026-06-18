# ==============================================================================
# UNIT TESTS: DATA PREPARATION PHASE
# ==============================================================================
# Tests pour valider les outputs de la phase de préparation des données
# (étapes 01-06)
# ==============================================================================

test_data_preparation <- function() {
  pipeline_message("Testing data preparation phase", level = 0, 
                   progress = "start", process = "calc")
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Test 1: OSM network augmented exists
  pipeline_message("Test 1: OSM network augmented file", level = 1, 
                   progress = "start", process = "search")
  
  if (file.exists(CFG$OSM_ROADS_CONNECTIVITY_FILEPATH)) {
    osm_aug <- sf::st_read(dsn = CFG$OSM_ROADS_CONNECTIVITY_FILEPATH, 
                           quiet = TRUE)
    
    if (nrow(osm_aug) > 0 && 
        all(c("osm_id", "highway", "DEGRE") %in% names(osm_aug))) {
      pipeline_message(sprintf("✓ OSM augmented: %s roads", fmt(nrow(osm_aug))), 
                       level = 1, progress = "end", process = "valid")
      tests_passed <- tests_passed + 1
    } else {
      pipeline_message("OSM augmented: missing required columns", level = 1, 
                       progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(sprintf("OSM augmented file not found: %s", 
                             CFG$OSM_ROADS_CONNECTIVITY_FILEPATH), 
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }

  # Test 2: OSM France engineered exists
  pipeline_message("Test 2: OSM France engineered file", level = 1, 
                   progress = "start", process = "search")
  
  if (file.exists(CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH)) {
    osm_eng <- sf::st_read(dsn = CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH, 
                           quiet = TRUE)
    
    required_features <- c("highway", "DEGRE", "ref_letter", "first_word", 
                          "oneway_osm", "lanes_osm", "speed",
                          "connectivity", "betweenness", "closeness", "pagerank",
                          "coreness", "dead_end_score", "edge_length_m")
    
    if (nrow(osm_eng) > 0 && all(required_features %in% names(osm_eng))) {
      pipeline_message(sprintf("OSM engineered: %s roads, %s features", 
                               fmt(nrow(osm_eng)), length(required_features)), 
                       level = 1, progress = "end", process = "valid")
      tests_passed <- tests_passed + 1
    } else {
      missing <- setdiff(required_features, names(osm_eng))
      pipeline_message(sprintf("OSM engineered: missing %s", 
                               paste(missing, collapse = ", ")), 
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(sprintf("OSM engineered file not found: %s",
                             CFG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH),
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Test 3: Avatar data
  pipeline_message("Test 3: Avatar traffic data", level = 1, 
                   progress = "start", process = "search")
  
  if (file.exists(CFG$AVATAR_RDS_DATA_FILEPATH)) {
    avatar <- readRDS(file = CFG$AVATAR_RDS_DATA_FILEPATH)
    
    if (nrow(avatar) > 0 && 
        all(c("count_point_id", "aggregate_flow") %in% names(avatar))) {
      pipeline_message(sprintf("Avatar data: %s observations", 
                               fmt(nrow(avatar))),
                       level = 1, progress = "end", process = "valid")
      tests_passed <- tests_passed + 1
    } else {
      pipeline_message("Avatar data: missing required columns", 
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(sprintf("Avatar data file not found: %s", 
                             CFG$AVATAR_RDS_DATA_FILEPATH), 
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Test 4: Training dataset
  pipeline_message("Test 4: Training dataset", level = 1, 
                   progress = "start", process = "search")
  
  if (file.exists(CFG$TRAINING_RDS_DATA_FILEPATH)) {
    training <- readRDS(file = CFG$TRAINING_RDS_DATA_FILEPATH)
    
    if (nrow(training) > 0 && 
        all(c("osm_id", "aggregate_flow", "highway") %in% names(training))) {
      pipeline_message(sprintf("Training data: %s observations", 
                               fmt(nrow(training))),
                       level = 1, progress = "end", process = "valid")
      tests_passed <- tests_passed + 1
    } else {
      pipeline_message("Training data: missing required columns",
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(sprintf("Training data file not found: %s", 
                             CFG$TRAINING_RDS_DATA_FILEPATH),
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Summary
  pipeline_message(sprintf("Data preparation tests: %d passed, %d failed", 
                           tests_passed, tests_failed),
                   level = 0, progress = "end", 
                   process = ifelse(test = tests_failed == 0, 
                                    yes = "valid", no = "fail"))
  
  return(list(passed = tests_passed, failed = tests_failed))
}

# Run tests if sourced with test = TRUE
if (exists("RUN_TESTS") && RUN_TESTS == TRUE) {
  test_data_preparation()
}
