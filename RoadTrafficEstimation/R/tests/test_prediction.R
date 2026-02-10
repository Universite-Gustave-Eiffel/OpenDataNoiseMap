# ==============================================================================
# UNIT TESTS: PREDICTION PHASE
# ==============================================================================
# Tests pour valider les outputs de la phase de prédiction
# (étapes 07d/07e)
# ==============================================================================

test_prediction <- function() {
  pipeline_message(text = "Testing prediction phase", 
                   level = 0, progress = "start", process = "test")
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Test 1: Nantes predictions file
  pipeline_message(text = "Test 1: Nantes predictions file", 
                   level = 1, progress = "start", process = "test")
  
  if (file.exists(CONFIG$NANTES_PREDICTION_FILEPATH)) {
    nantes <- sf::st_read(CONFIG$NANTES_PREDICTION_FILEPATH, quiet = TRUE)
    
    if (nrow(nantes) > 0 && all(c("osm_id", "TV", "HGV", "LV", "period") %in% names(nantes))) {
      # Check data quality
      issues <- 0
      if (any(nantes$TV < 0, na.rm = TRUE)) issues <- issues + 1
      if (any(nantes$HGV < 0, na.rm = TRUE)) issues <- issues + 1
      if (any(nantes$LV < 0, na.rm = TRUE)) issues <- issues + 1
      
      if (issues == 0) {
        pipeline_message(text = sprintf("✓ Nantes predictions: %s rows", fmt(nrow(nantes))),
                         level = 1, progress = "end", process = "pass")
        tests_passed <- tests_passed + 1
      } else {
        pipeline_message(text = sprintf("✗ Nantes predictions: %d data quality issues", issues),
                         level = 1, progress = "end", process = "fail")
        tests_failed <- tests_failed + 1
      }
    } else {
      pipeline_message(text = "✗ Nantes predictions: missing required columns",
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(text = sprintf("✗ Nantes predictions file not found: %s",
                                   CONFIG$NANTES_PREDICTION_FILEPATH),
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Test 2: Paris predictions file
  pipeline_message(text = "Test 2: Paris predictions file", 
                   level = 1, progress = "start", process = "test")
  
  if (file.exists(CONFIG$PARIS_PREDICTION_FILEPATH)) {
    paris <- sf::st_read(CONFIG$PARIS_PREDICTION_FILEPATH, quiet = TRUE)
    
    if (nrow(paris) > 0 && all(c("osm_id", "TV", "HGV", "LV", "period") %in% names(paris))) {
      # Check data quality
      issues <- 0
      if (any(paris$TV < 0, na.rm = TRUE)) issues <- issues + 1
      if (any(paris$HGV < 0, na.rm = TRUE)) issues <- issues + 1
      if (any(paris$LV < 0, na.rm = TRUE)) issues <- issues + 1
      
      if (issues == 0) {
        pipeline_message(text = sprintf("✓ Paris predictions: %s rows", fmt(nrow(paris))),
                         level = 1, progress = "end", process = "pass")
        tests_passed <- tests_passed + 1
      } else {
        pipeline_message(text = sprintf("✗ Paris predictions: %d data quality issues", issues),
                         level = 1, progress = "end", process = "fail")
        tests_failed <- tests_failed + 1
      }
    } else {
      pipeline_message(text = "✗ Paris predictions: missing required columns",
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(text = sprintf("✗ Paris predictions file not found: %s",
                                   CONFIG$PARIS_PREDICTION_FILEPATH),
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Test 3: Sensors predictions file
  pipeline_message(text = "Test 3: Sensors predictions file", 
                   level = 1, progress = "start", process = "test")
  
  if (file.exists(CONFIG$SENSORS_ALL_PREDICTION_FILEPATH)) {
    sensors <- sf::st_read(CONFIG$SENSORS_ALL_PREDICTION_FILEPATH, quiet = TRUE)
    
    if (nrow(sensors) > 0 && all(c("osm_id", "TV", "HGV", "LV", "period") %in% names(sensors))) {
      # Check data quality
      issues <- 0
      if (any(sensors$TV < 0, na.rm = TRUE)) issues <- issues + 1
      if (any(sensors$HGV < 0, na.rm = TRUE)) issues <- issues + 1
      if (any(sensors$LV < 0, na.rm = TRUE)) issues <- issues + 1
      
      if (issues == 0) {
        pipeline_message(text = sprintf("✓ Sensors predictions: %s rows", fmt(nrow(sensors))),
                         level = 1, progress = "end", process = "pass")
        tests_passed <- tests_passed + 1
      } else {
        pipeline_message(text = sprintf("✗ Sensors predictions: %d data quality issues", issues),
                         level = 1, progress = "end", process = "fail")
        tests_failed <- tests_failed + 1
      }
    } else {
      pipeline_message(text = "✗ Sensors predictions: missing required columns",
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(text = sprintf("✗ Sensors predictions file not found: %s",
                                   CONFIG$SENSORS_ALL_PREDICTION_FILEPATH),
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Test 4: Check consistency (HGV < TV)
  pipeline_message(text = "Test 4: Prediction data consistency", 
                   level = 1, progress = "start", process = "test")
  
  consistency_ok <- TRUE
  
  for (filepath in c(CONFIG$NANTES_PREDICTION_FILEPATH, 
                     CONFIG$PARIS_PREDICTION_FILEPATH,
                     CONFIG$SENSORS_ALL_PREDICTION_FILEPATH)) {
    if (file.exists(filepath)) {
      pred <- sf::st_read(filepath, quiet = TRUE)
      
      # Check HGV <= TV (HGV should be part of TV)
      inconsistent <- sum(pred$HGV > pred$TV, na.rm = TRUE)
      
      if (inconsistent > 0) {
        consistency_ok <- FALSE
        pipeline_message(text = sprintf("⚠ %s: %d rows with HGV > TV", 
                                       basename(filepath), inconsistent),
                         process = "warn")
      }
    }
  }
  
  if (consistency_ok) {
    pipeline_message(text = "✓ All predictions consistent (HGV ≤ TV)",
                     level = 1, progress = "end", process = "pass")
    tests_passed <- tests_passed + 1
  } else {
    pipeline_message(text = "✗ Consistency issues found",
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Summary
  pipeline_message(text = sprintf("Prediction tests: %d passed, %d failed", 
                                 tests_passed, tests_failed),
                   level = 0, progress = "end", process = ifelse(tests_failed == 0, "pass", "fail"))
  
  return(list(passed = tests_passed, failed = tests_failed))
}

# Run tests if sourced with test = TRUE
if (exists("RUN_TESTS") && RUN_TESTS == TRUE) {
  test_prediction()
}
