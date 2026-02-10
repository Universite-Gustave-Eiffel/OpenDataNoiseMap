# ==============================================================================
# UNIT TESTS: MODEL TRAINING PHASE
# ==============================================================================
# Tests pour valider les outputs de la phase d'entraînement des modèles
# (étape 06)
# ==============================================================================

test_model_training <- function() {
  pipeline_message(text = "Testing model training phase", 
                   level = 0, progress = "start", process = "test")
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Test 1: Models file exists and contains all expected models
  pipeline_message(text = "Test 1: XGBoost models file", 
                   level = 1, progress = "start", process = "test")
  
  if (file.exists(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)) {
    models_list <- readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
    
    expected_base <- c("flow_D", "truck_pct_D", "speed_D")
    expected_ratio_prefixes <- c("ratio_flow_", "ratio_truck_pct_", "ratio_speed_")
    
    if (all(expected_base %in% names(models_list))) {
      pipeline_message(text = sprintf("✓ All base models present (%d total models)", 
                                     length(models_list)),
                       level = 1, progress = "end", process = "pass")
      tests_passed <- tests_passed + 1
    } else {
      missing <- setdiff(expected_base, names(models_list))
      pipeline_message(text = sprintf("✗ Missing base models: %s", 
                                     paste(missing, collapse = ", ")),
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(text = sprintf("✗ Models file not found: %s",
                                   CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH),
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Test 2: Feature info file exists
  pipeline_message(text = "Test 2: Feature info file", 
                   level = 1, progress = "start", process = "test")
  
  if (file.exists(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)) {
    feature_info <- readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)
    
    if (!is.null(feature_info$road_feature_formula) && !is.null(feature_info$all_periods)) {
      pipeline_message(text = sprintf("✓ Feature info valid: %s periods", 
                                     length(feature_info$all_periods)),
                       level = 1, progress = "end", process = "pass")
      tests_passed <- tests_passed + 1
    } else {
      pipeline_message(text = "✗ Feature info incomplete",
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(text = sprintf("✗ Feature info file not found: %s",
                                   CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH),
                     level = 1, progress = "end", process = "fail")
    tests_failed <- tests_failed + 1
  }
  
  # Test 3: Check model metrics
  pipeline_message(text = "Test 3: Model quality metrics", 
                   level = 1, progress = "start", process = "test")
  
  if (exists("models_list") && length(models_list) > 0) {
    metrics_check <- TRUE
    
    for (model_name in names(models_list)) {
      model <- models_list[[model_name]]
      
      # Check if R² is in reasonable range (0-1, but typically > 0.3)
      if (!is.null(model$metrics$r2)) {
        if (model$metrics$r2 < -1 || model$metrics$r2 > 1) {
          metrics_check <- FALSE
          break
        }
      }
    }
    
    if (metrics_check) {
      pipeline_message(text = "✓ Model metrics valid",
                       level = 1, progress = "end", process = "pass")
      tests_passed <- tests_passed + 1
    } else {
      pipeline_message(text = "✗ Some model metrics out of range",
                       level = 1, progress = "end", process = "fail")
      tests_failed <- tests_failed + 1
    }
  } else {
    pipeline_message(text = "⊘ Models not loaded, skipping metrics check",
                     level = 1, progress = "end", process = "skip")
  }
  
  # Summary
  pipeline_message(text = sprintf("Model training tests: %d passed, %d failed", 
                                 tests_passed, tests_failed),
                   level = 0, progress = "end", process = ifelse(tests_failed == 0, "pass", "fail"))
  
  return(list(passed = tests_passed, failed = tests_failed))
}

# Run tests if sourced with test = TRUE
if (exists("RUN_TESTS") && RUN_TESTS == TRUE) {
  test_model_training()
}
