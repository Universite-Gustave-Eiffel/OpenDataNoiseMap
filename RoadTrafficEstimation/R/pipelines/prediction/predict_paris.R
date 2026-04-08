# ==============================================================================
# PREDICTION: PARIS METROPOLITAN AREA
# ==============================================================================
# Paris Metropolitan (Île-de-France) bounds in Lambert93 (EPSG:2154)
# Center: ~655000, 6862000 — Extend ~30km radius for greater Paris
# ==============================================================================

paris_output_config <- build_prediction_filepaths(extent = "paris", 
                                                  mode   = mode_suffix)

predict_traffic(
  region_name   = "Paris Metropolitan Area",
  cfg           = CFG,
  bbox = c(xmin = 625000, 
           ymin = 6832000, 
           xmax = 685000, 
           ymax = 6892000),
  output_config = list(filepath = paris_output_config$filepath),
  method        = "region"
)
