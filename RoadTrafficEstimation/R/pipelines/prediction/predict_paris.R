# ==============================================================================
# PREDICTION: PARIS METROPOLITAN AREA
# ==============================================================================
# Paris Metropolitan (Île-de-France) bounds in Lambert93 (EPSG:2154)
# Center: ~655000, 6862000 — Extend ~30km radius for greater Paris
# ==============================================================================

predict_region(
  region_name = "Paris Metropolitan Area",
  bbox = c(xmin = 625000, ymin = 6832000, xmax = 685000, ymax = 6892000),
  output_filepath = CONFIG$PARIS_PREDICTION_FILEPATH
)
