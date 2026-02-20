# ==============================================================================
# PREDICTION: NANTES MÉTROPOLE
# ==============================================================================
# Nantes Métropole bounds in Lambert93 (EPSG:2154)
# Center: ~347000, 6689000 — Extend ~15km radius
# ==============================================================================

predict_region(
  region_name = "Nantes Métropole",
  bbox = c(xmin = 332000, ymin = 6674000, xmax = 362000, ymax = 6704000),
  output_filepath = CONFIG$NANTES_PREDICTION_FILEPATH
)
