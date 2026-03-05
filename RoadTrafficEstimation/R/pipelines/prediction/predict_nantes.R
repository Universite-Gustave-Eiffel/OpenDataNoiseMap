# ==============================================================================
# PREDICTION: NANTES MÉTROPOLE
# ==============================================================================
# Nantes Métropole bounds in Lambert93 (EPSG:2154)
# Center: ~347000, 6689000 — Extend ~15km radius
# ==============================================================================

nantes_output_config <- build_prediction_filepaths(extent = "nantes", mode = mode_suffix)

predict_traffic(
  region_name = "Nantes Métropole",
  cfg = CFG,
  bbox = c(xmin = 332000, ymin = 6674000, xmax = 362000, ymax = 6704000),
  output_config = list(filepath = nantes_output_config$all),
  method = "region"
)
