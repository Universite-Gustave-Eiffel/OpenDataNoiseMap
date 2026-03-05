# ==============================================================================
# PREDICTION: PEMB (PARIS EST MARNE & BOIS)
# ==============================================================================
# Covered area: Eastern Paris - Vincennes, Nogent-sur-Marne, Champigny,
#   Fontenay-sous-Bois, Créteil, Maisons-Alfort, Joinville, Saint-Mandé,
#   Bois de Vincennes, vallée de la Marne.
# Bounds in Lambert93 (EPSG:2154)
# ==============================================================================

pemb_output_config <- build_prediction_filepaths(extent = "pemb", mode = mode_suffix)

predict_traffic(
  region_name = "PEMB (Paris Est Marne & Bois)",
  cfg = CFG,
  bbox = c(xmin = 654892, ymin = 6852748, xmax = 671006, ymax = 6862393),
  output_config = list(filepath = pemb_output_config$all),
  method = "region"
)
