# ==============================================================================
# PREDICTION: PEMB (PARIS EST MARNE & BOIS)
# ==============================================================================
# Zone couverte: Est parisien — Vincennes, Nogent-sur-Marne, Champigny,
#   Fontenay-sous-Bois, Créteil, Maisons-Alfort, Joinville, Saint-Mandé,
#   Bois de Vincennes, vallée de la Marne.
# Bounds in Lambert93 (EPSG:2154)
# ==============================================================================

predict_region(
  region_name = "PEMB (Paris Est Marne & Bois)",
  bbox = c(xmin = 654892, ymin = 6852748, xmax = 671006, ymax = 6862393),
  output_filepath = CONFIG$PEMB_PREDICTION_FILEPATH
)
