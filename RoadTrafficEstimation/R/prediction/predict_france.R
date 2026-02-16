# ==============================================================================
# PREDICTION: FRANCE ENTIÈRE (TILED + TEMPORAL CHUNKS)
# ==============================================================================
# Predicts traffic for all of France using spatial tiling (200 km tiles)
# and exports geometry separately from temporal traffic data.
#
# Output files (in CONFIG$FRANCE_OUTPUT_DIR):
#   - 07_france_network.gpkg             — road geometry + static attributes
#   - 07_france_traffic_DEN.gpkg         — D/E/N periods (3 periods)
#   - 07_france_traffic_hourly.gpkg      — h0..h23 (24 periods)       [optional]
#   - 07_france_traffic_hourly_wd.gpkg   — h0_wd..h23_wd (24 periods) [optional]
#   - 07_france_traffic_hourly_we.gpkg   — h0_we..h23_we (24 periods) [optional]
#
# Join key: osm_id (present in all files)
#
# Disk estimates (for 4.09M roads):
#   Geometry:   ~1.6 GB
#   DEN:        ~1.1 GB
#   hourly:     ~8.5 GB each (×3 = ~25.5 GB)
#   Total DEN:  ~2.7 GB   |   Total all: ~28 GB
#
# Set chunks parameter to control which temporal files are exported.
# ==============================================================================

# Default: DEN only (~2.7 GB total).
# For all chunks, use: chunks = c("DEN", "hourly", "hourly_wd", "hourly_we")
predict_france_tiled(
  config     = CONFIG,
  tile_size_m = 200000,
  chunks     = c("DEN")
)
