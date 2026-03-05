# ==============================================================================
# PREDICTION: ALL OF FRANCE (TILED + TEMPORAL CHUNKS)
# ==============================================================================
# This script runs France-wide traffic prediction using spatial tiling to manage
# memory efficiently. The output is split into separate GPKG files by temporal
# chunk (D/E/N, hourly, hourly weekday, hourly weekend) to allow selective
# loading and processing.
#
# Outputs (all tagged with current MODE):
#   - 07_france_network_{mode}.gpkg            — road geometry + static attributes
#   - 07_france_traffic_DEN_{mode}.gpkg        — D/E/N periods (3 periods) ~1 GB
#   - 07_france_traffic_hourly_{mode}.gpkg     — h0..h23 (24 periods) ~8.5 GB
#   - 07_france_traffic_hourly_wd_{mode}.gpkg  — h0_wd..h23_wd (24 periods) ~8.5 GB
#   - 07_france_traffic_hourly_we_{mode}.gpkg  — h0_we..h23_we (24 periods) ~8.5 GB
#
# For noise mapping applications, all temporal chunks are typically needed,
# as they capture the full temporal profile (working hours, evenings, nights,
# weekday vs. weekend variations). Total disk space: ~26 GB before compression.
#
# Note: This is the default implementation. For quick sanity checks on a subset
# of France, use predict_traffic(method="region", bbox=...) instead.
# ==============================================================================

pipeline_message("France-wide traffic prediction (tiled + all temporal chunks)",
                 level = 0, progress = "start", process = "calc")

# Build output paths for France extent
france_output_config <- build_prediction_filepaths(extent = "france", mode = mode_suffix)

# Run tiled prediction with all temporal chunks
# Chunks:
#   - "DEN": Day/Evening/Night periods (compact, ~1 GB, recommended minimum)
#   - "hourly": Generic hourly h0..h23 (all days combined, ~8.5 GB)
#   - "hourly_wd": Weekday hourly h0_wd..h23_wd (~8.5 GB)
#   - "hourly_we": Weekend hourly h0_we..h23_we (~8.5 GB)
#
# For full temporal detail, include all chunks: c("DEN", "hourly", "hourly_wd", "hourly_we")
# For disk/memory constrained environments, use: c("DEN") only
#
predict_traffic(
  region_name = "France",
  cfg = CFG,
  bbox = NULL,
  output_config = france_output_config,
  method = "tiled",
  chunks = c("DEN", "hourly", "hourly_wd", "hourly_we"),  # all temporal chunks
  tile_size_m = 200000  # 200 km tiles
)

pipeline_message("France-wide prediction completed",
                 level = 0, progress = "end", process = "valid")
