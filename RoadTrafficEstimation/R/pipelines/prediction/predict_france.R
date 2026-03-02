# ==============================================================================
# PREDICTION: ALL OF FRANCE
# ==============================================================================
# The France pipeline can run in two flavours:
#   1. simple full‑region prediction (identical to the PEMB script) –
#      loads the entire engineered network in one go and writes a single
#      GeoPackage containing geometry and all period columns.  The output path
#      is controlled by `CFG$FRANCE_PREDICTION_FILEPATH` (added in
#      config_predict.R).
#   2. tiled/temporal‑chunk export – the legacy behaviour retained in
#      `utils_prediction::predict_france_tiled()`.  Geometry is written once to
#      `CFG$FRANCE_GEOMETRY_FILEPATH` and traffic attributes are split into one
#      or more tables (`CFG$FRANCE_TRAFFIC_*_FILEPATH`).  This mode is memory
#      friendly for the full France dataset and is the default when the
developer
#      explicitly calls `predict_france_tiled()`.
#
# The simple mode is useful for quick sanity checks or when you just want a
# single GPKG file (e.g. for testing with `--region test`).  The tiled mode
# must be used when creating the standard production outputs described below.
#
# Legacy tiled outputs (see predict_france_tiled):
#   - 07_france_network.gpkg             — road geometry + static attributes
#   - 07_france_traffic_DEN.gpkg         — D/E/N periods (3 periods)
#   - 07_france_traffic_hourly.gpkg      — h0..h23 (24 periods)       [optional]
#   - 07_france_traffic_hourly_wd.gpkg   — h0_wd..h23_wd (24 periods) [optional]
#   - 07_france_traffic_hourly_we.gpkg   — h0_we..h23_we (24 periods) [optional]
#
# The configuration keys for the tiled export are defined in
# config_predict.R and propagated to TEST_CONFIG.R;
# see also `utils_prediction.R` for the implementation.
#
# We default to the simple full‑region call to make the behaviour consistent
# with predict_pemb.R.  To switch to tiled output uncomment the block at the
# bottom of this file.

# simple full‑region prediction (same pattern as PEMB)
predict_region(
  region_name = "FRANCE",
  bbox = c(xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf),
  output_filepath = CFG$FRANCE_PREDICTION_FILEPATH,
  cfg = CFG
)

# if you need the geometry‑separated, chunked files use the tiled helper
# instead of the call above.  the parameters and output paths are all pulled
# from `CFG` so you don't need to hard‑code anything.
#
# predict_france_tiled(
#   cfg         = CFG,
#   tile_size_m = 200000,
#   chunks      = c("DEN")           # override as desired
