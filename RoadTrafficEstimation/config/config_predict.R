# ==============================================================================
# Prediction pipeline configuration
# ==============================================================================

# Prediction paths and filenames
PREDICTION_DIR <- file.path("data", "prediction")

#' Generate prediction file paths based on spatial extent and mode
#'
#' This function creates a standardized naming scheme for prediction outputs,
#' consistent with the training pipeline. All output files are tagged with the
#' current MODE (e.g., "nantes", "paris", "pemb", "france") to avoid collisions
#' when running multiple regions.
#'
#' For regional predictions (Nantes, Paris, PEMB, sensors):
#'   - Single output file: 07_predictions_{mode}.gpkg
#'
#' For France-wide tiled predictions:
#'   - Geometry layer: 07_predictions_{mode}_network.gpkg
#'   - Temporal chunks: 07_predictions_{mode}_traffic_{CHUNK}.{ext}
#'       where {CHUNK} is DEN, hourly, hourly_wd, or hourly_we
#'       and {ext} is 'gpkg' for geometry, 'csv' for traffic data
#'
#' @param extent Character. Spatial extent: "sensors", "nantes", "paris", "pemb",
#'   or "france".
#' @param mode Character. Pipeline mode (typically from MODE variable: "nantes",
#'   "paris", "pemb", "france", "sensors", "all", or "test").  Default: "all".
#' @return List of file paths for the given extent.
build_prediction_filepaths <- function(extent, mode = NULL) {
  if (is.null(mode) || !nzchar(mode)) {
    mode <- if (exists("MODE") && nzchar(MODE)) MODE else "all"
  }

  switch(extent,
    sensors = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    nantes = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    paris = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    pemb = list(
      all = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode))
    ),
    france = list(
      output_dir = file.path(PREDICTION_DIR, mode),
      geom = file.path(PREDICTION_DIR, mode,
                       sprintf("07_predictions_%s_network.gpkg", mode)),
      den = file.path(PREDICTION_DIR, mode,
                      sprintf("07_predictions_%s_traffic_DEN.csv.gz", mode)),
      hourly = file.path(PREDICTION_DIR, mode,
                         sprintf("07_predictions_%s_traffic_hourly.csv.gz", mode)),
      hourly_wd = file.path(PREDICTION_DIR, mode,
                            sprintf("07_predictions_%s_traffic_hourly_wd.csv.gz", mode)),
      hourly_we = file.path(PREDICTION_DIR, mode,
                            sprintf("07_predictions_%s_traffic_hourly_we.csv.gz", mode))
    ),
    stop("Unknown extent: ", extent, ". Valid: sensors, nantes, paris, pemb, france")
  )
}

# Use the MODE variable to tag outputs (same as in training pipeline)
mode_suffix <- if (exists("MODE") && nzchar(MODE)) MODE else "all"

# Build all prediction paths for direct CONFIG access (backward-compatible)
CONFIG_PREDICT <- list(
  
  # Directories
  PREDICTION_DIR = PREDICTION_DIR,
  FRANCE_OUTPUT_DIR = file.path(PREDICTION_DIR, mode_suffix),
  
  # ============================================================================
  # Regional predictions (single file per extent)
  # ============================================================================
  
  # Sensors (all noise monitoring stations)
  SENSORS_ALL_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode_suffix)),
  
  # Nantes  
  NANTES_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode_suffix)),
  
  # Paris
  PARIS_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode_suffix)),
  
  # PEMB (Paris Est Marne & Bois)
  PEMB_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, sprintf("07_predictions_%s.gpkg", mode_suffix)),
  
  # ============================================================================
  # France tiled predictions (geometry + temporal chunks)
  # ============================================================================
  
  # Geometry layer (single file, appended tile-by-tile)
  FRANCE_GEOMETRY_FILEPATH = file.path(PREDICTION_DIR, mode_suffix,
                                       sprintf("07_predictions_%s_network.gpkg", mode_suffix)),
  
  # Traffic attributes split by temporal chunk (CSV.GZ format for efficiency)
  FRANCE_TRAFFIC_DEN_FILEPATH = file.path(PREDICTION_DIR, mode_suffix,
                                          sprintf("07_predictions_%s_traffic_DEN.csv.gz", mode_suffix)),
  FRANCE_TRAFFIC_HOURLY_FILEPATH = file.path(PREDICTION_DIR, mode_suffix,
                                             sprintf("07_predictions_%s_traffic_hourly.csv.gz", mode_suffix)),
  FRANCE_TRAFFIC_HOURLY_WD_FILEPATH = file.path(PREDICTION_DIR, mode_suffix,
                                                sprintf("07_predictions_%s_traffic_hourly_wd.csv.gz", mode_suffix)),
  FRANCE_TRAFFIC_HOURLY_WE_FILEPATH = file.path(PREDICTION_DIR, mode_suffix,
                                                sprintf("07_predictions_%s_traffic_hourly_we.csv.gz", mode_suffix)),
  
  # Legacy single-file export (rarely used; for testing only)
  FRANCE_PREDICTION_FILEPATH = file.path(PREDICTION_DIR, 
                                         "france",
                                         sprintf("07_predictions_france_%s.gpkg", mode_suffix))
)