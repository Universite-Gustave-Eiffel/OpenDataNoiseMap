# ==============================================================================
# Download and convert OpenStreetMap data from Geofabrik
# ==============================================================================
#
# This script:
#   1. Downloads OpenStreetMap PBF data from Geofabrik
#   2. Converts the downloaded PBF file into a GeoPackage (GPKG)
# ==============================================================================

pipeline_message(text = "Download OpenStreetMap data from Geofabrik and convert 
                        into a GeoPackage", 
                 level = 0, progress = "start", process = "download")

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Region to download: "France" or a valid French region name
OSM_REGION <- "France"

# Overwrite existing files
OVERWRITE <- FALSE

# ------------------------------------------------------------------------------
# Safety checks
# ------------------------------------------------------------------------------

pipeline_message(text = "Creating output directories", 
                 level = 1, progress = "start", process = "install")

dir.create(cfg_data$OSM_PBF_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg_data$OSM_GPKG_DIR, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(cfg_data$OSM_PBF_DIR)) {
  pipeline_message(
    text = "PBF output directory does not exist and could not be created.", 
    process = "stop")
}
if (!dir.exists(cfg_data$OSM_GPKG_DIR)) {
  pipeline_message(
    text = "GPKG output directory does not exist and could not be created.", 
    process = "stop")
}

pipeline_message(text = "Output directories successfully created", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Step 1 — Download OSM PBF data
# ------------------------------------------------------------------------------

pipeline_message(text = "Downloading OpenStreetMap data", 
                 level = 1, progress = "start", process = "download")

pbf_file <- download_geofabrik_pbf(
  region    = OSM_REGION,
  dest_dir = cfg_data$OSM_PBF_DIR,
  overwrite = OVERWRITE
)

pipeline_message(text = "OSM data successfully downloaded", 
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Step 2 — Convert PBF to GeoPackage
# ------------------------------------------------------------------------------

pipeline_message(text = "Converting PBF to GeoPackage", 
                 level = 1, progress = "start", process = "calc")

gpkg_filename <- sub("\\.osm\\.pbf$", ".gpkg", basename(pbf_file))
gpkg_file <- file.path(cfg_data$OSM_GPKG_DIR, gpkg_filename)

convert_pbf_to_gpkg(
  pbf_file  = pbf_file,
  gpkg_file = gpkg_file,
  overwrite = OVERWRITE
)

pipeline_message(text = "PBF successfully converted to GeoPackage", 
                 level = 1, progress = "end", process = "valid")


pipeline_message(text = sprintf("OpenStreetMap data successfully downloaded from 
                                Geofabrik, converted into a GeoPackage, and 
                                saved into file %s", rel_path(gpkg_file)), 
                 level = 0, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Completion
# ------------------------------------------------------------------------------
message("   Region : ", OSM_REGION)
message("   PBF    : ", pbf_file)
message("   GPKG   : ", gpkg_file)


pipeline_message(text = "", 
                 level = 0, progress = "end", process = "valid")