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

# Output directories
OSM_PBF_DIR  <- file.path("data", "input", "osm", "pbf")
OSM_GPKG_DIR <- file.path("data", "input", "osm", "gpkg")

# Overwrite existing files
OVERWRITE <- TRUE

# ------------------------------------------------------------------------------
# Safety checks
# ------------------------------------------------------------------------------


pipeline_message(text = "Loading packages and installing missing packages", 
                 level = 1, progress = "start", process = "install")


dir.create(OSM_PBF_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OSM_GPKG_DIR, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(OSM_PBF_DIR)) {
  pipeline_message(
    text = "PBF output directory does not exist and could not be created.", 
    level = 4, process = "stop")
}

if (!dir.exists(OSM_GPKG_DIR)) {
  stop("GPKG output directory does not exist and could not be created.")
}

# ------------------------------------------------------------------------------
# Step 1 — Download OSM PBF data
# ------------------------------------------------------------------------------

message("🌍 Step 1: Downloading OpenStreetMap data")

pbf_file <- download_geofabrik_pbf(
  region    = OSM_REGION,
  dest_dir = OSM_PBF_DIR,
  overwrite = OVERWRITE
)

# ------------------------------------------------------------------------------
# Step 2 — Convert PBF to GeoPackage
# ------------------------------------------------------------------------------

message("🔄 Step 2: Converting PBF to GeoPackage")

gpkg_filename <- sub("\\.osm\\.pbf$", ".gpkg", basename(pbf_file))
gpkg_file <- file.path(OSM_GPKG_DIR, gpkg_filename)

convert_pbf_to_gpkg(
  pbf_file  = pbf_file,
  gpkg_file = gpkg_file,
  overwrite = OVERWRITE
)

# ------------------------------------------------------------------------------
# Completion
# ------------------------------------------------------------------------------

message("✅ OSM download and conversion completed successfully")
message("   Region : ", OSM_REGION)
message("   PBF    : ", pbf_file)
message("   GPKG   : ", gpkg_file)


pipeline_message(text = "OpenStreetMap data successfully downloaded from 
                        Geofabrik and converted into a GeoPackage", 
                 level = 0, progress = "end", process = "valid")