# Road Traffic & Environmental Modelling Pipeline

This repository contains a modular R pipeline designed to build **road-level datasets from *OpenStreetMap* and *AVATAR* traffic measurements**, for noise modelling applications.

The pipeline is designed to run both locally and on HPC environments, and produces spatial datasets ready for feature engineering and predictive modelling.

## Pipeline overview

The pipeline is executed through a master script (```run_pipeline.R```) and is composed of several independent stages:

1. Environment setup
2. OSM road network processing
3. AVATAR traffic data download and matching
4. Data integration
5. Feature engineering
6. Machine learning model training
7. Prediction (mode-dependent)

## 01_setup_minimal.R - Environment setup (local / HPC)

This script initializes the execution environment and ensures all required dependencies are available.

In summary, it:

- Detects the execution context (local or hpc)
- **Local mode**
    - Uses ```renv``` to ensure full package reproducibility
    - Restores or initializes the project environment
- **HPC mode**
    - Uses the user library (```~/Rlibs```)
    - Avoids ```renv``` to remain compatible with shared clusters
- Installs missing R packages automatically
- Loads all required libraries silently
- Creates required directory structure:
    - ```data/```
    - ```data/avatar/```
    - ```rdsFiles/```
    - ```output/```
- Defines a global configuration object (```CONFIG```) containing:
    - Time period for AVATAR data
    - Buffer distances
    - Target CRS
    - Verbosity and download flags

➡️ This stage guarantees a consistent and reproducible runtime environment.

## 02_osm_processing_minimal.R - OSM road network processing

This script builds an **enriched road network dataset from *OpenStreetMap***, designed for modelling, traffic analysis, noise studies and machine learning.

In summary, it:

1. **Loads and filters the OSM road network**
    - Keeps only relevant road types (motorway, primary, secondary, residential, etc.)
2. **Associates each road segment with a commune typology**
    - Spatial join with commune polygons (DEGRE classification)
    - Chunk-based processing to avoid memory issues
    - Nearest-neighbour attribution for road segments outside polygons
3. **Builds a road network graph**
    - Roads are represented as edges
    - Intersections are represented as nodes
    - Computes topological network metrics:
        - Connectivity
        - Betweenness centrality
        - Closeness centrality
        - PageRank
4. **Extracts advanced OSM attributes**
    - Number of lanes
    - Speed limits
    - One-way information
    - Tunnels, bridges, lighting, etc.
    - Fast extraction using vectorised regular expressions
5. **Merges all information into a single dataset**
    - Road geometries
    - OSM attributes
    - Urban context
    - Network metrics

➡️ Final output: a GeoPackage containing a fully enriched road network, ready for feature engineering and machine learning.

## 03_avatar_download_minimal.R - AVATAR traffic data download and matching

This script corresponds to Stage 3 of the pipeline and handles the integration of **AVATAR traffic measurement points and time series data**.

In summary, it performs four main tasks:

1. **Load the enriched road network**
    - Loads the OSM dataset produced in Stage 2
    - Uses a projected CRS (Lambert-93)
2. **Download or reload AVATAR count points**
    - Downloads count points from the AVATAR API (if missing or forced)
    - Converts them into spatial objects (```sf```)
    - Preserves essential metadata:
        - Count point ID
        - Lane number (directional)
        - Geographic position
3. **Match AVATAR count points to OSM road segments**
    - Creates a 50 m buffer around each count point
    - Identifies intersecting road segments
    - If multiple candidates exist, selects the closest road
    - Filters the OSM network to keep only roads with actual measurements
4. **Download AVATAR traffic data**
    - Downloads hourly aggregated traffic data in chunks
    - Handles:
        - API rate limits
        - Network failures with retry logic
        - Missing or corrupted files
        - Merges all CSV chunks into a single dataset
        - Saves the final dataset as an ```.rds``` file

➡️ Final output: a cleaned and consolidated traffic dataset aligned with the OSM road network.

## run_pipeline.R - Master pipeline script

This script orchestrates the full pipeline execution.

In summary, it:

- Parses command-line arguments to select a run mode:
    - Nantes
    - Paris
    - Sensors
- Defines and exposes global data directories
- Runs all pipeline stages sequentially:
    1. Setup
    2. OSM processing
    3. AVATAR data download
    4. Data integration
    5. Feature engineering
    6. Machine learning model training
- Executes a mode-specific prediction stage:
    - Nantes area
    - Paris area
    - Sensor-based predictions

➡️ This script ensures reproducibility, modularity and scalability across study areas and execution environments.

## main.R

This is the main script of the project and is designed to be launched as:
```bash
R --vanilla -f main.R --args <nantes|paris|sensors>
```

## bootstrap.R

## config_pipeline.R