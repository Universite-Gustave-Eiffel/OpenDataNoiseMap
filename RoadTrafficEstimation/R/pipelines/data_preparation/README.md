# 🧩 Data Preparation Pipeline

This pipeline prepares all spatial and traffic data required for training and predicting road traffic noise models in the **OpenDataNoiseMap** project.

It integrates **OpenStreetMap (OSM)** road networks with **AVATAR traffic data**, performs spatial and temporal aggregation, and generates fully enriched datasets ready for machine learning in the context of road traffic noise mapping.

The pipeline is executed via the unified launcher:

``` bash
scripts/run_pipeline.sh data_prep
```

which internally triggers:

```         
main.R → pipelines/data_preparation/*.R
```

## 🎯 Objectives

The data preparation pipeline aims to:

-   build a clean and enriched **road network graph** from OSM,
-   integrate **traffic measurements** from Avatar data,
-   compute **topological and traffic features** at road-segment level,
-   produce **model-ready datasets** for training and predictiton pipelines,
-   ensure **reproducibility** through centralized configuration.

## ▶️ Entry point

The pipeline is registered under the mode: `MODE = "data_prep"` and is executed in `main.R` by:

``` r
source("pipelines/data_preparation/02_osm_processing.R")
source("pipelines/data_preparation/03_avatar_download.R")
source("pipelines/data_preparation/04_data_integration.R")
source("pipelines/data_preparation/05_feature_engineering.R")
```

⚠️ The script `01_osm_download.R` is optional and can be run manually if OSM data need to be re-downloaded.

------------------------------------------------------------------------

## 🔢 Pipeline steps

### 01 - 01_osm_download.R (optional)

#### 🎯 Purpose

Download and preprocess raw OpenStreetMap data.

#### ⚙️️ Main operation

-   Download OSM data (`.osm.pbf`) from Geofabrik,
-   Convert PBF files to GeoPackage format,
-   Prepare spatial layers for further processing.

#### 📥 Inputs

-   Geofabrik OSM servers,
-   Region definition (country / administrative area).

#### 📤 Outputs

-   Raw OSM PBF files (`cfg$OSM_PBF_DIR`),
-   Converted GeoPackages (`cfg$OSM_GPKG_DIR`).

#### 🔧 Functions used (`R/`)

-   download_osm_pbf()
-   convert_pbf_to_gpkg()
-   check_osm_files()

------------------------------------------------------------------------

### 02 - 02_osm_processing.R

#### 🎯 Purpose

Build and enrich the road network graph from OSM data.

#### ⚙️️ Main operation

-   Load OSM road network,
-   Filter relevant road types,
-   Spatial join with communes (urban degree),
-   Build graph representation of the network,
-   Compute topological metrics,
-   Extract attributes from other_tags,
-   Export enriched road network.

#### 📥 Inputs

-   OSM GeoPackages (`*.gpkg`),
-   Commune boundaries.

#### 📤 Outputs

-   Enriched road network in GeoPackage format (`cfg$OSM_DEGRE_FILEPATH`),
-   Connectivity and graph metrics (`cfg$OSM_ROADS_CONNECTIVITY_FILEPATH`).

#### 📊 Computed features

-   Degree,
-   Betweenness,
-   Closeness,
-   PageRank.

Road hierarchy and attributes

#### 🔧 Functions used (`R/`)

-   `load_osm_roads()`
-   `join_communes_to_roads()`
-   `build_road_graph()`
-   `compute_graph_metrics()`
-   `extract_other_tags()`

------------------------------------------------------------------------

### 03 - 03_avatar_download.R

#### 🎯 Purpose

Download, validate, and spatially match AVATAR traffic data to the road network.

#### ⚙️️ Main operations

-   Load AVATAR counting points,
-   Match counting points to OSM road segments,
-   Download traffic time series (chunked),
-   Validate and re-download missing or corrupted chunks,
-   Merge traffic data with road geometry.

#### 📥 Inputs

-   AVATAR API,
-   Enriched OSM road network,
-   Time range defined in configuration.

#### 📤 Outputs

-   AVATAR counting points (`cfg$AVATAR_COUNT_POINTS_FILEPATH`),
-   Raw AVATAR traffic data (`cfg$AVATAR_DATA_FILEPATH`),
-   OSM–AVATAR merged dataset (`cfg$AVATAR_MERGED_WITH_OSM_FILEPATH`).

#### 📊 Traffic variables

-   Vehicle counts,
-   Heavy vehicle ratios,
-   Average speeds,
-   Time stamps.

#### 🔧 Functions used (`R/`)

-   `download_avatar_counts()`
-   `download_avatar_timeseries()`
-   `validate_avatar_chunks()`
-   `match_avatar_to_osm()`

------------------------------------------------------------------------

### 04 - 04_data_integration.R

#### 🎯 Purpose

Aggregate traffic data temporally and compute diagnostic indicators.

#### ⚙️️ Main operations

-   Clean and filter AVATAR data,
-   Aggregate traffic by:
    -   hour,
    -   Day / Evening / Night (D/E/N).
-   Compute relative traffic indicators,
-   Generate diagnostic plots.

#### 📥 Inputs

OSM–AVATAR merged dataset

#### 📤 Outputs

-   Aggregated traffic dataset (`cfg$AVATAR_AGGREGATED_FILEPATH`),
-   Diagnostic figures(`figures/`).

#### 📊 Derived indicators

-   Hourly profiles,
-   D/E/N ratios,
-   Heavy vehicle percentages,
-   Speed distributions.

#### 🔧 Functions used (`R/`)

-   `aggregate_traffic_time()`
-   `compute_den_metrics()`
-   `plot_hourly_profiles()`
-   `check_data_completeness()`

------------------------------------------------------------------------

### 05 - 05_feature_engineering.R

#### 🎯 Purpose

Generate final features and datasets for model training and predictiton.

#### ⚙️️ Main operations

-   Define imputation rules for missing attributes,
-   Apply default values to roads without sensors,
-   Merge traffic and road features,
-   Validate final datasets,
-   Export modeling-ready data.

#### 📥 Inputs

-   Aggregated traffic dataset,
-   Enriched OSM road network.

#### 📤 Outputs

-   Final road–traffic dataset (RDS) (`cfg$TRAINING_RDS_DATA_FILEPATH`),
-   Spatial dataset (GeoPackage) (`cfg$TRAINING_GPKG_DATA_FILEPATH`),
-   Network-level dataset (`cfg$AVATAR_NETWORK_DATA_FILEPATH`).

#### 📊 Final feature types

-   Geometric features,
-   Topological metrics,
-   Traffic indicators,
-   Temporal aggregates.

#### 🔧 Functions used (`R/`)

-   `apply_default_road_values()`
-   `impute_missing_traffic()`
-   `build_training_dataset()`
-   `export_training_data()`

------------------------------------------------------------------------

### ⚙️ Configuration files

All parameters used by the pipeline are centralized in:

```         
config/
├── config_global.R
├── config_data_prep.R
├── config_training.R
├── config_predict.R
├── mode_registry.R
└── README.md
```

The file `config/README.md` documents:

-   configuration structure (CFG),
-   directory layout,
-   spatial reference systems,
-   temporal aggregation rules,
-   pipeline-specific parameters.

------------------------------------------------------------------------

### 🧰 Utility functions (`R/`)

The `R/` directory contains reusable functions for:

-   API communication (OSM, AVATAR),
-   spatial operations (`sf`),
-   graph analysis (`igraph`),
-   temporal aggregation,
-   data validation,
-   plotting and diagnostics.

These functions are loaded during the bootstrap stage:

``` r
source("bootstrap/bootstrap.R")
```

------------------------------------------------------------------------

### 📁 Related pipelines

The outputs of this pipeline are consumed by:

-   pipelines/training/,
-   pipelines/predict/.

Refer to the corresponding `README.md` files for details on model training and prediction generation.

------------------------------------------------------------------------
