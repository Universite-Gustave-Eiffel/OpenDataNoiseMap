# Road Traffic & Environmental Modelling Pipeline

This directory contains a modular **R pipeline** designed to build road-level datasets from **OpenStreetMap** and **AVATAR** traffic measurements for noise modelling applications.

The pipeline is designed to run both locally and on HPC environments, producing spatial datasets ready for feature engineering and predictive modelling.

## Pipeline Overview

The pipeline is executed through a master script (`run_pipeline.R`) and acts as a comprehensive "Road Traffic Estimation" package. It combines network geometry with existing traffic count data from the [CEREMA AVATAR API](https://avatar.cerema.fr/api/doc).

### Key Features
*   Automated data retrieval from OSM and AVATAR.
*   Network feature engineering with missing attribute imputation.
*   Hierarchical XGBoost modeling for baseline daytime metrics (flow, speed, heavy vehicle share).
*   Temporal ratio learning for consistent time-period profiling.

### Stages
1.  **Environment setup**: Loading libraries and configuration.
2.  **OSM road network processing**: Downloading and cleaning graph data.
3.  **AVATAR traffic data**: Downloading and matching counts to the network.
4.  **Data integration**: Merging traffic data with road segments.
5.  **Feature engineering**: Preparing variables for the ML model.
6.  **Model training**: Training the XGBoost models.
7.  **Prediction**: Generating predictions for sensors or specific areas (e.g., Nantes, Paris).

## Project Structure

```text
.
├── 01_setup_minimal.R               # Environment and library setup
├── 02_osm_processing_minimal.R      # OSM data processing
├── 03_avatar_download_minimal.R     # AVATAR data download
├── 04_data_integration_minimal.R    # Merging OSM and AVATAR data
├── 05_feature_engineering_minimal.R # Feature preparation
├── 06d_xgboost_training_with_ratios.R # ML Model training
├── 07d_predict_nantes_southeast.R   # Prediction example (Nantes)
├── 07d_predict_sensors.R            # Prediction on sensors
├── 07e_predict_paris_area.R         # Prediction example (Paris)
├── bootstrap.R                      # Bootstrapping helper
├── config_pipeline.R                # Configuration variables
├── main.R                           # Main entry point (alternative)
├── run_pipeline.R                   # Master execution script
├── README.md
├── R/                               # Utility functions
│   ├── utils_avatar.R
│   ├── utils_io.R
│   ├── utils_osm.R
│   └── utils_sf.R
└── scripts/                         # Shell scripts for execution
    ├── run_avatar_download.sh
    ├── run_local.sh
    ├── run_on_hpc_cerema.sh
    └── run_on_hpc_public.sh
```

## Usage

To run the pipeline, ensure you have the necessary R packages installed (see `01_setup_minimal.R`).

You can execute the pipeline using the master script:

```bash
Rscript run_pipeline.R
```

Or execute individual steps for debugging or specific tasks.
