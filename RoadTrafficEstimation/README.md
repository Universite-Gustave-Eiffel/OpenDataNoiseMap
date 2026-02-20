# Road Traffic & Environmental Modelling Pipeline

This directory contains a modular **R pipeline** designed to build road-level datasets from **OpenStreetMap** and **AVATAR** traffic measurements for noise modelling applications.

The pipeline is **organized into 3 distinct phases** (Data Preparation → Model Training → Prediction) with clear separation of concerns, phase-prefixed exports, and a central pre-engineered France network layer that eliminates redundant computations.

## Pipeline Architecture

The pipeline follows a **3-phase workflow**:

```
Phase 1: Data Preparation (Orange)
├─ 01_setup_environment.R          (Install packages, create directories)
├─ 02_osm_processing.R             (OSM network + connectivity metrics)
├─ 03_osm_feature_engineering.R    (Feature engineering on entire France network)
├─ 04_avatar_download.R            (Fetch sensor data from AVATAR API)
├─ 05_avatar_aggregation.R         (Hourly aggregation + period ratios)
└─ 06_training_dataset_merge.R     (Merge Avatar + pre-engineered France layer)
                                   Output: 05_training_dataset.gpkg

                                   ↓

Phase 2: Model Training (Blue)
└─ train_xgboost_models.R          (Train up to 81 XGBoost models: 3 base + 78 ratios)
                                   Output: 06_xgboost_trained_models.rds

                                   ↓

Phase 3: Prediction (Green)
├─ predict_nantes.R                (Generate predictions for Nantes area)
├─ predict_paris.R                 (Generate predictions for Paris area)
└─ predict_sensors.R               (Generate predictions for 13+ sensor networks)
                                   Output: 07_predictions_*.gpkg
```

### Central Pivot Layer: Pre-Engineered France Network

**Key Innovation**: The file `02_osm_network_france_engineered.gpkg` is created in Phase 1 (step 3) and serves as a centralized data source for all downstream prediction scripts. This eliminates redundant feature engineering:

- **Created by**: `03_osm_feature_engineering.R`
- **Contains**: 
  - OSM road geometry for entire France
  - Network connectivity metrics (betweenness, closeness, pagerank)
  - Urban density indicator (DEGRE communes)
  - Engineered features (highway factor, ref_letter, first_word, oneway, lanes, speed)
- **Used by**: `06_training_dataset_merge.R`, `predict_nantes.R`, `predict_paris.R`, `predict_sensors.R`, `predict_pemb.R`, `predict_france.R`
- **Benefit**: Features computed once, not redundantly in each prediction script

## Project Structure

```
RoadTrafficEstimation/
│
├── bootstrap.R                              # Bootstrapping helper (sourced first)
├── config_pipeline.R                        # Configuration & export paths
├── main.R                                   # Alternative entry point
├── run_pipeline.R                           # Master orchestrator (--phase/--mode/--test)
├── README.md                                # This file
│
├── R/
│   ├── data_preparation/                    # Phase 1: Data Preparation Scripts
│   │   ├── 01_setup_environment.R           # Install packages, create directories
│   │   ├── 02_osm_processing.R              # OSM network + connectivity metrics
│   │   ├── 03_osm_feature_engineering.R     # Feature engineering (entire France)
│   │   ├── 04_avatar_download.R             # Fetch sensor data from AVATAR API
│   │   ├── 05_avatar_aggregation.R          # Aggregate to periods + compute ratios
│   │   └── 06_training_dataset_merge.R      # Merge Avatar + pre-engineered France
│   │
│   ├── model_training/                      # Phase 2: Model Training Scripts
│   │   └── train_xgboost_models.R           # Train up to 81 XGBoost models
│   │
│   ├── prediction/                          # Phase 3: Prediction Scripts
│   │   ├── predict_nantes.R                 # Predict for Nantes bbox
│   │   ├── predict_paris.R                  # Predict for Paris bbox
│   │   └── predict_sensors.R                # Predict for 13+ sensor networks
│   │
│   ├── tests/                               # Unit Tests
│   │   ├── test_data_preparation.R          # Validate Phase 1 outputs
│   │   ├── test_model_training.R            # Validate Phase 2 outputs
│   │   └── test_prediction.R                # Validate Phase 3 outputs
│   │
│   ├── utils_data_preparation.R             # Phase 1 utility functions
│   ├── utils_model_training.R               # Phase 2 utility functions
│   ├── utils_prediction.R                   # Phase 3 utility functions
│   ├── utils_avatar.R                       # AVATAR API utilities
│   ├── utils_io.R                           # I/O utilities
│   ├── utils_osm.R                          # OSM utilities
│   ├── utils_plots.R                        # Plotting utilities
│   └── utils_sf.R                           # Spatial operations utilities
│
├── scripts/
│   ├── run_local.sh                         # Local execution with CLI arguments
│   ├── run_on_hpc_cerema.sh                 # CEREMA HPC execution
│   ├── run_on_hpc_public.sh                 # Public HPC execution
│   └── run_avatar_download.sh               # Avatar download only (login node)
│
└── [Data directories created at runtime]
    ├── 01_osm_network_augmented.gpkg        # OSM network with connectivity + DEGRE
    ├── 02_osm_network_france_engineered.gpkg # Pre-engineered France network (PIVOT)
    ├── 02_imputation_rules_france.rds       # Feature imputation rules for France
    ├── 03_avatar_raw_traffic.rds            # Raw sensor data from AVATAR API
    ├── 03_osm_network_with_avatar_ids.gpkg  # OSM network with sensor IDs
    ├── 04_avatar_aggregated_with_ratios.rds # Aggregated Avatar data + period ratios
    ├── 05_training_dataset.gpkg             # Training dataset (Avatar + engineered features)
    ├── 06_xgboost_trained_models.rds        # Trained XGBoost models (81 total)
    ├── 06_xgboost_feature_info.rds          # Feature info for all models
    ├── 07_predictions_nantes.gpkg           # Nantes area predictions
    ├── 07_predictions_paris.gpkg            # Paris area predictions
    └── 07_predictions_sensors_*.gpkg        # Per-source sensor predictions (13+ files)
```

## Export Naming Convention

All exports follow a **phase-based naming pattern**: `{phase_num}_{descriptive_name}.{ext}`

| Phase | Number | Export Pattern | Examples |
|-------|--------|----------------|----------|
| Data Preparation | 01-05 | `0X_{name}.gpkg` or `.rds` | `02_osm_network_france_engineered.gpkg`, `04_avatar_aggregated_with_ratios.rds` |
| Model Training | 06 | `06_{name}.rds` | `06_xgboost_trained_models.rds` |
| Prediction | 07 | `07_{name}.gpkg` | `07_predictions_nantes.gpkg`, `07_predictions_sensors_all.gpkg` |

### Format Migration
- **All spatial outputs**: GeoPackage (`.gpkg`) for consistency and no column name limitations
- **All models/config**: R data files (`.rds`) for serialization
- **Diagnostic outputs**: PDF (`.pdf`) for data quality diagnostics

## Usage

### Quick Start: Run Full Pipeline

```bash
# Run entire pipeline (all phases, all modes)
./scripts/run_local.sh

# Or from R:
Rscript run_pipeline.R --phase all --mode all
```

### Phase-Based Execution

Execute only specific pipeline phases:

```bash
# Phase 1: Data Preparation (OSM + AVATAR data)
./scripts/run_local.sh --phase preparation --mode all
Rscript run_pipeline.R --phase preparation --mode all

# Phase 2: Model Training (Train XGBoost models)
./scripts/run_local.sh --phase training
Rscript run_pipeline.R --phase training

# Phase 3: Prediction (Generate predictions)
./scripts/run_local.sh --phase prediction --mode nantes
Rscript run_pipeline.R --phase prediction --mode nantes
```

Note: when running `--phase prediction` with a specific `--mode`, the pipeline will
automatically include the preparation and training phases.

### Test Mode (Small Region)

Use `TEST_CONFIG.R` to run a smaller region (Nantes bbox) and write outputs to
`data/output/TEST_OUTPUTS`.

### Mode-Based Execution (Prediction Only)

Generate predictions for specific areas or sensors:

```bash
# Nantes area predictions
./scripts/run_local.sh --phase prediction --mode nantes

# Paris area predictions
./scripts/run_local.sh --phase prediction --mode paris

# All sensor network predictions
./scripts/run_local.sh --phase prediction --mode sensors

# All prediction modes
./scripts/run_local.sh --phase prediction --mode all
```

### Testing

Enable unit tests to validate each phase's outputs:

```bash
# Test after data preparation
./scripts/run_local.sh --phase preparation --test

# Test after model training
./scripts/run_local.sh --phase training --test

# Test after prediction
./scripts/run_local.sh --phase prediction --test

# Test entire pipeline
./scripts/run_local.sh --phase all --test
```

### Direct Script Execution

Run individual scripts directly for debugging:

```bash
# Data Preparation Phase
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/data_preparation/01_setup_environment.R')"
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/data_preparation/02_osm_processing.R')"
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/data_preparation/03_osm_feature_engineering.R')"
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/data_preparation/04_avatar_download.R')"
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/data_preparation/05_avatar_aggregation.R')"
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/data_preparation/06_training_dataset_merge.R')"

# Model Training Phase
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/model_training/train_xgboost_models.R')"

# Prediction Phase
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/prediction/predict_nantes.R')"
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/prediction/predict_paris.R')"
Rscript -e "source('bootstrap.R'); source('config_pipeline.R'); source('R/prediction/predict_sensors.R')"
```

### HPC Execution

#### CEREMA HPC

```bash
sbatch scripts/run_on_hpc_cerema.sh
# Or adapt parameters inside the script: PHASE, MODE, TEST_FLAG
```

#### Public HPC

```bash
sbatch scripts/run_on_hpc_public.sh
# Or adapt parameters inside the script: PHASE, MODE, TEST_FLAG
```

#### Avatar Download (Login Node Only)

Since Avatar data downloads require external API access, this is typically done on login nodes:

```bash
bash scripts/run_avatar_download.sh
# Executes: 01_setup_environment.R → 04_avatar_download.R
```

## Pipeline Dependencies & Data Flow

### Phase 1: Data Preparation

```
01_setup_environment.R
  ↓ (creates directories, installs packages)
02_osm_processing.R
  ↓ (produces: 01_osm_network_augmented.gpkg)
03_osm_feature_engineering.R                          ← KEY INNOVATION
  ↓ (produces: 02_osm_network_france_engineered.gpkg  ← PIVOT LAYER
                02_imputation_rules_france.rds)
             ↓
04_avatar_download.R
  ↓ (produces: 03_avatar_raw_traffic.rds,
                03_osm_network_with_avatar_ids.gpkg)
05_avatar_aggregation.R
  ↓ (produces: 04_avatar_aggregated_with_ratios.rds,
                04_diagnostic_*.pdf)
06_training_dataset_merge.R
  ↓ Loads: 02_osm_network_france_engineered.gpkg (filters to Avatar roads)
  ↓ Produces: 05_training_dataset.gpkg
```

### Phase 2: Model Training

```
05_training_dataset.gpkg
  ↓ (from Phase 1)
train_xgboost_models.R
  ↓ Produces: 06_xgboost_trained_models.rds (81 models)
             06_xgboost_feature_info.rds
```

### Phase 3: Prediction

```
06_xgboost_trained_models.rds + 06_xgboost_feature_info.rds
  ↓ (from Phase 2)
02_osm_network_france_engineered.gpkg                  ← PIVOT LAYER
  ↓ (pre-engineered features for entire France)
predict_nantes.R, predict_paris.R, predict_sensors.R
  ↓ Produce: 07_predictions_*.gpkg files
```

## Key Features

- **Phase-Based Organization**: Clear separation between data preparation, model training, and prediction
- **Pre-Engineered France Network**: Eliminates redundant feature engineering (~500 lines of duplicated code removed)
- **Consistent Export Format**: All spatial outputs in GeoPackage (`.gpkg`) format
- **Phase-Prefix Naming**: All exports follow `{phase}_{description}` pattern for easy tracking
- **CLI Argument Support**: `--phase`, `--mode`, `--test` flags for flexible execution
- **Unit Tests**: Comprehensive test suites for each phase (validation, consistency checks)
- **Sub-Module Utils**: Modular utility functions organized by phase (utils_data_preparation.R, utils_model_training.R, utils_prediction.R)
- **HPC Ready**: Compatible with CEREMA and public HPC environments via SLURM
- **Automated Data Integration**: Seamless OSM + AVATAR + spatial feature integration

## Technical Specifications

### Environment

- **Language**: R (≥4.0)
- **Required Libraries**: 
  - Spatial: `sf`, `sp`, `rgdal`, `raster`
  - Data: `data.table`, `tidyr`, `tidyverse`
  - ML: `xgboost`, `caret`
  - Network: `igraph`, `tidygraph`
  - Utilities: `rgeos`, `units`, `lubridate`
  - API: `httr`, `jsonlite`

### Data Specifications

- **Coordinate Reference System**: Lambert-93 (EPSG:2154) throughout
- **Network Coverage**: OSM roads for entire France + road segments around sensors/prediction areas
- **Temporal Coverage**: AVATAR sensor data aggregated to hourly then daily periods
- **Period Types**: 
  - Base (D) - Daytime (6am-10pm)
  - Ratio models: Night (N), Early Morning (EM), Peak Morning (PM), Peak Evening (PE), Evening (E)

### Model Configuration

- **Model Type**: XGBoost regression
- **Target Variables**: 
  - Traffic flow (vehicles/hour)
  - Truck percentage (%)
  - Speed (km/h)
- **Model Suite**: 81 total models
  - 3 base models (flow_D, truck_pct_D, speed_D)
  - 78 ratio models (3 targets × 26 ratios per target)
- **Features**: 25+ from OSM + connectivity + AVATAR data

## Troubleshooting

### Missing Data Files

If Phase 1 outputs are missing:

```bash
# Re-run full data preparation
./scripts/run_local.sh --phase preparation --mode all

# Or individual steps:
Rscript R/data_preparation/01_setup_environment.R
Rscript R/data_preparation/02_osm_processing.R
Rscript R/data_preparation/03_osm_feature_engineering.R
Rscript R/data_preparation/04_avatar_download.R
Rscript R/data_preparation/05_avatar_aggregation.R
Rscript R/data_preparation/06_training_dataset_merge.R
```

### Model Training Failures

Ensure Phase 1 outputs exist (especially `05_training_dataset.gpkg`):

```bash
# Check file exists:
ls -lh outputs/ | grep "05_training_dataset"

# Re-run training:
./scripts/run_local.sh --phase training
```

### Prediction Issues

Ensure both Phase 1 and Phase 2 outputs exist:

```bash
# Check required files:
ls -lh outputs/ | grep "02_osm_network_france_engineered\|06_xgboost_trained_models"

# Re-run prediction:
./scripts/run_local.sh --phase prediction --mode all
```

### Test Validation

Run tests to validate each phase:

```bash
# Test all phases with detailed output:
./scripts/run_local.sh --phase all --test

# Or test individual phases:
./scripts/run_local.sh --phase preparation --test
./scripts/run_local.sh --phase training --test
./scripts/run_local.sh --phase prediction --test
```

## References

- **OSM Data**: [OpenStreetMap](https://www.openstreetmap.org)
- **AVATAR API**: [CEREMA AVATAR Traffic Data](https://avatar.cerema.fr/api/doc)
- **Network Analysis**: `igraph` package for connectivity metrics
- **ML Framework**: `xgboost` for gradient boosting models
- **Spatial Data**: `sf` package for geographic operations (CRS: EPSG:2154)

## License & Attribution

Part of the **Open Data Noise Map** project. See main repository README for license details.

---

**Last Updated**: 2025
**Pipeline Version**: 3.0 (Phase-Based Architecture)
