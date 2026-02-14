# Copilot Instructions — OpenDataNoiseMap

## Project Overview

Environmental noise exposure estimation combining road traffic prediction (R/XGBoost) with noise propagation modeling (NoiseModelling/CNOSSOS-EU). Two independent modules:

- **`RoadTrafficEstimation/`** — R pipeline: OSM + AVATAR sensor data → XGBoost traffic models → spatiotemporal predictions → CNOSSOS-EU emission analysis
- **`NoiseMapProcess/`** — Bash orchestration: GeoClimate + NoiseModelling chain for noise map computation

## RoadTrafficEstimation Architecture

### 3-Phase Pipeline + Analysis

```
Phase 1: Data Preparation  (scripts 01–06 in R/data_preparation/)
Phase 2: Model Training     (R/model_training/train_xgboost_models.R)
Phase 3: Prediction          (R/prediction/predict_{nantes,paris,pemb,sensors}.R)
Analysis: Emission analysis  (run_emission_analysis.R, run_flow_comparison.R, etc.)
```

**Execution chain**: `main.R` → `bootstrap.R` (loads libraries + sources all `R/utils_*.R`) → `config_pipeline.R` (builds global `CONFIG` list) → `run_pipeline.R` (CLI orchestrator).

**Central pivot layer**: `02_osm_network_france_engineered.gpkg` — pre-engineered feature layer for all France, created once in Phase 1 step 3, reused by training merge (step 6) and all prediction scripts. Never duplicate feature engineering.

### Key Conventions

- **CRS**: Always EPSG:2154 (Lambert-93). All spatial operations use `sf::st_transform(crs = 2154)`.
- **Config**: All paths and parameters live in the global `CONFIG` list (built in `config_pipeline.R`). Access via `CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH`, etc. Never hardcode paths.
- **File naming**: Exports follow `{phase_number}_{description}.{gpkg|rds}` (e.g., `05_training_dataset.gpkg`, `06_xgboost_trained_models.rds`, `07_predictions_nantes.gpkg`).
- **Spatial format**: All spatial outputs use GeoPackage (`.gpkg`), never Shapefile. Models/config use `.rds`.
- **QGIS datetime fields**: All GPKG exports call `add_period_datetime_columns()` (from `utils_prediction.R`) before `st_write()`. This adds `datetimestart` / `datetimeend` POSIXct columns when a `period` column is present. The function returns data unchanged if no `period` column exists — safe to call on any data. Year-based convention: D/E/N → 1970, h*_wd → 1971-01-01, h*_we → 1972-01-01, h* → 1973-01-01.
- **Logging**: Use `pipeline_message(text, level, progress, process)` from `R/utils_io.R` — not `cat()` or bare `message()`. Levels: 0=section header, 1=timed step, 2=timed sub-step. Process icons: `"calc"`, `"load"`, `"save"`, `"info"`, `"valid"`, `"stop"`.
- **Memory safety**: Use `check_memory_available(operation_name, min_gb, warn_gb)` from `R/utils_io.R` before heavy operations (reading large GPKG, rbindlist, pivot). Reads `/proc/meminfo` on Linux. Use `gc(verbose = FALSE)` + `rm()` after large objects are no longer needed.
- **Test mode**: `TEST_CONFIG.R` overrides `CONFIG` paths to write to `data/output/TEST_OUTPUTS/`. Activated via `--region test`. Always override all relevant `CONFIG$*` paths when adding new outputs.

### Running the Pipeline

```bash
cd RoadTrafficEstimation
bash scripts/run_local.sh --phase all --mode all                    # Full run
bash scripts/run_local.sh --phase prediction --mode pemb            # PEMB only
bash scripts/run_local.sh --phase prediction --mode nantes --region test  # Test mode
bash scripts/run_local.sh --phase training --test                   # With unit tests
```

Direct R: `Rscript run_pipeline.R --phase <preparation|training|prediction|all> --mode <nantes|paris|pemb|sensors|all> [--region test] [--test]`

### Period Definitions

Temporal periods used throughout the pipeline:

| Period | Hours | Description |
|--------|-------|-------------|
| D | 06:00–18:00 | Day (12h) |
| E | 18:00–22:00 | Evening (4h) |
| N | 22:00–06:00 | Night (8h) |
| h0–h23 | hourly | All days combined |
| h0_wd–h23_wd | hourly | Weekday (Mon–Fri) |
| h0_we–h23_we | hourly | Weekend (Sat–Sun) |

75 periods total. Day type (`day_type`: wd/we) is assigned in `aggregate_avatar_hourly()` using `lubridate::wday()` (1=Sun, 7=Sat → weekend).

### XGBoost Model Structure

225 models: 3 base models (`flow_D`, `truck_pct_D`, `speed_D`) + 222 ratio models (74 non-D periods × 3 targets). Ratio models predict `target_period / target_D`. Truck ratio models require lower training thresholds (`min_train=10`) due to sparse data. Predictions are clamped: flow ≥ 0, 0 ≤ truck_pct ≤ 100, speed ≥ 0.

**14 features** (road characteristics from OSM + graph metrics): `highway`, `DEGRE`, `ref_letter`, `first_word`, `oneway_osm`, `lanes_osm`, `speed`, `connectivity`, `betweenness`, `closeness`, `pagerank`, `coreness`, `dead_end_score`, `edge_length_m`. These are identical across feature engineering, training formula, and prediction — enforced via `road_feature_formula` saved in `06_xgboost_feature_info.rds`.

**Feature matrix encoding**: Both training and prediction use `Matrix::sparse.model.matrix()` with the same formula. This is critical — never use `model.matrix()` in prediction as it encodes factor dummies differently. When using the sparse matrix on test/prediction data, factor levels present in training may be absent — use `align_matrix_to_model()` to add zero columns for missing levels and reorder columns to match the model's `feature_names`.

### Training Variants

Three training approaches exist, each producing a separate `.rds` model file:

1. **Standard** (`R/model_training/train_xgboost_models.R`): Standard XGBoost minimizing RMSE on native units. Output: `06_xgboost_trained_models.rds`.

2. **dB-aware** (`train_xgboost_db_aware.R`): Uses CNOSSOS sensitivity weights (|∂Lw/∂target|²) as sample weights so XGBoost's squared-error loss approximates dB² error. Output: `06_xgboost_trained_models_db_aware.rds`. **Result: did not improve over standard** — features are all static/topological and can't capture speed dynamics.

3. **Flow-augmented** (`train_xgboost_flow_augmented.R`): **Best approach.** Trains `flow_D` first, generates out-of-fold predictions (5-fold sensor CV, seed=123) to avoid leakage, then adds `pred_log10_flow` and `flow_per_lane` as extra features for `speed_D` and all `ratio_speed_*` models. Physics motivation: speed = f(flow, capacity) — the fundamental diagram of traffic. Output: `06_xgboost_trained_models_flow_augmented.rds` + `06_flow_augmented_lookup.rds`.

   **Key results vs standard**: MAE 4.27→4.09 dB (−4.1%), motorway MAE 5.34→3.21 (−40%), speed bin 90-110: MAE 5.42→2.79 (−49%).

   **Prediction with flow-augmented models**: Must predict `flow_D` first, then inject `pred_log10_flow` + `flow_per_lane` into the sparse matrix before predicting speed. Each model entry has a `flow_augmented = TRUE/FALSE` flag to indicate whether augmented features are needed.

### CNOSSOS-EU Emission Calculator

`compute_emission_cnossos()` in `R/utils_model_training.R` calls the NoiseModelling 5.x Java batch calculator via `system()`. The Java bridge lives in `NoiseModellingEmission/`:

- `CnossosEmissionBatch.java` — reads CSV (flow, truck_pct, speed) from stdin, outputs dB(A)/m per line
- `noisemodelling-emission-5.0.2-SNAPSHOT.jar` — NoiseModelling emission library
- Jackson JARs — JSON dependency for NoiseModelling

Computes emission over 8 octave bands (63–8000 Hz) with A-weighting, CNOSSOS-EU 2020 coefficients. Used for emission analysis scripts and dB-aware training weights.

### Analysis Scripts

Standalone analysis scripts (run with `Rscript <script>.R`):

- **`run_emission_analysis.R`**: Standard emission dB analysis on test set (bias, MAE, RMSE, per-variable contributions).
- **`run_correlation_analysis.R`**: Correlation between dB error and traffic variables (speed, flow).
- **`run_db_comparison.R`**: 2-way comparison: Standard vs dB-aware models.
- **`run_flow_comparison.R`**: 3-way comparison: Standard vs dB-aware vs Flow-augmented. Produces `figures/09_standard_vs_dbaware_vs_flowaugmented.pdf` (6 pages).
- **`run_flow_augmented_comparison.R`**: 2-way comparison: Standard vs Flow-augmented only.

All comparison scripts reconstruct the same test sensor split (seed=42, 80/20 grouped by `count_point_id`) to ensure fair evaluation.

### Code Organization Rules

- **Bootstrap** (`bootstrap.R`): Loads all core libraries (`sf`, `data.table`, `dplyr`, `tidyr`, `xgboost`) via `suppressPackageStartupMessages()`, then sources all `R/utils_*.R`. Do NOT add `source()` or `library()` calls in utility modules — they are already loaded by bootstrap.
- **Utility modules** (`R/utils_*.R`): Pure functions, no side effects. Each phase has its own utils file (`utils_data_preparation.R`, `utils_model_training.R`, `utils_prediction.R`). Also: `utils_avatar.R` (API + aggregation), `utils_osm.R` (download + feature engineering), `utils_io.R` (logging + memory), `utils_sf.R` (spatial helpers), `utils_plots.R` (figures).
- **Phase scripts** (`R/data_preparation/`, `R/model_training/`, `R/prediction/`): Procedural scripts sourced sequentially by `run_pipeline.R`. They read from `CONFIG` and call utility functions.
- **Training variant scripts** (root): `train_xgboost_db_aware.R`, `train_xgboost_flow_augmented.R` — standalone scripts that source `bootstrap.R` + `config_pipeline.R` and produce alternative model `.rds` files.
- **Analysis scripts** (root): `run_emission_analysis.R`, `run_correlation_analysis.R`, `run_db_comparison.R`, `run_flow_comparison.R`, `run_flow_augmented_comparison.R` — standalone scripts for model evaluation and comparison.
- **Tests** (`R/tests/test_*.R`): Validate phase outputs (file existence, column presence, model counts). Run via `--test` flag.
- **Dependencies**: `sf`, `data.table`, `dplyr`, `tidyr`, `xgboost`, `igraph`, `httr`, `lubridate`, `Matrix`. HPC uses system R libs; local uses `renv`.

### Data Flow

```
01_setup_environment.R  → [PBF download + GPKG conversion]
02_osm_processing.R     → 01_osm_network_augmented.gpkg (OSM + DEGRE + connectivity)
03_feature_engineering.R → 02_osm_network_france_engineered.gpkg (+ imputation rules .rds)
04_avatar_download.R    → 03_avatar_raw_traffic.rds + 03_osm_network_with_avatar_ids.gpkg
05_avatar_aggregation.R → 04_avatar_aggregated_with_ratios.rds
06_training_merge.R     → 05_training_dataset.rds/.gpkg
train_xgboost_models.R  → 06_xgboost_trained_models.rds + 06_xgboost_feature_info.rds
train_xgboost_db_aware.R      → 06_xgboost_trained_models_db_aware.rds
train_xgboost_flow_augmented.R → 06_xgboost_trained_models_flow_augmented.rds
                                  + 06_flow_augmented_lookup.rds
predict_*.R             → 07_predictions_*.gpkg
run_*_comparison.R      → figures/08_*.pdf, figures/09_*.pdf
```

### External Dependencies

- **AVATAR API** (`avatar.cerema.fr`): Traffic sensor data. Requires `AVATAR_API_TOKEN` in `.Renviron`. Downloads are chunked and cached to `data/output/avatar/`.
- **OSM PBF data**: France road network from Geofabrik, stored in `data/osm/pbf/`.
- **INSEE commune typology**: `data/insee/COMMUNE_TYPO_DENSITE.shp` for urban density classification (DEGRE).
- **NoiseModelling 5.x**: Java JAR in `NoiseModellingEmission/`. Requires Java runtime. Used by `compute_emission_cnossos()` for CNOSSOS-EU 2020 emission calculation.

### Common Pitfalls

- `DEFAULT_VEHICLE_SPEED` (in `config_pipeline.R`) imputes maxspeed for roads without OSM data — must be realistic (currently 50 km/h).
- Division by zero in truck percentage: always guard `aggregate_flow > 0` before computing `aggregate_flow_trucks / aggregate_flow`.
- `sprintf()` format strings must match argument count exactly — R silently drops extra `%s` but crashes on missing ones.
- When adding a new output file, update both `config_pipeline.R` (path definition + CONFIG list entry) AND `TEST_CONFIG.R` (test path override).
- All GPKG exports must call `add_period_datetime_columns()` before `st_write()` — this is the QGIS interop contract.
- Avatar download can OOM on machines with < 16 GB RAM — use `check_memory_available()` and the disk-based fallback merge in `04_avatar_download.R`.
- Prediction loads the France GPKG with `wkt_filter` (spatial filter at GDAL level) — never load the full 1.7 GB file into RAM unless absolutely necessary.
- **`sparse.model.matrix()` factor alignment**: Test/prediction data may lack factor levels present in training (e.g., `first_wordimpasse`). Always use `align_matrix_to_model(mat, model)` which adds zero columns for missing features AND reorders to match model's `feature_names`. Never assume column order matches after `cbind`.
- **Flow-augmented prediction order**: When using flow-augmented models, ALWAYS predict `flow_D` first, then inject `pred_log10_flow` + `flow_per_lane` into the sparse matrix before predicting speed. Check `models_list[[key]]$flow_augmented` flag to know which models need augmented features.
- **Train/test split reproducibility**: Sensor split uses `set.seed(42)` with 80/20 grouped by `count_point_id`. CV folds for out-of-fold predictions use `set.seed(123)`. These seeds must remain consistent across all training variants and comparison scripts.
