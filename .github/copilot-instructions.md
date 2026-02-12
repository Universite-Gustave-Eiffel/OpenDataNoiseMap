# Copilot Instructions — OpenDataNoiseMap

## Project Overview

Environmental noise exposure estimation combining road traffic prediction (R/XGBoost) with noise propagation modeling (NoiseModelling/CNOSSOS-EU). Two independent modules:

- **`RoadTrafficEstimation/`** — R pipeline: OSM + AVATAR sensor data → XGBoost traffic models → spatiotemporal predictions
- **`NoiseMapProcess/`** — Bash orchestration: GeoClimate + NoiseModelling chain for noise map computation

## RoadTrafficEstimation Architecture

### 3-Phase Pipeline

```
Phase 1: Data Preparation  (scripts 01–06 in R/data_preparation/)
Phase 2: Model Training     (R/model_training/train_xgboost_models.R)
Phase 3: Prediction          (R/prediction/predict_{nantes,paris,pemb,sensors}.R)
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

**Feature matrix encoding**: Both training and prediction use `Matrix::sparse.model.matrix()` with the same formula. This is critical — never use `model.matrix()` in prediction as it encodes factor dummies differently.

### Code Organization Rules

- **Bootstrap** (`bootstrap.R`): Loads all core libraries (`sf`, `data.table`, `dplyr`, `tidyr`, `xgboost`) via `suppressPackageStartupMessages()`, then sources all `R/utils_*.R`. Do NOT add `source()` or `library()` calls in utility modules — they are already loaded by bootstrap.
- **Utility modules** (`R/utils_*.R`): Pure functions, no side effects. Each phase has its own utils file (`utils_data_preparation.R`, `utils_model_training.R`, `utils_prediction.R`). Also: `utils_avatar.R` (API + aggregation), `utils_osm.R` (download + feature engineering), `utils_io.R` (logging + memory), `utils_sf.R` (spatial helpers), `utils_plots.R` (figures).
- **Phase scripts** (`R/data_preparation/`, `R/model_training/`, `R/prediction/`): Procedural scripts sourced sequentially by `run_pipeline.R`. They read from `CONFIG` and call utility functions.
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
predict_*.R             → 07_predictions_*.gpkg
```

### External Dependencies

- **AVATAR API** (`avatar.cerema.fr`): Traffic sensor data. Requires `AVATAR_API_TOKEN` in `.Renviron`. Downloads are chunked and cached to `data/output/avatar/`.
- **OSM PBF data**: France road network from Geofabrik, stored in `data/osm/pbf/`.
- **INSEE commune typology**: `data/insee/COMMUNE_TYPO_DENSITE.shp` for urban density classification (DEGRE).

### Common Pitfalls

- `DEFAULT_VEHICLE_SPEED` (in `config_pipeline.R`) imputes maxspeed for roads without OSM data — must be realistic (currently 50 km/h).
- Division by zero in truck percentage: always guard `aggregate_flow > 0` before computing `aggregate_flow_trucks / aggregate_flow`.
- `sprintf()` format strings must match argument count exactly — R silently drops extra `%s` but crashes on missing ones.
- When adding a new output file, update both `config_pipeline.R` (path definition + CONFIG list entry) AND `TEST_CONFIG.R` (test path override).
- All GPKG exports must call `add_period_datetime_columns()` before `st_write()` — this is the QGIS interop contract.
- Avatar download can OOM on machines with < 16 GB RAM — use `check_memory_available()` and the disk-based fallback merge in `04_avatar_download.R`.
- Prediction loads the France GPKG with `wkt_filter` (spatial filter at GDAL level) — never load the full 1.7 GB file into RAM unless absolutely necessary.
