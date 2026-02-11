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
Phase 3: Prediction          (R/prediction/predict_{nantes,paris,sensors}.R)
```

**Execution chain**: `main.R` → `bootstrap.R` (sources all `R/utils_*.R`) → `config_pipeline.R` (builds global `CONFIG` list) → `run_pipeline.R` (CLI orchestrator).

**Central pivot layer**: `02_osm_network_france_engineered.gpkg` — pre-engineered feature layer for all France, created once in Phase 1 step 3, reused by training merge (step 6) and all prediction scripts. Never duplicate feature engineering.

### Key Conventions

- **CRS**: Always EPSG:2154 (Lambert-93). All spatial operations use `sf::st_transform(crs = 2154)`.
- **Config**: All paths and parameters live in the global `CONFIG` list (built in `config_pipeline.R`). Access via `CONFIG$OSM_ROADS_FRANCE_ENGINEERED_FILEPATH`, etc. Never hardcode paths.
- **File naming**: Exports follow `{phase_number}_{description}.{gpkg|rds}` (e.g., `05_training_dataset.gpkg`, `06_xgboost_trained_models.rds`, `07_predictions_nantes.gpkg`).
- **Spatial format**: All spatial outputs use GeoPackage (`.gpkg`), never Shapefile. Models/config use `.rds`.
- **Logging**: Use `pipeline_message(text, level, progress, process)` from `R/utils_io.R` — not `cat()` or bare `message()`. Levels: 0=section header, 1=timed step, 2=timed sub-step. Process icons: `"calc"`, `"load"`, `"save"`, `"info"`, `"valid"`, `"stop"`.
- **Test mode**: `TEST_CONFIG.R` overrides `CONFIG` paths to write to `data/output/TEST_OUTPUTS/`. Activated via `--region test`. Always override all relevant `CONFIG$*` paths when adding new outputs.

### Running the Pipeline

```bash
cd RoadTrafficEstimation
bash scripts/run_local.sh --phase all --mode all                    # Full run
bash scripts/run_local.sh --phase prediction --mode nantes --region test  # Test mode
bash scripts/run_local.sh --phase training --test                   # With unit tests
```

Direct R: `Rscript run_pipeline.R --phase <preparation|training|prediction|all> --mode <nantes|paris|sensors|all> [--region test] [--test]`

### XGBoost Model Structure

222 model configs: 3 base models (`flow_D`, `truck_pct_D`, `speed_D`) + 219 ratio models (73 period ratios × 3 targets). Ratio models predict `target_period / target_D`. Periods: D (daytime base), E, N, h0–h23 (all days), h0_wd–h23_wd (weekday Mon–Fri), h0_we–h23_we (weekend Sat–Sun). Truck ratio models require lower training thresholds (`min_train=10`) due to sparse data. Predictions are clamped: flow ≥ 0, 0 ≤ truck_pct ≤ 100, speed ≥ 0. Day type (`day_type`: wd/we) is assigned in `aggregate_avatar_hourly()` using `lubridate::wday()` (1=Sun, 7=Sat → weekend).

### Code Organization Rules

- **Utility modules** (`R/utils_*.R`): Pure functions, no side effects, sourced by `bootstrap.R` at startup. Each phase has its own utils file (`utils_data_preparation.R`, `utils_model_training.R`, `utils_prediction.R`).
- **Phase scripts** (`R/data_preparation/`, `R/model_training/`, `R/prediction/`): Procedural scripts sourced sequentially by `run_pipeline.R`. They read from `CONFIG` and call utility functions.
- **Tests** (`R/tests/test_*.R`): Validate phase outputs (file existence, column presence, model counts). Run via `--test` flag.
- **Dependencies**: `sf`, `data.table`, `tidyr`, `xgboost`, `igraph`, `httr`, `lubridate`. HPC uses system R libs; local uses `renv`.

### External Dependencies

- **AVATAR API** (`avatar.cerema.fr`): Traffic sensor data. Requires `AVATAR_API_TOKEN` in `.Renviron`. Downloads are chunked and cached to `data/output/avatar/`.
- **OSM PBF data**: France road network from Geofabrik, stored in `data/osm/pbf/`.
- **INSEE commune typology**: `data/insee/COMMUNE_TYPO_DENSITE.shp` for urban density classification (DEGRE).

### Common Pitfalls

- `DEFAULT_VEHICLE_SPEED` (in `config_pipeline.R`) imputes maxspeed for roads without OSM data — must be realistic (currently 50 km/h).
- Division by zero in truck percentage: always guard `aggregate_flow > 0` before computing `aggregate_flow_trucks / aggregate_flow`.
- `sprintf()` format strings must match argument count exactly — R silently drops extra `%s` but crashes on missing ones.
- When adding a new output file, update both `config_pipeline.R` (path definition + CONFIG list entry) AND `TEST_CONFIG.R` (test path override).
