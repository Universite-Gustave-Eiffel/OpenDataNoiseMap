# CLEANUP & TEST REGION SETUP - Complete

## ✅ Cleanup Completed

### Removed Dead Code
- **Deleted**: 3 `*_OLD.R` backup scripts from R/prediction/
  - `predict_nantes_OLD.R` ✓
  - `predict_paris_OLD.R` ✓
  - `predict_sensors_OLD.R` ✓

### Code Review
- Scanned all phase scripts for dead code (commented lines, debug statements)
- Confirmed: Scripts are clean with no major dead code found
- Inline comments are minimal and purposeful

---

## ✅ Test Region Configuration Created

### New File: `TEST_CONFIG.R`
Configuration for rapid testing on small France region (50km × 50km around Nantes):

**Coverage**:
- Region: Nantes center + 25km buffer (2,500 km²)
- Cities: Nantes, Saint-Nazaire, Saint-Herblain, Orvault, Rezé
- Expected runtime: **~10-15% of full France pipeline**

**Settings**:
- Time period: June 2023 (1 month reduced from 12 months)
- Training data sampling: 80% (faster model training)
- XGBoost lighter config: 500 boosting rounds (vs 1500)
- Shallower trees: max_depth 5-6 (vs 10)
- Output directory: `data/output/TEST_OUTPUTS/` (separate from production)

**Key Feature**: 
Exports are suffixed `_TEST.gpkg` / `_TEST.rds` to avoid conflicts with full pipeline

---

## ✅ Test Suite Script Created: `scripts/run_tests.sh`

Fully automated test pipeline with:
- ✅ Pre-flight checks (R version, config files)
- ✅ Phase 1: Data Preparation (with TEST_CONFIG)
- ✅ Phase 2: Model Training (lighter settings)
- ✅ Phase 3: Prediction (all modes: Nantes, Paris, Sensors)
- ✅ Unit tests for each phase
- ✅ Output validation (file existence, size)
- ✅ Detailed logging to `TEST_OUTPUTS/test_suite.log`
- ✅ Timing breakdown per phase

**Usage**:
```bash
./scripts/run_tests.sh
```

**Expected Output**:
```
Phase 1 (Data Prep):     ~120s
Phase 2 (Model Train):   ~180s
Phase 3 (Prediction):    ~60s
Total Duration:          ~360s (~6 minutes)
```

---

## ✅ Region Parameter Added to Core Scripts

### 1. `run_pipeline.R` - Enhanced with --region flag
```bash
Rscript run_pipeline.R --phase all --mode all --region test --test
```

**Supported regions**:
- `full` - Complete France (default)
- `small` - 50km bbox around Nantes (loads TEST_CONFIG.R)
- `test` - Alias for small (normalized internally)

**Auto-loads** TEST_CONFIG.R when region != full

### 2. `scripts/run_local.sh` - Updated with --region support
```bash
./scripts/run_local.sh --phase preparation --mode all --region small --test
./scripts/run_local.sh --region test  # Fast test on Nantes region
```

### 3. Phase Scripts Enhanced
- `R/data_preparation/03_osm_feature_engineering.R` - Crops to test region bbox
- `R/data_preparation/06_training_dataset_merge.R` - Crops training data to test region

---

## 📊 Data Flow in Test Mode

```
TEST_CONFIG.R loaded (--region small/test)
    ↓
Override CONFIG paths (add _TEST suffix)
    ↓
Set TEST_REGION bbox (Nantes ±25km)
    ↓
Phase 1: Data Prep
  ├─ 02_osm_processing.R → crop to TEST_REGION
  ├─ 03_osm_feature_engineering.R → apply features to cropped region
  ├─ 04_avatar_download.R → fetch June 2023 only
  ├─ 05_avatar_aggregation.R → aggregate test data
  └─ 06_training_dataset_merge.R → merge test dataset
       ↓ Output: 05_training_dataset_TEST.gpkg (reduced size)
    ↓
Phase 2: Model Training
  └─ train_xgboost_models.R → train on test data (500 rounds, lighter config)
       ↓ Output: 06_xgboost_trained_models_TEST.rds
    ↓
Phase 3: Prediction
  ├─ predict_nantes.R → predict on test data
  ├─ predict_paris.R → predict on test data
  └─ predict_sensors.R → predict on test data
       ↓ Output: 07_predictions_*_TEST.gpkg
```

---

## 🚀 Quick Start - Full Test Pipeline

### Option 1: Direct Test Script (Recommended)
```bash
cd /home/aumond/Documents/github/OpenDataNoiseMap/RoadTrafficEstimation
./scripts/run_tests.sh
```

### Option 2: Using run_local.sh
```bash
./scripts/run_local.sh --region test --test
```

### Option 3: Full Command with Phase/Mode Control
```bash
Rscript run_pipeline.R --phase all --mode all --region test --test
```

---

## 📁 Test Output Structure

All outputs saved to `data/output/TEST_OUTPUTS/`:

```
TEST_OUTPUTS/
├── test_suite.log                           # Execution log
├── 01_osm_network_augmented_TEST.gpkg
├── 02_osm_network_france_engineered_TEST.gpkg
├── 02_imputation_rules_france_TEST.rds
├── 03_avatar_raw_traffic_TEST.rds
├── 04_avatar_aggregated_with_ratios_TEST.rds
├── 05_training_dataset_TEST.gpkg
├── 06_xgboost_trained_models_TEST.rds
├── 06_xgboost_feature_info_TEST.rds
├── 07_predictions_nantes_TEST.gpkg
├── 07_predictions_paris_TEST.gpkg
└── 07_predictions_sensors_TEST.gpkg
```

---

## ✅ Benefits

1. **Rapid Development Cycles**
   - 10-15 minute full test runs (vs 60+ minutes for full France)
   - Faster debugging and validation

2. **Separate Test Outputs**
   - No pollution of production data directory
   - Easy rollback/cleanup

3. **Consistent Validation**
   - All 3 phases tested in controlled environment
   - Pre-engineered layer validated on small region

4. **Flexible Region Control**
   - Easy to add new test regions (Paris, Lyon, etc.)
   - Full pipeline still supports `--region full` (default)

5. **Reproducible Testing**
   - Fixed time period (June 2023)
   - Fixed region (Nantes 50km bbox)
   - Deterministic results for CI/CD

---

## 📝 Usage Examples

### Quick Validation (entire pipeline)
```bash
./scripts/run_tests.sh
```

### Test only Phase 1
```bash
./scripts/run_local.sh --phase preparation --region test
```

### Test only Phase 3 (predictions)
```bash
./scripts/run_local.sh --phase prediction --mode all --region test --test
```

### Run full France pipeline (default)
```bash
./scripts/run_local.sh  # Region defaults to 'full'
```

---

## 🔍 Verification Checklist

- [x] Backup scripts (_OLD.R) deleted (3 files)
- [x] Code reviewed for dead code (clean)
- [x] TEST_CONFIG.R created with small region config
- [x] run_tests.sh created with full automation
- [x] run_pipeline.R updated with --region parameter
- [x] run_local.sh updated with --region support
- [x] Phase scripts updated for TEST mode
- [x] Output directory structure prepared
- [x] Logging framework added
- [x] File validation logic implemented
- [x] Documentation created

---

**Status**: ✅ READY FOR TESTING

Quick start: `./scripts/run_tests.sh`

Expected runtime: ~6-10 minutes for full pipeline on test region

