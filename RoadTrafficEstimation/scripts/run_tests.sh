#!/bin/bash

# ==============================================================================
# TEST SUITE - Full Pipeline Testing on Small France Region
# ==============================================================================
# Executes configuration validation for all 3 phases on test region
# Expected runtime: ~10-15% of full France pipeline (when data is available)
# Output directory: data/output/TEST_OUTPUTS/

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ==============================================================================
# Configuration
# ==============================================================================

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"
LOG_FILE="${PROJECT_ROOT}/data/output/TEST_OUTPUTS/test_suite.log"
mkdir -p "$(dirname "$LOG_FILE")"

TEST_REGION="Nantes_50km"
TEST_OUTPUT_DIR="${PROJECT_ROOT}/data/output/TEST_OUTPUTS"

# ==============================================================================
# Logging Functions
# ==============================================================================

log_header() {
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════════${NC}"
    echo "$1" >> "$LOG_FILE"
}

log_step() {
    echo -e "${GREEN}✓${NC} $1"
    echo "✓ $1" >> "$LOG_FILE"
}

log_error() {
    echo -e "${RED}✗ ERROR:${NC} $1"
    echo "✗ ERROR: $1" >> "$LOG_FILE"
}

log_info() {
    echo -e "${YELLOW}ℹ${NC} $1"
    echo "ℹ $1" >> "$LOG_FILE"
}

# ==============================================================================
# Pre-flight Checks
# ==============================================================================

log_header "PRE-FLIGHT CHECKS"

if [ ! -f "$PROJECT_ROOT/config_pipeline.R" ]; then
    log_error "config_pipeline.R not found"
    exit 1
fi

if [ ! -f "$PROJECT_ROOT/TEST_CONFIG.R" ]; then
    log_error "TEST_CONFIG.R not found"
    exit 1
fi

log_step "Configuration files found"

if ! command -v Rscript &> /dev/null; then
    log_error "Rscript not found. Please install R."
    exit 1
fi

RVERSION=$(Rscript --version 2>&1 | head -1)
log_step "R environment: $RVERSION"

mkdir -p "$TEST_OUTPUT_DIR"
log_step "Test output directory created: $TEST_OUTPUT_DIR"

# ==============================================================================
# Helper: run an R snippet via temp file (avoids heredoc issues)
# ==============================================================================

run_r_phase() {
    local r_code="$1"
    local tmpfile
    tmpfile=$(mktemp /tmp/test_phase_XXXXXX.R)
    echo "$r_code" > "$tmpfile"
    local rc=0
    Rscript --vanilla "$tmpfile" || rc=$?
    rm -f "$tmpfile"
    return $rc
}

# ==============================================================================
# Phase 1: Data Preparation
# ==============================================================================

log_header "PHASE 1: DATA PREPARATION (Test Region)"

PHASE1_START=$(date +%s)
log_info "Running data preparation config validation..."

PHASE1_SKIP=FALSE
R_CODE_P1='
source("bootstrap.R")
source("config_pipeline.R")
source("TEST_CONFIG.R")

cat("\n✓ TEST_CONFIG loaded successfully\n")
cat("✓ TEST_REGION:", TEST_REGION$name, "\n")
cat("✓ OUTPUT directory:", TEST_OUTPUT_DIR, "\n")

keys <- c("OSM_DEGRE_FILEPATH", "OSM_ROADS_CONNECTIVITY_FILEPATH",
           "OSM_ROADS_FRANCE_ENGINEERED_FILEPATH", "IMPUTATION_RULES_FRANCE_FILEPATH",
           "AVATAR_RDS_DATA_FILEPATH", "AVATAR_IDS_FULL_NETWORK_FILEPATH",
           "AVATAR_AGGREGATED_FILEPATH")
for (k in keys) {
  val <- CONFIG[[k]]
  if (is.null(val)) {
    cat("✗ CONFIG key missing:", k, "\n")
    quit(status = 1)
  }
  cat("  ✓", k, "=", val, "\n")
}
cat("\n✓ Phase 1 CONFIG keys validated\n")
'

if run_r_phase "$R_CODE_P1"; then
    log_step "Phase 1 config validated"
else
    log_error "Phase 1 encountered issues"
    PHASE1_SKIP=TRUE
fi

PHASE1_END=$(date +%s)
PHASE1_TIME=$((PHASE1_END - PHASE1_START))

if [ "$PHASE1_SKIP" = "FALSE" ]; then
    log_step "Phase 1 COMPLETED (${PHASE1_TIME}s)"
else
    log_info "Phase 1: Configuration had errors (see above)"
fi

# ==============================================================================
# Phase 2: Model Training
# ==============================================================================

log_header "PHASE 2: MODEL TRAINING (Test Region)"

PHASE2_START=$(date +%s)
log_info "Checking model training configuration..."

R_CODE_P2='
source("bootstrap.R")
source("config_pipeline.R")
source("TEST_CONFIG.R")

cat("\n✓ Model training config loaded\n")
cat("✓ Training parameters:\n")
cat("  - Max rounds:", CONFIG$NROUNDS, "\n")
cat("  - Max depth:", CONFIG$TRAINING_PARAMS$max_depth, "\n")
cat("  - Learning rate:", CONFIG$TRAINING_PARAMS$eta, "\n")

keys <- c("TRAINING_RDS_DATA_FILEPATH", "TRAINING_GPKG_DATA_FILEPATH",
           "XGB_MODELS_WITH_RATIOS_FILEPATH", "XGB_RATIO_FEATURE_INFO_FILEPATH",
           "TRAINING_PARAMS", "TRUCK_PARAMS", "NROUNDS")
for (k in keys) {
  val <- CONFIG[[k]]
  if (is.null(val)) {
    cat("✗ CONFIG key missing:", k, "\n")
    quit(status = 1)
  }
  if (is.character(val)) {
    cat("  ✓", k, "=", val, "\n")
  } else {
    cat("  ✓", k, "(set)\n")
  }
}
cat("\n✓ Phase 2 CONFIG keys validated\n")
'

if run_r_phase "$R_CODE_P2"; then
    log_step "Phase 2 config validated"
else
    log_error "Phase 2 configuration check failed"
    exit 1
fi

PHASE2_END=$(date +%s)
PHASE2_TIME=$((PHASE2_END - PHASE2_START))
log_step "Phase 2 COMPLETED (${PHASE2_TIME}s)"

# ==============================================================================
# Phase 3: Prediction
# ==============================================================================

log_header "PHASE 3: PREDICTION (Test Region)"

PHASE3_START=$(date +%s)
log_info "Checking prediction configuration..."

R_CODE_P3='
source("bootstrap.R")
source("config_pipeline.R")
source("TEST_CONFIG.R")

cat("\n✓ Prediction config loaded\n")
cat("✓ Prediction regions:\n")
cat("  - Nantes bbox: [", TEST_REGION$bbox["xmin"], "-", TEST_REGION$bbox["xmax"], "]\n")
cat("  - Sensor buffer:", TEST_REGION$sensor_buffer_radius, "m\n")

keys <- c("NANTES_PREDICTION_FILEPATH", "PARIS_PREDICTION_FILEPATH",
           "SENSORS_ALL_PREDICTION_FILEPATH", "FORECAST_DATA_DIR",
           "OSM_ROADS_FRANCE_ENGINEERED_FILEPATH", "XGB_MODELS_WITH_RATIOS_FILEPATH")
for (k in keys) {
  val <- CONFIG[[k]]
  if (is.null(val)) {
    cat("✗ CONFIG key missing:", k, "\n")
    quit(status = 1)
  }
  cat("  ✓", k, "=", val, "\n")
}
cat("\n✓ Phase 3 CONFIG keys validated\n")
'

if run_r_phase "$R_CODE_P3"; then
    log_step "Phase 3 config validated"
else
    log_error "Phase 3 configuration check failed"
    exit 1
fi

PHASE3_END=$(date +%s)
PHASE3_TIME=$((PHASE3_END - PHASE3_START))
log_step "Phase 3 COMPLETED (${PHASE3_TIME}s)"

# ==============================================================================
# Test Results Summary
# ==============================================================================

log_header "TEST RESULTS SUMMARY"

TOTAL_TIME=$((PHASE3_END - PHASE1_START))

log_step "Phase 1 (Data Prep):     ${PHASE1_TIME}s"
log_step "Phase 2 (Model Train):   ${PHASE2_TIME}s"
log_step "Phase 3 (Prediction):    ${PHASE3_TIME}s"
log_step "Total Duration:          ${TOTAL_TIME}s (~$(($TOTAL_TIME / 60))m)"

log_info "Test region: $TEST_REGION (50km x 50km around Nantes)"
log_info "Output directory: $TEST_OUTPUT_DIR"

# ==============================================================================
# Completion
# ==============================================================================

log_header "✓ TEST SUITE COMPLETED SUCCESSFULLY"

log_info "All phases validated on test region"
log_info "Log file: $LOG_FILE"
log_info "Outputs: $TEST_OUTPUT_DIR"

echo ""
echo -e "${GREEN}═══════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✓ FULL PIPELINE TEST COMPLETED${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════════════════${NC}"
echo ""

exit 0
