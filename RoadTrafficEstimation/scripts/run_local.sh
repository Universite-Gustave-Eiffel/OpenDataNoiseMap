#!/bin/bash
# ---------------------------------------------------------------------------
# Local execution of OpenDataNoiseMap traffic estimation pipeline
# ---------------------------------------------------------------------------
# 
# Usage:
#   ./scripts/run_local.sh [--phase PHASE] [--mode MODE] [--region REGION] [--test]
#
# Arguments:
#   --phase PHASE     preparation | training | prediction | all (default: all)
#   --mode MODE       nantes | paris | sensors | all (default: all)
#   --region REGION   full | small | test (default: full)
#   --test            Enable unit tests
#
# Examples:
#   ./scripts/run_local.sh --phase preparation
#   ./scripts/run_local.sh --phase prediction --mode nantes --test
#   ./scripts/run_local.sh --region small --test
#   ./scripts/run_local.sh
#
# ---------------------------------------------------------------------------

# Default values
PHASE="all"
MODE="all"
REGION="full"
TEST_FLAG=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --phase)
      PHASE="$2"
      shift 2
      ;;
    --mode)
      MODE="$2"
      shift 2
      ;;
    --region)
      REGION="$2"
      shift 2
      ;;
    --test)
      TEST_FLAG="--test"
      shift
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

echo "================================================================================"
echo "OpenDataNoiseMap Traffic Estimation Pipeline"
echo "================================================================================"
echo "Phase:  $PHASE"
echo "Mode:   $MODE"
echo "Region: $REGION"
echo "Tests:  $([ -z "$TEST_FLAG" ] && echo "OFF" || echo "ON")"
echo "================================================================================"

# Run the pipeline in R (non-interactive)
R --vanilla -f main.R --args --phase "$PHASE" --mode "$MODE" --region "$REGION" $TEST_FLAG
