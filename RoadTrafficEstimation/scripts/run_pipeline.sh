#!/bin/bash
# ==============================================================================
# Unified pipeline launcher (local -> sh / HPC -> sbatch)
# ==============================================================================

# ------------------------------------------------------------------------------
# Detect execution context
# ------------------------------------------------------------------------------

# Detect HPC via hostname
if hostname | grep -qi "hpc\|cluster"; then
  export R_LIBS_USER=$HOME/R/x86_64-pc-linux-gnu-library/4.4.2
fi

# Determine RUN_CONTEXT
if [ -n "$RUN_CONTEXT" ]; then
  echo "🧭 RUN_CONTEXT forced to $RUN_CONTEXT"
elif [ -n "$SLURM_JOB_ID" ] || [ -n "$SLURM_STEP_ID" ]; then
  RUN_CONTEXT="slurm"
  echo "🖥️ SLURM execution detected"
else
  RUN_CONTEXT="local"
  echo "💻 Local execution detected"
fi

export RUN_CONTEXT
echo "🌐 RUN_CONTEXT=${RUN_CONTEXT}"

# ------------------------------------------------------------------------------
# Project root
# ------------------------------------------------------------------------------

# Determine project root based on execution context
if [ -n "$SLURM_JOB_ID" ]; then
  PROJECT_ROOT="${SLURM_SUBMIT_DIR}"
else
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
fi

export PROJECT_ROOT
echo "🌱 PROJECT_ROOT: ${PROJECT_ROOT}"
cd "${PROJECT_ROOT}"

# ------------------------------------------------------------------------------
# Extract args for log naming
# ------------------------------------------------------------------------------

# Preserve original args and extract --phase/--mode/--region for log naming
ALL_ARGS=("$@")

# Pre-initialize from environment if already exported
PHASE=${PHASE:-}
MODE=${MODE:-}
REGION=${REGION:-}

i=0
while [ $i -lt ${#ALL_ARGS[@]} ]; do
  arg="${ALL_ARGS[$i]}"
  case "$arg" in
    --phase)
      PHASE="${ALL_ARGS[$((i+1))]}"; i=$((i+2));;
    --mode)
      MODE="${ALL_ARGS[$((i+1))]}"; i=$((i+2));;
    --region)
      REGION="${ALL_ARGS[$((i+1))]}"; i=$((i+2));;
    *)
      i=$((i+1));;
  esac
done

# Default region name when none supplied should be 'full'
if [ -z "$REGION" ]; then
  REGION="full"
fi
# Safe log suffix (fallbacks when flags not provided)
LOG_SUFFIX="${PHASE:-noPhase}_${MODE:-noMode}_${REGION}"

# ------------------------------------------------------------------------------
# HPC-specific setup (login + slurm): load modules, set env vars, verify R
# ------------------------------------------------------------------------------

if hostname | grep -qi "hpc\|cluster"; then

  echo "🖥️ HPC environment detected"
  
  source ~/.bashrc
  set -a
  source ./.Renviron
  set +a
  module purge || true
  module load gcc/gcc-12 || { echo "❌ gcc module failed"; exit 1; }
  module load R/R-4.4.2 || { echo "❌ R module failed"; exit 1; }
  module load gdal/gdal-2.4.4 || { echo "❌ gdal module failed"; exit 1; }
  
  export R_LIBS_USER=$HOME/R/x86_64-pc-linux-gnu-library/4.4.2
  export UDUNITS2_INCLUDE=$HOME/local/udunits/include
  export UDUNITS2_LIBS=$HOME/local/udunits/lib
  export LD_LIBRARY_PATH=$HOME/local/udunits/lib:$LD_LIBRARY_PATH
  export PKG_CONFIG_PATH=$HOME/local/udunits/lib/pkgconfig:$PKG_CONFIG_PATH
  
  echo "📦 R_LIBS_USER=$R_LIBS_USER"
  
fi

echo "🔎 R path: $(which R)"
R --version

# ------------------------------------------------------------------------------
# Run pipeline
# ------------------------------------------------------------------------------

MAIN_R="${PROJECT_ROOT}/main.R"
LOG_DIR="${PROJECT_ROOT}/logs"
OUT_LOG="${LOG_DIR}/pipeline_${LOG_SUFFIX}.Rout"

# Ensure log directory exists and remove old log if it exists
mkdir -p "${LOG_DIR}"
[ -f "$OUT_LOG" ] && rm "$OUT_LOG"

echo "🚀 RUN PIPELINE: ${MAIN_R}"
echo "📜 R LOG: ${OUT_LOG}"

echo "🔧 Using line-buffered Rscript output"

# Pass all arguments to Rscript and redirect output to log file
stdbuf -oL -eL Rscript --vanilla "${MAIN_R}" "$@" > "${OUT_LOG}" 2>&1