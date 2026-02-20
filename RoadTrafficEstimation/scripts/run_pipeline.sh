#!/bin/bash
# ==============================================================================
# Unified pipeline launcher (local / HPC)
# ==============================================================================

# ------------------------------------------------------------------------------
# Detect execution context
# ------------------------------------------------------------------------------
# Detect HPC via hostname
if hostname | grep -qi "hpc\|cluster"; then
  export R_LIBS_USER=$HOME/R/x86_64-pc-linux-gnu-library/4.4.2
fi

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
if [ -n "$SLURM_JOB_ID" ]; then
  PROJECT_ROOT="${SLURM_SUBMIT_DIR}"
else
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
fi

export PROJECT_ROOT
echo "🌱 PROJECT_ROOT: ${PROJECT_ROOT}"

cd "${PROJECT_ROOT}"

# ------------------------------------------------------------------------------
# HPC-specific setup (login + slurm)
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
# Pipeline mode (argument or default)
# ------------------------------------------------------------------------------
MODE="${1:-data_prep}"   # default = sensors

case "$MODE" in
  data_prep|avatar|training|nantes|paris|sensors)
    ;;
  *)
    echo "❌ Unknown MODE: $MODE"
    echo "⚠️ Allowed values: data_prep | avatar | training | nantes | paris | sensors | pemb | france"
    exit 1
    ;;
esac

echo "🎯 PIPELINE MODE: ${MODE}"

# ------------------------------------------------------------------------------
# Run pipeline
# ------------------------------------------------------------------------------
MAIN_R="${PROJECT_ROOT}/main.R"
OUT_LOG="${PROJECT_ROOT}/logs/${MODE}.Rout"

mkdir -p "${PROJECT_ROOT}/logs"

[ -f $OUT_LOG ] && rm $OUT_LOG

echo "🚀 RUN PIPELINE: ${MAIN_R}"
echo "📜 R LOG: ${OUT_LOG}"
R --vanilla -f "${MAIN_R}" --args "${MODE}" > "${OUT_LOG}" 2>&1

