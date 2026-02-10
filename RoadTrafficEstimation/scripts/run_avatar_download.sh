#!/bin/bash

# ============================================================
# Avatar data download (LOGIN NODE ONLY)
# ============================================================

# Load environment
source ~/.bashrc

module purge
module load R/R-4.3.0
module load gdal/gdal-2.4.4

# Explicit context
export RUN_CONTEXT=local

# Safety
echo "⚠️  Running Avatar download on LOGIN NODE"
echo "RUN_CONTEXT=${RUN_CONTEXT}"

# Project root
PROJECT_ROOT="/home2020/home/cerema/gguillau/NM_OSM"
echo "PROJECT_ROOT=${PROJECT_ROOT}"

# Run ONLY the Avatar download phase (Phase 1, step 2)
# This uses the new phase-based directory structure
R --vanilla << EOF
source("bootstrap.R")
source("config_pipeline.R")
source("R/data_preparation/01_setup_environment.R")
source("R/data_preparation/04_avatar_download.R")
EOF

