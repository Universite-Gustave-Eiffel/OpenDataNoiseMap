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

# Run ONLY the required scripts
R --vanilla << EOF
source("bootstrap.R")
source("config_pipeline.R")
source("01_setup_minimal.R")
source("03_avatar_download_minimal.R")
EOF

