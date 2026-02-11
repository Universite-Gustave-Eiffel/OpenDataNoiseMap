#!/bin/bash
#SBATCH -n 4                                                    # nombre de coeurs
#SBATCH -N 1                                                    # nombre de noeuds
#SBATCH -A cerema -p public					# partition ceremagpu
#SBATCH --mem=40G                                               # espace mémoire
#SBATCH --time=08:00:00                                         # temps alloué
#SBATCH --job-name=NM_OSM                                       # nom du job
#SBATCH -D /home2020/home/cerema/gguillau/NM_OSM/               # répertoire de travail
#SBATCH -o /home2020/home/cerema/gguillau/NM_OSM/output.log     # fichier de sortie
#SBATCH -e /home2020/home/cerema/gguillau/NM_OSM/error.log      # fichier d'erreur

source ~/.bashrc

# Load R environment
module unload R
module load R/R-4.3.0
module load gdal/gdal-2.4.4

# Computing environment is HPC
export RUN_CONTEXT=hpc

# Project root
export PROJECT_ROOT="/home2020/home/cerema/gguillau/NM_OSM"

# Execution parameters (adapt as needed)
PHASE="all"        # preparation | training | prediction | all
MODE="paris"       # nantes | paris | pemb | sensors | all
REGION="full"      # full | small | test
TEST_FLAG=""       # add "--test" to enable tests

# Single R session with new parameter structure
Rscript --vanilla "${PROJECT_ROOT}/main.R" --phase "$PHASE" --mode "$MODE" --region "$REGION" $TEST_FLAG
