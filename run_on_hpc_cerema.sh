#!/bin/bash
#SBATCH -N 12                                                   # nombre de coeurs
#SBATCH -N 1                                                    # nombre de noeuds
#SBATCH -A ceremagpu -p ceremagpu                               # partition ceremagpu
#SBATCH --mem=30G                                               # espace mémoire
#SBATCH --time=08:00:00                                         # temps alloué
#SBATCH --job-name=NM_OSM                                       # nom du job
#SBATCH -D /home2020/home/cerema/gguillau/NM_OSM/               # répertoire de travail
#SBATCH -o /home2020/home/cerema/gguillau/NM_OSM/output.log     # fichier de sortie

source ~/.bashrc

# Load R environment
module purge
module load R/R-4.3.0
module load gdal/gdal-2.4.4

# Computing environment is HPC
export RUN_CONTEXT=hpc

# Choose execution mode: nantes | paris | sensors
MODE="paris"

# Single R session
R --vanilla -f main.R --args ${MODE}
