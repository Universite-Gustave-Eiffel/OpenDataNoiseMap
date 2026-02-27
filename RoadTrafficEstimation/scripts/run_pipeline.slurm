#!/bin/bash
#SBATCH -N 1                                                    # nombre de noeuds
#SBATCH -c 2                                                    # nombre de coeurs
#SBATCH -A ceremagpu -p ceremagpu                               # partition ceremagpu
#SBATCH --mem=30G                                               # espace mémoire
#SBATCH --time=01:00:00                                         # temps alloué
#SBATCH --job-name=OpenDataNoiseMap                             # nom du job
#SBATCH -o logs/slurm_%j.out                                    # fichier de sortie

bash scripts/run_pipeline.sh data_prep

