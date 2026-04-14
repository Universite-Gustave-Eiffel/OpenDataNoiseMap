#!/bin/bash
#SBATCH -N 1                                                    # number of nodes
#SBATCH -c 8                                                    # number of cores
#SBATCH -A ceremagpu -p ceremagpu                               # partition ceremagpu
#SBATCH --mem=30G                                               # memory space
#SBATCH --time=24:00:00                                         # allocated time
#SBATCH --job-name=OpenDataNoiseMap                             # job name
#SBATCH -o logs/slurm_%j.out                                    # output file

bash scripts/run_pipeline.sh data_prep

