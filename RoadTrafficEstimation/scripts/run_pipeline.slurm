#!/bin/bash
#SBATCH -N 1                                                    # number of nodes
#SBATCH -c 12                                                   # number of cores
#SBATCH -A ceremagpu -p ceremagpu                               # partition ceremagpu
#SBATCH --mem=512G                                              # memory space
#SBATCH --time=24:00:00                                         # allocated time
#SBATCH --job-name=OpenDataNoiseMap                             # job name
#SBATCH -o logs/slurm_%j.out                                    # output file
#
# This script launches the main R pipeline on the HPC.

# Define PROJECT_ROOT relative to the SLURM submission directory
PROJECT_ROOT="${SLURM_SUBMIT_DIR}"

