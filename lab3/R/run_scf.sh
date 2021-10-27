#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --nodes=1

#SBATCH --mail-user=sahilsaxena18@berkeley.edu
#SBATCH --mail-type=ALL

R CMD BATCH --no-save full_algorithm.R job.out