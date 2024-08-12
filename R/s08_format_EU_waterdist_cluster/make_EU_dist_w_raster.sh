#!/bin/bash

#SBATCH --partition=main
#SBATCH --requeue
#SBATCH --job-name=distwrast
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem=150GB
#SBATCH --time=1-10:00:00
#SBATCH -o distwrast.%j_%N.out
#SBATCH -e distwrast.%j_%N.err

Rscript make_EU_dist_w_raster.R