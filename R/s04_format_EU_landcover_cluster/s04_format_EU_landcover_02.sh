#!/bin/bash

#SBATCH --partition=main
#SBATCH --requeue
#SBATCH --job-name=lc02
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem=95GB
#SBATCH --time=1-10:00:00
#SBATCH -o l2.%j_%N.out
#SBATCH -e l2.%j_%N.err

Rscript s04_format_EU_landcover_02.R
