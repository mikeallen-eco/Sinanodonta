#!/bin/bash

#SBATCH --partition=main
#SBATCH --requeue
#SBATCH --job-name=distw5
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem=95GB
#SBATCH --time=1-10:00:00
#SBATCH -o d5.%j_%N.out
#SBATCH -e d5.%j_%N.err

Rscript distance_w_cluster5.R