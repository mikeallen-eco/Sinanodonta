#!/bin/bash

#SBATCH --partition=main
#SBATCH --requeue
#SBATCH --job-name=distw8
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem=95GB
#SBATCH --time=1-10:00:00
#SBATCH -o d8.%j_%N.out
#SBATCH -e d8.%j_%N.err

Rscript distance_w_cluster8.R