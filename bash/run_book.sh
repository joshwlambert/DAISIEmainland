#!/bin/bash
#SBATCH --time=4-23:00:00
#SBATCH --partition=gelifes
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=book
#SBATCH --output=/data/p287218/DAISIEmainland/logs/book%a.log
#SBATCH --array=1-46
#SBATCH --mem=5GB

ml R
Rscript /data/p287218/DAISIEmainland/scripts/run_book.R ${SLURM_ARRAY_TASK_ID}
