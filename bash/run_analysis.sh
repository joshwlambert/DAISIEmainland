#!/bin/bash
#SBATCH --time=9-23:00:00
#SBATCH --partition=regular
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=analysis
#SBATCH --output=/data/p287218/DAISIEmainland/logs/analysis%a.log
#SBATCH --array=1-46
#SBATCH --mem=5GB

ml R
Rscript /data/p287218/DAISIEmainland/scripts/run_analysis.R ${SLURM_ARRAY_TASK_ID}
