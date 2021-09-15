#!/bin/bash
#SBATCH --time=2-23:00:00
#SBATCH --partition=gelifes
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=analysis
#SBATCH --output=/data/p287218/DAISIEmainland/logs/analysis%a.log
#SBATCH --array=1-10
#SBATCH --mem=1GB

ml R
Rscript /data/p287218/DAISIEmainland/scripts/analysis/run_analysis.R ${SLURM_ARRAY_TASK_ID}
