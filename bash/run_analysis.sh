#!/bin/bash
#SBATCH --time=4-23:00:00
#SBATCH --partition=gelifes
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=analysis
#SBATCH --output=/home3/p287218/DAISIEmainland/logs/analysis%a.log
#SBATCH --array=1-46
#SBATCH --mem=5GB

ml R
Rscript /home3/p287218/DAISIEmainland/scripts/run_analysis.R ${SLURM_ARRAY_TASK_ID}
