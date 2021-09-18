#!/bin/bash
#SBATCH --time=2-23:00:00
#SBATCH --partition=gelifes
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=mainland_ex_analysis
#SBATCH --output=/data/p287218/DAISIEmainland/logs/mainland_ex_analysis%a.log
#SBATCH --array=1-51
#SBATCH --mem=1GB

ml R
Rscript /data/p287218/DAISIEmainland/scripts/analysis/run_mainland_ex_analysis.R ${SLURM_ARRAY_TASK_ID}
