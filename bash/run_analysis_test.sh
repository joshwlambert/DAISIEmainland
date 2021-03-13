#!/bin/bash
#SBATCH --time=9-23:00:00
#SBATCH --partition=gelifes
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=analysis_test
#SBATCH --output=/data/p287218/DAISIEmainland/analysis_test%a.log
#SBATCH --array=1-10
#SBATCH --mem=1GB

Rscript /data/p287218/DAISIEmainland/scripts/run_analysis_test.R ${SLURM_ARRAY_TASK_ID}
