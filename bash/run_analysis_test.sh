#!/bin/bash
#SBATCH --time=9-23:00:00
#SBATCH --partition=gelifes
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=analysis_test
#SBATCH --output=/data/p287218/DAISIEmainland/analysis_test.log
#SBATCH --mem=1GB

mkdir -p logs
mkdir -p results
ml R
Rscript -e "remotes::install_github('joshwlambert/DAISIEmainland')"
Rscript /data/p287218/DAISIEmainland/scripts/run_analysis_test.R $1
