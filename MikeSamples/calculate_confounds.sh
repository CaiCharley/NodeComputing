#!/bin/bash
#SBATCH --time=3:00:00
#SBATCH --job-name=calculate_confounds
#SBATCH --output=/home/aphil/logs/pseudobulk-benchmark/calculate_confounds-%j-%a.out
#SBATCH --mem=32G

module load gcc/7.3.0
module load r/3.6.0

cd ~/git/pseudobulk-benchmark

# get parameters from array job index
GRID_FILE=sh/analysis/confounds/grids/calculate_confounds.txt
LINE_IDX=$((SLURM_ARRAY_TASK_ID + 1))
LINE=`sed "${LINE_IDX}q;d" $GRID_FILE`
IFS=$'\t' PARAMS=($LINE)
INPUT_FILE=${PARAMS[0]}

# set up output directory
BASE_DIR=~aphil/projects/rrg-aphil/aphil/pseudobulk-benchmark
OUTPUT_DIR=${BASE_DIR}/analysis/confounds

# run R script
Rscript R/analysis/confounds/inner-calculate-confounds.R \
  --input_file $INPUT_FILE \
  --output_dir $OUTPUT_DIR
