#!/bin/bash
#SBATCH --time=72:00:00
#SBATCH --job-name=fdr_control
#SBATCH --output=/home/aphil/logs/pseudobulk-benchmark/fdr_control-%j-%a.out
#SBATCH --mem=32G

module load gcc/7.3.0
module load r/3.6.0

cd ~/git/pseudobulk-benchmark

# get parameters from array job index
GRID_FILE=sh/analysis/fdr_control/grids/fdr_control.txt
LINE_IDX=$((SLURM_ARRAY_TASK_ID + 1))
LINE=`sed "${LINE_IDX}q;d" $GRID_FILE`
IFS=$'\t' PARAMS=($LINE)
INPUT_FILE=${PARAMS[0]}
DE_TEST=${PARAMS[1]}
SHUFFLE=${PARAMS[2]}
DOWNSAMPLE_N=${PARAMS[3]}

# set up output directory
BASE_DIR=~aphil/projects/rrg-aphil/aphil/pseudobulk-benchmark
OUTPUT_DIR=${BASE_DIR}/analysis/fdr_control/DE

# run R script
Rscript R/analysis/fdr_control/inner-fdr-control.R \
  --input_file $INPUT_FILE \
  --output_dir $OUTPUT_DIR \
  --de_test $DE_TEST \
  --shuffle $SHUFFLE \
  --downsample_n $DOWNSAMPLE_N
