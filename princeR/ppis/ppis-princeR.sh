#!/bin/bash
#SBATCH --time=5:00:00
#SBATCH --output=/home/caic/projects/rrg-ljfoster-ab/caic/PrInCE/%x-%A-%a.out
#SBATCH --mem=16G
#SBATCH --mail-user=charley.cai113@gmail.com
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Runs PrInCE with set arguments
cd ~/OneDrive/git/PrinceR

# load R if on Compute Canada
if [[ $SLURM_CLUSTER_NAME =~ "cedar" ]]; then
    module load nixpkgs/16.09  
    module load gcc/7.3.0
    module load r/4.0.0
fi

# R Script Location
if [[ $SLURM_CLUSTER_NAME =~ "cedar" ]]; then
    RSCRIPTPATH=~/OneDrive/git/PrinceR/$NAME-prince.R
else 
    RSCRIPTPATH=~/OneDrive/git/PrinceR/$1-prince.R
fi

# get job array
if [[ $SLURM_CLUSTER_NAME =~ "cedar" ]]; then
    GRID_FILE=~/OneDrive/git/PrinceR/grids/$NAME'_grid.txt'
else 
    GRID_FILE=~/OneDrive/git/PrinceR/grids/$1'_grid.txt'
fi

# get job parameters from array job index
LINE_IDX=$((SLURM_ARRAY_TASK_ID + 1))
LINE=$(sed "${LINE_IDX}q;d" "$GRID_FILE")
IFS=$'\t' PARAMS=($LINE)
INPUT_FILE=${PARAMS[0]}
CLASSIFIER=${PARAMS[1]}
NMODELS=${PARAMS[2]}

# set output directory
if [[ $SLURM_CLUSTER_NAME =~ "cedar" ]]; then
    OUTPUT_DIR=~/projects/rrg-ljfoster-ab/caic/PrInCE/$NAME
else 
    OUTPUT_DIR=/home/charley/OneDrive/2019\ Term\ 1/Foster\ Lab/PrInCER/CC/$1
fi

# run inner R script
Rscript $RSCRIPTPATH \
    --input_file "$INPUT_FILE" \
    --output_dir "$OUTPUT_DIR" \
    --classifier "$CLASSIFIER" \
    --nmodels "$NMODELS" 