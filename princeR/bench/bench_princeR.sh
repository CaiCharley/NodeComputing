#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --mem=32G
#SBATCH --mail-user=charley.cai113@gmail.com
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Runs PrInCE with set arguments
cd $(dirname $(readlink -f $0))

# check if script is called with arguments
if [ -z ${1+x} ]; then
    LINE_IDX=$((SLURM_ARRAY_TASK_ID + 1))
else
    PROJECT=$1
    NAME=$2
    LINE_IDX=$3
fi

# set node specific variables
if [[ $SLURM_CLUSTER_NAME =~ "cedar" ]]; then
    # load R
    module load nixpkgs/16.09
    module load gcc/7.3.0
    module load r/4.0.0

    # R Script Location
    RSCRIPTPATH=/home/caic/OneDrive/git/NodeComputing/princeR/$NAME/${NAME}_princeR.R

    # get job array
    GRID_FILE=/home/caic/OneDrive/git/NodeComputing/princeR/$NAME/$NAME'_grid.txt'

    # set output directory
    OUTPUT_DIR=~/projects/rrg-ljfoster-ab/caic/princeR/$NAME
else
    # R Script Location
    RSCRIPTPATH=./$1-princeR.R

    # get job array
    GRID_FILE=./$1'_grid.txt'

    # set output directory
    OUTPUT_DIR=/home/charley/OneDrive/2019\ Term\ 1/Foster\ Lab/PrInCER/CC/$1
fi

# get job parameters from array job index
LINE=$(sed "${LINE_IDX}q;d" "$GRID_FILE")
IFS=$'\t' PARAMS=($LINE)
INPUT_FILE=${PARAMS[0]}
CLASSIFIER=${PARAMS[1]}
NMODELS=${PARAMS[2]}

# run inner R script
Rscript $RSCRIPTPATH \
    --input_file "$INPUT_FILE" \
    --output_dir "$OUTPUT_DIR" \
    --classifier "$CLASSIFIER" \
    --nmodels "$NMODELS"
