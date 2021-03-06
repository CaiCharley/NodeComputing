#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --mem=16G
#SBATCH --mail-user=charley.cai113@gmail.com
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Runs PrInCE with set arguments

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
    # load matlab
    module load matlab/2018a

    # Master Matlab Folder Location
    MATLAB_DIR=/home/caic/projects/rrg-ljfoster-ab/caic/princeR/princeML

    # get output directory
    OUTPUT_DIR=/home/caic/projects/rrg-ljfoster-ab/caic/princeR/$NAME

    # get coreComplexes
    GOLDSTD=/home/caic/projects/rrg-ljfoster-ab/caic/princeR/dataML/coreComplexes.txt

    # get job array
    GRID_FILE=/home/caic/OneDrive/git/NodeComputing/princeR/$NAME/$NAME'_grid.txt'
else
    GRID_FILE=./$1'_grid.txt'
fi

# get job parameters from array job index
LINE=$(sed "${LINE_IDX}q;d" "$GRID_FILE")
IFS=$'\t' PARAMS=($LINE)
INPUT_FILE=${PARAMS[0]}
BASENAME=${PARAMS[1]}
OUTPUT_FILE=${PARAMS[2]}
FRACTIONS=${PARAMS[3]}
REPLICATES=${PARAMS[4]}

# copy PrInCE individual output directory
cd $OUTPUT_DIR
NOEXT=${BASENAME/".csv"/""}
if [[ ! -d $NOEXT ]]; then
    mkdir $NOEXT
    cp -a $MATLAB_DIR/* $NOEXT

    cd $NOEXT
    cp $INPUT_FILE .
    cp $GOLDSTD .
else
    cd $NOEXT
fi

# run matlab PrInCE
/home/caic/OneDrive/git/NodeComputing/princeR/mlppis/memusg -tH -o ../${BASENAME/.csv/.benchmark} matlab -nodisplay -nojvm -r "prince('${BASENAME}', 'coreComplexes.txt', '${OUTPUT_FILE}', ${FRACTIONS}, ${REPLICATES}); exit"

# cleanup
# cd ..
# rm -r $NOEXT
