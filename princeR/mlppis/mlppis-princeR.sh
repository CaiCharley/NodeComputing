#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --mem=16G
#SBATCH --mail-user=charley.cai113@gmail.com
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Runs PrInCE with set arguments
# for testing !!!
SLURM_ARRAY_TASK_ID=1
NAME=mlppis

# set node specific variables
if [[ $SLURM_CLUSTER_NAME =~ "cedar" ]]; then
    # load matlab
    module load matlab/2018a

    # Master Matlab Folder Location
    MATLAB_DIR=/home/caic/projects/rrg-ljfoster-ab/caic/princeR/princeML

    # get output directory
    OUTPUT_DIR=/home/caic/projects/rrg-ljfoster-ab/caic/princeR/$NAME
    
    # get coreComplexes
    GOLDSTD=coreComplexes.txt

    # get job array
    GRID_FILE=/home/caic/OneDrive/git/NodeComputing/princeR/$NAME/$NAME'_grid.txt'
else
    GRID_FILE=./$1'_grid.txt'
fi


# get job parameters from array job index
LINE_IDX=$((SLURM_ARRAY_TASK_ID + 1))
LINE=$(sed "${LINE_IDX}q;d" "$GRID_FILE")
IFS=$'\t' PARAMS=($LINE)
INPUT_FILE=${PARAMS[0]}
BASENAME=${PARAMS[1]}
OUTPUT_FILE="ppi_list.csv"
FRACTIONS=${PARAMS[3]}
REPLICATES=${PARAMS[4]}

# copy PrInCE individual output directory
cd $OUTPUT_DIR
NOEXT=${BASENAME/".csv"/""}
if [[ ! -d $NOEXT ]]; then
    mkdir $NOEXT
    cp -a $MATLAB_DIR/* $NOEXT
fi

cd $NOEXT
cp $INPUT_FILE .
cp $GOLDSTD .

# run inner R script
matlab -nodisplay -nojvm -r "prince('${BASENAME}', '${GOLDSTD}', '${OUTPUT_FILE}', ${FRACTIONS}, ${REPLICATES}); exit"