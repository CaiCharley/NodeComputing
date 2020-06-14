# Runs PrInCE with set arguments
#!/bin/bash

cd ~/OneDrive/git/PrinceR
[[ $HOSTNAME =~ "computecanada" ]] && ONCC="true" 

# load R if on Compute Canada


if [[ -v ONCC ]]; then
    # module load nixpkgs/16.09  
    # module load gcc/7.3.0
    # module load r/4.0.0
    echo "On CC"
fi

# R Script Location
RSCRIPTPATH=~/OneDrive/git/PrinceR/inner-prince.R

# R arguments
INPUT_FILE='input file'
OUTPUT_DIR='output dir'
CLASSIFER='classifer'
NMODELS=10

Rscript $RSCRIPTPATH \
    --input_file "$INPUT_FILE" \
    --output_dir "$OUTPUT_DIR" \
    --classifer "$CLASSIFER" \
    --nmodels $NMODELS \

# Rscript $RSCRIPTPATH --input_file "hi"


