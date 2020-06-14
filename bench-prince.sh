# Runs PrInCE with set arguments
#!/bin/bash

cd ~/OneDrive/git/PrinceR

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


