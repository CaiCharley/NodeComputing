# Run PrInCE on Scott's dataset with set arguments
setwd("~/OneDrive/git/PrinceR")

# Parse arguments
library(argparse)
parser = ArgumentParser()
parser$add_argument('--input_file', type = 'character', required = T)
parser$add_argument('--output_dir', type = 'character', required = T)
parser$add_argument('--classifer', type = 'character', required = T)
parser$add_argument('--nmodels', type = 'integer', required = T)
args = parser$parse_args()
for (arg in args) {
   message(arg)
}

# Load libraries
#library(PrInCE, quietly = T)