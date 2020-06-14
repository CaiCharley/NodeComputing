# Run PrInCE on Scott's dataset with set arguments
setwd("~/OneDrive/git/PrinceR")

# Parse arguments
library(argparse)
parser = ArgumentParser()
parser$add_argument('--input_file', type = 'character', required = T)
parser$add_argument('--output_dir', type = 'character', required = T)
parser$add_argument('--classifier', type = 'character', required = T)
parser$add_argument('--nmodels', type = 'integer', required = T)
args = parser$parse_args()
for (arg in args) {
  message(arg)
}

# Load Libraries
library(tidyverse)
# library(PrInCE)

# setup output filepath
if (!dir.exists(args$output_dir))
  dir.create(args$output_dir, recursive = T)
conditions = paste0(args$classifier, args$nmodels)
output_filename = paste0("scottppi_", conditions, ".txt")
output_file = file.path(args$output_dir, output_filename)


# results
results = c(args$input_file, args$classifier, args$nmodels)

# write out
write(results, file = output_file)