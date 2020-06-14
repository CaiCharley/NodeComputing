# Run PrInCE on Scott's dataset with set arguments
setwd("~/OneDrive/git/PrinceR")

# larse arguments
library(argparse)
parser = ArgumentParser()
parser$add_argument('--input_file', type = 'character', required = T)
parser$add_argument('--output_dir', type = 'character', required = T)
parser$add_argument('--classifier', type = 'character', required = T)
parser$add_argument('--nmodels', type = 'integer', required = T)
args = parser$parse_args()

# load Libraries
library(tidyverse)
library(PrInCE)

# load input files
goldstd = readRDS(dirname(args$input_file) %>% paste0("/goldstd.rds"))
dataname = gsub("\\.rds$", "", basename(args$input_file))
dataset = readRDS(args$input_file)

# setup output filepath
if (!dir.exists(args$output_dir))
  dir.create(args$output_dir, recursive = T)
conditions = paste(dataname, args$classifier, args$nmodels, sep = "_")
output_filename = paste0(conditions, ".txt")
output_file = file.path(args$output_dir, output_filename)


# results
results = c(dataname, args$classifier, args$nmodels)

# write out
write(results, file = output_file)

head(goldstd)
dataset[[1]][1:5,1:5]