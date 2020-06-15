# Benchmarking PrInCE with different conditions using Compute Canada Node
setwd("~/OneDrive/git/PrinceR")

# parse arguments
library(argparse)
parser = ArgumentParser(prog = "outer-prince.R")
parser$add_argument('--allocation', type = 'character', default = "rrg-ljfoster-ab")
args = parser$parse_args()

# load libraries
library(tidyverse)
library(magrittr)

# list input files
input_dir <- "~/projects/rrg-ljfoster-ab/caic/PrInCE/scottdata"
input_files <- file.path(input_dir, c(
  "bn_unstim.rds",
  "bn_stim.rds",
  "sec_unstim.rds",
  "sec_stim.rds")
) 
  
# generate grid of argument permutations
options <- list(
  input_files,
  classifier <- c("NB", "SVM", "RF", "LR", "ensemble"),
  nmodels <- c(1, 3, 10)
)

grid <- expand.grid(options, stringsAsFactors = F)
colnames(grid) <- c("input_file", "classifer", "nmodels")

# write grid
write.table(grid, "grid.txt", quote = F, row.names = F, sep = "\t")


