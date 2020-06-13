# Benchmarking PrInCE with different conditions using Compute Canada Node
library(argparse, quietly = T)
library(tidyverse, quietly = T)

# Parse Arguments
parser = ArgumentParser(prog = "outer-prince.R")
parser$add_argument('--allocation', type = 'character', default = "rrg-ljfoster-ab")
parser$add_argument('--dir', type = 'character', default = "./")
args = parser$parse_args()


