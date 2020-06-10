# Analyze FDR control in single-cell or pseudobulk DE analyses by shuffling
# the labels of individual cells or entire replicates.
setwd("~/git/pseudobulk-benchmark")
options(stringsAsFactors = F)
library(argparse)

# parse arguments
parser = ArgumentParser(prog = 'outer-fdr-control.R')
parser$add_argument('--allocation', type = 'character', default = "rrg-aphil")
args = parser$parse_args()

library(tidyverse)
library(magrittr)
source("R/functions/datasets.R")
source("R/functions/submit_job.R")

# what system are we on?
system = 'cedar'
base_dir = "~aphil/projects/rrg-aphil/aphil/pseudobulk-benchmark"
if (!dir.exists(base_dir)) {
  base_dir = "/scratch/st-bkkwon-1/pseudobulk-benchmark"
  system = 'sockeye'
  if (!dir.exists(base_dir)) {
    base_dir = "~ubuntu/projects/rrg-aphil/aphil/pseudobulk-benchmark"
    system = 'elasti'
  }
}

# list input files
input_dir = file.path(base_dir, "rnaseq", "seurat")
input_files = file.path(input_dir, paste0(c(datasets, "Cuomo2020"), '.rds'))
inputs = data.frame(input_file = input_files)

# establish grid of analyses
opts = list(
  de_test = c(
    ## single-cell methods, implemented in Seurat
    "wilcox", "bimod", "t", "negbinom", "poisson", "LR", "MAST",
    ## pseudobulk methods
    "pseudobulk_DESeq2", "pseudobulk_limma", "pseudobulk_edgeR"
  ),
  shuffle = c('cells', 'replicates'),
  downsample_n = c(NA, 3, 5, 10, 25, 50)
)
grid = do.call(expand.grid, c(opts, stringsAsFactors = F)) 

# rep analysis grid over input files
grid %<>%
  dplyr::slice(rep(1:n(), each = nrow(inputs))) %>%
  mutate(input_file = rep(inputs$input_file, nrow(grid))) %>%
  left_join(inputs, by = 'input_file') %>%
  # reorder columns
  dplyr::select(input_file, de_test, shuffle, downsample_n) %>%
  # downsample only Cuomo
  filter(!(!is.na(downsample_n) & !grepl("Cuomo2020", input_file))) %>%
  # downsample only replicates
  filter(!(!is.na(downsample_n) & shuffle != 'replicates'))

# define output directory where results are stored
output_dir = file.path(base_dir, "analysis/fdr_control/DE")

# check which parameters are already complete
overwrite = F
grid0 = grid
if (!overwrite) {
  grid0 = grid %>%
    mutate(output_filename = paste0(basename(input_file) %>%
                                      gsub("\\.rds$", "", .),
                                    '-de_test=', de_test,
                                    '-shuffle=', shuffle,
                                    ifelse(!is.na(downsample_n),
                                           paste0("-downsample_n=", downsample_n),
                                           ""),
                                    '.rds'),
           output_file = file.path(output_dir, output_filename),
           exists = file.exists(output_file)) %>%
    filter(!exists) %>%
    dplyr::select(-output_file, -output_filename, -exists)
}

# subset grid, if needed
if (nrow(grid0) >= 10000) {
  grid0 %<>% dplyr::slice(1:9900) ## allow for some other running jobs or sh
}

# write the grid that still needs to be run
grid_file = "sh/analysis/fdr_control/grids/fdr_control.txt"
grid_dir = dirname(grid_file)
if (!dir.exists(grid_dir))
  dir.create(grid_dir, recursive = T)
write.table(grid0, grid_file, quote = F, row.names = F, sep = "\t")

# finally, run the job on whatever system we're on
sh_dir = "~/git/pseudobulk-benchmark/sh/analysis/fdr_control"
script = ifelse(system == 'cedar',
                file.path(sh_dir, "fdr_control.sh"),
                ifelse(system == 'sockeye',
                  file.path(sh_dir, "fdr_control.torque.sh"),
                  file.path(sh_dir, "fdr_control.elasti.sh"))
                )
submit_job(grid0, script, args$allocation, system)
