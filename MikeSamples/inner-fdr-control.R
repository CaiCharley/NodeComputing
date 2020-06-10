# Analyze FDR control in single-cell or pseudobulk DE analyses by shuffling
# the labels of individual cells or entire replicates.
setwd("~/git/pseudobulk-benchmark")
options(stringsAsFactors = F)
library(argparse)

# parse arguments
parser = ArgumentParser(prog = 'inner-fdr-control.R')
parser$add_argument('--input_file', type = 'character', required = T)
parser$add_argument('--output_dir', type = 'character', required = T)
parser$add_argument('--de_test', type = 'character', required = T)
parser$add_argument('--downsample_n', type = 'character', required = T)
parser$add_argument('--shuffle', type = 'character', 
                    choices = c('cells', 'replicates'), required = T)
args = parser$parse_args()
print(args)

library(tidyverse)
library(magrittr)
library(Seurat)
library(Matrix)
library(DropletUtils)
source("R/functions/run_DE.R")
source("R/functions/get_comparisons.R")

# fix downsample_n
args$downsample_n %<>% as.integer()

# set up output filepath
if (!dir.exists(args$output_dir))
  dir.create(args$output_dir, recursive = T)
dataset = args$input_file %>%
  basename() %>% 
  gsub("\\.rds$", "", .)
output_filename = paste0(dataset, 
                         "-de_test=", args$de_test,
                         "-shuffle=", args$shuffle,
                         ifelse(!is.na(args$downsample_n),
                                paste0("-downsample_n=", args$downsample_n),
                                ""),
                         ".rds")
output_file = file.path(args$output_dir, output_filename)

# read input file and extract matrix/metadata
sc = readRDS(args$input_file)
expr = GetAssayData(sc, slot = 'counts')
meta = sc@meta.data

# get all combinations of conditions
results = list()
comparisons = get_comparisons(dataset, expr, meta)
for (comparison_idx in seq_along(comparisons)) {
  comparison = comparisons[[comparison_idx]]
  comparison_name = names(comparisons)[comparison_idx]
  if (is.null(comparison_name))
    comparison_name = 1
  
  message("[", comparison_idx, "/", length(comparisons), "] ",
          "analyzing comparison ", comparison_name, " ...")
  message("##############################")
  
  # get subset expression and metadata
  expr0 = comparison$expr
  meta0 = comparison$meta %>%
    # labels must be characters to swap replicates!
    mutate(label = as.character(label)) %>%
    set_rownames(colnames(expr0))
  
  # optionally, downsample to a set of random replicates
  if (!is.na(args$downsample_n)) {
    # sample a random set of replicates
    set.seed(0)
    replicates = meta0 %>%
      distinct(replicate, label) %>%
      group_by(label) %>%
      sample_n(args$downsample_n) %>%
      ungroup()
    meta0 %<>%
      rownames_to_column(var = 'tmp_rownames') %>%
      inner_join(replicates, by = c('replicate', 'label')) %>%
      column_to_rownames(var = 'tmp_rownames')
    expr0 %<>% extract(, rownames(meta0))
  }
  
  # shuffle metadata
  if (args$shuffle == "cells") {
    set.seed(0)
    meta0 %<>%
      group_by(cell_type) %>%
      mutate(label = sample(label)) %>%
      ungroup()
  } else if (args$shuffle == "replicates") {
    # shuffle the label associated with each replicate
    replicates = meta0 %>%
      distinct(label, replicate) %>%
      arrange(label, replicate) %>%
      # swap labels on even numbered rows
      mutate(idx = row_number(),
             swap = idx %% 2 == 0)
    meta0 %<>% left_join(replicates, by = c('replicate', 'label'))
    meta0$label %<>% 
      map2_chr(meta0$swap, ~ ifelse(.y, setdiff(unique(meta0$label), .x), .x))
  } else {
    stop("not sure what to do for shuffle: '", args$shuffle, "'")
  }
  
  # reconstruct the Seurat object
  sc0 = CreateSeuratObject(expr0, min.cells = 0, min.features = 0,
                           meta.data = meta0 %>% set_rownames(colnames(expr0)))
  
  # run DE analysis
  DE = run_DE(sc0, de_test = args$de_test)
  
  # append to list
  results[[comparison_name]] = DE
}

# save results
saveRDS(results, output_file)
