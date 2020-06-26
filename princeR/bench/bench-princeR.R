# Benchmark PrInCE on Scott's dataset with set arguments
setwd("~/OneDrive/git/NodeComputing/princeR")

# parse arguments
library(argparse)
parser <- ArgumentParser()
parser$add_argument("--input_file", type = "character", required = T)
parser$add_argument("--output_dir", type = "character", required = T)
parser$add_argument("--classifier", type = "character", required = T)
parser$add_argument("--nmodels", type = "integer", required = T)
args <- parser$parse_args()
print(args)

# load Libraries
library(tidyverse)
library(PrInCE)
library(microbenchmark)
library(peakRAM)

# load input files
goldstd <-
  readRDS(dirname(args$input_file) %>% paste0("/goldstd.rds"))
dataname <- gsub("\\.rds$", "", basename(args$input_file))
dataset <- readRDS(args$input_file)

# setup output file path
if (!dir.exists(args$output_dir)) {
  dir.create(args$output_dir, recursive = T)
}

conditions <- args[grep("input_file|output_dir", names(args), invert = T)]

condnames <- ""
for (i in seq_len(length(conditions))) {
  condnames <- paste0(
    condnames, "-",   # conditions will be listed alphabetically
    names(conditions)[i], "=",
    conditions[i]
  )
}
output_filename <- paste0(
  dataname,  # input file
  condnames, # conditions
  ".csv"     # file type
)
output_file <- file.path(args$output_dir, output_filename)

# benchmarks Prince
ram <- peakRAM(
  temp <- microbenchmark("Prince" = {
    PrInCE(dataset, goldstd,
      classifier = args$classifier,
      models = args$nmodels
    )
  }, times = 3, unit = "s")
)

time <- summary(temp)

benchmark <- cbind(time, ram["Peak_RAM_Used_MiB"])
benchmark[1] <- paste0(dataname, condnames)

# creates master file for all the benchmarks
benchmarks_path <- file.path(args$output_dir, "benchmarks.rds")

if (!file.exists(benchmarks_path)) {
  saveRDS(benchmark, benchmarks_path)
} else {
  benchmarks <- readRDS(benchmarks_path)
  benchmarks <- rbind(benchmarks, benchmark)
  saveRDS(benchmarks, benchmarks_path)
}

# write out (only to update grid file)
write(NULL, file = paste0(output_file, ".gz"))
message("Done")