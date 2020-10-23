# Run PrInCE on Scott's dataset with set arguments
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

# load input files
goldstd <-
  readRDS(file.path(dirname(args$input_file), "goldstd.rds"))
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
    condnames, "-", # conditions will be listed alphabetically
    names(conditions)[i], "=",
    conditions[i]
  )
}
output_filename <- paste0(
  dataname, # input file
  condnames, # conditions
  ".csv" # file type
)
runtime_filename <- paste0(
  dataname, # input file
  condnames, # conditions
  "-runtime.csv" # file type
)

output_file <- file.path(args$output_dir, output_filename)
runtime_file <- file.path(args$output_dir, runtime_filename)

# results
results <- PrInCE(dataset,
  goldstd,
  classifier = args$classifier,
  models = args$nmodels,
  runtime = T
)

# runtime
runtime <- attributes(results)[-3:-1]

# write out
write.csv(results, file = output_file)
write.csv(runtime, file = runtime_file)
system(paste("gzip --force", output_file))
message("Done")