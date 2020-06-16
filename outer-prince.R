# Benchmarking PrInCE with different conditions using Compute Canada Node
setwd("~/OneDrive/git/PrinceR")

# parse arguments
library(argparse)
parser = ArgumentParser()
parser$add_argument('--allocation', type = 'character', default = "rrg-ljfoster-ab")
parser$add_argument('--name', type = 'character', default = "ppis") 
args = parser$parse_args()

# load libraries
library(tidyverse)
library(magrittr)

# system type
system = Sys.info()[["nodename"]]

if (grepl("cedar", system)) {
  base_dir = "/home/caic/projects/rrg-ljfoster-ab/caic/PrInCE"
} else {
  base_dir = "/home/charley/OneDrive/2019 Term 1/Foster Lab/PrInCER/CC"
}

# list input files
input_dir <- file.path(base_dir, "scottdata")
input_files <- file.path(input_dir, c(
  "bn_unstim.rds",
  "bn_stim.rds",
  "sec_unstim.rds",
  "sec_stim.rds")
) 
  
# set output directory
output_dir = file.path(base_dir, args$name)
if (!dir.exists(output_dir))
  dir.create(output_dir, recursive = T)

# generate grid of argument permutations
options <- list(
  input_file = input_files,
  classifier = c("NB", "SVM", "RF", "LR", "ensemble"),
  nmodels = c(1, 3, 10)
)

grid <- expand.grid(options, stringsAsFactors = F)
colnames(grid) <- names(options)

#check which jobs are already complete
overwrite = F
if (!overwrite) {
  grid = grid %>%
    filter(!(                       # negates if file exists
      paste(
        basename(input_file) %>%    
          gsub("\\.rds$", "", .),
        classifier,
        nmodels,
        sep = "_") %>%              # creates file name with conditions
      paste0(".csv.gz") %>%         # output file type
      file.path(output_dir, .) %>%  # creates expect file dir
      file.exists()                 # checks if file exists
    ))
}

# write grid
write.table(grid, file.path(base_dir, paste(args$name, "grid.txt", sep = "_")), 
            quote = F, row.names = F, sep = "\t")

# submits job
script = file.path(getwd(), "bench-prince.sh")

if (grepl("cedar", system)) {
    system(
      paste0("cd ", "'", base_dir,"'; ",
            "sbatch ", "--account=", args$allocation,
            " --job-name=", args$name, 
            " --array=1-", nrow(grid),
            " --export=ALL,NAME=,", args$name, " ", script)
  )
} else {
  system(paste(script, args$name))
}