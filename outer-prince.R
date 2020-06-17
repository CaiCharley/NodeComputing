# Benchmarking PrInCE with different conditions using Compute Canada Node
setwd("~/OneDrive/git/PrinceR")

# "!" indicated things user may need to change

# parse arguments
library(argparse)
parser = ArgumentParser()
parser$add_argument('--allocation', type = 'character', default = "rrg-ljfoster-ab")
parser$add_argument('--name', type = 'character', default = "ppis") 
parser$add_argument("-s", "--submit", action="store_true", default=FALSE,
                    help="Submits Job. Otherwise only updates grid")
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

# list input files ! 
input_dir <- file.path(base_dir, "scottdata")
input_files <- file.path(input_dir, c(
  "bn_unstim.rds",
  "bn_stim.rds",
  "sec_unstim.rds",
  "sec_stim.rds")
) 
  
# generate grid of argument permutations !
options <- list(
  input_file = input_files,   # make sure input_file is first
  classifier = c("NB", "SVM", "RF", "LR", "ensemble"),
  nmodels = c(1, 3, 10)
)

grid <- expand.grid(options, stringsAsFactors = F)
colnames(grid) <- names(options)

# set output directory
output_dir = file.path(base_dir, args$name)
if (!dir.exists(output_dir))
  dir.create(output_dir, recursive = T)

#check which jobs are already complete
overwrite = F
if (!overwrite) {
  optionprefix <- lapply(names(options[-1]), function(x) paste0("-", x, "="))
  not_done = NULL
   for(job in 1:nrow(grid)) {
    expected_output = grid[job, "input_file"] %>% 
      basename() %>%
      gsub("\\.rds$", "", .)  # ! input file type
    expected_output <- 
      do.call(paste0, 
              append(expected_output, 
                      lapply(names(options[-1]), function(x) grid[job, x]) %>%
                        paste0(optionprefix, .) %>%
                        as.list()) %>%
                append(".csv.gz"))       # ! output file extension
    if (!(file.path(output_dir, expected_output) %>% file.exists()))
      not_done <- c(not_done, job)
    
   }
  grid <- slice(grid, not_done)
}

# write grid
if (plyr::empty(grid)) {
  message("All Jobs Completed")
} else {
  write.table(grid, file.path(base_dir, paste(args$name, "grid.txt", sep = "_")), 
              quote = F, row.names = F, sep = "\t")
  
  message(paste(nrow(grid), "jobs remaining.",
                "\nUpdated", args$name, "grid file at", 
                file.path(base_dir, paste(args$name, "grid.txt", sep = "_"))))
}

# submits job
script = file.path(getwd(), "bench-prince.sh")
if(args$submit){
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
}