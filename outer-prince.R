# Submits job arrays to Compute Canada node
# "!" indicated things user may need to change

# parse arguments
library(argparse)
parser <- ArgumentParser()
parser$add_argument("--allocation",
  type = "character", default = "rrg-ljfoster-ab"
)
parser$add_argument("--name",
  type = "character", default = "ppis", choices = c("ppis", "bench")
)
parser$add_argument("--project",
  type = "character", default = "princeR", choices = c("princeR")
)
parser$add_argument("-s", "--submit",
  action = "store_true", default = FALSE,
  help = "Submits Job. Otherwise only updates grid"
)
args <- parser$parse_args()

setwd(file.path("~/OneDrive/git/NodeComputing/", args$project))

# load libraries
library(tidyverse)
library(magrittr)

# system type
system <- Sys.info()[["nodename"]]

if (grepl("cedar", system)) {
  base_dir <- file.path(
    "/home/caic/projects/rrg-ljfoster-ab/caic/", args$project
  )
} else {
  base_dir <- "/home/charley/OneDrive/Academic/Foster Lab/PrInCER/CC"
}

# list input files !
input_dir <- file.path(base_dir, "scottdata")
input_files <- file.path(input_dir, c(
  "bn_unstim.rds",
  "bn_stim.rds",
  "sec_unstim.rds",
  "sec_stim.rds"
))

# generate grid of argument permutations !
options <- list(
  input_file = input_files, # make sure input_file is first
  classifier = c("NB", "SVM", "RF", "LR", "ensemble"),
  nmodels = c(1, 3, 10)
)

grid <- expand.grid(options, stringsAsFactors = F)
colnames(grid) <- names(options)

# set output directory
output_dir <- file.path(base_dir, args$name)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = T)
}

# check which jobs are already complete
overwrite <- F
if (!overwrite) {
  optionprefix <- paste0("-", names(options[-1]), "=")
  not_done <- NULL
  for (job in seq_len(nrow(grid))) {
    expected_output <- grid[job, "input_file"] %>%
      basename() %>%
      gsub("\\.rds$", "", .) # ! input file type
    expected_output <-
      paste0(
        expected_output,
        paste0(optionprefix, grid[job, names(options[-1])], collapse = ""),
        ".csv.gz" # ! output file extension
      )
    if (!(file.path(output_dir, expected_output) %>% file.exists())) {
      not_done %<>% c(job)
    }
  }
  grid %<>% slice(not_done)
}

# write grid
if (plyr::empty(grid)) {
  message("All Jobs Completed")
} else {
  grid_path <- file.path(
    getwd(), args$name,
    paste(args$name, "grid.txt", sep = "_")
  )
  write.table(grid, grid_path, quote = F, row.names = F, sep = "\t")
  message(sprintf(
    "%d jobs remaining.\nUpdated %s grid file at %s.",
    nrow(grid), args$name, grid_path
  ))
}

# submits job
script <- file.path(
  getwd(), args$name,
  paste0(args$name, "-", args$project, ".sh")
)
logs_path <- file.path(base_dir, args$name, "logs", "%x-%A-%a.out")
if (!dir.exists(dirname(logs_path))) {
  dir.create(dirname(logs_path), recursive = T)
}

if (args$submit) {
  if (grepl("cedar", system)) {
    system(
      sprintf(
        paste(
          "cd '%s'; sbatch --account=%s --job-name=%s --array=1-%d",
          "--output=%s --export=ALL,NAME=%s,PROJECT=%s %s"
        ),
        base_dir, args$allocation, args$name, nrow(grid), logs_path,
        args$name, args$project, script
      )
    )
  } else {
    system(paste(script, args$name))
  }
}