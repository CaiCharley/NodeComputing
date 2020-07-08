# Submits job arrays to Compute Canada node

# parse arguments
library(argparse)
parser <- ArgumentParser()
parser$add_argument("--allocation",
  type = "character", default = "rrg-ljfoster-ab"
)
parser$add_argument("--name",
  type = "character", required = T
)
parser$add_argument("--project",
  type = "character", default = "princeR"
)
parser$add_argument("-s", "--submit",
  action = "store_true", default = FALSE,
  help = "Submits Job. Otherwise only updates grid"
)
parser$add_argument("-r", "--removelogs",
  action = "store_true", default = FALSE,
  help = "Removes log files in which the job successfully completed"
)
args <- parser$parse_args()

# check project and job directories exist
if (!dir.exists(
  file.path("~/OneDrive/git/NodeComputing", args$project, args$name)
)) {
  message("The project or job scripts folder does not exist.")
  stop()
}

setwd(file.path("~/OneDrive/git/NodeComputing", args$project))

# load libraries
library(tidyverse)
library(magrittr)

# system type
system <- Sys.info()[["nodename"]]

if (grepl("cedar", system)) {
  base_dir <- file.path(
    "/home/caic/projects/rrg-ljfoster-ab/caic", args$project
  )
} else {
  base_dir <- "/home/charley/OneDrive/Academic/Foster Lab/PrInCER/CC"
}

# set output directory
output_dir <- file.path(base_dir, args$name)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = T)
}

# source job specific functions
source(file.path(getwd(), args$name, "outer_helper.R"))

# generate grid of argument permutations
input_files <- get_inputs()
options <- get_options()

grid <- expand.grid(options, stringsAsFactors = F)
colnames(grid) <- names(options)

grid %<>% modify_grid()

# check which jobs are already complete
overwrite <- F
if (!overwrite) {
  optionprefix <- paste0("-", names(options[-1]), "=")
  not_done <- vector("integer", nrow(grid))
  for (row in seq_len(nrow(grid))) {
    if (!job_done(as.list(grid[row, ]))) {
      not_done[[row]] <- row
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

# handles logs
logs_dir <- file.path(base_dir, args$name, "logs")
logs_path <- file.path(logs_dir, "%x-%A-%a.out")
if (!dir.exists(logs_dir)) {
  dir.create(logs_dir, recursive = T)
}

if (args$removelogs) {
  system(paste(
    file.path(dirname(getwd()), "remove_logs.sh"), logs_dir
  ))
}

# submits job
script <- file.path(
  getwd(), args$name,
  paste0(args$name, "_", args$project, ".sh")
)

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