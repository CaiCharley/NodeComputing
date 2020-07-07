# convert data from R Prince .rds to Matlab .csv
setwd("~/Temp")

library(tidyverse)
library(magrittr)

# import files
read_as_lists <- function(file) {
  data <- readRDS(file)
  if (is.data.frame(data)) {
    data %<>%
      as_tibble(rownames = "ProteinID")
    return(list(data))
  } else if (is.list(data)) {
    data %<>%
      map(~ as_tibble(., rownames = "ProteinID"))
    return(data)
  }
}

file_dirs <- list.files(getwd(), recursive = T)
file_names <- map_chr(file_dirs, ~ str_replace(., ".rds", ""))
files <- file_dirs %>%
  map(~ read_as_lists(.)) %>%
  setNames(file_names)

# aggregate replicates
files %<>%
  map(~ unname(.) %>%
    bind_rows(.id = "Replicate") %>%
    relocate(ProteinID))

# get replicate and fraction number
string_args <- function(datas) {
  replicates <- map_dbl(datas, ~ select(., "Replicate") %>%
    unique() %>%
    nrow())
  fractions <- map_dbl(datas, ~ ncol(.) - 2)
  sprintf("-frac=%i-rep=%i", fractions, replicates)
}

# save files
output_dir <- file.path(getwd(), "dataML")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

map2(
  files, paste0(file_names, string_args(files), ".csv") %>%
    file.path(output_dir, .),
  ~ write_csv(.x, .y)
)