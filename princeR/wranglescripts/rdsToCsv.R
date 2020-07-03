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
  map(~ bind_rows(., .id = "Replicate") %>%
    relocate(ProteinID))

# save files
map2(
  files, paste0(file_names, ".csv") %>%
    file.path(getwd(), "dataML", .),
  ~ write_csv(.x, .y)
)