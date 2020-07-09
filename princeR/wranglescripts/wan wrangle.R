# wrangle wan et al.'s data for Prince

setwd("/mnt/d/Downloads/iBAQ")

library(tidyverse)
library(magrittr)

# load data
names <- paste0("wan_iBAQ_", 1:11)

file_dirs <- list.files(getwd())
files <- map(file_dirs, ~ readRDS(.) %>%
  as_tibble(rownames = "Proteins") %>%
  mutate(
    Protein = str_replace(Proteins, ";.*$", ""),
    .after = 1, .keep = "unused"
  ) %>%
  mutate_if(is.numeric, ~ na_if(., 0)) %>%
  column_to_rownames(var = "Protein")) %>%
  setNames(names)

# save each individually
for (i in seq_along(files)) {
  saveRDS(files[[i]], paste0(names[i], ".rds"))
}

# save all as replicates
saveRDS(files, "wan_all_replicates.rds")