# wrangle wan et al.'s data for Prince

setwd("/mnt/d/Downloads/iBAQ")

library(tidyverse)
library(magrittr)

# load data
names <- paste0("wan_iBAQ_", 1:9)

file_dirs <- list.files(getwd())
files <- map(file_dirs, ~ readRDS(.) %>%
  as_tibble(rownames = "Proteins") %>%
  mutate(
    Protein = str_replace(Proteins, ";.*$", ""),
    .after = 1, .keep = "unused"
  ) %>% 
  column_to_rownames(var = "Protein")) %>% 
  setNames(names)

for (i in seq_along(files)) {
  saveRDS(files[[i]], paste0(names[i], ".rds"))
}