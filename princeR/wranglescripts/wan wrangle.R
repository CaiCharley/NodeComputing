# wrangle wan et al.'s data for Prince

setwd("C:/Users/User/Downloads/wan")

library(tidyverse)
library(magrittr)

# load data
file_dirs <- list.files(getwd())
files <- map(file_dirs, ~ readRDS(.) %>%
  as_tibble(rownames = "Proteins") %>%
  mutate(
    Protein = str_replace(Proteins, ";.*$", ""),
    .after = 1, .keep = "unused"
  )) %>%
  setNames(str_replace(file_dirs, ".rds$", ""))