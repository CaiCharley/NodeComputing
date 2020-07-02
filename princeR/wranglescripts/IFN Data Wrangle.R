# wrangle IFN Data
setwd("D:/Downloads/Craig")

# load libraries
library(tidyverse)
library(magrittr)

# load data
file_name <- list.files(getwd())

files <- map(file_name, ~ read_csv(., col_types = cols(
  Protein = col_character(),
  Gene = col_character(),
  .default = col_double()
)) %>%
  select(-Gene) %>%
  column_to_rownames(var = "Protein")) %>%
  setNames(str_replace(file_name, ".csv$", ""))

# save data
saveRDS(files[1:3], "craig_ifnstim.rds")
saveRDS(files[4:6], "craig_ifnunstim.rds")