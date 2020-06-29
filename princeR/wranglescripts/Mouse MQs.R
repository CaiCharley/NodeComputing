# wrangle michael's mouse MQ files

setwd("C:/Users/User/Downloads/Temp/Mouse")

library(tidyverse)
library(magrittr)

# load data

file_dirs <- list.files(getwd())

files <- map(file_dirs, ~ read_csv(., col_types = cols(
  `Major Protein group` = col_character(),
  `Replicate number` = col_factor(),
  .default = col_double()
)) %>%
  group_split(`Replicate number`, .keep = F)) %>%
  setNames(str_replace_all(file_dirs, c("^maxquant" = "mouse", ".csv$" = "")))

names <- names(files)
for (i in seq_along(files)) {
  saveRDS(files[[i]], paste0(names[i], ".rds"))
}