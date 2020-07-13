# generate heatmap with detect_complexes on scott dataset
setwd("/mnt/d/Downloads/scottdata")

library(tidyverse)
library(magrittr)
library(heatmap3)

# load data as individual list element
goldstd <- readRDS("goldstd.rds")
file_names <- list.files(pattern = "[^goldstd.rds]")
files <- map(file_names, ~ readRDS(.)) %>%
  setNames(str_replace(file_names, ".rds", "")) %>%
  unlist(recursive = F)

# detect complexes
goldcmplx <- goldstd[lengths(goldstd) > 2]

scott_complexes <- map(files, ~ detect_complexes(., goldcmplx) %>%
  na.omit() %>%
  as_tibble(rownames = "complex"))

# full join
joined_complexes <- reduce(scott_complexes, full_join, by = "complex") %>%
  column_to_rownames(var = "complex") %>%
  set_colnames(names(scott_complexes)) %>%
  as.matrix()
joined_complexes[is.na(joined_complexes)] <- 0

# save heatmap
png(file = "heatmap.png", 8000, 6000, pointsize = 12)
heatmap3(joined_complexes, margins = c(8, 25), na.rm = T)
dev.off()