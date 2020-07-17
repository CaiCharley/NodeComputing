# generate heatmap with detect_complexes on scott dataset
setwd("/mnt/d/Downloads/scottdata")

library(tidyverse)
library(magrittr)

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
  set_colnames(names(scott_complexes))
joined_complexes[is.na(joined_complexes)] <- 0

# filter significant complexes in at least one sample
significant_complexes <- joined_complexes %>% 
  as_tibble(rownames = "complex") %>% 
  rowwise() %>% 
  mutate(
    nsig = sum(c_across(where(is.numeric)) >= 1.96),
    bnsig = sum(c_across(contains("bn")) >= 1.96),
    secsig = sum(c_across(contains("sec")) >= 1.96),
    ) %>% # ! z score
  filter(bnsig >= 0 && secsig == 3) %>% # ! number of significant replicates
  select(-nsig, -bnsig, -secsig) %>% 
  column_to_rownames(var = "complex")

# hcluster rows and columns
rlevels <- hclust(dist(as.matrix(significant_complexes)))
rlevels <- rlevels$labels[rlevels$order]
clevels <- hclust(dist(t(as.matrix(significant_complexes))))
clevels <- clevels$labels[clevels$order]

# make tidy tibble
tidy_complexes <- pivot_longer(
  significant_complexes %>%
    rownames_to_column(var = "complex"),
  cols = is_numeric,
  names_to = "dataset",
  values_to = "zscore"
)

# cluster
tidy_complexes %<>%
  mutate(
    complex = factor(complex, levels = rlevels),
    dataset = factor(dataset, levels = clevels)
  )

# make heatmap
ggplot(tidy_complexes, aes(x = dataset, y = complex, fill = zscore)) +
  geom_tile() +
  theme(text = element_text(size = 20))
ggsave(
  "heatmap_0bn3sec.png",
  plot = last_plot(), device = "png", width = 30, height = 25)