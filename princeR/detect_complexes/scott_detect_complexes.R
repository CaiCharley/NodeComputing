# generate heatmap with detect_complexes on scott dataset
setwd("/mnt/c/Users/User/Downloads/scottdata")

library(tidyverse)
library(magrittr)
library(pals)

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
  filter(bnsig >= 3 && secsig == 0) %>% # ! number of significant replicates
  # filter(nsig >= 3) %>% # ! number of significant replicates
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
  geom_tile(color = "white") +
  scale_fill_gradientn(colours = gnuplot(25), guide = "colourbar") +
  scale_x_discrete(
    expand = c(0, 0),
    breaks = clevels,
    labels = str_replace_all(
      clevels,
      c(
        "sec" = "SEC,", "bn" = "BN,", "_" = " ",
        "unstim" = "M/L", "stim" = "H/L", "1" = " (1)",
        "2" = " (2)", "3" = " (3)"
      )
    )
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(
    text = element_text(size = 10, family = "TT Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "grey50", fill = NA),
    aspect.ratio = 1,
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
ggsave(
  "heatmap_3bn0sec.png",
  plot = last_plot(), device = "png", unit = "cm", width = 15, height = 10
)