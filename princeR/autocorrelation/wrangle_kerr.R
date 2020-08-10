# apply autocorrelation function on Kerr et al. data

setwd("D:/Downloads/foster/autocorrelation/Kerr et al/")

library(tidyverse)
library(magrittr)
library(PrInCE)

# import data
wrangle_kerr <- function(file) {
  read_csv(
    file,
    cols(
      Protein = col_character(),
      Gene = col_character(),
      .default = col_double()
    )
  ) %>%
    select(-Gene) %>%
    column_to_rownames(var = "Protein") %>%
    as.matrix()
}

files <- map(1:3, ~ list.files(getwd(), paste0(., ".csv$")))

chroms <- map(files, ~ map(., ~ wrangle_kerr(.)))

# calculate autocorrelation
cor <- map(chroms, ~ do.call(autocorrelation, .))

autocor <- names(cor[[3]]) %>%
  map_dbl(~ mean(c(
    cor[[1]][.],
    cor[[2]][.],
    cor[[3]][.]
  ), na.rm = T)) %>%
  setNames(names(cor[[3]])) %>%
  as_tibble(rownames = "Protein")

# relate to gene names
genes <- read_csv(file.path(getwd(), "M_L-replicate-3.csv")) %>%
  select(1, 2)

autocor %<>% left_join(genes, "Protein") %>%
  relocate(Gene, .after = Protein) %>%
  arrange(value)

# generate histogram
ggplot(autocor, aes(value)) +
  geom_histogram()