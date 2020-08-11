# wrangle RBP data

setwd("D:/Downloads/foster/autocorrelation/RBP/")

library(tidyverse)
library(magrittr)

# import data
rbpdb <- list.files(getwd(), "RBPDB") %>%
  read_csv(col_names = F) %>%
  select(5, 7) %>%
  setNames(c("Gene", "Species")) %>%
  filter(!is.na(Gene))

attract <- list.files(getwd(), "ATtRACT") %>%
  read_tsv() %>%
  filter(Organism == "Homo_sapiens") %>%
  arrange(desc(Score)) %>%
  distinct(Gene_name, .keep_all = T) %>%
  transmute(
    Gene = Gene_name,
    Species = Organism,
  )

rbps <- list(rbpdb, attract) %>%
  map(~ (.$Gene)) %>%
  setNames(c("rbpdb", "attract"))