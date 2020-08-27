# Make Protein-Gene key
setwd("D:/Downloads/foster/autocorrelation/")

library(tidyverse)

key <- read_tsv("HUMAN_9606_idmapping.dat", col_names = F) %>%
  filter(X2 == "Gene_Name") %>%
  select(1,3) %>%
  setNames(c("Protein", "Gene"))