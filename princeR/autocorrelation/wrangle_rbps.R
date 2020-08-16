# wrangle RBP data

setwd("D:/Downloads/foster/autocorrelation/RBP/")

library(tidyverse)
library(magrittr)
library(readxl)

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

# Labelled as RNA binding
baltz2012 <- list.files(getwd(), "Baltz") %>%
  read_excel(skip = 1) %>%
  filter(`RNA binding` == "x") %>%
  transmute(Gene = `Offical gene symbol`, Species = "Homo Sapiens")

# mRNA interactome identified in HeLa
castello2012 <- list.files(getwd(), "Castello") %>%
  read_excel() %>%
  filter(`mRNAbinding` == "mRNA-interactome", IdentifiedCL == "identified") %>%
  transmute(Gene = `Symbol`, Species = "Homo Sapiens")

# RICK RBPs
bao2018 <- list.files(getwd(), "Bao") %>%
  read_excel(sheet = "Table 3i", skip = 2) %>%
  transmute(Gene = Gene, Species = "Homo Sapiens")

# enigmRBPs
beckmann2015 <- list.files(getwd(), "Beckmann") %>%
  read_excel(skip = 2) %>%
  filter(enigmRBP == "yes") %>%
  transmute(
    Gene = `human mRNA interactomes_ENSEMBL Gene Name`,
    Species = "Homo Sapiens"
  )


# join datasets
rbps <- list(
  rbpdb,
  attract,
  baltz2012,
  castello2012,
  bao2018,
  beckmann2015
) %>%
  map(~ (.$Gene)) %>%
  setNames(c("rbpdb", "attract"))