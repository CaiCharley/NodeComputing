# Analyze RNA-binding protein enrichment

library(tidyverse)
library(magrittr)
library(fgsea)

# load RBPs
source("C:/Users/Charley/OneDrive/git/NodeComputing/princeR/autocorrelation/wrangle_rbps.R")
setwd("D:/Downloads/foster/autocorrelation/")

# load autocorrelations
autocors <- readRDS(file.path(getwd(), "diffrac", "autocorrelations.rds")) %>%
  filter(str_detect(dataset, "Homo")) %>%
  group_by(dataset, type)

humanauto <- autocors %>%
  filter(!is.na(Gene)) %>%
  group_split() %>%
  setNames(do.call(paste, group_keys(autocors))) %>%
  map(
    ~ select(., Gene, value) %>%
      distinct(Gene, .keep_all = T)
  )

# run enrichment analysis
calc_enrichment <- function(cor) {
  fgseaSimple(
    rbps,
    pull(cor, value, name = Gene),
    nperm = 10^6,
    scoreType = "neg"
  )
}

enrichment <- map(humanauto, ~ calc_enrichment(.))