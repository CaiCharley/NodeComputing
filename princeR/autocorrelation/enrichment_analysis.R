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

# generate negative control pathway
control <- pull(key, Gene) %>%
  sample(1000)

rbps <- c(rbps, list(control))
names(rbps)[8] <- "Control"

# run enrichment analysis
calc_enrichment <- function(cor, pathways) {
  fgsea(
    pathways,
    pull(cor, value, name = Gene),
    nperm = 10^6,
    scoreType = "neg"
  )
}

enrichment <- map(humanauto, ~ calc_enrichment(., rbps))

# graph
tidyenrichDB <- bind_rows(enrichment, .id = "replicate") %>%
  filter(pathway %in% c("rbpdb", "attract", "Control"))
tidyenrichDB$pathway <- factor(tidyenrichDB$pathway,
  levels = c("attract", "rbpdb", "Control")
)

tidyenrichHTS <- bind_rows(enrichment, .id = "replicate") %>%
  filter(!(pathway %in% c("rbpdb", "attract")))

ggplot(tidyenrichDB, aes(replicate, -log10(pval))) +
  geom_point(aes(color = pathway)) +
  geom_hline(yintercept = -log10(0.05)) +
  ylim(0, 7) +
  scale_colour_discrete(
    name = "Database",
    labels = c("ATtRACT", "RBPDB", "Control")
  ) +
  ggtitle("1.a) P values for RBP Databases") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  )
ggsave("RBP Database Enrichment+ctrl.png", width = 5, height = 5)

ggplot(tidyenrichHTS, aes(replicate, -log10(pval))) +
  geom_point(aes(color = pathway)) +
  geom_hline(yintercept = -log10(0.05)) +
  ylim(0, 7) +
  ggtitle("1.b) P values for HTS Databases") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
  )
ggsave("HTS Enrichment.png", width = 5, height = 5)

# find how many times each protein occurs

HTSRBPnames <- reduce(rbps[-c(1, 2)], union)

HTSRBPntimes <- map(
  HTSRBPnames,
  function(x) {
    map_lgl(
      rbps,
      function(y) {
        x %in% y
      }
    ) %>%
      sum()
  }
) %>%
  setNames(HTSRBPnames)

HTSRBP <- map(1:5, ~ HTSRBPntimes[HTSRBPntimes >= .] %>%
  names()) %>%
  setNames(paste0(1:5, "+"))

# calculate enrichment
enrichmentHTSn <- map(humanauto, ~ calc_enrichment(., HTSRBP)) %>%
  bind_rows(.id = "replicate")

# graph
ggplot(enrichmentHTSn, aes(pathway, -log10(pval))) +
  geom_point(aes(color = replicate)) +
  geom_hline(yintercept = -log10(0.05)) +
  ylim(0, 7) +
  xlab("Times Found") +
  ggtitle("2) P values for RBP Found Across Multiple HTS")

ggsave("RBP across HTS.png", width = 5, height = 5)