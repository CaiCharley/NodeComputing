# apply autocorrelation to DIFFRAC dataset

setwd("D:/Downloads/foster/autocorrelation/diffrac/")

library(tidyverse)
library(magrittr)
library(PrInCE)

# import files
read_diffrac <- function(file) {
  read_tsv(file) %>%
    select(
      `Majority protein IDs`,
      `Gene names`,
      contains("iBAQ"),
      contains("LFQ"),
      -iBAQ
    ) %>%
    mutate(
      Gene = str_remove(`Gene names`, ";.*"),
      Protein = str_remove(`Majority protein IDs`, ";.*"),
      .keep = "unused",
      .before = 1
    )
}

files <- list.files(getwd(), ".txt.gz") %>%
  map(~ read_diffrac(.)) %>%
  setNames(str_remove(list.files(getwd()), ".txt.gz"))

# separate iBAQ and FLQ fractions
split_fraction_type <- function(tbl) {
  lfq <- select(
    tbl,
    `Protein`,
    contains("LFQ")
  )
  ibaq <- select(
    tbl,
    `Protein`,
    contains("iBAQ")
  )
  tmp <- list(ibaq, lfq)
  names(tmp) <- c("iBAQ", "LFQ")
  return(tmp)
}

split <- map(files, ~ split_fraction_type(.))

# convert to matricies
to_matrix <- function(tbl) {
  tbl %>%
    column_to_rownames(var = "Protein") %>%
    as.matrix()
}
mats <- map(split, ~ map(., ~ to_matrix(.)))

# separate controls and experimental
control <- mats[str_detect(names(mats), "control")]
rnase <- mats[str_detect(names(mats), "control", negate = T)]

# calculate autocorrelation
map_auto <- function(ctrl, rnase) {
  map2(
    ctrl, rnase,
    ~ calculate_autocorrelation(.x, .y) %>%
      sort()
  )
}

autocors <- map2(control, rnase, ~ map_auto(.x, .y)) %>%
  setNames(str_remove(names(control), "_.*"))

# tidy data
tblcors <- map(autocors, ~ map(., ~ as_tibble(., rownames = "Protein")))

tidycors <- bind_rows(
  map(tblcors, ~ bind_rows(., .id = "type")),
  .id = "dataset"
)

# add genes
genekey <- bind_rows(
  files$Homo15406_control,
  files$Mus14607_control,
  files$Xenopus17650_rnase
) %>%
  select(1, 2)

tidycors %<>% left_join(genekey, "Protein") %>%
  relocate(Gene, .after = Protein)

saveRDS(tidycors, "autocorrelations.R")