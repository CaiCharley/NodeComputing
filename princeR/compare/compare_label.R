# compare first x interactions between R and Matlab ppis
setwd("D:/Downloads/foster/compare")

library(tidyverse)
library(magrittr)
library(PrInCE)

# wrangle ml files
mlfiles <- list.files(getwd(), "ml.csv$")

read_ml <- function(file) {
  read_csv(file) %>%
    select(1, 5:8) %>%
    arrange(desc(`Interaction score (avg.)`)) %>%
    mutate(
      n = row_number(),
      label =
        ifelse(!`Both proteins in Corum`, NA,
          ifelse(`Interaction in Corum`, 1, 0)
        )
    )
}

mlppis <- map(mlfiles, ~ read_ml(.)) %>%
  setNames(str_replace(mlfiles, ".csv", ""))

mlppis %<>% map(~ cbind(.,
  precision = calculate_precision(as_vector(select(., label)))
) %>%
  select(1, 5:8) %>%
  setNames(c("Protein Pair", "score", "n", "label", "precision")))

# wrangle R files
rfiles <- list.files(getwd(), "r.csv$")
rppis <- map(rfiles, ~ read_csv(., n_max = 100000) %>%
  mutate(n = row_number()) %>%
  select(1, 4:7) %>%
  setNames(c("Protein Pair", "score", "label", "precision", "n"))) %>%
  setNames(str_replace(rfiles, ".csv", ""))

# join
tmp <- do.call(bind_rows, c(rppis, mlppis, .id = "dataset")) %>%
  separate(col = dataset, into = c("dataset", "lang"), sep = "-")

# get first 1000
ppishead <- filter(tmp, n <= 10000)

# pivot
bn_stim <- inner_join(
  filter(ppishead, dataset == "bn_stim", lang == "r"),
  filter(ppishead, dataset == "bn_stim", lang == "ml"),
  by = "Protein Pair",
  suffix = c(".r", ".ml")
) %>%
  select(-contains("dataset"), -contains("lang"))

bn_unstim <- inner_join(
  filter(ppishead, dataset == "bn_unstim", lang == "r"),
  filter(ppishead, dataset == "bn_unstim", lang == "ml"),
  by = "Protein Pair",
  suffix = c(".r", ".ml")
) %>%
  select(-contains("dataset"), -contains("lang"))

# find labelling descrepancies
`%noteq%` <- function(e1, e2) {
  (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) &
    !(is.na(e1) & is.na(e2))
}
stim_descrep <- filter(bn_stim, label.r %noteq% label.ml)
unstim_descrep <- filter(bn_unstim, label.r %noteq% label.ml)