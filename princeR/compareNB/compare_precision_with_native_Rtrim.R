# rerun scott data
library(tidyverse)
library(PrInCE)

# R precision
goldstd <- readRDS("D:/Downloads/foster/Rinputs/goldstd.rds")
bn_unstim <- readRDS("D:/Downloads/foster/Rinputs/bn_unstim.rds")
sec_stim <- readRDS("D:/Downloads/foster/Rinputs/sec_stim.rds")
sec_unstim <- readRDS("D:/Downloads/foster/Rinputs/sec_unstim.rds")
bn_stim <- readRDS("D:/Downloads/foster/Rinputs/bn_stim.rds")

bn_unstim_ppis <- PrInCE(bn_unstim, goldstd, classifier = "NB", models = 1)
bn_stim_ppis <- PrInCE(bn_stim, goldstd, classifier = "NB", models = 1)
sec_unstim_ppis <- PrInCE(sec_unstim, goldstd, classifier = "NB", models = 1)
sec_stim_ppis <- PrInCE(sec_stim, goldstd, classifier = "NB", models = 1)

# ML features with R classifier
trim_euc <- function(feats, max_euc_quantile = 0.995) {
  eucs <- feats[c("euc1", "euc2", "euc3")]
  trimmed <- map_df(
    eucs,
    function(x) {
      threshold <- quantile(x, max_euc_quantile, na.rm = T)
      x[x > threshold] <- threshold
      return(x)
    }
  )
  feats[c("euc1", "euc2", "euc3")] <- trimmed
  return(feats)
}

ml_bn_unstim_feats <- read_csv("D:/Downloads/foster/NB/all_imputed_feats_bnunstim.csv") %>%
  select(-c(3:5))
ml_bn_unstim_feats_trim995 <- trim_euc(ml_bn_unstim_feats)
ml_bn_unstim_feats_trim950 <- trim_euc(ml_bn_unstim_feats, .95)
ml_bn_unstim_feats_trim900 <- trim_euc(ml_bn_unstim_feats, .90)
goldstd_mat <- adjacency_matrix_from_list(goldstd)
ml_bn_unstim_ppis_trim995 <- predict_interactions(ml_bn_unstim_feats_trim995, goldstd_mat, "NB", F, 1, 10, 500)
ml_bn_unstim_ppis_trim950 <- predict_interactions(ml_bn_unstim_feats_trim950, goldstd_mat, "NB", F, 1, 10, 500)
ml_bn_unstim_ppis_trim900 <- predict_interactions(ml_bn_unstim_feats_trim900, goldstd_mat, "NB", F, 1, 10, 500)


# ML ppis
ml_bn_unstim <- read_csv("D:/Downloads/foster/mlppis/bn_unstim-frac=55-rep=3.csv") %>%
  mutate(precision = `Interaction score (avg.)`, .keep = "unused") %>%
  arrange(desc(precision))
ml_bn_stim <- read_csv("D:/Downloads/foster/mlppis/bn_stim-frac=55-rep=3.csv") %>%
  mutate(precision = `Interaction score (avg.)`, .keep = "unused") %>%
  arrange(desc(precision))
ml_sec_unstim <- read_csv("D:/Downloads/foster/mlppis/sec_unstim-frac=55-rep=3.csv") %>%
  mutate(precision = `Interaction score (avg.)`, .keep = "unused") %>%
  arrange(desc(precision))
ml_sec_stim <- read_csv("D:/Downloads/foster/mlppis/sec_stim-frac=55-rep=3.csv") %>%
  mutate(precision = `Interaction score (avg.)`, .keep = "unused") %>%
  arrange(desc(precision))

count <- 10000

join_precision <- function(r_ppis, ml_ppis) {
  tibble(
    index = seq(count),
    R = r_ppis[seq(count), ]$precision,
    ML = ml_ppis[seq(count), ]$precision
  )
}

scott_precision <- list(
  join_precision(bn_unstim_ppis, ml_bn_unstim),
  join_precision(bn_stim_ppis, ml_bn_stim),
  join_precision(sec_unstim_ppis, ml_sec_unstim),
  join_precision(sec_stim_ppis, ml_sec_stim)
) %>%
  setNames(c("bn_unstim", "bn_stim", "sec_unstim", "sec_stim")) %>%
  bind_rows(.id = "dataset") %>%
  pivot_longer(c("R", "ML"), names_to = "type", values_to = "precision")

bn_unstim_precision <- tibble(
  index = seq(count),
  R = bn_unstim_ppis[seq(count), ]$precision,
  ML = ml_bn_unstim[seq(count), ]$`Interaction score (condition-specific)`,
  `ML in R 99.5%` = ml_bn_unstim_ppis_trim995[seq(count), ]$precision,
  `ML in R 95%` = ml_bn_unstim_ppis_trim950[seq(count), ]$precision,
  `ML in R 90%` = ml_bn_unstim_ppis_trim900[seq(count), ]$precision
) %>%
  pivot_longer(cols = -1, names_to = "type", values_to = "precision")

ggplot(bn_unstim_precision, aes(index, precision)) +
  geom_path(aes(color = type))

ggplot(scott_precision, aes(index, precision)) +
  geom_path(aes(color = type)) +
  facet_wrap("dataset", 2, 2)