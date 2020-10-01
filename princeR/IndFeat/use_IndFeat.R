# run R features through indfeat before classification

library(PrInCE)
library(tidyverse)

# import custom PrInCE
source("C://Users/Charley/OneDrive/git/NodeComputing/princeR/IndFeat/myPrInCE.R")
source("C://Users/Charley/OneDrive/git/NodeComputing/princeR/IndFeat/IndFeat.R")
source("C://Users/Charley/OneDrive/git/NodeComputing/princeR/IndFeat/trim.R")

# import scott data
bn_unstim <- readRDS("D:/Downloads/foster/Rinputs/bn_unstim.rds")
bn_stim <- readRDS("D:/Downloads/foster/Rinputs/bn_stim.rds")

goldstd <- readRDS("D:/Downloads/foster/Rinputs/goldstd.rds")

# calculate features
r_bn_unstim_feats <- myPrInCE(bn_unstim, goldstd, onlyfeat = T)
r_bn_unstim_feats_impute <- replace_missing_data(r_bn_unstim_feats)

r_bn_stim_feats <- myPrInCE(bn_stim, goldstd, onlyfeat = T)
r_bn_stim_feats_impute <- replace_missing_data(r_bn_stim_feats)

# calculate labels
goldstd_mat <- adjacency_matrix_from_list(goldstd)
r_bn_unstim_labels <- make_labels(goldstd_mat, r_bn_unstim_feats)
r_bn_stim_labels <- make_labels(goldstd_mat, r_bn_stim_feats)

# remove unlabelled
r_bn_unstim_labelled_feats <- r_bn_unstim_feats_impute[is.finite(r_bn_unstim_labels), ]
r_bn_stim_labelled_feats <- r_bn_stim_feats_impute[is.finite(r_bn_stim_labels), ]

# run IndFeat
bn_unstim_sigfeats <- indfeat(
  r_bn_unstim_labelled_feats[,-c(1,2)],
  r_bn_unstim_labels[!is.na(r_bn_unstim_labels)])

bn_stim_sigfeats <- indfeat(
  r_bn_stim_labelled_feats[,-c(1,2)],
  r_bn_stim_labels[!is.na(r_bn_stim_labels)])
# indfeat(ml_bn_unstim_feats[is.finite(ml_bn_unstim_labels), ][,-c(1,2)], ml_bn_unstim_labels[!is.na(ml_bn_unstim_labels)])

# run NB with selected features
bn_unstim_selectedfeats <- r_bn_unstim_feats_impute[, c(T, T, (bn_unstim_sigfeats > 10))]
r_bn_unstim_ppis <- predict_interactions(bn_unstim_selectedfeats, goldstd_mat, "NB", models = 1)
r_bn_unstim_ppis_allfeats <- predict_interactions(r_bn_unstim_feats_impute, goldstd_mat, "NB", models = 1)
r_bn_unstim_ppis_models10 <- predict_interactions(r_bn_unstim_feats_impute, goldstd_mat, "NB", models = 1)

bn_stim_selectedfeats <- r_bn_stim_feats_impute[, c(T, T, (bn_stim_sigfeats > 10))]
r_bn_stim_ppis <- predict_interactions(bn_stim_selectedfeats, goldstd_mat, "NB", models = 1)
r_bn_stim_ppis_allfeats <- predict_interactions(r_bn_stim_feats_impute, goldstd_mat, "NB", models = 1)

# trim everything
r_bn_unstim_feattrim <- map(r_bn_unstim_feats_impute, ~ trim(.))

all_trimmed <- map_df(r_bn_unstim_feats_impute, ~ trim(.))
all_trimmed_ppis <- predict_interactions(all_trimmed, goldstd_mat, "SVM", models = 1)
# import precision of ML
ml_bn_unstim_precision <- read_csv("D://Downloads/foster/mlppis/bn_unstim-frac=55-rep=3.csv") %>%
  select(9) %>%
  setNames("precision") %>%
  arrange(desc(precision))

ml_bn_unstim_feats <- read_csv("D://Downloads/foster/NB/all_imputed_feats_bnunstim_trim.csv") %>%
  select(-c(3:5, 24))
ml_bn_unstim_labels <- read_csv("D://Downloads/foster/NB/all_imputed_feats_bnunstim_trim.csv") %>%
  pull(24)
ml_bn_unstim_ppis <- predict_interactions(ml_bn_unstim_feats, goldstd_mat, "SVM", models = 1)
# graph
max <- 20000
ggplot(r_bn_unstim_ppis[1:max,], aes(1:max, precision)) +
  geom_path() +
  geom_path(aes(1:max, ml_bn_unstim_precision[1:max,]$precision), color = "red") +
  geom_path(aes(1:max, r_bn_unstim_ppis_allfeats[1:max,]$precision), color = "blue") +
  geom_path(aes(1:max, ml_bn_unstim_ppis[1:max,]$precision), color = "yellow") +
  geom_path(aes(1:max, all_trimmed_ppis[1:max,]$precision), color = "brown") +
  geom_path(aes(1:max, trim80_ppis[1:max,]$precision), color = "orange") +
  ggtitle("Using IndFeat bn_unstim - ML (red), R IndFeat > 10 (black), R all features (blue), ML feat in R (yellow), R's SVM (brown)")

ggplot(r_bn_unstim_ppis_models10[1:max,], aes(1:max, precision)) +
  geom_path() +
  geom_path(aes(1:max, ml_bn_unstim_precision[1:max,]$precision), color = "red") +
  geom_path(aes(1:max, ml_bn_unstim_ppis[1:max,]$precision), color = "yellow") +
  geom_path(aes(1:max, R_feat_in_ML[1:max,] %>% pull(1)), color = "blue") +
  geom_path(aes(1:max, ML_in_ML_precision[1:max,] %>% pull(1)), color = "magenta") +
  geom_path(aes(1:max, all_trimmed_ppis[1:max,]$precision), color = "brown") +
  ggtitle("BN_UNSTIM - R feat in R (black), R feat in ML (blue), ML feat in ML (red), ML feat in R (yellow), Ml feat in ML after giving it to Charley (Magenta)")
  
ml_feats_intersect <-
  ml_bn_unstim_feats %>% mutate(
    pair = paste(protA, protB, sep = "_"),
    .keep = "unused",
    .before = 1
  ) %>% filter(pair %in% intersect_pairs) %>%
  arrange(pair)
r_feats_intersect <-
  r_bn_unstim_feats %>% mutate(
    pair = paste(protein_A, protein_B, sep = "_"),
    .keep = "unused",
    .before = 1
  ) %>% filter(pair %in% intersect_pairs) %>%
  arrange(pair)


x <- c(5,3,2,4,7,6)

map2_dbl(
  ml_feats_intersect %>%
    select(-1),
  r_feats_intersect %>%
    select(c(x, x + 6, x + 12)),
  function(x,y) {
    valid <- map_lgl(x, ~!is.na(.)) & map_lgl(y, ~!is.na(.))
    cor(x[valid], y[valid])
  })