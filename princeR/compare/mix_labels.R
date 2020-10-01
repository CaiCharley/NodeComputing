# combine R/ML features with R/ML Labels for bn_unstim

library(tidyverse)
library(PrInCE)

# load features
ml_raw <- read_csv("D://Downloads/foster/mix labels/all_imputed_feats_bnunstim_trim.csv")
ml_features <- ml_raw %>%
  select(-c(3:5, 24))
r_features <- read_csv("D://Downloads/foster/mix labels/R_bn_unstim_imputedfeats.csv")

max_euc_quantile <- 0.90

trim_euc <- function(feats) {
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

# trim ML features
ml_features <- trim_euc(ml_features)

# make labels
goldstd <-  readRDS("D://Downloads/foster/Rinputs/goldstd.rds") %>%
  adjacency_matrix_from_list()

r_labels_for_ML_features <- make_labels(goldstd, ml_features)
ml_labels_for_ML_features <- pull(ml_raw, 24)

# predict_emsemble
r_labels_ppi <- predict_ensemble(ml_features, r_labels_for_ML_features, "NB", 1, 10, 500)
ml_labels_ppi <- predict_ensemble(ml_features, ml_labels_for_ML_features, "NB", 1, 10, 500)

r_labels_ppi$precision <- calculate_precision(r_labels_ppi$label)
ml_labels_ppi$precision <- calculate_precision(ml_labels_ppi$label)

max <- 10000
scott_precision <- readRDS("D://Downloads/foster/scott_precision.rds") %>%
  filter(dataset == "bn_unstim", index <= max)

ggplot(filter(scott_precision, lang == "ML"), aes(index, precision)) +
  geom_path() +
  geom_path(data = filter(scott_precision, lang == "R"), aes(index, precision), color = "green") +
  geom_path(aes(1:max, r_labels_ppi[1:max,]$precision), color = "blue") +
  geom_path(aes(1:max, ml_labels_ppi[1:max,]$precision), color = "orange") +
  geom_path(aes(1:max, calculate_precision(ml_labels_for_ML_features)[1:max]), color = "red") +
  ggtitle("bn_unstim Precision | ML (Black), R (Green), ML Feat, ml label (orange), ml feat, r label (blue), all_imputed_feats' precision calculated in R (red) ")