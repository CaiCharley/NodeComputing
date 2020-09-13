setwd("D:/Downloads/foster/NB/")

library(tidyverse)
library(naivebayes)
library(PrInCE)

input.Matlab <- as.data.frame(read_csv("./all_imputed_feats_bnunstim.csv"))
input.Matlab$protInCorum[input.Matlab$protInCorum == ""] <- 0
input.Matlab$protInCorum[input.Matlab$protInCorum == "\001"] <- 1
input.Matlab$label <- rep(NA, nrow(input.Matlab))
input.Matlab$label[input.Matlab$protInCorum == 1 & input.Matlab$intInCorum == 0] <- 0
input.Matlab$label[input.Matlab$protInCorum == 1 & input.Matlab$intInCorum == 1] <- 1

input.Matlab.labels <- input.Matlab$label

# trim euclideans
max_euc_quantile <- 0.995

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

input.Matlab_Trim <- trim_euc(input.Matlab)

# training data, Xtr
input <- as_tibble(input.Matlab_Trim) %>%
  mutate(id = paste(protA, protB, sep = "_"))
labeled <- filter(input, !is.na(label))
set.seed(1)
sample <- labeled[sample(1:nrow(labeled), nrow(labeled) * 0.1), ]$id
Xtr <- labeled %>%
  filter(id %in% sample) %>%
  select(-c(1, 2, 3, 4, 5, 24, 25)) %>%
  as.data.frame()

write_csv(Xtr, "Full Xtr.csv")

# training labels, ytr
ytr <- labeled %>%
  filter(id %in% sample) %>%
  pull(label)

write_csv(as.data.frame(ytr), "Full Ytr.csv")

# testing data, Xnew
Xnew <- filter(input, !(id %in% sample))
new_ids <- Xnew$id
Xnew <- select(Xnew, -c(1, 2, 3, 4, 5, 24, 25))

write_csv(Xnew, "Full Xnew.csv")

# R precision
rppis <- predict_ensemble(input.Matlab_Trim[-c(3, 4, 5)], input.Matlab.labels, "NB", cv_folds = 10, models = 1)

rprecision <- calculate_precision(rppis$label)

ggplot(filter(ppis, n < 6000), aes(n, precision)) +
  geom_path()

# calculate classifier score for rows of Xnew
clf <- naive_bayes(Xtr, factor(ytr, levels = c(0, 1)))
score.r <- predict(clf, Xnew, type = "prob", threshold = 1e-10)[, 2]

# read matlab.score
score.matlab <- as.data.frame(read_csv("matlab.csv", col_names = F))$X1

difference <- abs(score.r - score.matlab)
print(max(difference))