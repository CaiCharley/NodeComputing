setwd("D:/Downloads/foster/NB/")

library(tidyverse)
library(PrInCE)
library(progress)
library(stats)
library(robustbase)
library(LiblineaR)
library(speedglm)
library(ranger)

input.Matlab <- as.data.frame(read_csv("./all_imputed_feats_bnunstim.csv"))
input.Matlab$protInCorum[input.Matlab$protInCorum == ""] <- 0
input.Matlab$protInCorum[input.Matlab$protInCorum == "\001"] <- 1
input.Matlab$label <- rep(NA, nrow(input.Matlab))
input.Matlab$label[input.Matlab$protInCorum == 1 & input.Matlab$intInCorum == 0] <- 0
input.Matlab$label[input.Matlab$protInCorum == 1 & input.Matlab$intInCorum == 1] <- 1

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
input.Matlab.labels <- input.Matlab$label

# ML precision
mlprecision <- input.Matlab_Trim %>%
  pull(label) %>%
  calculate_precision()

# R precision
rppis <- predict_ensemble(input.Matlab_Trim[-c(3, 4, 5, 24)], input.Matlab.labels, "NB", cv_folds = 10, models = 1)
rprecision <- calculate_precision(rppis$label)

# R precision modified
my_predict_ensemble <- function(dat, labels, classifier = c("NB", "SVM", "RF", "LR"),
                                models = 10, cv_folds = 10, trees = 500, node_columns = c(1, 2),
                                folds = NULL) {
  classifier <- match.arg(classifier)
  if (length(node_columns) != 2) {
    stop("length of `node_columns` must be exactly 2")
  }
  score <- NULL
  if (classifier != "NB") {
    dat <- replace_missing_data(dat)
  }
  if (is.numeric(node_columns)) {
    node_colnames <- colnames(dat)[node_columns]
  }
  else if (is.character(node_columns)) {
    node_colnames <- node_columns
  }
  else {
    stop("`node_columns` must be an integer or character vector")
  }
  if (classifier == "SVM") {
    dat[, !colnames(dat) %in% node_colnames] <- sapply(dat[
      ,
      !colnames(dat) %in% node_colnames
    ], scale)
  }
  training_idxs <- which(!is.na(labels))
  training_labels <- as.factor(labels[training_idxs])
  training <- dat[training_idxs, !colnames(dat) %in% node_colnames]
  n_interactions <- nrow(dat)
  col1 <- node_columns[1]
  col2 <- node_columns[2]
  interaction_names <- paste0(dat[[col1]], "_", dat[[col2]])
  ensembled <- matrix(NA,
    ncol = models, nrow = n_interactions,
    dimnames = list(interaction_names)
  )
  total_models <- models * cv_folds
  pb <- progress_bar$new(
    format = "running fold :what [:bar] :percent eta: :eta",
    clear = FALSE, total = total_models, width = 80
  )
  counter <- 0
  for (i in seq_len(models)) {
    # greg greg greg greg greg greg greg greg
    if (is.null(folds)) {
      folds <- cut(seq_len(nrow(training)),
        breaks = cv_folds,
        labels = FALSE
      )
      folds <- sample(folds)
    }
    # greg greg greg greg greg greg greg greg
    clf_scores <- matrix(NA,
      ncol = cv_folds, nrow = n_interactions,
      dimnames = list(interaction_names)
    )
    for (fold in seq_len(cv_folds)) {
      counter <- counter + 1
      pb$tick(tokens = list(what = sprintf(paste0(
        "%-",
        nchar(total_models), "s"
      ), counter)))
      clf_data <- training[which(folds != fold), ]
      clf_labels <- as.factor(training_labels[which(folds != fold)])
      clf_data_labeled <- cbind(clf_data, label = clf_labels)
      clf <- switch(classifier,
        NB = naive_bayes(clf_data, clf_labels),
        SVM = LiblineaR(clf_data, clf_labels, type = 2),
        RF = ranger(
          data = clf_data_labeled,
          dependent.variable.name = "label", probability = TRUE,
          num.trees = trees
        ),
        LR = speedglm(label ~ ., clf_data_labeled, family = binomial())
      )
      # originally, defined Ipred as -Itrain
      # withheld_idxs <- as.integer(rownames(training))[folds == fold]
      # test_data <- dat[-withheld_idxs, !colnames(dat) %in% node_colnames]
      # new definition of Ipred
      Ipred <- !(1:nrow(dat)) %in% as.integer(rownames(training))[folds != fold]
      test_data <- dat[Ipred, !colnames(dat) %in% node_colnames]
      predictions <- switch(classifier,
        NB = predict(clf, test_data, type = "prob", threshold = 1e-10),
        SVM = predict(clf, test_data, decisionValues = TRUE),
        RF = predict(clf, test_data), LR = predict(clf, test_data, type = "response")
      )
      predictions <- switch(classifier,
        NB = predictions[, "1"],
        SVM = -1 * predictions$decisionValues[, "0"],
        RF = predictions[[1]][, "1"], LR = predictions
      )
      clf_scores[Ipred, fold] <- predictions
      gc()
    }
    medians <- setNames(rowMedians(clf_scores, na.rm = TRUE), rownames(clf_scores))
    ensembled[, i] <- medians
  }
  ensembled_medians <- setNames(
    rowMedians(ensembled, na.rm = TRUE),
    rownames(ensembled)
  )
  interactions <- cbind(dat[, node_columns],
    score = ensembled_medians,
    label = labels
  )
  # interactions = interactions[order(interactions$score), ]
  interactions <- arrange(interactions, -score)
  return(list(interactions, clf_scores))
}

rmodppis <- my_predict_ensemble(input.Matlab_Trim[-c(3, 4, 5, 24)], input.Matlab.labels, "NB", cv_folds = 10, models = 1)
rmodprecision <- calculate_precision(rmodppis[[1]]$label)

nn <- 1e4
precisions <- bind_cols(rprecision[1:nn], mlprecision[1:nn], rmodprecision[1:nn]) %>%
  as_tibble() %>%
  setNames(c("R", "ML", "Rmod")) %>%
  mutate(n = row_number()) %>%
  pivot_longer(1:3, names_to = "type")

ggplot(precisions, aes(n, value)) +
  geom_path(aes(color = type))