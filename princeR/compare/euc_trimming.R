# check euclidean trimming

library(tidyverse)
library(PrInCE)

myPrInCE <-
  function(profiles,
           gaussians = NULL,
           precision = NULL,
           verbose = FALSE,
           min_points = 1,
           min_consecutive = 5,
           min_pairs = 3,
           impute_NA = TRUE,
           smooth = TRUE,
           smooth_width = 4,
           max_gaussians = 5,
           max_iterations = 50,
           min_R_squared = 0.5,
           method = c(
             "guess",
             "random"
           ),
           criterion = c(
             "AICc", "AIC",
             "BIC"
           ),
           pearson_R_raw = TRUE,
           pearson_R_cleaned = TRUE,
           pearson_P = TRUE,
           euclidean_distance = TRUE,
           co_peak = TRUE,
           co_apex = TRUE,
           n_pairs = FALSE,
           classifier = c(
             "NB",
             "SVM", "RF", "LR", "ensemble"
           ),
           models = 10,
           cv_folds = 10,
           trees = 500) {
    method <- match.arg(method)
    criterion <- match.arg(criterion)
    classifier <- match.arg(classifier)
    if (is.list(profiles) & !is.data.frame(profiles)) {
      for (replicate_idx in seq_along(profiles)) {
        replicate <- profiles[[replicate_idx]]
        if (is(replicate, "MSnSet")) {
          profile_matrix <- exprs(replicate)
        }
        else {
          profile_matrix <- data.matrix(replicate)
        }
        if (!is.numeric(profile_matrix)) {
          stop(
            "list input (item #",
            replicate_idx,
            ") could not be coerced to numeric matrix"
          )
        }
        profiles[[replicate_idx]] <- profile_matrix
      }
    } else {
      if (is(profiles, "MSnSet")) {
        profile_matrix <- exprs(profiles)
      }
      else {
        profile_matrix <- data.matrix(profiles)
      }
      if (!is.numeric(profile_matrix)) {
        stop("input could not be coerced to numeric matrix")
      }
      profiles <- list(profile_matrix)
    }

    features <- list()
    for (replicate_idx in seq_along(profiles)) {
      mat <- profiles[[replicate_idx]]

      if (verbose) {
        message("  fitting Gaussians ...")
      }
      gauss <- build_gaussians(
        mat,
        min_points = min_points,
        min_consecutive = min_consecutive,
        impute_NA = impute_NA,
        smooth = smooth,
        smooth_width = smooth_width,
        max_gaussians = max_gaussians,
        max_iterations = max_iterations,
        min_R_squared = min_R_squared,
        method = method
      )

      before <- nrow(mat)
      mat <- mat[names(gauss), ]
      after <- nrow(mat)
      if (verbose) {
        message(
          "  fit mixtures of Gaussians to ",
          after,
          " of ",
          before,
          " profiles"
        )
      }
      feat <- calculate_features(
        mat,
        gauss,
        min_pairs = min_pairs,
        pearson_R_raw = pearson_R_raw,
        pearson_R_cleaned = pearson_R_cleaned,
        pearson_P = pearson_P,
        euclidean_distance = euclidean_distance,
        co_peak = co_peak,
        co_apex = co_apex,
        n_pairs = n_pairs
      )
      features[[replicate_idx]] <- feat
    }
    if (verbose) {
      message("concatenating features across replicates ...")
    }
    # input <- concatenate_features(features)
    return(input)
  }
my_concat <- function(feats) {
  reduce(
    feats,
    function(x, y) {
      full_join(x, y, by = colnames(x)[c(1, 2)])
    }
  )
}