# compare feature matrixes (bn_unstim) of ML/R after passing through R classifier
library(tidyverse)
library(PrInCE)
library(Hmisc)

myPrInCE <- function(profiles, gold_standard, impute = F, concat = T,
                     onlyfeat = F, gaussians = NULL, precision = NULL, trim = T,
                     verbose = FALSE, min_points = 1, min_consecutive = 5, min_pairs = 3,
                     impute_NA = TRUE, smooth = TRUE, smooth_width = 4, max_gaussians = 5,
                     max_iterations = 50, min_R_squared = 0.5, method = c(
                       "guess",
                       "random"
                     ), criterion = c("AICc", "AIC", "BIC"), pearson_R_raw = TRUE,
                     pearson_R_cleaned = TRUE, pearson_P = TRUE, euclidean_distance = TRUE,
                     co_peak = TRUE, co_apex = TRUE, n_pairs = FALSE, classifier = c(
                       "NB",
                       "SVM", "RF", "LR", "ensemble"
                     ), models = 1, cv_folds = 10,
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
        stop("list input (item #", replicate_idx, ") could not be coerced to numeric matrix")
      }
      profiles[[replicate_idx]] <- profile_matrix
      if (!is.null(gaussians)) {
        if (length(gaussians) < replicate_idx) {
          stop("fewer Gaussians than profiles provided")
        }
        check_gaussians(
          gaussians[[replicate_idx]],
          rownames(profile_matrix), replicate_idx
        )
      }
    }
  }
  else {
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
    if (!is.null(gaussians)) {
      check_gaussians(gaussians)
      gaussians <- list(gaussians)
    }
  }
  if (is.data.frame(gold_standard)) {
    gold_standard <- adjacency_matrix_from_data_frame(gold_standard)
  }
  else if (is.list(gold_standard)) {
    gold_standard <- adjacency_matrix_from_list(gold_standard)
  }
  if (!is_unweighted(gold_standard)) {
    stop("could not convert supplied gold standards to adjacency matrix")
  }
  features <- list()
  for (replicate_idx in seq_along(profiles)) {
    if (verbose) {
      message(
        "generating features for replicate ", replicate_idx,
        " ..."
      )
    }
    mat <- profiles[[replicate_idx]]
    if (!is.null(gaussians)) {
      gauss <- gaussians[[replicate_idx]]
    }
    else {
      if (verbose) {
        message("  fitting Gaussians ...")
      }
      gauss <- build_gaussians(mat,
        min_points = min_points,
        min_consecutive = min_consecutive, impute_NA = impute_NA,
        smooth = smooth, smooth_width = smooth_width,
        max_gaussians = max_gaussians, max_iterations = max_iterations,
        min_R_squared = min_R_squared, method = method
      )
    }
    before <- nrow(mat)
    mat <- mat[names(gauss), ]
    after <- nrow(mat)
    if (verbose) {
      message(
        "  fit mixtures of Gaussians to ", after,
        " of ", before, " profiles"
      )
    }
    if (trim) {
      feat <- calculate_features(mat, gauss,
        min_pairs = min_pairs,
        pearson_R_raw = pearson_R_raw, pearson_R_cleaned = pearson_R_cleaned,
        pearson_P = pearson_P, euclidean_distance = euclidean_distance,
        co_peak = co_peak, co_apex = co_apex, n_pairs = n_pairs
      )
    } else {
      feat <- mycalculate_features(mat, gauss,
        min_pairs = min_pairs,
        pearson_R_raw = pearson_R_raw, pearson_R_cleaned = pearson_R_cleaned,
        pearson_P = pearson_P, euclidean_distance = euclidean_distance,
        co_peak = co_peak, co_apex = co_apex, n_pairs = n_pairs
      )
    }

    features[[replicate_idx]] <- feat
  }
  if (verbose) {
    message("concatenating features across replicates ...")
  }
  if (concat) {
    input <- concatenate_features(features)
  }

  if (impute) {
    input <- replace_missing_data(input)
  }
  if (!onlyfeat) {
    interactions <- predict_interactions(input, gold_standard,
      classifier = classifier, models = models, cv_folds = cv_folds,
      trees = trees, verbose = verbose
    )
    if (!is.null(precision)) {
      before <- nrow(interactions)
      interactions <- threshold_precision(interactions, precision)
      if (is.null(interactions)) {
        warning(
          "none of ", before, " ranked protein pairs had precision >= ",
          precision
        )
      }
    }
    return(interactions)
  } else {
    return(input)
  }
}

# calculate feature with no trimming
mycalculate_features <- function(profile_matrix, gaussians, min_pairs = 0, pearson_R_raw = TRUE,
                                 pearson_R_cleaned = TRUE, pearson_P = TRUE, euclidean_distance = TRUE,
                                 co_peak = TRUE, co_apex = TRUE, n_pairs = FALSE) {
  if (is(profile_matrix, "MSnSet")) {
    profile_matrix <- exprs(profile_matrix)
  }
  if (min_pairs < 3) {
    min_pairs <- 3
  }
  cleaned <- clean_profiles(profile_matrix,
    impute_NA = FALSE,
    smooth = FALSE, noise_floor = 0.05
  )
  proteins <- rownames(cleaned)
  n_proteins <- length(proteins)
  feature_matrices <- list()
  pairs <- crossprod(t(!is.na(profile_matrix)))
  if (pearson_R_raw) {
    cor_R_raw <- suppressWarnings(1 - cor(t(profile_matrix),
      use = "pairwise.complete.obs"
    ))
    feature_matrices[["cor_R_raw"]] <- cor_R_raw
  }
  if (pearson_R_cleaned) {
    cor_R_cleaned <- 1 - cor(t(cleaned))
    feature_matrices[["cor_R_cleaned"]] <- cor_R_cleaned
  }
  if (pearson_P) {
    cor_P <- suppressWarnings(rcorr(t(profile_matrix))$P)
    feature_matrices[["cor_P"]] <- cor_P
  }
  if (euclidean_distance) {
    eucl <- as.matrix(dist(cleaned, method = "euclidean"))
    feature_matrices[["euclidean_distance"]] <- eucl
  }
  if (co_peak) {
    maxes <- apply(cleaned, 1, which.max)
    co_peak <- as.matrix(dist(maxes))
    feature_matrices[["co_peak"]] <- co_peak
  }
  if (co_apex) {
    CA <- co_apex(gaussians, proteins)
    feature_matrices[["co_apex"]] <- CA
  }
  feature_matrices[["n_pairs"]] <- pairs
  if (!all(map_int(feature_matrices, nrow) == n_proteins) |
    !all(map_int(feature_matrices, ncol) == n_proteins)) {
    stop("at least one feature matrix did not have correct dimensions")
  }
  if (length(feature_matrices) == 0) {
    stop("no features were calculated")
  }
  first <- feature_matrices[[1]]
  tri <- upper.tri(first)
  idxs <- which(tri, arr.ind = TRUE)
  dat <- data.frame(
    protein_A = rownames(first)[idxs[, 1]],
    protein_B = rownames(first)[idxs[, 2]], stringsAsFactors = FALSE
  )
  dat <- cbind(dat, map(feature_matrices, ~ .[tri]))
  dat <- filter(dat, n_pairs >= min_pairs)
  # remove n_pairs from the feature list
  if (!n_pairs) {
    dat$n_pairs <- NULL
  }
  return(dat)
}

max_euc_quantile <- 0.90

trim_euc <- function(feats) {
  eucs <- feats[c("euclidean_distance.x", "euclidean_distance.y", "euclidean_distance")]
  trimmed <- map_df(
    eucs,
    function(x) {
      threshold <- quantile(x, max_euc_quantile, na.rm = T)
      x[x > threshold] <- threshold
      return(x)
    }
  )
  feats[c("euclidean_distance.x", "euclidean_distance.y", "euclidean_distance")] <- trimmed
  return(feats)
}

# get files
bn_unstim <- readRDS("D:/Downloads/foster/Rinputs/bn_unstim.rds")
goldstd <- readRDS("D:/Downloads/foster/Rinputs/goldstd.rds")
ml_feats <- read_csv("D:/Downloads/foster/NB/all_imputed_feats_bnunstim_trim.csv") %>%
  select(-c(3:5, 24))
ml_feats_untrim <- read_csv("D:/Downloads/foster/NB/all_imputed_feats_bnunstim.csv") %>%
  select(-c(3:5))

# calculate R features
r_feats <- myPrInCE(bn_unstim, goldstd, onlyfeat = T, classifier = "NB", models = 1)
r_feats_impute <- myPrInCE(bn_unstim, goldstd, onlyfeat = T, impute = T, classifier = "NB", models = 1)
r_feats_untrim <- myPrInCE(bn_unstim, goldstd, onlyfeat = T, trim = F, impute = T, classifier = "NB", models = 1)
r_feats_impute_trim <- r_feats_untrim %>%
  replace_missing_data() %>%
  trim_euc()
r_feats_trim <- r_feats_untrim %>%
  trim_euc()
# get intersection of protein pairs of ML/R
ml_pairs <- ml_feats %>%
  transmute(pair = paste(protA, protB, sep = "_")) %>%
  as_vector()
r_pairs <- r_feats %>%
  transmute(pair = paste(protein_A, protein_B, sep = "_")) %>%
  as_vector()
intersect_pairs <- intersect(ml_pairs, r_pairs)

# subset features for pairs in both ML/R
ml_feats_intersect <- ml_feats %>%
  filter(paste(protA, protB, sep = "_") %in% intersect_pairs)
ml_feats_untrim_intersect <- ml_feats_untrim %>%
  filter(paste(protA, protB, sep = "_") %in% intersect_pairs)
r_feats_intersect <- r_feats %>%
  filter(paste(protein_A, protein_B, sep = "_") %in% intersect_pairs)

# train and predict
goldstd_mat <- adjacency_matrix_from_list(goldstd)

ml_ppis <- predict_interactions(
  ml_feats, goldstd_mat, "NB", F, 1, 10, 500
)
ml_ppis_intersect <- predict_interactions(
  ml_feats_intersect, goldstd_mat, "NB", F, 1, 10, 500
)
ml_ppis_untrim_intersect <- predict_interactions(
  ml_feats_untrim_intersect, goldstd_mat, "NB", F, 1, 10, 500
)
r_ppis_intersect <- predict_interactions(
  r_feats_intersect, goldstd_mat, "NB", F, 1, 10, 500
)
r_ppis_impute_trim <- predict_interactions(
  r_feats_impute_trim, goldstd_mat, "NB", F, 1, 10, 500
)
r_ppis_trim <- predict_interactions(
  r_feats_trim, goldstd_mat, "NB", F, 1, 10, 500
)

ggplot(ml_ppis_intersect[1:1e5, ], aes(1:1e5, precision)) +
  geom_path() +
  geom_path(data = r_ppis_intersect[1:1e5, ], color = "red") +
  ggtitle("Precision of ML(black)/R(red). bn_unstim, intersection of feature pairs")
ggplot(ml_ppis_untrim_intersect[1:1e5, ], aes(1:1e5, precision)) +
  geom_path() +
  geom_path(data = r_ppis_intersect[1:1e5, ], color = "red") +
  ggtitle("Precision of ML_untrimmed(black)/R(red). bn_unstim, intersection of feature pairs")

# compare rankings
r_pair_index_in_ml <- match(
  rownames(r_ppis_intersect),
  rownames(ml_ppis_intersect)
)
r_pair_index_in_ml_untrim <- match(
  rownames(r_ppis_intersect),
  rownames(ml_ppis_untrim_intersect)
)
ml_pair_index_in_r <- match(
  rownames(ml_ppis_intersect),
  rownames(r_ppis_intersect)
)
ml_untrim_pair_index_in_r <- match(
  rownames(ml_ppis_untrim_intersect),
  rownames(r_ppis_intersect)
)

pairs <- 1000
ggplot(NULL, aes(1:pairs, ml_pair_index_in_r[1:pairs])) +
  geom_point(alpha = .2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(ml_pair_index_in_r[1:pairs]))) +
  labs(
    title = "Compare Pair Rankings",
    x = "Ranking in ML",
    y = "Ranking in R"
  )
ggplot(NULL, aes(1:pairs, r_pair_index_in_ml[1:pairs])) +
  geom_point(alpha = .2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(ml_pair_index_in_r[1:pairs]))) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, pairs)) +
  labs(
    title = "Compare Pair Rankings",
    x = "Ranking in R",
    y = "Ranking in ML"
  )
ggplot(NULL, aes(1:pairs, r_pair_index_in_ml_untrim[1:pairs])) +
  geom_point(alpha = .2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(expand = c(0, 0)) +
  #  scale_y_continuous(expand = c(0, 0), limits = c(0, max(r_pair_index_in_ml_untrim[1:pairs]))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, pairs)) +
  labs(
    title = "Compare Pair Rankings",
    x = "Ranking in R",
    y = "Ranking in ML untrimmed"
  )
ggplot(NULL, aes(1:pairs, ml_untrim_pair_index_in_r[1:pairs])) +
  geom_point(alpha = .2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, pairs)) +
  labs(
    title = "Compare Pair Rankings",
    x = "Ranking in ML untrimmed",
    y = "Ranking in R"
  )



ggplot(r_ppis_impute_trim[1:pairs, ], aes(1:pairs, precision)) +
  geom_path() +
  geom_path(data = ml_ppis_intersect[1:pairs, ], color = "blue") +
  geom_path(data = ml_ppis[1:pairs, ], color = "green") +
  ggtitle("Precision R impute then trim 90% (black), ML impute trim no intersection (green), ML impute trim (blue)")
ggplot(ppis[1:5e4, ], aes(1:5e4, precision)) +
  geom_path() +
  geom_path(data = ppis_imputed[1:5e4, ], color = "red") +
  ggtitle("bn_unstim (red = with imputation)")