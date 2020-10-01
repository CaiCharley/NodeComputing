myPrInCE <- function(profiles,
  gold_standard,
  impute = F,
  concat = T,
  onlyfeat = F,
  gaussians = NULL,
  precision = NULL,
  trim = T,
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
  method = c("guess",
    "random"),
  criterion = c("AICc", "AIC", "BIC"),
  pearson_R_raw = TRUE,
  pearson_R_cleaned = TRUE,
  pearson_P = TRUE,
  euclidean_distance = TRUE,
  co_peak = TRUE,
  co_apex = TRUE,
  n_pairs = FALSE,
  classifier = "NB",
  models = 1,
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
        stop("list input (item #",
          replicate_idx,
          ") could not be coerced to numeric matrix")
      }
      profiles[[replicate_idx]] <- profile_matrix
      if (!is.null(gaussians)) {
        if (length(gaussians) < replicate_idx) {
          stop("fewer Gaussians than profiles provided")
        }
        check_gaussians(gaussians[[replicate_idx]],
          rownames(profile_matrix), replicate_idx)
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
      message("generating features for replicate ", replicate_idx,
        " ...")
    }
    mat <- profiles[[replicate_idx]]
    if (!is.null(gaussians)) {
      gauss <- gaussians[[replicate_idx]]
    }
    else {
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
    }
    before <- nrow(mat)
    mat <- mat[names(gauss),]
    after <- nrow(mat)
    if (verbose) {
      message("  fit mixtures of Gaussians to ",
        after,
        " of ",
        before,
        " profiles")
    }
    if (trim) {
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
    } else {
      feat <- mycalculate_features(
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
    interactions <- predict_interactions(
      input,
      gold_standard,
      classifier = classifier,
      models = models,
      cv_folds = cv_folds,
      trees = trees,
      verbose = verbose
    )
    if (!is.null(precision)) {
      before <- nrow(interactions)
      interactions <- threshold_precision(interactions, precision)
      if (is.null(interactions)) {
        warning("none of ",
          before,
          " ranked protein pairs had precision >= ",
          precision)
      }
    }
    return(interactions)
  } else {
    return(input)
  }
}