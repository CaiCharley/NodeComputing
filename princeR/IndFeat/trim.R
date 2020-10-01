# trim all features


trim <- function(feat, hi = .99, low = .01) {
  if (is.numeric(feat[[1]])) {
    high_threshold <- quantile(feat, hi, na.rm = T)
    low_threshold <- quantile(feat, low, na.rm = T)
    feat[feat > high_threshold] <- high_threshold
    feat[feat < low_threshold] <- low_threshold
  }
  return(feat)
}

