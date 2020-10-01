# IndFeat Implementation
indfeat <-
  function(x, y) {
    # x is a subject by feature matrix -> columns hold features or variables
    
    # finds the class names
    uniqueclass <- unique(y)
    
    # find indexes for both classes
    classA <- (y == uniqueclass[1])
    classB <- (y == uniqueclass[2])
    
    nA <- sum(classA)
    nB <- sum(classB)
    
    # Compute significance
    utils::globalVariables("sig", add = TRUE)
    
    sig <-
      (abs(colMeans(x[classA, ]) - colMeans(x[classB, ]))) / (sqrt(diag(var(x[classA, ])) /
          nA + diag(var(x[classB, ])) / nB))
    
    return(sig)
    # the diag is used to extract the column i by column i variances whereas matlab's var only extracts this
# 
#     # now we need to extract the features and construct the new reduced data set
#     dat <- x
#     # do not use variable data -> It's an R function for loading data sets
#     sig <- data.matrix(sig)
#     # has to be done so we can count
#     vv <- matrix(0, dim(sig)[1])
# 
#     # Automate the selection of how many features to allow into the algorithm
#     sig[is.na(sig)] <<- 0
#     # replace NAs with 0s
# 
#     # determine a reasonable accuracy for the univariate reduction
#     qu <<- quantile(sig, probs = seq(0, 1, .1))
# 
#     acc <<-
#       as.numeric(qu[9])
#     # top quartile - alter this if you wish to allow more or less features into the model.
#     # NOTE THAT IF THE NUMBER OF FEATURES IS LARGE (OVER 1000) THIS SHOULD BE ADJUSTED TO SPEED UP PROCESSING TIME.
# 
#     for (i in 1:dim(sig)[1]) {
#       if (sig[i] > acc) {
#         # this value can be adjusted depending on how discriminating the variables are
#         vv[i, 1] <<- i
# 
#       }
#     }
# 
#     vv <<- vv[vv != 0]
#     # remove all zeros
# 
#     vv <<- data.matrix(vv) # these are the selected variable positions from the original data set
# 
#     s <<- matrix(0, dim(y)[1], dim(vv)[1])
# 
#     for (g in 1:dim(vv)[1]) {
#       s[, g] <<- dat[, vv[g]]
#     }
# 
#     return(t(s)) # transposed for Butterfly
  }