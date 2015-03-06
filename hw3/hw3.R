compute_test_stats <- function(x, y){
  # Computes difference in means and t statistic for tr. group x and control group y
  n <- length(x); m <- length(y);
  diff_means <- mean(x) - mean(y)
  t <- diff_means/sqrt(var(x)/n + var(y)/m) # Do we need to multiply variance by n-1/n and m-1/m?
  return(c("diff_means" = diff_means, "t" = t))
}

perm_test_mean <- function(x, y, iters = 10000){
  # Input
  # x = vector of observations for treatment group
  # y = vector of observations for control group
  # iters = number of permutations

  # Output
  # TestStats = vector containing difference in means and the t-statistic
  # Pvalue = vector with lower, upper, and two-tailed p-values

  box <- c(x, y)
  obs <- compute_test_stats(x, y)
  distr <- replicate(iters, {
    shuffle <- sample(box)
    xnew <- shuffle[1:n]
    ynew <- shuffle[-(1:n)]
    compute_test_stats(xnew, ynew)
  })
  pval_diffmeans <- c("upper" = sum(distr[1,] >= obs[1])/iters, "lower" = sum(distr[1,] <= obs[1])/iters, "both" = sum(abs(distr[1,]) >= abs(obs[1]))/iters)
  pval_t <- c("upper" = sum(distr[2,] >= obs[2])/iters, "lower" = sum(distr[2,] <= obs[2])/iters, "both" = sum(abs(distr[2,]) >= abs(obs[2]))/iters)
  return(list("TestStats" = obs, distr, "MeansPvalue" = pval_diffmeans, "tPvalue" = pval_t))
}
