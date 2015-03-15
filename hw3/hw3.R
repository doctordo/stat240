set.seed(50)

compute_test_stats <- function(x, y){
  # Computes difference in means and t statistic for tr. group x and control group y
  n <- length(x); m <- length(y);
  diff_means <- mean(x) - mean(y)
  # Do we need to multiply variance by n-1/n and m-1/m?
  # Andrew - I don't think so, we are estimating the variance
  #   in a t-test
  t <- diff_means/sqrt(var(x)/n + var(y)/m) 
  return(c("diff_means" = diff_means, "t" = t))
}

perm_test_mean <- function(x, y, iters = 10000){
  # Performs permutation tests on the difference of means
  #   and the t-statistic
  # Input:
  #   x - vector of observations for treatment group
  #   y - vector of observations for control group
  #   iters - number of permutations

  # Output:
  #   TestStats - vector containing difference in means and the t-statistic
  #   Pvalue - vector with lower, upper, and two-tailed p-values
  n <- length(x)
  box <- c(x, y)
  obs <- compute_test_stats(x, y)
  distr <- replicate(iters, {
    shuffle <- sample(box)
    xnew <- shuffle[1:n]
    ynew <- shuffle[-(1:n)]
    compute_test_stats(xnew, ynew)
  })
  
  pval_diffmeans <- c("upper" = sum(distr[1,] >= obs[1])/iters, 
                      "lower" = sum(distr[1,] <= obs[1])/iters, 
                      "both" = sum(abs(distr[1,]) >= abs(obs[1]))/iters)
  
  pval_t <- c("upper" = sum(distr[2,] >= obs[2])/iters, 
              "lower" = sum(distr[2,] <= obs[2])/iters, 
              "both" = sum(abs(distr[2,]) >= abs(obs[2]))/iters)
  
  return(list("TestStats" = obs, 
              "distr" = distr, 
              "MeansPvalue" = pval_diffmeans, 
              "tPvalue" = pval_t))
}




### Problem 2: Construct a hypothetical dataset 
# (with at least 3 data points in treatment and at least 3 in control) 
# for which the p-value of a permutation test based on X ̄ is smaller
# than the p-value of a permutation test based on t.
tr_2 <- rnorm(5, mean = 0, sd = 20)
# [1]  10.9933979 -16.8320749   0.6599587  10.4829942 -34.5520822
ctrl_2 <- rnorm(100, mean = 0, sd = 1)
res2 <- perm_test_mean(tr_2, ctrl_2)
res2$MeansPvalue
#upper lower  both
#0.95  0.05  0.05
#
res2$tPvalue
#upper lower  both
#0.861 0.139 0.559
res2$TestStats
#diff_means          t
#-5.7512729 -0.6559599



### Problem 3: Construct a hypothetical dataset 
# (with at least 3 data points in treatment and at least 3 in control) 
# for which the p-value of a permutation test based on t is smaller than 
# the p-value of a permutation test based on X ̄.

# this sort of works but it's not a "HUGE" difference. Let's play with it more; 
# increasing variability in the ctrl group helps

# Andrew - This always gives a higher p-value to the mean test with the mean of
# the ratio of the p-values at 4 and median at 2.  It took roughly an hour to run 
# 1000 simulations for the p-values.  I think it has to do with the spread
# and number of realized points as the t-test gains power from increased
# sample size

tr_3 <- rnorm(10, mean = 0, sd = 1)
ctrl_3 <- rnorm(1000, mean = 1, sd = 100)
res3 <- perm_test_mean(tr_3, ctrl_3, iters = 10000)
res3$TestStats
res3$tPvalue
res3$MeansPvalue

# Simulating P values, change playing to TRUE to run the simulation
playing = FALSE

if (playing) {
SimulateP <- function(iterations = 1000) {
  p <- rep(NA, iterations)
  for (i in 1:iterations) {
    tr <- rnorm(10, mean = 0, sd = 1)
    ctrl <- rnorm(1000, mean = 1, sd = 100)
    res <- perm_test_mean(tr, ctrl, iters = 10000)
    p[i] <- res$MeansPvalue[3] / res$tPvalue[3]
  }
  return(p)
}

ggplot(data.frame(p=p)) + geom_histogram(aes(x=p), binwidth=1) + xlim(c(1,25))
mean(p)
median(p)

}

### Problem 4: Construct a hypothetical dataset 
# (with at least 3 data points in treatment and at least 3 in control) 
# for which the p-value of a permutation test based on X ̄ is smaller
# than the p-value of a standard t test.

tr_4 <- c(-10:10, 500, 1000, 2000, 5000)
ctrl_4 <- rep(0:5, 5)
res4 <- perm_test_mean(tr_4, ctrl_4)
t_obs <- res4$TestStats[2]
#       t
# 1.580465
res4$MeansPvalue
#upper  lower   both
#0.0394 0.9608 0.0394
2*pt(t_obs, df = length(tr_4) + length(ctrl_4) -2, lower.tail=F)
#t
#0.1199507
