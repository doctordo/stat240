library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2)
library(xtable)
library(gridExtra)

setwd(Sys.getenv('R_WD'))

permutation_test = function(x, y, normal_approx=FALSE, L=100000)
{
  n = length(x)
  m = length(y)
  if (normal_approx) {
    test_statistic = mean(x) - mean(y)
    # Exact value under the strong null
    standard_error = sqrt( (n+m)*(var(c(x,y))/(n*m)) )  
    z = test_statistic / standard_error
    # One sided test; Alternative hypothesis is positive treatment effect   
    pval = 1-pnorm(z)   
    # No continuity correction; x and y need not be integers.
  } else {
    
    test_statistic = mean(x) - mean(y)
    test_statistic_stars = rep(0,L)
    
    for (l in 1:L) {
      w = c(x,y)
      treatment_assignment_star = sample(c(rep(TRUE,n), rep(FALSE,m)))
      x_star = w[treatment_assignment_star]
      y_star = w[!treatment_assignment_star]
      test_statistic_stars[l] = mean(x_star) - mean(y_star)
    }
    pval = sum(test_statistic_stars >= test_statistic)/L  # One sided test    
  }
  return(pval)
}


wilcoxon_rank_sum_test = function(x, y, normal_approx=FALSE, L=100000) {
  n = length(x)
  m = length(y)
  xranks = rank(c(x,y))[1:n]  # Do not worry about ties in this assignment
  
  if (normal_approx) {
    test_statistic <- sum(xranks)
    mu <- n*(n+m+1)/2
    sigma2 <- n*m*(n+m+1)/12
    z <- ((test_statistic + 0.5) - mu) / sqrt(sigma2) # Continuity correction.
    pval = 1-pnorm(z) # One sided test.
    
  } else {
    test_statistic <- sum(xranks)
    test_statistic_stars = rep(0,L)
    
    for (l in 1:L) {
      xranks_star = sample(1:(n+m), n, replace = FALSE)
      test_statistic_stars[l] <- sum(xranks_star)
    }
    pval = sum(test_statistic_stars >= test_statistic)/L  # One sided test
  }
  
  return(pval)
}

paired_permutation_test = function(x, y, normal_approx=FALSE, L=100000) {
  n = length(x) # paired data; m = n
  if (normal_approx) {
    test_statistic = mean(x)  # Or mean(x). Your choice.
    mu <- mean(x+y)/2
    sigma2 <- sum((x-y)^2)/(4*n^2)
    
    # One sided test; Alternative hypothesis is positive treatment effect    
    # No continuity correction; x and y need not be integers.
    pval <- 1-pnorm(test_statistic, mu, sqrt(sigma2))
    
  } else {
    test_statistic = mean(x)  # Or mean(x). Your choice.
    test_statistic_stars = rep(0,L)
    for (l in 1:L) {
      w <- c(x,y)
      x_star <- w[sample(1:(2*n), n, replace = FALSE)]
      test_statistic_stars[l] <- mean(x_star)
    }
    # One sided test; Alternative hypothesis is positive treatment effect 
    pval = sum(test_statistic_stars >= test_statistic)/L  
    
  }
  return(pval)
}


sign_test = function(x, y) {
  n <- length(x)
  identical <- (x == y)
  if (any(identical)) {
    x <- x[!identical]
    y <- y[!identical]
  }
  S <- sum(x > y)
  # Provide an exact p-value
  pval <- 1-pbinom(S, n, 0.5)
  return(pval)
}


wilcoxon_signed_rank_test = function(x, y, normal_approx=FALSE, L=100000)
{
  n = length(x)
  pair_ranks = rank(abs(x-y))  # Do not worry about ties in this assignment
  xranks <- rep(0, n)
  yranks <- rep(0, n)
  for(i in 1:n){
    if(x[i] > y[i]){
      xranks[i] <- pair_ranks[i]
    }else{ yranks[i] <- pair_ranks[i]}
  }
  
  if (normal_approx)
  {
    test_statistic <- mean(xranks)
    mu <- (n+1)/4
    sigma2 <- (n+1)*(2*n + 1)/(24*n)
    z <- (test_statistic - (0.5/n) - mu)/sqrt(sigma2) # Use continuity correction.
    pval = 1-pnorm(z) # One sided test.
  }
  else 
  {
    test_statistic <- mean(xranks)
    test_statistic_stars = rep(0,L)
    for (l in 1:L)
    {
      xranks_star = sapply(1:n, function(i) sample(c(xranks[i], yranks[i]), 1))
      test_statistic_stars[l] <- mean(xranks_star)
    }
    pval = sum(test_statistic_stars >= test_statistic)/L    # One sided test; Alternative hypothesis is positive treatment effect    
  }
  return(pval)
}


### Without Normal Approximation
# Y Cauchy, X has the same distribution as Y, except shifted up by 0.5
L <- 10000
n <- 50
# define Y
Y <- rcauchy(n*L)
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X1
X1 <- rcauchy(n*L) + 0.5
X1 <- matrix(X1, ncol = L)
X1 <- split(X1, col(X1)) # each list entry is a dataset

# define X2
X2 <- rcauchy(n*L)
X2 <- matrix(X2, ncol = L)
X2 <- split(X2, col(X2)) # each list entry is a dataset

# With shift
p3b1.z.test.cauchy <- mapply(function(x,y) permutation_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.z.cauchy <- sum(p3b1.z.test.cauchy<=0.05)/L


p3b1.rank.sum.test.cauchy <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.rank.sum.cauchy <- sum(p3b1.rank.sum.test.cauchy<=0.05)/L


p3b1.paired.test.cauchy <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.paired.cauchy <- sum(p3b1.paired.test.cauchy<=0.05)/L


p3b1.sign.test.cauchy <- mapply(function(x,y) sign_test(x, y), X1, Y)
p3b1.power.sign.cauchy <- sum(p3b1.sign.test.cauchy<=0.05)/L


p3b1.signed.rank.test.cauchy <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.signed.rank.cauchy <- sum(p3b1.signed.rank.test.cauchy<=0.05)/L


# Without shift
p3b2.z.test.cauchy <- mapply(function(x,y) permutation_test(x, y, normal_approx=FALSE, L=1000), X2, Y)
p3b2.power.z.cauchy <- sum(p3b2.z.test.cauchy<=0.05)/L


p3b2.rank.sum.test.cauchy <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=FALSE, L=1000), X2, Y)
p3b2.power.rank.sum.cauchy <- sum(p3b2.rank.sum.test.cauchy<=0.05)/L


p3b2.paired.test.cauchy <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=FALSE, L=1000), X2, Y)
p3b2.power.paired.cauchy <- sum(p3b2.paired.test.cauchy<=0.05)/L


p3b2.sign.test.cauchy <- mapply(function(x,y) sign_test(x, y), X2, Y)
p3b2.power.sign.cauchy <- sum(p3b2.sign.test.cauchy<=0.05)/L


p3b2.signed.rank.test.cauchy <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=FALSE, L=1000), X2, Y)
p3b2.power.signed.rank.cauchy <- sum(p3b2.signed.rank.test.cauchy<=0.05)/L

save.image(file = "permutation_cauchy.RData")