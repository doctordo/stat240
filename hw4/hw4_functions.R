permutation_test = function(x, y, normal_approx=FALSE, L=100000)
{
  n = length(x)
  m = length(y)
  if (normal_approx)
  {
    test_statistic = mean(x) - mean(y)
    standard_error = sqrt(var(x)/n + var(y)/m)      # Exact value under the strong null
      z = test_statistic / standard_error
    pval = 1-pnorm(z)  # One sided test; Alternative hypothesis is positive treatment effect    
    # No continuity correction; x and y need not be integers.
  }
  else 
  {
    test_statistic = mean(x) - mean(y)
    test_statistic_stars = rep(0,L)
    for (l in 1:L)
    {
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


wilcoxon_rank_sum_test = function(x, y, normal_approx=FALSE, L=100000)
{
  n = length(x)
  m = length(y)
  xranks = rank(c(x,y))[1:n]  # Do not worry about ties in this assignment
  if (normal_approx)
  {
    test_statistic <- sum(xranks)
    mu <- n*(n+m+1)/2
    sigma2 <- n*m*(n+m+1)/12
    z <- ((test_statistic + 0.5) - mu)/sqrt(sigma2) # Use continuity correction.
    pval = pnorm(z) # One sided test.
  }
  else 
  {
    test_statistic <- sum(xranks)
    test_statistic_stars = rep(0,L)
    for (l in 1:L)
    {
      xranks_star = sample(1:(n+m), n, replace = FALSE)
      test_statistic_stars[l] <- sum(xranks_star)
    }
    pval = sum(test_statistic_stars <= test_statistic)/L  # One sided test
  }
  return(pval)
}



paired_permutation_test = function(x, y, normal_approx=FALSE, L=100000)
{
  n = length(x) # paired data; m = n
  if (normal_approx)
  {
    test_statistic = mean(x)  # Or mean(x). Your choice.
    mu <- mean(x+y)/2
    sigma2 <- sum((x-y)^2)/(4*n^2)
    pval <- 1-pnorm(test_statistic, mu, sqrt(sigma2))# One sided test; Alternative hypothesis is positive treatment effect    
    # No continuity correction; x and y need not be integers.
  }
  else 
  {
    test_statistic = mean(x)  # Or mean(x). Your choice.
    test_statistic_stars = rep(0,L)
    for (l in 1:L)
    {
      w <- c(x,y)
      x_star <- w[sample(1:(2*n), n, replace = FALSE)]
      test_statistic_stars[l] <- mean(x_star)
    }
    pval = sum(test_statistic_stars >= test_statistic)/L  # One sided test
    # One sided test; Alternative hypothesis is positive treatment effect    
  }
  return(pval)
}


sign_test = function(x, y)
{
  n <- length(x)
  identical <- (x == y)
  if(any(identical)){
    x <- x[!identical]
    y <- y[!identical]
  }
  S <- sum(x > y)
  # Provide an exact p-value
  pval <- pbinom(S, n, 0.5)
  return(pval)
}

wilcoxon_signed_rank_test = function(x, y, normal_approx=FALSE, L=100000)
{
  # Insert code here
  # One sided test; Alternative hypothesis is positive treatment effect    
  # Use continuity correction for normal approximation
}


