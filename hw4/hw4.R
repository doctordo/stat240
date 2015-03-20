permutation_test = function(x, y, normal_approx=FALSE, L=100000)
{
  n = length(x)
  m = length(y)
  if (normal_approx)
  {
    test_statistic = mean(x) - mean(y)
    standard_error =       # Exact value under the strong null
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
      y_star =
        test_statistic_stars[l] = 
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
    # Insert code here.  
    # One sided test.
    # Use continuity correction.
  }
  else 
  {
    # Insert code here.  
    # One sided test.
  }
  return(pval)
}



paired_permutation_test = function(x, y, normal_approx=FALSE, L=100000)
{
  n = length(x) # paired data; m = n
  if (normal_approx)
  {
    test_statistic = mean(x - y)  # Or mean(x). Your choice.
    # Insert code here
    # One sided test; Alternative hypothesis is positive treatment effect    
    # No continuity correction; x and y need not be integers.
  }
  else 
  {
    test_statistic = mean(x - y)  # Or mean(x). Your choice.
    # Insert code here
    # One sided test; Alternative hypothesis is positive treatment effect    
  }
  return(pval)
}


sign_test = function(x, y)
{
  # Provide an exact p-value
  return(pval)
}

wilcoxon_signed_rank_test = function(x, y, normal_approx=FALSE, L=100000)
{
  # Insert code here
  # One sided test; Alternative hypothesis is positive treatment effect    
  # Use continuity correction for normal approximation
}


