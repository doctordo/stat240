################################################################################

# Stat 240 - Non-parametric Statistics
# Homework 1
# Due: 02/20/15

# Rebecca Barter
# Andrew Do
# Kellie Ottobonni

################################################################################

# Libraries and seed
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2)
library(xtable)
set.seed(1337)

################################################################################

# Functions

SampleFromBox <- function(x, size, replace= TRUE, prob = NULL, iter = 1) {
  # Samples tickets from the box.  In the case of one ticket, overrides R's
  # default of sampling from 1:x
  # Arguments:
  #   x - a vector supplying elements from which to choose.  "Tickets in box"
  #   size - positive number, size of sample to draw
  #   replace - boolean that toggles replacement
  #   prob - vector of probability weights or ticket counts
  #   iter - number of trials to perform
  # Returns:
  #   A vector of length iter with the means of samples of size elements 
  #   drawn from x
  if (length(x) <= 1) {
    return(rep(x, iter))
  } else {
    return(vapply(1:iter, function(n){
      mean(sample(x, size, replace = replace, prob = prob))
    }, FUN.VALUE = 1))
  }
}

StandardError <- function(x, n, replace = TRUE) {
  # Computes the standard error
  # Arguments:
  #   x - a vector of ticket values from the box model
  #   n - sample size
  # Returns
  #   standard error of the sample mean in the box model
  N <- length(x) # Number of tickets in box
  if (replace) {
    se <- sqrt((N-1)/N/n) * sd(x)
  } else {
    # Finite population correction
    se <- sqrt((N-n)/N/n) * sd(x)
  }
  return(se)
}

ExactProb <- function(z.values, mean, se, cdf, sample.size, lower.tail = FALSE, ...) {
  # Computes the exact probability of P(Xbar > z_i) for z_1, ..., z_max
  # Arguments:
  #   z.values - vector of z's to consider
  #   mean - expected value of Xbar
  #   se - standard error of xbar
  #   cdf - distribution function to use (e.g. pbinom, phyper)
  #   sample.size - size of samples
  #   lower.tail - logical value.  If TRUE, calculates P(Xbar <= z*se).
  #     else calculates P(Xbar < z*se)
  #   ... - arguments to pass to cdf
  # Returns:
  #   Vector of length(z) probabilities
  vapply(z.values, 
         function(z) {sample.size*mean + z * sample.size * se},
         FUN.VALUE = 1) %>%  
    vapply(FUN = function(q) cdf(q, lower.tail = lower.tail, ...), FUN.VALUE = 1)
}

EmpiricalProbEst <- function(z.values, sample, mean, se, lower.tail = FALSE) {
  # Estimates probability of a certain z-score from data
  # Arguments:
  #   z.values - vector of z's to consider
  #   sample - data from which to make the estimates
  #   mean - expected value of Xbar
  #   se - standard error of Xbar
  #   lower.tail - logical value.  If TRUE, calculates P(Xbar <= z*se).
  #     else calculates P(Xbar < z*se)
  # Returns:
  #   Vector of length(z) probability estimates
  n <- length(sample)
  vapply(z.values, function(z) mean + z * se, FUN.VALUE = 1) %>%
    vapply(FUN = function(q) {
      if (lower.tail) {
        sum(sample <= q)/n
      } else {
        sum(sample > q)/n
      }
    }, FUN.VALUE = 1)
}

NormalProbEst <- function(z.values, mean, se, n, lower.tail = FALSE) {
  # Estimates probability of a certain z-score using normal approximation
  # with continuity correction
  # Arguments:
  #   z.values - vector of z's to consider
  #   mean - expected value of Xbar
  #   se - standard error of Xbar
  #   n - the size of the samples
  #   lower.tail - logical value.  If TRUE, calculates P(Xbar <= z*se).
  #     else calculates P(Xbar < z*se)
  # Returns:
  #   Vector of max.z probability estimates
  vapply(z.values, function(z) n*mean + z * n * se, FUN.VALUE = 1) %>%
    vapply(FUN = function(q) {
      pnorm(q - 1/2*(1-lower.tail) + 1/2*lower.tail, 
            mean = n*mean, sd = n*se, lower.tail = lower.tail)
    }, FUN.VALUE = 1)
}
################################################################################

########################
# Scripts for problems 1-4

ProblemTickets <- function(problem) {
  # Only relevant for problems 1-4
  # Just a readable code thing
  # Arguments:
  #   problem - problem number
  # Returns:
  #   Vector of ticket values as specified in the assignment
  if (problem == 1) {
    tickets <- c(rep(0, 7), rep(1, 5))
  } else if (problem == 2) {
    tickets <- c(rep(0, 98), rep(1, 2))
  } else if (problem == 3) {
    tickets <- 1:12
  } else if (problem == 4) {
    tickets <- c(1:11, 30)
  } else {
    tickets <- 'a load of rubbish'
  }
}


Analysis <- function(tickets, samp.size = 6, iter = 100000) {
  # All the analysis for problems 1-4 wrapped up in a function
  # Arguments:
  #   tickets - a vector of ticket values
  #   samp.size - sample size, defaults to 6
  #   iter - number of iterations for the simulation
  # Returns:
  #   mean.wr and mean.nr - expected value of sample mean
  #   se.wr and se.nr - standard error of sample mean
  #   hist.wr and hist.nr - histogram of the sample means
  #   prob.wr and prob.nr - probability table as specified in the assignment
  # Example usage:
  #   q <- Analysis(c(0,0,0,1,1), samp.size = 10)
  #   q$mean.wr
  #   q$se.nr
  
  # With replacement
  mean.wr <- mean(tickets)
  se.wr <- StandardError(tickets, samp.size , replace = TRUE)
  
  sample.wr <- SampleFromBox(x = tickets, size = samp.size , 
                             replace = TRUE, iter = iter)
  
  hist.wr <- data.frame(Mean = sample.wr) %>%
    ggplot(aes(x=Mean)) + geom_histogram(aes( y=..density..), 
                                         binwidth = .15, col = "white") +
    stat_function(data=data.frame(x=c(0,1)), aes(x=x), 
                  fun=dnorm, arg=list(mean=mean.wr, sd=se.wr))
  
  # Table for with replacement
  prob.wr <- data.frame(z = c(-4:-1, 1:4))
  
  # Does the exact probabilities if tickets are distributed
  # as Bernoulli
  if(sum(!duplicated(tickets)) == 2) {
    
    one.count <- sum(tickets == 1) # Number of 1's
    zero.count <- sum(tickets == 0) # Number of 0's
    prob1 <- one.count/(one.count + zero.count)
    
    tmp1 <- ExactProb(1:4, mean = mean.wr, se = se.wr, 
              cdf = pbinom, sample.size = samp.size, 
              size = samp.size , prob = prob1, lower.tail = FALSE)
    tmp2 <- ExactProb(-4:-1, mean = mean.wr, se = se.wr, 
              cdf = pbinom, sample.size = samp.size, 
              size = samp.size , prob = prob1, lower.tail = TRUE)
    prob.wr <- mutate(prob.wr, Exact = c(tmp2, tmp1))
  }
  
  # Empirical estimates from simulation
  tmp3 <- EmpiricalProbEst(1:4, sample = sample.wr, 
                           mean = mean.wr, se = se.wr, lower.tail = FALSE)
  tmp4 <- EmpiricalProbEst(-4:-1, sample = sample.wr, 
                           mean = mean.wr, se = se.wr, lower.tail = TRUE)
  
  # Normal approximation
  tmp5 <- NormalProbEst(1:4, mean = mean.wr, se = se.wr, 
                        n = samp.size , lower.tail = FALSE)
  tmp6 <- NormalProbEst(-4:-1, mean = mean.wr, se = se.wr,
                        n = samp.size, lower.tail = TRUE)
  
  # Put everything into a nice data frame
  prob.wr <- mutate(prob.wr, 
                    EmpiricalEst = c(tmp4, tmp3),
                    NormalApprox = c(tmp6, tmp5)) %>%
    round(4)
  
  # No replacement
  mean.nr <- mean(tickets)
  se.nr <- StandardError(tickets, samp.size , replace = FALSE)
  
  sample.nr <- SampleFromBox(x = tickets, size = samp.size, 
                             replace = FALSE, iter = iter)
  
  hist.nr <- data.frame(Mean = sample.nr) %>%
    ggplot(aes(x=Mean)) + geom_histogram(aes( y=..density..), 
                                         binwidth = .15, col = "white") + 
    stat_function(data=data.frame(x=c(0,1)), aes(x=x), 
                  fun=dnorm, arg=list(mean=mean.nr, sd=se.nr))
  
  # Table for without replacement
  prob.nr <- data.frame(z = c(-4:-1, 1:4))
  
  # Does the exact probabilities if tickets are distributed
  # as Bernoulli
  if(sum(!duplicated(tickets)) == 2) {
    tmp1 <- ExactProb(1:4, mean = mean.nr, se = se.nr, 
              cdf = phyper, sample.size = samp.size, 
              m = one.count, n = zero.count, 
              k = samp.size, lower.tail = FALSE)
    tmp2 <- ExactProb(-4:-1, mean = mean.nr, se = se.nr, 
              cdf = phyper, sample.size = samp.size , 
              m = one.count, n = zero.count, 
              k = samp.size , lower.tail = TRUE)
    prob.nr <- mutate(prob.nr, Exact = c(tmp2, tmp1))
  }
  
  # Empirical estimates from simulation without replacement
  tmp3 <- EmpiricalProbEst(1:4, sample = sample.nr, mean = mean.nr, 
                           se = se.nr, lower.tail = FALSE)
  tmp4 <- EmpiricalProbEst(-4:-1, sample = sample.nr, 
                           mean = mean.nr, se = se.nr, lower.tail = TRUE)
  
  # Normal approximation
  tmp5 <- NormalProbEst(1:4, mean = mean.nr, se = se.nr, 
                        n = samp.size , lower.tail = FALSE)
  tmp6 <- NormalProbEst(-4:-1, mean = mean.nr, se = se.nr, 
                        n = samp.size, lower.tail = TRUE)
  
  prob.nr <- mutate(prob.nr, 
                    EmpiricalEst = c(tmp4, tmp3),
                    NormalApprox = c(tmp6, tmp5)) %>%
    round(4)
  
  return(list(mean.wr = mean.wr,
              mean.nr = mean.nr,
              se.wr = se.wr,
              se.nr = se.nr,
              prob.wr = prob.wr,
              prob.nr = prob.nr,
              hist.wr = hist.wr,
              hist.nr = hist.nr))
}


q1 <- Analysis(ProblemTickets(1), samp.size = 6, iter = 100000)
q2 <- Analysis(ProblemTickets(2), samp.size = 6, iter = 100000)
q3 <- Analysis(ProblemTickets(3), samp.size = 6, iter = 100000)
q4 <- Analysis(ProblemTickets(4), samp.size = 6, iter = 100000)
