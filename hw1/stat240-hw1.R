################################################################################

# Stat 240 - Non-parametric Statistics
# Homework 1
# Due: 02/20/15

# Rebecca Barter
# Andrew Do
# Kellie Ottobonni

################################################################################

# Libraries and seed

set.seed(1337)

################################################################################

# Functions

SampleFromBox <- function(x, size, replace= TRUE, prob = NULL) {
  # Samples tickets from the box.  In the case of one ticket, overrides R's
  # default of sampling from 1:x
  # Arguments:
  #   x - a vector supplying elements from which to choose.  "Tickets in box"
  #   size - positive number, size of sample to draw
  #   replace - boolean that toggles replacement
  #   prob - vector of probability weights or ticket counts
  # Returns:
  #   A vector of length size with elements drawn from x
  if (length(x) <= 1) {
    return(rep(x, size))
  } else {
    return(sample(x, size, replace = replace, prob = prob))
  }
}

StandardError <- function(x, n) {
  # Computes the standard error
  # Arguments:
  #   x - a vector of ticket values from the box model
  #   n - sample size
  # Returns
  #   standard error of the sample mean in the box model
  
  N <- length(x) # Number of tickets in box
  se <- sqrt((N-1)/N/n) * sd(x)
  
  return(se)
}
################################################################################

# Problem 1
tickets1 <- c(rep(0, 7), rep(1, 5))
sample1 <- SampleFromBox(x = tickets1, size = 100000, replace = TRUE)
mean1 <- mean(tickets1)
se1 <- StandardError(tickets1, 6)

# Problem 2
tickets2 <- c(rep(0, 98), rep(1, 2))
sample2 <- SampleFromBox(x = tickets2, size = 100000, replace = TRUE)
mean2 <- mean(tickets2)
se2 <- StandardError(tickets2, 6)

# Problem 3
tickets3 <- 1:12
sample3 <- SampleFromBox(x = tickets3, size = 100000, replace = TRUE)
mean3 <- mean(tickets3)
se3 <- StandardError(tickets3, 6)

# Problem 4
tickets4 <- c(1:11, 30)
sample4 <- SampleFromBox(x = tickets4, size = 100000, replace = TRUE)
mean4 <- mean(tickets4)
se4 <- StandardError(tickets4, 6)

# Extra Credit