################################################################################

# Stat 240 - Non-parametric Statistics
# Homework 4
# Due: 5/1/15

# Rebecca Barter
# Andrew Do
# Kellie Ottoboni

################################################################################

# Libraries and seed
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2)
library(xtable)
library(gridExtra)
set.seed(1337)

source("hw4_functions.R")

############# Problem 1: test the functions.
### Test set 1
X <- c(-1.0, 1.7, -2.0, 0.6, 0.9, 3.5)
Y <- c(1.9, -0.3, 2.8, -0.7, 1.6, -2.4)

permutation_test(X, Y, normal_approx=TRUE)
# [1] 0.4505772 -- matches
permutation_test(X, Y, normal_approx=FALSE, L=1000000)

wilcoxon_rank_sum_test(X, Y, normal_approx=TRUE)
# [1] 0.5319069 -- matches
wilcoxon_rank_sum_test(X, Y, normal_approx=FALSE, L=1000000)
# [1] 0.531657

paired_permutation_test(X, Y, normal_approx = TRUE)
# [1] 0.4625569 -- matches
paired_permutation_test(X, Y, normal_approx = FALSE, L=1000000)
# [1] 0.455443
sign_test(X,Y)
# [1] 0.65625 -- matches

wilcoxon_signed_rank_test(X, Y, normal_approx = TRUE)
# [1] 0.5
wilcoxon_signed_rank_test(X, Y, normal_approx = FALSE, L=1000000)
# [1] 0.499427


### Second test set
X <- c(0.5, 2.4, -4.1, 5.9, 3.7, 3.6)
Y <- c(2.7, 0.8, 1.6, 5.7, 4.3, 0.3)

permutation_test(X, Y, normal_approx=TRUE)
permutation_test(X, Y, normal_approx=FALSE, L=1000000)

wilcoxon_rank_sum_test(X, Y, normal_approx=TRUE)
wilcoxon_rank_sum_test(X, Y, normal_approx=FALSE, L=1000000)

paired_permutation_test(X, Y, normal_approx = TRUE)
paired_permutation_test(X, Y, normal_approx = FALSE, L=1000000)

sign_test(X,Y)

wilcoxon_signed_rank_test(X, Y, normal_approx = TRUE)
wilcoxon_signed_rank_test(X, Y, normal_approx = FALSE, L=1000000)