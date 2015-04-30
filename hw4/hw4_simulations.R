
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

#------------------------------------------------------------------------------#
#
# @Problem 1
#
#------------------------------------------------------------------------------#

### Test set 1
X <- c(-1.0, 1.7, -2.0, 0.6, 0.9, 3.5)
Y <- c(1.9, -0.3, 2.8, -0.7, 1.6, -2.4)

permutation_test(X, Y, normal_approx=TRUE)
# [1] 0.4505772 -- matches
permutation_test(X, Y, normal_approx=FALSE, L=1000000)
# [1] 0.454514

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
# [1] 0.6393479
permutation_test(X, Y, normal_approx=FALSE, L=1000000)
# [1] 0.626417

wilcoxon_rank_sum_test(X, Y, normal_approx=TRUE)
# [1] 0.5319069
wilcoxon_rank_sum_test(X, Y, normal_approx=FALSE, L=1000000)
# [1] 0.531804

paired_permutation_test(X, Y, normal_approx = TRUE)
# [1] 0.6826982
paired_permutation_test(X, Y, normal_approx = FALSE, L=1000000)
# [1] 0.626229

sign_test(X,Y)
# [1] 0.65625

wilcoxon_signed_rank_test(X, Y, normal_approx = TRUE)
# [1] 0.6625066
wilcoxon_signed_rank_test(X, Y, normal_approx = FALSE, L=1000000)
# [1] 0.655774



#------------------------------------------------------------------------------#
#
# @Problem 2
#
#------------------------------------------------------------------------------#

set.seed(2804)
# Y standard normal, X has the same distribution as Y, except shifted up by 0.3
L <- 100000
n <- 50
# define Y
Y <- rnorm(n*L, 0, 1)
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X
X <- rnorm(n*L, 0.3, 1)
X <- matrix(X, ncol = L)
X <- split(X, col(X)) # each list entry is a dataset

z.test.norm <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X, Y)
power.z.norm <- sum(z.test.norm<=0.05)/L
# [1] 0.43992

rank.sum.test.norm <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X, Y)
power.rank.sum.norm <- sum(rank.sum.test.norm<=0.05)/L
# [1] 0.427

paired.test.norm <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X, Y)
power.paired.norm <- sum(paired.test.norm<=0.05)/L
# [1] 0.43591

sign.test.norm <- mapply(function(x,y) sign_test(x, y), X, Y)
power.sign.norm <- sum(sign.test.norm<=0.05)/L
# [1] 0.35847

signed.rank.test.norm <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X, Y)
power.signed.rank.norm <- sum(signed.rank.test.norm<=0.05)/L
# [1] 0.42039









# Y standard exponential, X has the same distribution as Y, except shifted up by 0.3
L <- 100000
n <- 50
# define Y
Y <- rexp(n*L)
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X
X <- rexp(n*L) + 0.3
X <- matrix(X, ncol = L)
X <- split(X, col(X)) # each list entry is a dataset

z.test.exp <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X, Y)
power.z.exp <- sum(z.test.exp<=0.05)/L
# [1] 0.45279

rank.sum.test.exp <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X, Y)
power.rank.sum.exp <- sum(rank.sum.test.exp<=0.05)/L
# [1] 0.73091

paired.test.exp <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X, Y)
power.paired.exp <- sum(paired.test.exp<=0.05)/L
# [1] 0.44992

sign.test.exp <- mapply(function(x,y) sign_test(x, y), X, Y)
power.sign.exp <- sum(sign.test.exp<=0.05)/L
# [1] 0.61785

signed.rank.test.exp <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X, Y)
power.signed.rank.exp <- sum(signed.rank.test.exp<=0.05)/L
# [1] 0.55044









# Y standard lognormal, X has the same distribution as Y, except shifted up by 0.3
L <- 100000
n <- 50
# define Y
Y <- exp(rnorm(n*L))
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X
X <- exp(rnorm(n*L)) + 0.3
X <- matrix(X, ncol = L)
X <- split(X, col(X)) # each list entry is a dataset

z.test.lognorm <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X, Y)
power.z.lognorm <- sum(z.test.lognorm<=0.05)/L
# [1] 0.20708

rank.sum.test.lognorm <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X, Y)
power.rank.sum.lognorm <- sum(rank.sum.test.lognorm<=0.05)/L
# [1] 0.55199

paired.test.lognorm <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X, Y)
power.paired.lognorm <- sum(paired.test.lognorm<=0.05)/L
# [1] 0.20579

sign.test.lognorm <- mapply(function(x,y) sign_test(x, y), X, Y)
power.sign.lognorm <- sum(sign.test.lognorm<=0.05)/L
# [1] 0.45735

signed.rank.test.lognorm <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X, Y)
power.signed.rank.lognorm <- sum(signed.rank.test.lognorm<=0.05)/L
# [1] 0.35292









# Y standard Uniform[0,3], X has the same distribution as Y, except shifted up by 0.3
L <- 100000
n <- 50
# define Y
Y <- runif(n*L, 0, 3)
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X
X <- runif(n*L, 0, 3) + 0.3
X <- matrix(X, ncol = L)
X <- split(X, col(X)) # each list entry is a dataset

z.test.unif <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X, Y)
power.z.unif <- sum(z.test.unif<=0.05)/L
# [1] 0.52829

rank.sum.test.unif <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X, Y)
power.rank.sum.unif <- sum(rank.sum.test.unif<=0.05)/L
# [1] 0.50395

paired.test.unif <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X, Y)
power.paired.unif <- sum(paired.test.unif<=0.05)/L
# [1] 0.52455

sign.test.unif <- mapply(function(x,y) sign_test(x, y), X, Y)
power.sign.unif <- sum(sign.test.unif<=0.05)/L
# [1] 0.41725

signed.rank.test.unif <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X, Y)
power.signed.rank.unif <- sum(signed.rank.test.unif<=0.05)/L
# [1] 0.48594












# Y Cauchy, X has the same distribution as Y, except shifted up by 0.3
L <- 100000
n <- 50
# define Y
Y <- rcauchy(n*L)
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X
X <- rcauchy(n*L) + 0.3
X <- matrix(X, ncol = L)
X <- split(X, col(X)) # each list entry is a dataset

z.test.cauchy <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X, Y)
power.z.cauchy <- sum(z.test.cauchy<=0.05)/L
# [1] 0.05261

rank.sum.test.cauchy <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X, Y)
power.rank.sum.cauchy <- sum(rank.sum.test.cauchy<=0.05)/L
# [1] 0.20469

paired.test.cauchy <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X, Y)
power.paired.cauchy <- sum(paired.test.cauchy<=0.05)/L
# [1] 0.05182

sign.test.cauchy <- mapply(function(x,y) sign_test(x, y), X, Y)
power.sign.cauchy <- sum(sign.test.cauchy<=0.05)/L
# [1] 0.18585

signed.rank.test.cauchy <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X, Y)
power.signed.rank.cauchy <- sum(signed.rank.test.cauchy<=0.05)/L
# [1] 0.14219










# Y mixture of standard normal with prob 0.9 and normal with mean 0 and standard error 10 with probability 0.1, X has the same distribution as Y, except shifted up by 0.3
L <- 100000
n <- 50
# define Y
U <- as.numeric(runif(n*L) <= 0.9)
Y <- U*rnorm(n*L) + (1-U)*rnorm(n*L, 0, 10)
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X
U <- as.numeric(runif(n*L) <= 0.9)
X <- U*rnorm(n*L) + (1-U)*rnorm(n*L, 0, 10) + 0.3
X <- matrix(X, ncol = L)
X <- split(X, col(X)) # each list entry is a dataset

z.test.mix <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X, Y)
power.z.mix <- sum(z.test.mix<=0.05)/L
# [1] 0.13511

rank.sum.test.mix <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X, Y)
power.rank.sum.mix <- sum(rank.sum.test.mix<=0.05)/L
# [1] 0.33475

paired.test.mix <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X, Y)
power.paired.mix <- sum(paired.test.mix<=0.05)/L
# [1] 0.13398

sign.test.mix <- mapply(function(x,y) sign_test(x, y), X, Y)
power.sign.mix <- sum(sign.test.mix<=0.05)/L
# [1] 0.28751

signed.rank.test.mix <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X, Y)
power.signed.rank.mix <- sum(signed.rank.test.mix<=0.05)/L
# [1] 0.27213









# Y normal, X has the same distribution as Y, except shifted up by 0.3. Correlated
L <- 100000
n <- 50
# define Y
W = U = V = rnorm(n*L)
Y <- U + W
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X
X <- V + W + 0.3
X <- matrix(X, ncol = L)
X <- split(X, col(X)) # each list entry is a dataset

z.test.normcor <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X, Y)
power.z.normcor <- sum(z.test.normcor<=0.05)/L
# [1] 0

rank.sum.test.normcor <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X, Y)
power.rank.sum.normcor <- sum(rank.sum.test.normcor<=0.05)/L
# [1] 0

paired.test.normcor <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X, Y)
power.paired.normcor <- sum(paired.test.normcor<=0.05)/L
# [1] 1

sign.test.normcor <- mapply(function(x,y) sign_test(x, y), X, Y)
power.sign.normcor <- sum(sign.test.normcor<=0.05)/L
# [1] 1

signed.rank.test.normcor <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X, Y)
power.signed.rank.normcor <- sum(signed.rank.test.normcor<=0.05)/L
# [1] 1




# put results into data frame
results.df <- data.frame(power = c(power.z.norm, power.rank.sum.norm, power.paired.norm, power.sign.norm, power.signed.rank.norm, 
                                   power.z.exp, power.rank.sum.exp, power.paired.exp, power.sign.exp, power.signed.rank.exp, 
                                   power.z.lognorm, power.rank.sum.lognorm, power.paired.lognorm, power.sign.lognorm, power.signed.rank.lognorm, 
                                   power.z.unif, power.rank.sum.unif, power.paired.unif, power.sign.unif, power.signed.rank.unif, 
                                   power.z.cauchy, power.rank.sum.cauchy, power.paired.cauchy, power.sign.cauchy, power.signed.rank.cauchy, 
                                   power.z.mix, power.rank.sum.mix, power.paired.mix, power.sign.mix, power.signed.rank.mix,
                                   power.z.normcor, power.rank.sum.normcor, power.paired.normcor, power.sign.normcor, power.signed.rank.normcor),
                         test = factor(rep(c("Z", "Wilcoxon Rank Sum", "Paired Z", "Sign", "Wilcoxon Signed Rank"), times = 7), levels = c("Z", "Wilcoxon Rank Sum", "Paired Z", "Sign", "Wilcoxon Signed Rank")),
                         distribution = factor(rep(c("standard normal", "exponential", "log-normal","uniform","cauchy","normal mixture","normal correlated"), each = 5), levels = c("standard normal", "exponential", "log-normal","uniform","cauchy","normal mixture","normal correlated")))
# plot results
ggplot(results.df) + geom_bar(aes(x = distribution, y =  power, fill = test), size = 2, position = "dodge", stat = "identity")

# put results in table
results.table <- dcast(results.df, test ~ distribution, mean, value.var = "power")




#------------------------------------------------------------------------------#
#
# @Problem 3
#
#------------------------------------------------------------------------------#
set.seed(0416)
### With Normal Approximation
# Y Cauchy, X1 has the same distribution as Y, except shifted up by 0.5
L <- 100000
n <- 50
# define Y
Y <- rcauchy(n*L)
Y <- matrix(Y, ncol = L)
Y <- split(Y, col(Y)) # each list entry is a dataset

# define X1 with a shift
X1 <- rcauchy(n*L) + 0.5
X1 <- matrix(X1, ncol = L)
X1 <- split(X1, col(X1)) # each list entry is a dataset

# define X2 without a shift
X2 <- rcauchy(n*L)
X2 <- matrix(X2, ncol = L)
X2 <- split(X2, col(X2)) # each list entry is a dataset

# With shift
p3a1.z.test.cauchy <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X1, Y)
p3a1.power.z.cauchy <- sum(p3a1.z.test.cauchy<=0.05)/L
# [1] 0.07326

p3a1.rank.sum.test.cauchy <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X1, Y)
p3a1.power.rank.sum.cauchy <- sum(p3a1.rank.sum.test.cauchy<=0.05)/L
# [1] 0.00132

p3a1.paired.test.cauchy <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X1, Y)
p3a1.power.paired.cauchy <- sum(p3a1.paired.test.cauchy<=0.05)/L
# [1] 0.07278

p3a1.signed.rank.test.cauchy <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X1, Y)
p3a1.power.signed.rank.cauchy <- sum(p3a1.signed.rank.test.cauchy<=0.05)/L
# [1] 0.24735

# Without Shift
p3a2.z.test.cauchy <- mapply(function(x,y) permutation_test(x, y, normal_approx=TRUE), X2, Y)
p3a2.power.z.cauchy <- sum(p3a2.z.test.cauchy<=0.05)/L
# [1] 0.033107

p3a2.rank.sum.test.cauchy <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=TRUE), X2, Y)
p3a2.power.rank.sum.cauchy <- sum(p3a2.rank.sum.test.cauchy<=0.05)/L
# [1] 0.04859

p3a2.paired.test.cauchy <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=TRUE), X2, Y)
p3a2.power.paired.cauchy <- sum(p3a2.paired.test.cauchy<=0.05)/L
# [1] 0.03036

p3a2.signed.rank.test.cauchy <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=TRUE), X2, Y)
p3a2.power.signed.rank.cauchy <- sum(p3a2.signed.rank.test.cauchy<=0.05)/L
# [1] 0.05018

### Without Normal Approximation
# Y Cauchy, X has the same distribution as Y, except shifted up by 0.3
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
X2 <- rcauchy(n*L) + 0.5
X2 <- matrix(X2, ncol = L)
X2 <- split(X2, col(X2)) # each list entry is a dataset

# With shift
p3b1.z.test.cauchy <- mapply(function(x,y) permutation_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.z.cauchy <- sum(z.test.cauchy<=0.05)/L


p3b1.rank.sum.test.cauchy <- mapply(function(x,y) wilcoxon_rank_sum_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.rank.sum.cauchy <- sum(rank.sum.test.cauchy<=0.05)/L


p3b1.paired.test.cauchy <- mapply(function(x,y) paired_permutation_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.paired.cauchy <- sum(paired.test.cauchy<=0.05)/L


p3b1.sign.test.cauchy <- mapply(function(x,y) sign_test(x, y), X1, Y)
p3b1.power.sign.cauchy <- sum(sign.test.cauchy<=0.05)/L


p3b1.signed.rank.test.cauchy <- mapply(function(x,y) wilcoxon_signed_rank_test(x, y, normal_approx=FALSE, L=1000), X1, Y)
p3b1.power.signed.rank.cauchy <- sum(signed.rank.test.cauchy<=0.05)/L


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

# Put the results in a data frame
p3.results.df <- data.frame(Method = c(rep(c('Z Test', 
                                           'Wilcoxon Rank Sum Test',
                                           'Paired Z Test',
                                           'Wilcoxon Signed Rank Test'), 2),
                                       rep(c('Z Test', 
                                             'Wilcoxon Rank Sum Test',
                                             'Paired Z Test',
                                             'Sign Test',
                                             'Wilcoxon Signed Rank Test'), 2)),
                            Calculation = c(rep('Normal', 8), rep('Exact', 10)),
                            Shift = c(rep(.5, 4), rep(0, 4),
                                      rep(.5, 5), rep(0, 5)),
                            Power = c(p3a1.power.z.cauchy,
                                      p3a1.power.rank.sum.cauchy,
                                      p3a1.power.paired.cauchy,
                                      p3a1.power.signed.rank.cauchy,
                                      p3a2.power.z.cauchy,
                                      p3a2.power.rank.sum.cauchy,
                                      p3a2.power.paired.cauchy,
                                      p3a2.power.signed.rank.cauchy,
                                      p3b1.power.z.cauchy,
                                      p3b1.power.rank.sum.cauchy,
                                      p3b1.power.paired.cauchy,
                                      p3b1.power.sign.cauchy,
                                      p3b1.power.signed.rank.cauchy,
                                      p3b2.power.z.cauchy,
                                      p3b2.power.rank.sum.cauchy,
                                      p3b2.power.paired.cauchy,
                                      p3b2.power.sign.cauchy,
                                      p3b2.power.signed.rank.cauchy))
                                      
                                      
                                      
                                      
