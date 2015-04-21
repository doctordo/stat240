
rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(xtable)
setwd("Documents/stat240/final_project/")
set.seed(650)
#if(!("ModelMatch" %in% rownames(installed.packages()))){library(devtools); install_github("kellieotto/ModelMatch/ModelMatch")} 
#library("ModelMatch")

############################################ Functions ###########################################
##################################################################################################
permute_within_groups <- function(x, groups){
  # Permutes values within each group
  for(g in unique(groups)){
    ind <- (groups == g)
    x[ind] <- sample(x[ind])
  }
  return(x)
}

testStatistic_diffmeans <- function(x, treatment){
  mean(x[treatment ==  1], na.rm=T) - mean(x[treatment == 0], na.rm=T)
}

testStatistic_t <- function(x, treatment){
  numer <- testStatistic_diffmeans(x, treatment)
  denom <- sqrt( (var(x[treatment == 1], na.rm=T)/sum(!is.na(x[treatment==1]))) + (var(x[treatment == 0], na.rm=T)/sum(!is.na(x[treatment==0]))))
  return(numer/denom)
}

stratified_permute_means <- function(values, groups, treatment, ts_function, nsims){
  # set up storage
  sims <- matrix(rep(NA, nsims*(length(unique(groups))+1)), ncol = length(unique(groups))+1)
  teststat <- c()
  
  # set up indexing for groups
  ng <- table(groups)
  group_names <- names(ng)
  ng <- as.vector(ng)
  ind <- lapply(group_names, function(x) groups == x)
  
  
  # Compute the test statistic for the given data
  for(g in seq_along(ind)){
    gg <- ind[[g]]
    teststat <- c(teststat, ts_function(values[gg], treatment[gg]))
  }
  teststat <- c(teststat, sum(teststat*ng)/length(groups))
  
  # Compute test stats for permuted data
  for(i in 1:nsims){
    treatment <- permute_within_groups(treatment, groups)
    for(g in seq_along(ind)){
      gg <- ind[[g]]
      sims[i,g] <- ts_function(values[gg], treatment[gg])
    }
  }
  sims[,ncol(sims)] <- apply(sims[,-ncol(sims)], 1, function(x) {sum(x*ng)/length(groups)})
  
  
  # Compute p-values
  groups_tested <- c(group_names, "Overall")
  pval <- matrix(rep(NA, 4*(length(unique(groups))+1)), ncol = length(unique(groups))+1)
  for(i in seq_along(groups_tested)){
    pupper <- mean(sims[,i] >= teststat[i])
    plower <- mean(sims[,i] <= teststat[i])
    pboth  <- mean(abs(sims[,i]) >= abs(teststat[i]))
    pval[,i] <- c(teststat[i], pupper, plower, pboth)
  }
  colnames(pval) <- groups_tested; rownames(pval) <- c("teststat", "pupper", "plower", "pboth")
  return(pval)
}


######################################### Data cleanup ###########################################
#################################################################################################

dat <- read.table("student_test_data.tab.tsv", sep = "\t", header = T)

dat <- dat %>%
  select(pupilid,
         schoolid,
         district,
         division,
         zone,
         
         tracking,
         sbm, # School-based Management
         girl,
         etpteacher,
         
         lowstream, # Assigned to low stream
         
         bottomhalf, # Actually in bottom half
         tophalf,
         
         bottomquarter,
         secondquarter,
         thirdquarter,
         topquarter,
         
         std_mark, # Standardized initial score
         percentile,
         quantile5p, # Quantile by 5%
         
         attrition, # Could not be found for endline test
         agetest, # age at testing
         
         wordscore,
         sentscore, # Sentence score
         letterscore,
         spellscore,
         litscore,
         mathscoreraw,
         totalscore,
         
         r2_attrition, # Could not be found for ltfu
         r2_age, # age at long-term follow-up
         
         r2_wordscore,
         r2_sentscore,
         r2_letterscore,
         r2_spellscore,
         r2_litscore,
         r2_mathscoreraw,
         r2_totalscore
  )

quartiles <- c('bottomquarter','secondquarter','thirdquarter','topquarter')
dat$quartile <- rep(NA, nrow(dat))
for(i in 1:4){
  keep <- which(dat[,quartiles[i]] == 1)
  dat$quartile[keep] <- quartiles[i]
}


######################################### Data analysis #########################################
#################################################################################################


#### Question 1: does tracking affect follow-up exam score, stratifying by baseline quartile? Look overall and w/in strata. Use difference in means as test statistic.
res1 <- stratified_permute_means(values = dat$totalscore, groups = dat$quartile, treatment = dat$tracking, ts_function = testStatistic_diffmeans, nsims = 10000)
res1t <- stratified_permute_means(values = dat$totalscore, groups = dat$quartile, treatment = dat$tracking, ts_function = testStatistic_t, nsims = 10000)

p1 <- filter(dat, !is.na(tracking)) %>% select(bottomquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_density(aes(alpha = 0.4)) #+ facet_grid(.~quartile)
p2 <- filter(dat, !is.na(tracking)) %>% select(secondquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_density(aes(alpha = 0.4)) #+ facet_grid(.~quartile)
p3 <- filter(dat, !is.na(tracking)) %>% select(thirdquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_density(aes(alpha = 0.4)) #+ facet_grid(.~quartile)
p4 <- filter(dat, !is.na(tracking)) %>% select(topquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_density(aes(alpha = 0.4)) #+ facet_grid(.~quartile)
plist <- list(p1, p2, p3, p4)
do.call(grid.arrange, plist)
p <-  filter(dat, !is.na(tracking)) %>% ggplot(aes(litscore, fill = as.factor(tracking))) + geom_density(aes(alpha = 0.4))

mat1 <- rbind(res1[1,], res1t[c(1,4),])
rownames(mat1) <- c("Difference in means", "t", "P-value")
colnames(mat1) <- c("Bottom quarter", "Second quarter", "Third quarter", "Top quarter", "Overall")
xtable(mat1)


#### Question 2: does tracking affect follow-up exam score, stratifying by baseline 5% quantiles? Look overall and w/in strata
dat_cleanquantiles <- filter(dat, !is.na(quantile5p) & !is.na(totalscore))
res2 <- stratified_permute_means(values = dat_cleanquantiles$totalscore, groups = dat_cleanquantiles$quantile5p, treatment = dat_cleanquantiles$tracking, ts_function = testStatistic_diffmeans, nsims = 1000)

ggplot(dat_cleanquantiles, aes(factor(quantile5p), totalscore, fill = as.factor(tracking))) + geom_boxplot( ) + labs(x = "5% Quantile", y= "Score", title = "18 month follow-up scores") + guides(fill = guide_legend(title = "Tracking"))

xtable(res2[c(1,4),], caption = "Difference in mean follow-up exam score between tracking and non-tracking students, by 5% quantile and overall.")
