
rm(list=ls())
library("dplyr")
library("reshape2")
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

testStatistic <- function(x, treatment){
  mean(x[treatment ==  1], na.rm=T) - mean(x[treatment == 0], na.rm=T)
}

stratified_permute_means <- function(values, groups, treatment, nsims){
  # set up storage
  sims <- matrix(rep(NA, nsims*(length(unique(groups))+1)), ncol = length(unique(groups))+1)
  teststat <- c()
  
  # set up indexing for groups
  ng <- table(groups)
  group_names <- names(ng)
  ng <- as.vector(ng)
  ind <- lapply(unique(groups), function(x) groups == x)
  
  
  # Compute the test statistic for the given data
  for(g in seq_along(ind)){
    gg <- ind[[g]]
    teststat <- c(teststat, testStatistic(values[gg], treatment[gg]))
  }
  teststat <- c(teststat, sum(teststat*ng)/length(groups))
  
  # Compute test stats for permuted data
  for(i in 1:nsims){
    treatment <- permute_within_groups(treatment, groups)
    for(g in seq_along(ind)){
      gg <- ind[[g]]
      sims[i,g] <- testStatistic(values[gg], treatment[gg])
    }
  }
  sims[,5] <- apply(sims[,-5], 1, function(x) {sum(x*ng)/length(groups)})
  
  
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
stratified_permute_means(values = dat$totalscore, groups = dat$quartile, treatment = dat$tracking, nsims = 1000)
