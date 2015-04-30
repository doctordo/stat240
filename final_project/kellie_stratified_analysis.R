
rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(xtable)
setwd("~/Documents/stat240/final_project/")
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



# get "real percentiles" based on baseline score. We don't want to use ones that are imputed based on FINAL score as that's what we're trying to compare with!
school.quantiles.std.mark <- dat %>% group_by(schoolid) %>% summarize(bottomq = quantile(std_mark,0.25 ,na.rm=T), medq = quantile(std_mark,0.5 ,na.rm=T), upperq  = quantile(std_mark,0.75 ,na.rm=T))
dat <- inner_join(school.quantiles.std.mark, dat, by = "schoolid")
dat <- dat %>% mutate(fake_quartile = quartile)
dat$quartile = sapply(1:nrow(dat), function(x) {
                                  if(is.na(dat$std_mark[x])){NA}
                                  else if(dat$std_mark[x] <= dat$bottomq[x]){
                                    quartiles[1]
                                  }else if(dat$std_mark[x] > dat$bottomq[x] & dat$std_mark[x] <= dat$medq[x]){
                                    quartiles[2]
                                  }else if(dat$std_mark[x] > dat$medq[x] & dat$std_mark[x] <= dat$upperq[x]){
                                    quartiles[3]
                                  }else{
                                    quartiles[4]
                                  }})

### Number of kids I'll remove
with(dat, table(is.na(std_mark), tracking))
# tracking
#        0    1
# FALSE 2653 3611
# TRUE   756    2
# I will remove 758 kids from subsequent analyses, most of whom are in the non-tracked schools.
with(dat, table(is.na(std_mark), fake_quartile))
# bottomquarter secondquarter thirdquarter topquarter
# FALSE          1483          1580         1569       1632
# TRUE             41            42           42        633
# Most of the kids with missing baseline scores are marked as "top quartile" - why is that??

# How bad is the misclassification?
with(dat, table(quartile, fake_quartile))

# Are baseline scores missing at random?
with(dat, table(is.na(std_mark), schoolid))


######################################### Data analysis #########################################
#################################################################################################


#### Question 1a: does tracking affect follow-up exam score, stratifying by baseline quartile? Look overall and w/in strata. Use difference in means as test statistic.
dat_complete <- dat %>% filter(!is.na(quartile))
res1 <- stratified_permute_means(values = dat_complete$totalscore, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_diffmeans, nsims = 10000)
res1t <- stratified_permute_means(values = dat_complete$totalscore, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_t, nsims = 10000)

p1 <- filter(dat_complete, !is.na(tracking)) %>% select(bottomquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_bar(alpha = 0.6, position = "identity") + labs(x = "Total Score", y = "Density", title = "Bottom quarter")+ guides(fill = guide_legend(title = "Tracking"))
p2 <- filter(dat_complete, !is.na(tracking)) %>% select(secondquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_bar(alpha = 0.6, position = "identity") + labs(x = "Total Score", y = "Density", title = "Second quarter")+ guides(fill = guide_legend(title = "Tracking"))
p3 <- filter(dat_complete, !is.na(tracking)) %>% select(thirdquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_bar(alpha = 0.6, position = "identity")  + labs(x = "Total Score", y = "Density", title = "Third quarter")+ guides(fill = guide_legend(title = "Tracking"))
p4 <- filter(dat_complete, !is.na(tracking)) %>% select(topquarter == 1) %>% ggplot(aes(totalscore, fill = as.factor(tracking))) + geom_bar(alpha = 0.6, position = "identity")  + labs(x = "Total Score", y = "Density", title = "Upper quarter") + guides(fill = guide_legend(title = "Tracking"))
plist <- list(p1, p2, p3, p4)
do.call(grid.arrange, plist)
filter(dat_complete, !is.na(tracking)) %>% ggplot(aes(factor(quartile), totalscore, fill = as.factor(tracking))) + geom_boxplot( ) + labs(x = "Quartile", y= "Score", title = "18 month follow-up scores") + guides(fill = guide_legend(title = "Tracking"))


mat1 <- rbind(res1[1,], res1t[c(1,4),])
rownames(mat1) <- c("Difference in means", "t", "P-value")
colnames(mat1) <- c("Bottom quarter", "Second quarter", "Third quarter", "Top quarter", "Overall")
xtable(mat1, digits = 3, caption = "Test for differences in final score, stratified by baseline quartile and overall.")


#### Question 1b: does tracking affect different dimensions of the follow-up exam score, stratifying by baseline quartile? Look overall and w/in strata. Use difference in means as test statistic.

res1t_wordscore <- stratified_permute_means(values = dat_complete$wordscore, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_t, nsims = 10000)
res1t_sentscore <- stratified_permute_means(values = dat_complete$sentscore, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_t, nsims = 10000)
res1t_letterscore <- stratified_permute_means(values = dat_complete$letterscore, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_t, nsims = 10000)
res1t_spellscore <- stratified_permute_means(values = dat_complete$spellscore, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_t, nsims = 10000)
res1t_litscore <- stratified_permute_means(values = dat_complete$litscore, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_t, nsims = 10000)
res1t_mathscoreraw <- stratified_permute_means(values = dat_complete$mathscoreraw, groups = dat_complete$quartile, treatment = dat_complete$tracking, ts_function = testStatistic_t, nsims = 10000)

mat1_comp <- rbind(res1t_wordscore[c(1,4),], res1t_sentscore[c(1,4),], res1t_letterscore[c(1,4),], res1t_spellscore[c(1,4),],res1t_litscore[c(1,4),], res1t_mathscoreraw[c(1,4),])
rownames(mat1_comp) <- c("Word Score t", "P-value","Sentence Score t", "P-value","Letter Score t", "P-value","Spelling Score t", "P-value","Literacy Score t", "P-value","Math Score t", "P-value")
colnames(mat1_comp) <- c("Bottom quarter", "Second quarter", "Third quarter", "Top quarter", "Overall")
xtable(mat1_comp, digits = 3, caption = "Test for differences in subject-level final score, stratified by baseline quartile and overall.")





#### Question 2: does tracking affect follow-up exam score, stratifying by baseline 5% quantiles? Look overall and w/in strata
dat_cleanquantiles <- filter(dat, !is.na(quantile5p) & !is.na(totalscore))
res2 <- stratified_permute_means(values = dat_cleanquantiles$totalscore, groups = dat_cleanquantiles$quantile5p, treatment = dat_cleanquantiles$tracking, ts_function = testStatistic_diffmeans, nsims = 10000)
res2t <- stratified_permute_means(values = dat_cleanquantiles$totalscore, groups = dat_cleanquantiles$quantile5p, treatment = dat_cleanquantiles$tracking, ts_function = testStatistic_t, nsims = 10000)


ggplot(dat_cleanquantiles, aes(factor(quantile5p), totalscore, fill = as.factor(tracking))) + geom_boxplot( ) + labs(x = "5% Quantile", y= "Score", title = "18 month follow-up scores") + guides(fill = guide_legend(title = "Tracking"))

mat2 <- rbind(res2[1,], res2t[c(1,4),])
rownames(mat2) <- c("Difference in means", "t", "P-value")
xtable(mat2, caption = "Difference in mean follow-up exam score between tracking and non-tracking students, by 5% quantile and overall.")


#### Question 3: does tracking affect follow-up exam score, stratifying by teacher type (ETP vs civil servant)? Look overall and w/in strata
res3 <- stratified_permute_means(values = dat$totalscore, groups = dat$etp, treatment = dat$tracking, ts_function = testStatistic_diffmeans, nsims = 10000)
res3t <- stratified_permute_means(values = dat$totalscore, groups = dat$etp, treatment = dat$tracking, ts_function = testStatistic_t, nsims = 10000)


filter(dat, !is.na(etpteacher) & !is.na(totalscore)) %>% ggplot(aes(as.factor(etpteacher), totalscore, fill = as.factor(tracking))) + geom_boxplot( ) + labs(x = "ETP Teacher", y= "Score", title = "18 month follow-up scores") + guides(fill = guide_legend(title = "Tracking"))

mat3 <- rbind(res3[1,], res3t[c(1,4),])
rownames(mat3) <- c("Difference in means", "t", "P-value")
colnames(mat3) <- c("Civil Servant", "ETP", "Overall")
xtable(mat3, caption = "Difference in mean follow-up exam score between tracking and non-tracking students, by teacher type.")


#### Question 4: does teacher type (ETP or civil servant) affect follow-up exam score, stratifying by quartile? Look overall and w/in strata
res4 <- stratified_permute_means(values = dat$totalscore, groups = dat$quartile, treatment = dat$etpteacher, ts_function = testStatistic_diffmeans, nsims = 10000)
res4t <- stratified_permute_means(values = dat$totalscore, groups = dat$quartile, treatment = dat$etpteacher, ts_function = testStatistic_t, nsims = 10000)


filter(dat, !is.na(etpteacher) & !is.na(totalscore)) %>% ggplot(aes(as.factor(quartile), totalscore, fill = as.factor(etpteacher))) + geom_boxplot( ) + labs(x = "Baseline Quartile", y= "Score", title = "18 month follow-up scores") + guides(fill = guide_legend(title = "ETP Teacher"))

mat4 <- rbind(res4[1,], res4t[c(1,4),])
rownames(mat4) <- c("Difference in means", "t", "P-value")
colnames(mat4) <- c("Bottom quarter", "Second quarter", "Third quarter", "Top quarter", "Overall")
xtable(mat4, caption = "Difference in mean follow-up exam score between students with ETP vs civil servant teachers, by quartile.")


#### Question 5: is there an interaction between ETP and tracking?
res5_etp <- stratified_permute_means(values = dat_complete$totalscore[dat_complete$etpteacher == 1], groups = dat_complete$quartile[dat_complete$etpteacher == 1], treatment = dat_complete$tracking[dat_complete$etpteacher == 1], ts_function = testStatistic_diffmeans, nsims = 10000)
res5t_etp <- stratified_permute_means(values = dat_complete$totalscore[dat_complete$etpteacher == 1], groups = dat_complete$quartile[dat_complete$etpteacher == 1], treatment = dat_complete$tracking[dat_complete$etpteacher == 1], ts_function = testStatistic_t, nsims = 10000)
res5_noetp <- stratified_permute_means(values = dat_complete$totalscore[dat_complete$etpteacher == 0], groups = dat_complete$quartile[dat_complete$etpteacher == 0], treatment = dat_complete$tracking[dat_complete$etpteacher == 0], ts_function = testStatistic_diffmeans, nsims = 10000)
res5t_noetp <- stratified_permute_means(values = dat_complete$totalscore[dat_complete$etpteacher == 0], groups = dat_complete$quartile[dat_complete$etpteacher == 0], treatment = dat_complete$tracking[dat_complete$etpteacher == 0], ts_function = testStatistic_t, nsims = 10000)




p15 <- filter(dat_complete, etpteacher==1) %>% ggplot(aes(as.factor(quartile), totalscore, fill = as.factor(tracking))) + geom_boxplot( ) + labs(x = "Quartile", y= "Score", title = "18 month follow-up scores, ETP Schools") + guides(fill = guide_legend(title = "Tracking"))
p25 <- filter(dat_complete, etpteacher==0) %>% ggplot(aes(as.factor(quartile), totalscore, fill = as.factor(tracking))) + geom_boxplot( ) + labs(x = "Quartile", y= "Score", title = "18 month follow-up scores, Non-ETP Schools") + guides(fill = guide_legend(title = "Tracking"))
plist5 <- list(p15, p25)
do.call(grid.arrange, plist5)


mutate(dat_complete, etp = ifelse(etpteacher == 1, "ETP", "Non-ETP"), tracking2 = ifelse(tracking == 1, "Tracking", "Non-tracking")) %>% ggplot(aes(quartile, totalscore, linetype = etp, fill = tracking2)) + geom_boxplot() + labs(x = "Baseline Quartile", y= "Score", title = "18 month follow-up scores") + guides(linetype = guide_legend(title = "Teacher"),fill = guide_legend(title = "Tracking"))

mat5 <- rbind(res5_etp[1,], res5t_etp[c(1,4),], res5_noetp[1,], res5t_noetp[c(1,4),])
rownames(mat5) <- c("ETP: Difference in means", "t", "P-value", "Non-ETP: Difference in means", "t", "P-value")
colnames(mat5) <- c("Bottom quarter", "Second quarter", "Third quarter", "Top quarter", "Overall")
xtable(mat5, digits = 3, caption = "Difference in mean follow-up exam score between students in tracking vs non-tracking schools, stratified by ETP vs civil servant teachers.")



