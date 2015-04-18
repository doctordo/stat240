rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)



######################################### Data cleanup ###########################################

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





##################################### Regression discontinuity ##################################

# GENERATE MANUAL PERCENTILE BASED ON STD_MARK
### percentile does not match up with std_mark!!!
dat %>% group_by(schoolid) %>% mutate(percentile.new = sum(std.mark <= std.mark)

tracking.schools <- dat %>% filter(tracking == 1)

# keep in mind that the assignment into stream was done WITHIN schools:
# but standardization of baseline/initial marks was done over ALL schools
ggplot(filter(tracking.schools, schoolid %in% sample(unique(tracking.schools$schoolid),4))) + 
  geom_boxplot(aes(x = factor(bottomhalf), y = std_mark)) + 
  facet_wrap(~schoolid, ncol = 2) 



# how many students in each school (~60):
dat %>% group_by(schoolid) %>% summarize(n = n())
# how many students are within the (40,60) percentile interval in each school -- try this window
filter(tracking.schools, percentile < 60 & percentile > 40) %>% group_by(schoolid) %>% summarize(n = n())

# restrict to small window of "equivalent students"
dat.window <- tracking.schools %>% filter(percentile < 65 & percentile > 35)

# remove students who were put into the wrong stream
dat.window <- dat.window %>% filter(!is.na(totalscore) & !is.na(std_mark))
dat.window <- dat.window %>% mutate(right.stream = ((percentile < 50 && lowstream == 1) | (percentile >= 50 && lowstream == 0)))


# by school:
ggplot(filter(dat.window, schoolid %in% sample(unique(dat.window$schoolid), 6))) +
  geom_point(aes(x = percentile, y = totalscore, col = factor(lowstream))) +
  stat_smooth(aes(x = percentile, y = totalscore, col = factor(lowstream)), se = FALSE) +
  facet_wrap(~schoolid, ncol = 3)

# overall:
ggplot(dat.window) +
  geom_point(aes(x = percentile, y = totalscore, col = factor(lowstream))) +
  stat_smooth(aes(x = percentile, y = totalscore, col = factor(lowstream)), se = FALSE) 
# there are some observations which have been put into the upper stream but are significantly below the 50th percentile... 



