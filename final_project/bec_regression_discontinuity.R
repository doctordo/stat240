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
score_med <- dat %>% group_by(schoolid) %>% summarize(med = median(std_mark, na.rm = TRUE))
# identify median score in each school
dat2 <- inner_join(dat, score_med, by = "schoolid")
dim(dat2)
# remove students who were put into the wrong stream
dat3 <- dat2 %>% filter((std_mark < med & lowstream == 1) | (std_mark > med & lowstream == 0) | (tracking == 0))
dat3 <- dat3 %>% mutate(std_mark2 = std_mark - med)
# remove students without one of the two scores
dat3 <- dat3 %>% filter(!is.na(std_mark) & !is.na(totalscore))


tracking.schools <- dat3 %>% filter(tracking == 1)

# keep in mind that the assignment into stream was done WITHIN schools:
# but standardization of baseline/initial marks was done over ALL schools
ggplot(filter(tracking.schools, schoolid %in% sample(unique(tracking.schools$schoolid),4))) + 
  geom_boxplot(aes(x = factor(bottomhalf), y = std_mark)) + 
  facet_wrap(~schoolid, ncol = 2) 



# how many students in each school (~60):
dat %>% group_by(schoolid) %>% summarize(n = n())
# how many students are within the (40,60) percentile interval in each school -- try this window
n.window <- filter(tracking.schools, percentile < 65 & percentile > 35) %>% 
  group_by(schoolid) %>% 
  summarize(n = n())
n.window

# restrict to small window of "equivalent students"
dat.window <- tracking.schools %>% filter(percentile < 65 & percentile > 35)


# by school:
ggplot(filter(dat.window, schoolid %in% sample(unique(dat.window$schoolid), 6))) +
  geom_point(aes(x = percentile, y = totalscore, col = factor(lowstream))) +
  stat_smooth(aes(x = percentile, y = totalscore, col = factor(lowstream)), method = "loess", se = FALSE) +
  facet_wrap(~schoolid, ncol = 3)

# overall:
ggplot(dat.window) +
  geom_point(aes(x = std_mark2, y = totalscore, col = factor(lowstream))) +
  stat_smooth(aes(x = std_mark2, y = totalscore, col = factor(lowstream)), method = "loess", se = FALSE) 
# there is a slight difference!




# let's identify schools for which there is a big difference:
stream.med <- dat.window %>% group_by(schoolid, lowstream) %>% summarize(medstream = median(totalscore))
stream.med.diff <- stream.med %>% group_by(schoolid) %>% summarize(streamdiff = medstream[1] - medstream[2])
most.diff.schools <- stream.med.diff %>% filter(streamdiff > 10)
most.diff.schools$schoolid
# plot the regression for the classes of interest
ggplot(filter(dat.window, schoolid %in% most.diff.schools$schoolid)) +
  geom_point(aes(x = std_mark2, y = totalscore, col = factor(lowstream))) +
  stat_smooth(aes(x = std_mark2, y = totalscore, col = factor(lowstream)), method = "loess", se = FALSE) +
  facet_wrap(~schoolid, ncol = 3)
# make each point the mean of a class, rather than an individual student
dat.window.join <- inner_join(dat.window, stream.med, by = c("schoolid", "lowstream"))
ggplot(dat.window.join ) +
  geom_point(aes(x = std_mark2, y = medstream, col = factor(lowstream))) +
  stat_smooth(aes(x = std_mark2, y = medstream, col = factor(lowstream)), method = "loess", se = FALSE) 
