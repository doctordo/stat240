rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(binr)


#################### Permutation test functions ####################
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

dat <- read.table("student_test_data.tab.tsv", sep = "\t", header = T)

dat <- dat %>%
  select(pupilid,
         schoolid,
         district,
         division,
         zone,
         
         tracking,
         sbm, # School-based Management
         gender,
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

# convert binary variables to factors
dat$gender <-dat$girl
dat$gender <- factor(dat$gender)
levels(dat$gender) <- c("male", "female")
dat$sbm <- factor(dat$sbm)
levels(dat$sbm) <- c("not monitored", "monitored")


################### functions to identify bins for various binwidths #####################
mfloor <- function(x,base){ 
  base*floor(x/base) 
} 

mceiling <- function(x,base){ 
  base*ceiling(x/base) 
} 

# find percentiles of x 
Percentile <- function(x)  sapply(x, function(y) sum(x <= y)/length(x)*100)



dat <- dat %>% group_by(schoolid) %>% mutate(initial_percentile = Percentile(std_mark))
##################################### Preparing datasets ##################################
# remove extraneous variables for now
#dat <- dat %>% select(pupilid, schoolid, tracking, sbm, gender, etpteacher, lowstream, std_mark, percentile, totalscore, r2_totalscore)
# remove students who do not have all of initial, final and follow-up test scores recorded
dat2 <- dat %>% filter(!is.na(totalscore) & !is.na(std_mark) & !is.na(r2_totalscore))
# restrict to tracking schools:
dat.tracking <- dat2 %>% filter(tracking == 1)


# # standardize initial scores within schools so that they're comparable
school.med.std.mark <- dat.tracking %>% group_by(schoolid) %>% summarize(school_med_std_mark = median(std_mark))
dat.tracking <- inner_join(school.med.std.mark, dat.tracking, by = "schoolid")
dat.tracking <- dat.tracking %>% mutate(std_std_mark = std_mark - school_med_std_mark)


# standardize final scores within schools so that they're comparable
school.med.totalscore <- dat.tracking %>% group_by(schoolid) %>% summarize(school_med_totalscore = median(totalscore))
dat.tracking <- inner_join(school.med.totalscore, dat.tracking, by = "schoolid")
dat.tracking <- dat.tracking %>% mutate(std_totalscore = totalscore - school_med_totalscore)


dat.tracking <- dat.tracking %>% group_by(schoolid) %>% mutate(initial_percentile = Percentile(std_mark))
dat.tracking.med.percentile <- dat.tracking %>% group_by(schoolid) %>% summarize(median_initial_percentile = median(initial_percentile))
dat.tracking <- inner_join(dat.tracking.med.percentile, dat.tracking, by = "schoolid")

# standardize followup scores within schools so that the median is 0 for all schools
school.med.r2.totalscore <- dat.tracking %>% group_by(schoolid) %>% summarize(school_med_r2_totalscore = median(r2_totalscore))
dat.tracking <- inner_join(school.med.r2.totalscore, dat.tracking, by = "schoolid")
dat.tracking <- dat.tracking %>% mutate(std_r2_totalscore = r2_totalscore - school_med_r2_totalscore)




# check to make sure those below the median were put into the lower stream and vice versa
dat.tracking <- dat.tracking %>% mutate(right_stream = ((initial_percentile < median_initial_percentile) & (lowstream == 1)) | ((initial_percentile >= median_initial_percentile) & (lowstream == 0)))
#dat.tracking2



# calcualte proportion of students in each school who have been put into the wrong stream
n.correct.stream <- dat.tracking %>% group_by(schoolid) %>% summarize(prop = sum(right_stream)/length(right_stream))
# the lowest amount of correct assignment in a school is 80% -- not too bad
min(n.correct.stream$prop)
# the total proportion of crossover is 5.3%
nobs <- nrow(dat.tracking)
dat.tracking %>% summarize(prop = sum(!(right_stream))/nobs)




n.correct.stream.zone <- dat.tracking %>% group_by(zone) %>% summarize(prop = sum(right_stream)/length(right_stream))


# how many schools in each zone
dat %>% group_by(zone) %>% summarize(schools = length(unique(schoolid)))




################################### Probability of being assigned to treatment plot ######################

# identify prop of a student crossing into the lowstream
# p.cross.to.low <- dat.tracking %>% filter(!(right_stream) & (lowstream == 1)) %>% summarize(p_cross_to_low = n()/nobs)
# p.cross.to.low 
# # identify prop of a student crossing into the highstream
# p.cross.to.high <- dat.tracking %>% filter(!(right_stream) & (lowstream == 0)) %>% summarize(p_cross_to_high = n()/nobs)
# p.cross.to.high
# # put into a data frame
# p.cross <- data.frame(cross_to_high = p.cross.to.high$p_cross_to_high, cross_to_low = p.cross.to.low$p_cross_to_low)
# 

# df <- data.frame(x = c(0,1), y = c(p.cross$cross_to_high, 1 - p.cross$cross_to_low)) 
# ggplot(df) + 
#   geom_bar(aes(x = x, y = y, fill = factor(x)), width=1, position=position_dodge(width=0), stat="identity") + 
#   scale_x_continuous(name = "", breaks = 0.5, labels = "initial grade cutpoint") +
#   scale_y_continuous(name = "Probability of assignment to the high stream") + 
#   geom_hline(yintercept = 1) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = -0.5) +
#   geom_vline(xintercept = 1.5)


# arrange by initial percentile
dat.tracking.arrange <- dat.tracking
dat.tracking.arrange <- ungroup(dat.tracking) %>% arrange(initial_percentile)
# cut into 5 percentile intervals
dat.tracking.arrange <- dat.tracking.arrange %>% mutate(percentile_intervals = cut2(initial_percentile, cuts = seq(0,100, 5))) 
# identify proportion of upperstream students in each percentile
prob_highstream <- dat.tracking.arrange %>% group_by(percentile_intervals) %>% dplyr::summarize(p_high = (sum(lowstream == 0)/n()))
# generate a factor for which hald of the initial distribution the student is in
prob_highstream$half <- factor(c(rep("Lower",10), rep("Upper", 10)))

# plot prob of being assigned to upperstream versus percentile
ggplot(prob_highstream) + 
  geom_bar(aes(x = percentile_intervals, y = p_high, fill = half), position = "dodge", stat = "identity") +
  scale_x_discrete(name = "Percentile", labels = (seq(0, 95, 5) + 2.5)) +
  scale_y_continuous(name = "Probability of assignment to upperstream") + 
  scale_fill_discrete(name = "Half of initial\ndistribution") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size = 16),
        legend.title=element_text(size = 18, face = "bold"))



########################## Inconsistencies of percentiles, 5percentiles and std_mark ##############################

ggplot(dat) + geom_point(aes(x = initial_percentile, y = percentile), alpha = 0.5) + 
  scale_x_continuous(name = "Our Percentile") +
  scale_y_continuous(name = "Their Percentile") +
  geom_smooth(aes(x = initial_percentile, y = percentile), method = "lm")
# ggplot(dat) + geom_point(aes(x = std_mark, y = realpercentile))
# 
# ggplot(dat) + 
#   geom_boxplot(aes(x = factor(quartile), y = percentile, fill = factor(quartile))) +
#   scale_x_discrete(name = "quartile", labels = c("bottom quarter", "second quarter", "third quarter", "top quarter")) +
#   scale_y_continuous(name = "Baseline score")
# 
# 
# 
# ggplot(filter(dat, !is.na(tophalf))) + 
#   geom_boxplot(aes(x = factor(tophalf), y = percentile, fill = factor(tophalf))) +
#   scale_x_discrete(name = "quartile", labels = c("bottom half", "top half")) +
#   scale_y_continuous(name = "Baseline score")
# 
# 
# 
# # ######################## We chose to go with std_mark 
# gg.mark <- ggplot(filter(dat, tracking == 1)) + 
#   geom_boxplot(aes(x = factor(lowstream), y = std_mark, fill = factor(lowstream))) +
#   scale_x_discrete(name = "stream", labels = c("low stream", "high stream")) +
#   scale_y_continuous(name = "Standardized baseline score") + 
#   theme(legend.position="none") +
#   geom_hline(aes(yintercept = 0), col = "red") +
#   ggtitle("Standardized baseline score")
# 
# 
# 
# gg.percentile <- ggplot(filter(dat, tracking == 1)) + 
#   geom_boxplot(aes(x = factor(lowstream), y = percentile, fill = factor(lowstream))) +
#   scale_x_discrete(name = "stream", labels = c("low stream", "high stream")) +
#   scale_y_continuous(name = "Baseline percentile") +
#   theme(legend.position="none") +
#   geom_hline(aes(yintercept = 50), col = "red") +
#   ggtitle("Baseline Percentile")
# 
# 
# grid.arrange(gg.mark, gg.percentile, ncol = 2)




################################# Aggregated scatterplot ###########################


dat.tracking <- dat.tracking %>% 
  mutate(interval = cut2(percentile, cuts = seq(0,100,10)))

aggregate.points <- dat.tracking %>% 
  group_by(interval) %>% 
  mutate(med.totalscore = median(totalscore), med.percentile = median(percentile), n = n())



ggplot(aggregate.points) + 
  geom_point(aes(x = med.percentile, y = med.totalscore, col = factor(lowstream), size = n)) +
  geom_vline(aes(xintercept = 50), linetype = "dotted") +
  scale_x_continuous(name = "Baseline percentile") + 
  scale_y_continuous(name = "18 Month endline score") + 
  scale_color_discrete(name = "Stream", labels = c("High","Low")) + 
  scale_size_continuous(name = "Number of\nstudents")


######################### Regression discontinuity for type II fuzzy (double crossover) designs ########################


library(rdd)
rd  <- RDestimate(totalscore ~ std_mark + lowstream | girl + agetest, data = dat.tracking, bw = seq(0.1, 0.5, 0.1))
summary(rd) # estimates is the wald estimator (Lee and Lemieux 2010) -- discussion of parametric and non parametric approach


# regression discontinuity where loess is ignoring crossover points -- 18 month endpoint
ggplot(filter(dat.tracking, (percentile < 55) & (percentile > 45))) + 
  geom_point(aes(x = percentile, y= totalscore, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= totalscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 55) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= totalscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 45))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low"))





# regression discontinuity where loess is ignoring crossover points -- 18 month endpoint
ggplot(filter(dat.tracking, (percentile < 60) & (percentile > 40))) + 
  geom_point(aes(x = percentile, y= totalscore, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= totalscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 60) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= totalscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 40))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low")) + 
  facet_wrap(~zone, ncol = 3)




# regression discontinuity where loess is ignoring crossover points -- 18 month math endpoint
rd_math <- ggplot(filter(dat.tracking, (percentile < 60) & (percentile > 40))) + 
  geom_point(aes(x = percentile, y= mathscoreraw, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= mathscoreraw, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 60) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= mathscoreraw, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 40))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline math score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low")) +
  theme(legend.position="none")





# regression discontinuity where loess is ignoring crossover points -- 18 month literacy endpoint
rd_lit <- ggplot(filter(dat.tracking, (percentile < 60) & (percentile > 40))) + 
  geom_point(aes(x = percentile, y= litscore, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= litscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 60) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= litscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 40))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline literacy score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low")) +
  theme(legend.position="none")




# regression discontinuity where loess is ignoring crossover points -- 18 month spelling endpoint
rd_spell <- ggplot(filter(dat.tracking, (percentile < 60) & (percentile > 40))) + 
  geom_point(aes(x = percentile, y= spellscore, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= spellscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 60) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= spellscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 40))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline spelling score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low")) +
  theme(legend.position="none")




# regression discontinuity where loess is ignoring crossover points -- 18 month letters endpoint
rd_letters <- ggplot(filter(dat.tracking, (percentile < 60) & (percentile > 40))) + 
  geom_point(aes(x = percentile, y= letterscore, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= letterscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 60) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= letterscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 40))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline letters score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low")) +
  theme(legend.position="none")



# regression discontinuity where loess is ignoring crossover points -- 18 month words endpoint
rd_words <- ggplot(filter(dat.tracking, (percentile < 60) & (percentile > 40))) + 
  geom_point(aes(x = percentile, y= wordscore, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= wordscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 60) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= wordscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 40))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline words score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low")) +
  theme(legend.position="none")


# regression discontinuity where loess is ignoring crossover points -- 18 month words endpoint
rd_sent <- ggplot(filter(dat.tracking, (percentile < 60) & (percentile > 40))) + 
  geom_point(aes(x = percentile, y= sentscore, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= sentscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 0) & (percentile < 60) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= sentscore, col = factor(lowstream)), data = filter(dat.tracking, ((lowstream == 1) & (percentile <= 50) & (percentile > 40))), method = "loess", size = 1, se = T) +
  scale_x_continuous(name = "Baseline percentile") +
  scale_y_continuous(name = "18 Month endline sentence score") +
  scale_color_discrete(name="Stream", labels=c("High", "Low")) + 
  theme(legend.position="none")

dat.tracking.words <- dat.tracking %>% 
  filter((percentile < 60) & (percentile > 40)) %>% 
  select(percentile, wordscore, lowstream) %>% 
  mutate(score = wordscore/max(wordscore, na.rm = TRUE)*10, subject = "Words") %>%
  select(percentile, score, lowstream, subject)
dat.tracking.sent <- dat.tracking %>% 
  filter((percentile < 60) & (percentile > 40)) %>% 
  select(percentile, sentscore, lowstream) %>% 
  mutate(score = sentscore/max(sentscore, na.rm = TRUE)*10, subject = "Sentences") %>%
  select(percentile, score, lowstream, subject)
dat.tracking.letters <- dat.tracking %>% 
  filter((percentile < 60) & (percentile > 40)) %>% 
  select(percentile, letterscore, lowstream) %>% 
  mutate(score = letterscore/max(letterscore, na.rm = TRUE)*10, subject = "Letters") %>%
  select(percentile, score, lowstream, subject)
dat.tracking.spell <- dat.tracking %>% 
  filter((percentile < 60) & (percentile > 40)) %>% 
  select(percentile, spellscore, lowstream) %>% 
  mutate(score = spellscore/max(spellscore, na.rm = TRUE)*10, subject = "Spelling") %>%
  select(percentile, score, lowstream, subject)
dat.tracking.lit <- dat.tracking %>% 
  filter((percentile < 60) & (percentile > 40)) %>% 
  select(percentile, litscore, lowstream) %>% 
  mutate(score = litscore/max(litscore, na.rm = TRUE)*10, subject = "Literacy") %>%
  select(percentile, score, lowstream, subject)
dat.tracking.math <- dat.tracking %>% 
  filter((percentile < 60) & (percentile > 40)) %>% 
  select(percentile, mathscoreraw, lowstream) %>% 
  mutate(score = mathscoreraw/max(mathscoreraw, na.rm = TRUE)*10, subject = "Math") %>%
  select(percentile, score, lowstream, subject)

dat.tracking.subject <- rbind(dat.tracking.words, dat.tracking.sent, dat.tracking.letters, dat.tracking.spell, dat.tracking.lit, dat.tracking.math)

ggplot(dat.tracking.subject) + 
  geom_point(aes(x = percentile, y = score, col = factor(lowstream))) + 
  stat_smooth(aes(x = percentile, y= score, col = factor(lowstream)), data = filter(dat.tracking.subject, ((lowstream == 0) & (percentile >= 50))), method = "loess", size = 1, se = T) +
  stat_smooth(aes(x = percentile, y= score, col = factor(lowstream)), data = filter(dat.tracking.subject, ((lowstream == 1) & (percentile < 50))), method = "loess", size = 1, se = T) +
  facet_wrap(~subject) + 
  theme(legend.position = "none") + 
  scale_y_continuous(name = "18 month endline score")
  






######################################### Difference in means in different windows ###############################

# the different window widths
window <- c(5, 10, 15, 20)
res1 <- list()
k <- 1
# compute difference in means for streams in 0.5 window
for(width in window) {
  # restrict data to window
  dat.window <- dat.tracking %>% filter((initial_percentile < 50 + width/2) & (initial_percentile > 50 - width/2))
  # conduct the permutation test when stratifying by zone 
  res1.window <- stratified_permute_means(values = dat.window$totalscore, groups = dat.window$zone, treatment = dat.window$lowstream, ts_function = testStatistic_diffmeans, nsims = 10000)
  res1[[k]] <- res1.window
  k <- k + 1
}

# combine two-sided p-values for each width into data frame
p.val <- sapply(res1, function(x) x["pboth",])
colnames(p.val) <- window
p.val <- melt(p.val)
names(p.val) <- c("Zone", "Width", "p")
p.val$Width <- factor(p.val$Width)

# plot p-values for each zone
ggplot(p.val) + 
  geom_bar(aes(x = Zone, y = p, fill = Width), stat = "identity", position = "dodge") +
  geom_hline(aes(yintercept = 0.05), linetype = "dotted", col = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  



