rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(binr)


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





##################################### Regression discontinuity for tracking schools ##################################
# remove extraneous variables for now
#dat <- dat %>% select(pupilid, schoolid, tracking, sbm, girl, etpteacher, lowstream, std_mark, percentile, totalscore, r2_totalscore)
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



# standardize followup scores within schools so that they're comparable
school.med.r2.totalscore <- dat.tracking %>% group_by(schoolid) %>% summarize(school_med_r2_totalscore = median(r2_totalscore))
dat.tracking <- inner_join(school.med.r2.totalscore, dat.tracking, by = "schoolid")
dat.tracking <- dat.tracking %>% mutate(std_r2_totalscore = r2_totalscore - school_med_r2_totalscore)




# check to make sure those below the median were put into the lower stream and vice versa
dat.tracking2 <- dat.tracking %>% mutate(right_stream = ((std_std_mark < 0) & (lowstream == 1)) | ((std_std_mark >= 0) & (lowstream == 0)))
#dat.tracking2



# calcualte proportion of students in each school who have been put into the wrong stream
n.correct.stream <- dat.tracking2 %>% group_by(schoolid) %>% summarize(prop = sum(right_stream)/length(right_stream))
# the lowest amount of correct assignment is 93% -- not too bad
min(n.correct.stream$prop)



# for now, remove studnets who are not in the correct stream
dat.tracking2 <- dat.tracking2 %>% filter(right_stream)


################### identify bins for various binwidths #####################
mfloor <- function(x,base){ 
  base*floor(x/base) 
} 

mceiling <- function(x,base){ 
  base*ceiling(x/base) 
} 


# Do a plot to identify visually if there is any kind of gap between those near the median
RdBinPlot <- function(width, test = "endline") {
    # identify bins for the lowstream: length of bins is specified by "width"
    bins.lowstream <- seq(mfloor(min(dat.tracking2$std_std_mark), width), 0, width)
    # identify bins for the high stream
    bins.highstream <- seq(0, mceiling(max(dat.tracking2$std_std_mark),width), width)
    # add a variable which contains the bins for the low stream
    datlow <- dat.tracking2 %>% filter(lowstream == 1) %>% mutate(bin = cut(std_std_mark, bins.lowstream, right = TRUE))
    # add a variable which contains the bins for the high stream
    dathigh <- dat.tracking2 %>% filter(lowstream == 0) %>% mutate(bin = cut(std_std_mark, bins.highstream, right = FALSE))
    # combine to create new data frame with bin variables
    dat.tracking4 <- rbind(datlow, dathigh)
    
    # how many students in each bin:
    n.in.bin <- dat.tracking4 %>% group_by(bin) %>% summarize(n_bin = n())
    # add the number of students in each bin into the data frame (will be used for size)
    dat.tracking4 <- inner_join(dat.tracking4, n.in.bin, by = "bin")
    
    
    # summarize each bin by mean std_score in bin
    dat.tracking5 <- dat.tracking4 %>% select(pupilid, schoolid, lowstream, tracking, std_std_mark, std_mark, totalscore, r2_totalscore, std_totalscore, std_r2_totalscore, bin, n_bin)
    bin.std.mark <- dat.tracking5 %>% group_by(bin, lowstream) %>% summarize(bin_std_mark = mean(std_std_mark))
    dat.tracking5 <- inner_join(dat.tracking5, bin.std.mark, by = c("bin", "lowstream"))
    
    
    # take the mean of each totalscore in each bin
    if(test == "endline") {
      bin.totalscore <- dat.tracking5 %>% group_by(bin, lowstream) %>% summarize(bin_totalscore = mean(std_totalscore))
      dat.tracking5 <- inner_join(dat.tracking5, bin.totalscore, by = c("bin", "lowstream"))
    } else if (test == "follow-up") {
      bin.totalscore <- dat.tracking5 %>% group_by(bin, lowstream) %>% summarize(bin_totalscore = mean(std_r2_totalscore))
      dat.tracking5 <- inner_join(dat.tracking5, bin.totalscore, by = c("bin", "lowstream"))
    }
    
    
    # do the plot by considering individual students
    gg.bin.student <- ggplot(dat.tracking5) + 
      geom_point(aes(x = bin_std_mark, y = bin_totalscore, size = n_bin, col = factor(lowstream))) +
      scale_x_continuous(name = "mean initial score") +
      scale_y_continuous(name = paste("mean", test,"score")) +
      ggtitle(paste("Binwidth =", width)) +
      scale_size_continuous(name = "no. of students") +
      scale_colour_discrete(name = "low stream")
    

    return(list(dat = dat.tracking5, plot.student = gg.bin.student))
}

width <- 0.25
bin.select.endline <- RdBinPlot(width, "endline") 
bin.select.endline$plot # again, no big jump

bin.select.followup <- RdBinPlot(width, "follow-up")
bin.select.followup$plot.student # doesn't look like there's a big jump, but there may be some disadvantage seen by those in the higher stream



####################### Using a binwidth = 0.4 ####################

dat.tracking5 <- bin.select.followup$dat

dat.window <- dat.tracking5 %>% filter(((std_std_mark < 0.25) & (lowstream == 0)) | ((std_std_mark > -0.25) & (lowstream == 1)))


ggplot(dat.window, aes(x = std_std_mark, y = std_totalscore)) + 
  geom_point(aes(col = factor(lowstream))) +
  stat_smooth(aes(col = factor(lowstream)), method = "loess", se = FALSE)

ggplot(dat.window, aes(x = std_std_mark, y = std_r2_totalscore)) + 
  geom_point(aes(col = factor(lowstream))) +
  stat_smooth(aes(col = factor(lowstream)), method = "loess", se = FALSE)









##################################### Regression discontinuity for non-tracking schools ##################################
# remove extraneous variables for now
#dat <- dat %>% select(pupilid, schoolid, tracking, sbm, girl, etpteacher, lowstream, std_mark, percentile, totalscore, r2_totalscore)
# remove students who do not have all of initial, final and follow-up test scores recorded
dat2 <- dat %>% filter(!is.na(totalscore) & !is.na(std_mark) & !is.na(r2_totalscore))
# restrict to tracking schools:
dat.non.tracking <- dat2 %>% filter(tracking == 0)


# # standardize initial scores within schools so that they're comparable
school.med.std.mark <- dat.non.tracking %>% group_by(schoolid) %>% summarize(school_med_std_mark = median(std_mark))
dat.non.tracking <- inner_join(school.med.std.mark, dat.non.tracking, by = "schoolid")
dat.non.tracking <- dat.non.tracking %>% mutate(std_std_mark = std_mark - school_med_std_mark)


# standardize final scores within schools so that they're comparable
school.med.totalscore <- dat.non.tracking %>% group_by(schoolid) %>% summarize(school_med_totalscore = median(totalscore))
dat.non.tracking <- inner_join(school.med.totalscore, dat.non.tracking, by = "schoolid")
dat.non.tracking <- dat.non.tracking %>% mutate(std_totalscore = totalscore - school_med_totalscore)



# standardize followup scores within schools so that they're comparable
school.med.r2.totalscore <- dat.non.tracking %>% group_by(schoolid) %>% summarize(school_med_r2_totalscore = median(r2_totalscore))
dat.non.tracking <- inner_join(school.med.r2.totalscore, dat.non.tracking, by = "schoolid")
dat.non.tracking <- dat.non.tracking %>% mutate(std_r2_totalscore = r2_totalscore - school_med_r2_totalscore)


# add a "would have been in low stream variable"
dat.non.tracking <- dat.non.tracking %>% mutate(lowstream = as.numeric(std_std_mark < 0))



################### identify bins for various binwidths #####################
mfloor <- function(x,base){ 
  base*floor(x/base) 
} 

mceiling <- function(x,base){ 
  base*ceiling(x/base) 
} 


# Do a plot to identify visually if there is any kind of gap between those near the median
RdBinPlot_nontracking <- function(width, test = "endline") {
  # identify bins for the lowstream: length of bins is specified by "width"
  bins.lowstream <- seq(mfloor(min(dat.non.tracking$std_std_mark), width), 0, width)
  # identify bins for the high stream
  bins.highstream <- seq(0, mceiling(max(dat.non.tracking$std_std_mark),width), width)
  # add a variable which contains the bins for the low stream
  datlow <- dat.non.tracking %>% filter(std_std_mark < 0) %>% mutate(bin = cut(std_std_mark, bins.lowstream, right = TRUE))
  # add a variable which contains the bins for the high stream
  dathigh <- dat.non.tracking %>% filter(std_std_mark >= 0) %>% mutate(bin = cut(std_std_mark, bins.highstream, right = FALSE))
  # combine to create new data frame with bin variables
  dat.non.tracking2 <- rbind(datlow, dathigh)
  
  # how many students in each bin:
  n.in.bin <- dat.non.tracking2 %>% group_by(bin) %>% summarize(n_bin = n())
  # add the number of students in each bin into the data frame (will be used for size)
  dat.non.tracking2 <- inner_join(dat.non.tracking2, n.in.bin, by = "bin")
  
  
  # summarize each bin by mean std_score in bin
  dat.non.tracking5 <- dat.non.tracking2 %>% select(pupilid, schoolid, lowstream, tracking, std_std_mark, std_mark, totalscore, r2_totalscore, std_totalscore, std_r2_totalscore, bin, n_bin)
  bin.std.mark <- dat.non.tracking5 %>% group_by(bin, lowstream) %>% summarize(bin_std_mark = mean(std_std_mark))
  dat.non.tracking5 <- inner_join(dat.non.tracking5, bin.std.mark, by = c("bin", "lowstream"))
  
  
  # take the mean of each totalscore in each bin
  if(test == "endline") {
    bin.totalscore <- dat.non.tracking5 %>% group_by(bin, lowstream) %>% summarize(bin_totalscore = mean(std_totalscore))
    dat.non.tracking5 <- inner_join(dat.non.tracking5, bin.totalscore, by = c("bin", "lowstream"))
  } else if (test == "follow-up") {
    bin.totalscore <- dat.non.tracking5 %>% group_by(bin, lowstream) %>% summarize(bin_totalscore = mean(std_r2_totalscore))
    dat.non.tracking5 <- inner_join(dat.non.tracking5, bin.totalscore, by = c("bin", "lowstream"))
  }
  
  
  # do the plot by considering individual students
  gg.bin.student <- ggplot(dat.non.tracking5) + 
    geom_point(aes(x = bin_std_mark, y = bin_totalscore, size = n_bin, col = factor(lowstream))) +
    scale_x_continuous(name = "mean initial score") +
    scale_y_continuous(name = paste("mean", test,"score")) +
    ggtitle(paste("Binwidth =", width)) +
    scale_size_continuous(name = "no. of students") +
    scale_colour_discrete(name = "low stream")
  
  
  return(list(dat = dat.non.tracking5, plot.student = gg.bin.student))
}

bin.select.endline.nontracking <- RdBinPlot_nontracking(width, "endline") 
bin.select.endline.nontracking$plot # again, no big jump

bin.select.followup.nontracking <- RdBinPlot_nontracking(width, "follow-up")
bin.select.followup.nontracking$plot.student # doesn't look like there's a big jump, but there may be some disadvantage seen by those in the higher stream






######################## combine tracking and non-tracking schools on plot ########################
bin.dat <- rbind(bin.select.endline.nontracking$dat, bin.select.endline$dat)
bin.dat$tracking <- factor(bin.dat$tracking)
levels(bin.dat$tracking) <- c("non-tracking","tracking")
gg.bin.endline <- ggplot(bin.dat) + 
  geom_point(aes(x = bin_std_mark, y = bin_totalscore, size = n_bin, col = factor(lowstream))) +
  scale_x_continuous(name = "mean initial score") +
  scale_y_continuous(name = paste("mean", test,"score")) +
  ggtitle(paste("Binwidth =", width)) +
  scale_size_continuous(name = "no. of students") +
  scale_colour_discrete(name = "low stream") +
  facet_wrap(~tracking, ncol = 2)
gg.bin.endline



# 
# 
# # GENERATE MANUAL PERCENTILE BASED ON STD_MARK
# ### percentile does not match up with std_mark!!!
# score_med <- dat %>% group_by(schoolid) %>% summarize(med = median(std_mark, na.rm = TRUE))
# # identify median score in each school
# dat2 <- inner_join(dat, score_med, by = "schoolid")
# dim(dat2)
# # remove students who were put into the wrong stream
# dat3 <- dat2 %>% filter((std_mark < med & lowstream == 1) | (std_mark > med & lowstream == 0) | (tracking == 0))
# dat3 <- dat3 %>% mutate(std_mark2 = std_mark - med)
# # remove students without one of the two scores
# dat3 <- dat3 %>% filter(!is.na(std_mark) & !is.na(totalscore))
# 
# # scale the total score within each school so that different schools are comparable and we are simply comparing tracking versus non-tracking
# dat3 <- dat3 %>% group_by(schoolid) %>% 
#   mutate(std_totalscore = scale(totalscore, scale = FALSE))
# 
# 
# 
# tracking.schools <- dat3 %>% filter(tracking == 1)
# 
# # keep in mind that the assignment into stream was done WITHIN schools:
# # but standardization of baseline/initial marks was done over ALL schools
# ggplot(filter(tracking.schools, schoolid %in% sample(unique(tracking.schools$schoolid),4))) + 
#   geom_boxplot(aes(x = factor(bottomhalf), y = std_mark)) + 
#   facet_wrap(~schoolid, ncol = 2) 
# 
# 
# 
# # how many students in each school (~60):
# dat %>% group_by(schoolid) %>% summarize(n = n())
# # how many students are within the (40,60) percentile interval in each school -- try this window
# n.window <- filter(tracking.schools, percentile < 65 & percentile > 35) %>% 
#   group_by(schoolid) %>% 
#   summarize(n = n())
# n.window
# 
# # restrict to small window of "equivalent students"
# dat.window <- tracking.schools %>% filter( std_mark2 < 0.5 & std_mark2 > -0.5)
# 
# 
# # by school:
# ggplot(filter(dat.window, schoolid %in% sample(unique(dat.window$schoolid), 6))) +
#   geom_point(aes(x = std_mark2, y = std_totalscore, col = factor(lowstream))) +
#   stat_smooth(aes(x = std_mark2, y = std_totalscore, col = factor(lowstream)), method = "loess", se = FALSE) +
#   facet_wrap(~schoolid, ncol = 3)
# 
# # overall:
# ggplot(dat.window) +
#   geom_point(aes(x = std_mark2, y = std_totalscore, col = factor(lowstream))) +
#   stat_smooth(aes(x = std_mark2, y = std_totalscore, col = factor(lowstream)), method = "loess", se = FALSE) 
# # there is a slight difference!
# 
# 
# 
# 
# # let's identify schools for which there is a big difference:
# stream.med <- dat.window %>% group_by(schoolid, lowstream) %>% summarize(medstream = median(totalscore))
# stream.med.diff <- stream.med %>% group_by(schoolid) %>% summarize(streamdiff = medstream[1] - medstream[2])
# most.diff.schools <- stream.med.diff %>% filter(streamdiff > 10)
# most.diff.schools$schoolid
# # plot the regression for the classes of interest
# ggplot(filter(dat.window, schoolid %in% most.diff.schools$schoolid)) +
#   geom_point(aes(x = std_mark2, y = std_totalscore, col = factor(lowstream))) +
#   stat_smooth(aes(x = std_mark2, y = std_totalscore, col = factor(lowstream)), method = "loess", se = FALSE) +
#   facet_wrap(~schoolid, ncol = 3)
# # # make each point the mean of a class, rather than an individual student
# # dat.window.join <- inner_join(dat.window, stream.med, by = c("schoolid", "lowstream"))
# # ggplot(dat.window.join ) +
# #   geom_point(aes(x = std_mark2, y = medstream, col = factor(lowstream))) +
# #   stat_smooth(aes(x = std_mark2, y = medstream, col = factor(lowstream)), method = "loess", se = FALSE) 
# # 
# 
# 
# 
# 
# 
# 
# ################ Following steps outlined in Jaco 
# 
# 
