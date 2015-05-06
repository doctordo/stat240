#------------------------------------------------------------------------------#
#
# @Libraries
#
#------------------------------------------------------------------------------#

library("dplyr")
library("reshape2")
library("ggplot2")
library("xtable")

#------------------------------------------------------------------------------#
#
# @Data
#
#------------------------------------------------------------------------------#

setwd("D:/coursework/stat240/final_project")

load("student_pres_data.Rdata")
pres <- x
load("student_test_data.Rdata")
test <- x
rm(x)


#------------------------------------------------------------------------------#
#
# @Functions
#
#------------------------------------------------------------------------------#

CalcPercentiles <- function(x) {
  # Calculates percentiles of a vector
  # Input:
  #   x - a vector of data
  # Output:
  #   a vector of percentiles
  
  percentiles <- trunc(rank(x))/length(x)

  return(percentiles)
  
}

ComputeTStat <- function(x, y, var.x, var.y) {
  # Computes t-statistic
  nx <- length(x)
  ny <- length(y)
  dmeans <- mean(x) - mean(y)
  varx <- sum(var.x)
  vary <- sum(var.y)
  
  tstat <- dmeans/sqrt(varx/nx + vary/ny)
  return(list(dmeans = dmeans, tstat = tstat))
} 

PermTTest <- function(data, groups, obs, var.obs, method = "right.tailed",
                      output.null = FALSE, strata = NULL, iters = 10000){
  # Performs permutation test on t-statistic
  # Input:
  #   data - a data frame
  #   group - a string naming the column with the group designation
  #   obs - a string naming the column with the observed values
  #   var.obs - a string naming the column with the variance of observations
  #   output.null - a logical argument. If true, empirical null will be returned
  #   strata - a string naming the column with a covariate for stratification.
  #     Performs analysis without grouped shuffling if left NULL
  #   iters - number of permutations
  
  # Output:
  #   t.stat - the t-value of the observed data
  #   p.value - p.value for one-sided test
  x <- data[(data[,groups] == 1), obs] %>% unlist()
  y <- data[(data[,groups] == 0), obs] %>% unlist()
  var.x <- data[(data[,groups] == 1), var.obs] %>% unlist()
  var.y <- data[(data[,groups] == 0), var.obs] %>% unlist()
  n <- length(x)
  values <- c(x,y)
  var.values <- c(var.x, var.y)
  
  dmeans <- ComputeTStat(x, y, var.x, var.y)
  t.stat <- dmeans$tstat
  dmeans <- dmeans$dmeans
  
  null.distr <- replicate(iters, {
    
    # No grouping 
    if (is.null(strata)) {
      shuffle <- sample(values)
      shuffle.var <- sample(var.values)
      
    # Grouped shuffling
    } else {
      strata.names <- unique(data[,strata])
      for (stratum in strata.names) {
        shuffle <- values
        shuffle.var <- var.values
        indices <- (data[,strata] == stratum)
        shuffle[indices] <- sample(shuffle[indices])
        shuffle.var[indices] <- sample(shuffle.var[indices])  
      }
    }
    
    xnew <- shuffle[1:n]
    ynew <- shuffle[-(1:n)]
    var.xnew <- shuffle.var[1:n]
    var.ynew <- shuffle.var[-(1:n)]  
    
    t <- ComputeTStat(xnew, ynew, var.xnew, var.ynew)$tstat
    
    return(t)
  })
  
  if (method == 'right.tailed') {
    p.value <- sum(null.distr >= t.stat)/iters
  } else if (method == 'left.tailed') {
    p.value <- sum(null.distr <= t.stat)/iters
  } else if (method == 'two.tailed') {
    p.value <- sum(abs(null.distr) >= abs(t.stat))/iters
  } else {
    stop('Please specify type of test. left-tailed, right-tailed, or two-tailed')
  }
  
  if (output.null == TRUE) {
    return(list(dmeans = dmeans,
                t.stat = t.stat, 
                null.distr = null.distr,
                p.value = p.value))
  } else {
    return(list(dmeans = dmeans,
                t.stat = t.stat, 
                p.value = p.value))
  }
}


#------------------------------------------------------------------------------#
#
# @Cleanup
#
#------------------------------------------------------------------------------#

# Select the features
data <- test %>%
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
         realpercentile,
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
  ) %>%
  rename(r1_attrition = attrition,
         r1_age = agetest,
         
         r1_wordscore = wordscore,
         r1_sentscore = sentscore,
         r1_letterscore = letterscore,
         r1_spellscore = spellscore,
         r1_litscore = litscore,
         r1_mathscoreraw = mathscoreraw,
         r1_totalscore = totalscore)

#------------------------------------------------------------------------------#
#
# @Analysis
#
#------------------------------------------------------------------------------#

# Building features
data <- data %>%
  filter(!is.na(std_mark)) %>%
  group_by(schoolid) %>%
  mutate(my.percentile = CalcPercentiles(std_mark), # Recalculate Percentiles
         
         # Categorial for "true" stream
         my.bottomhalf = ifelse(my.percentile <= .5, 1, 0), 
         
         # Median score of school
         r0.median.mark = median(std_mark),
         
         # Median-centered initial score
         r0.scale.mark = std_mark - r0.median.mark
         ) %>%

  ungroup() %>%
  mutate(schoolid = factor(schoolid))

# School-level data
data.sl <- data %>%
  group_by(schoolid, tracking, sbm, zone) %>%
  summarise(r1.overall.avg = mean(r1_totalscore, na.rm = T),
            r1.word.avg = mean(r1_wordscore, na.rm = T),
            r1.sent.avg = mean(r1_sentscore, na.rm = T),
            r1.letter.avg = mean(r1_letterscore, na.rm = T),
            r1.spell.avg = mean(r1_spellscore, na.rm = T),
            r1.lit.avg = mean(r1_litscore, na.rm = T),
            r1.math.avg = mean(r1_mathscoreraw, na.rm = T),
            r2.overall.avg = mean(r2_totalscore, na.rm = T),
            r2.word.avg = mean(r2_wordscore, na.rm = T),
            r2.sent.avg = mean(r2_sentscore, na.rm = T),
            r2.letter.avg = mean(r2_letterscore, na.rm = T),
            r2.spell.avg = mean(r2_spellscore, na.rm = T),
            r2.lit.avg = mean(r2_litscore, na.rm = T),
            r2.math.avg = mean(r2_mathscoreraw, na.rm = T),
            r1.overall.var = var(r1_totalscore, na.rm = T),
            r1.word.var = var(r1_wordscore, na.rm = T),
            r1.sent.var = var(r1_sentscore, na.rm = T),
            r1.letter.var = var(r1_letterscore, na.rm = T),
            r1.spell.var = var(r1_spellscore, na.rm = T),
            r1.lit.var = var(r1_litscore, na.rm = T),
            r1.math.var = var(r1_mathscoreraw, na.rm = T),
            r2.overall.var = var(r2_totalscore, na.rm = T),
            r2.word.var = var(r2_wordscore, na.rm = T),
            r2.sent.var = var(r2_sentscore, na.rm = T),
            r2.letter.var = var(r2_letterscore, na.rm = T),
            r2.spell.var = var(r2_spellscore, na.rm = T),
            r2.lit.var = var(r2_litscore, na.rm = T),
            r2.math.var = var(r2_mathscoreraw, na.rm = T),
            r1r2.overall.cov = cov(r1_totalscore, r2_totalscore, use = "complete"),
            r1r2.word.var = cov(r1_wordscore, r2_wordscore, use = "complete"),
            r1r2.sent.var = cov(r1_sentscore, r2_sentscore, use = "complete"),
            r1r2.letter.var = cov(r1_letterscore, r2_letterscore, use = "complete"),
            r1r2.spell.var = cov(r1_spellscore, r2_spellscore, use = "complete"),
            r1r2.lit.var = cov(r1_litscore, r2_litscore, use = "complete"),
            r1r2.math.var = cov(r1_mathscoreraw, r2_mathscoreraw, use = "complete")
            ) %>%
  mutate(va.overall = r2.overall.avg - r1.overall.avg,
         va.word = r2.word.avg - r1.word.avg,
         va.sent = r2.sent.avg - r1.sent.avg,
         va.letter = r2.letter.avg - r1.letter.avg,
         va.spell = r2.spell.avg - r1.spell.avg,
         va.lit = r2.lit.avg - r1.lit.avg,
         va.math = r2.math.avg - r1.math.avg,
         va.overall.var = r2.overall.var + r1.overall.var - 2*r1r2.overall.cov,
         va.word.var = r2.word.var + r1.word.var - 2*r1r2.word.var,
         va.sent.var = r2.sent.var + r1.sent.var - 2*r1r2.sent.var,
         va.letter.var = r2.letter.var + r1.letter.var - 2*r1r2.letter.var,
         va.spell.var = r2.spell.var + r1.spell.var - 2*r1r2.spell.var,
         va.lit.var = r2.lit.var + r1.lit.var - 2*r1r2.lit.var,
         va.math.var = r2.math.var + r1.math.var - 2*r1r2.math.var)

# Value added by tracking
nonsense <- FALSE

if (nonsense) { # Analyzing the wrong thing!
tracking.va <- mapply(function(obs, var.obs) {
  PermTTest(data = data.sl,
            groups = 'tracking',
            obs = obs,
            var.obs = var.obs,
            method = 'right.tailed',
            strata = 'zone')
  },
  obs = c('va.overall', 'va.word', 'va.sent', 'va.letter',
          'va.spell', 'va.lit', 'va.math'),
  var.obs = c('va.overall.var', 'va.word.var', 'va.sent.var', 
              'va.letter.var', 'va.spell.var', 'va.lit.var', 'va.math.var')
)
}

tracking.va <- mapply(function(obs, var.obs) {
  PermTTest(data = data.sl,
            groups = 'tracking',
            obs = obs,
            var.obs = var.obs,
            method = 'right.tailed',
            strata = 'zone')
},
obs = c('r2.overall.avg', 'r2.word.avg', 'r2.sent.avg', 'r2.letter.avg',
        'r2.spell.avg', 'r2.lit.avg', 'r2.math.avg'),
var.obs = c('r2.overall.var', 'r2.word.var', 'r2.sent.var', 'r2.letter.var',
            'r2.spell.var', 'r2.lit.var', 'r2.math.var')
)

rownames(tracking.va) <- c('Value Added', 't-statistic', 'p-value')
colnames(tracking.va) <- c('Overall', 'Word', 'Sent', 'Letter', 'Spell',
                      'Literacy', 'Math')
xtable(tracking.va, digits = 3, 
       caption = "Test for differences between 24-month test scores.  
       Shuffling was done within school-zone groups")

# Value added by SBM
sbm.va <- mapply(function(obs, var.obs) {
  PermTTest(data = data.sl,
            groups = 'tracking',
            obs = obs,
            var.obs = var.obs,
            method = 'right.tailed',
            strata = 'sbm')
},
obs = c('r2.overall.avg', 'r2.word.avg', 'r2.sent.avg', 'r2.letter.avg',
        'r2.spell.avg', 'r2.lit.avg', 'r2.math.avg'),
var.obs = c('r2.overall.var', 'r2.word.var', 'r2.sent.var', 'r2.letter.var',
            'r2.spell.var', 'r2.lit.var', 'r2.math.var')
)
rownames(sbm.va) <- c('Value Added', 't-statistic', 'p-value')
colnames(sbm.va) <- c('Overall', 'Word', 'Sent', 'Letter', 'Spell',
                      'Literacy', 'Math')
xtable(sbm.va, digits = 3, 
       caption = "Test for differences 24-month test scores in SBM and non-SBM schools.  
       Shuffling was done within school-zone groups")

# Value added Tracking Histogram
ggplot(data) +
  geom_histogram(aes(x = r2_totalscore, 
                     fill = factor(tracking)),
                 binwidth = .75,
                 alpha = 0.5,
                 position="identity") +
  xlab("Value-added Overall") +
  ylab("Count") +
  guides(fill = guide_legend(title = "Tracking"))

# Value added Tracking + SBM Plot
data %>% ungroup() %>%
  mutate(tracking = ifelse(tracking == 1, 
                           'Tracking Schools',
                           'Non-Tracking Schools'),
         sbm = ifelse(sbm == 1, 
                      'SBM Funding',
                      'No SBM Funding')) %>%
ggplot() +
  geom_boxplot(aes(x = factor(tracking), 
                   y = r2_totalscore,
                   fill = factor(sbm))) +
  guides(fill = guide_legend(title = "School-Based Management")) +
  xlab("Tracking") +
  ylab("Value Added")

#------------------------------------------------------------------------------#
#
# @Exploration
#
#------------------------------------------------------------------------------#

exploration = FALSE
if (exploration) {
# Percent of students placed in the correct streams
data.tracking <- data %>%
  filter(tracking == 1) %>%
  group_by(schoolid) %>%
  mutate(correct.stream = ifelse(my.bottomhalf == lowstream, 1, 0)) %>%
  ungroup()

prop.complier <- data.tracking %>%
  group_by(schoolid) %>%
  summarise(proportion = sum(correct.stream)/length(correct.stream))
prop.complier.total <- sum(data.tracking$correct.stream) / nrow(data.tracking)

# std_mark exploration
ggplot(data, aes(schoolid, r0.scale.mark)) +
  geom_boxplot()

data %>%
  filter(!is.na(std_mark) & !is.na(r1_totalscore)) %>%
  ggplot() +
    geom_point(aes(x=my.percentile, y=percentile, color = schoolid))

# Dropout
data %>% filter(is.na(std_mark)) %>%
  ggplot() +
    geom_histogram(aes(x = r1_totalscore))

# Tracking and SBM counts
data.sl %>% group_by(tracking, sbm) %>%
  summarise(n = n())

data.sl %>% group_by(zone, sbm, tracking) %>%
  summarise(n = n())

trk.sbm.va <- mapply(function(obs, var.obs) {
  PermTTest(data = data.sl,
            groups = 'tracking',
            obs = obs,
            var.obs = var.obs,
            method = 'right.tailed',
            strata = 'sbm')
},
obs = c('va.overall', 'va.word', 'va.sent', 'va.letter',
        'va.spell', 'va.lit', 'va.math'),
var.obs = c('va.overall.var', 'va.word.var', 'va.sent.var', 
            'va.letter.var', 'va.spell.var', 'va.lit.var', 'va.math.var')
)
rownames(trk.sbm.va) <- c('Value Added', 't-statistic', 'p-value')
colnames(trk.sbm.va) <- c('Overall', 'Word', 'Sent', 'Letter', 'Spell',
                      'Literacy', 'Math')

}