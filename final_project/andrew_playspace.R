#------------------------------------------------------------------------------#
#
# @Libraries
#
#------------------------------------------------------------------------------#

library("dplyr")
library("reshape2")

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
# @Cleanup
#
#------------------------------------------------------------------------------#

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

