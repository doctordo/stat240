#-------------------------------------------------------------------------------
#
# Libraries
#
#-------------------------------------------------------------------------------

library(dplyr)
library(reshape2)

#-------------------------------------------------------------------------------
#
# Chapter 26, Problem 10
#
#-------------------------------------------------------------------------------
births <- c(451, 468, 429, 448, 466, 377, 344,
			448, 438, 455, 468, 462, 405, 377,
			451, 497, 458, 429, 434, 410, 351,
			467, 508, 432, 426)
distr <- c()
for(i in 1:(length(births)-2)){
	for(j in (i+1):(length(births)-1)){
		for(k in (j+1):length(births)){
			distr <- c(distr, mean(births[c(i,j,k)]))
		}
	}
}
sd(distr)*(length(births)-1)/length(births) # this is the empirical SD of the average of three randomly selected days
# [1] 21.12005

# to use the z-test for avg of 3 days, it seems like we should use a SE like this
sqrt(var(births/3))
#[1] 13.53715

#-------------------------------------------------------------------------------
#
# Chapter 26, Problem 12
#
#-------------------------------------------------------------------------------

data <- data.frame(Experiment = c(rep('#1', 11),
                                  rep('#2', 12),
                                  rep('#3', 12),
                                  rep('#4', 12),
                                  rep('#5', 12)),
                   Treatment = c(689, 656, 668, 660, 679, 663, 
                                 664, 647, 694, 633, 653,
                                 
                                 707, 740, 745, 652, 649, 676,
                                 699, 696, 712, 708, 749, 690,
                                 
                                 690, 701, 685, 751, 647, 647,
                                 720, 718, 718, 696, 658, 680,
                                 
                                 700, 718, 679, 742, 728, 677,
                                 696, 711, 670, 651, 711, 710,
                                 
                                 640, 655, 624, 682, 687, 653,
                                 653, 660, 668, 679, 638, 649),
                   
                   Control =   c(657, 623, 652, 654, 658, 646,
                                 600, 640, 605, 635, 642,
                                 
                                 669, 650, 651, 627, 656, 642,
                                 698, 648, 676, 657, 692, 621,
                                 
                                 668, 667, 647, 693, 635, 644,
                                 665, 689, 642, 673, 675, 641,
                                 
                                 662, 705, 656, 652, 578, 678,
                                 670, 647, 632, 661, 670, 694,
                                 
                                 641, 589, 603, 642, 612, 603,
                                 593, 672, 612, 678, 593, 602)
                                 ) %>%
  mutate(WhichHeavier = ifelse(Treatment>Control, 'Treatment', 'Control'),
         Difference = Treatment - Control)

# Part B
# Average and Standard Error Calculations
weight.table <- dcast(data, 
                      Experiment ~ WhichHeavier, 
                      length, 
                      margins = TRUE,
                      value.var = 'WhichHeavier')

avg.d <- mean(data$Difference)
sd.d <- sd(data$Difference)
se.d.avg <- sd.d/sqrt(59)
t.stat <- avg.d/se.d.avg
t.test(x=data$Difference, alternative = "greater", mu = 0)



