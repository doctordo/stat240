#EDA
library(dplyr)

dat <- read.table("student_test_data.tab.tsv", sep = "\t", header = T)

# histogram of age
ggplot(dat) + geom_histogram(aes(x = agetest), binwidth = 1, col = "white") +
  scale_x_continuous(name = "age")



# comparing features for tracking and non-tracking
dat %>% group_by(tracking) %>% summarize(n = n())
dat %>% group_by(tracking) %>% summarize(gender = sum(girl, na.rm = TRUE)/n())
dat %>% group_by(tracking) %>% summarize(n = mean(agetest,na.rm = TRUE))
dat %>% group_by(tracking) %>% summarize(n = sd(agetest,na.rm = TRUE))
dat %>% group_by(tracking, district) %>% summarize(n = n())

dat %>% group_by(tracking) %>% summarize(n = sum(attrition)/n())
dat %>% group_by(tracking) %>% summarize(n = sum(r2_attrition, na.rm = T))



# comparing features for high and low stream
dat %>% filter(tracking == 1) %>% group_by(lowstream) %>% summarize(n = n())
dat %>% filter(tracking == 1) %>% group_by(lowstream) %>% summarize(gender = sum(girl, na.rm = TRUE)/n())
dat %>% filter(tracking == 1) %>% group_by(lowstream)  %>% summarize(n = mean(agetest,na.rm = TRUE))
dat %>% filter(tracking == 1) %>% group_by(lowstream)  %>% summarize(n = sd(agetest,na.rm = TRUE))
dat %>% filter(tracking == 1) %>% group_by(lowstream) %>% summarize(n = sum(attrition)/n())
dat %>% group_by(tracking) %>% summarize(n = sum(r2_attrition, na.rm = T))


# comparing number of tracked and non tracked schools in each zone
school.zones <- dat %>% group_by(tracking, zone) %>% summarize(n = length(unique(schoolid))) # imbalance in school zones!
ggplot(school.zones) + 
  geom_bar(aes(x = zone, y = n, fill = factor(tracking)), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name="", labels=c("Non Tracked", "Tracked")) + 
  scale_y_continuous(name = "number of schools")


# compare final scores by region
scores.zones <- dat %>% filter(tracking == 1) %>% group_by(zone)# %>% summarize(endline = mean(totalscore,na.rm = T)) # imbalance in school zones!
ggplot(scores.zones) + 
  geom_boxplot(aes(x = zone, y = r2_totalscore, fill = zone))

ggplot(dat) + 
  geom_boxplot(aes(x = zone, y = std_mark, fill = zone)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(tracking~.)
