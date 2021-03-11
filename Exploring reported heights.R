#Data set from 2016 Biostatistics class where students were asked to fill in sex and height questionaire that is now included in reported_heights data set.
#Download required packages
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
#Percentage of students taking classes in person or online
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))
0.667(in class), 0.378(online)

#Assume that for each class type the students are either all male or all female, based on the most prevalent sex in each class type we calculated in previous percentage.
#Here we report the accuracy of our prediction of sex based on type
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)
0.6333(accuracy)

#Testing sensitivity of prediction
library(caret)
sensitivity(y_hat, y)
0.3823(sensitivity)

#Specificity of prediction
library(caret)
specificity(y_hat, y)
0.8414(specificity)

#Prevalence of females
mean(y == "Female")
0.4533

