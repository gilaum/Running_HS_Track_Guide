# Tom Alig
# June 18, 2020
# Saline High School xc times, 2010-2019

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
library(mice)
source("RunningProjectR/scripts/my_track_scripts.R")
source("smsxc.R")

# Read in the data
shs <- read.csv("shsxc.csv")
str(shs)

# Copy the df
shs2 <- shs
dim(shs2)

names(shs2)

shs2 <- shs2 %>% 
  filter(Grade == "9" | Grade == "10" | Grade == "11" | Grade == "12") 

shs2 %>% 
  View()

names(shs2)

# Delete unused columns and move Year to third column
shs2 <- shs2 %>% 
  #select(-c(Marauder, X, X.1, X.2, X.3, X.4, X.5)) %>% 
  select(-c(Marauder, X)) %>% 
  select(Grade, Athlete, Year, everything()) 

# Get all time in one column
shs2.long <- shs2 %>% 
  pivot_longer(cols = EndOfSummer:States,
               names_to = "Meet",
               values_to = "Time")


#shs2.long %>% 
  #filter(Time > 0) %>% 
#  View()

# Get Time field to be in numeric decimal format
shs3 <- shs2.long %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = round((minute + second), digits = 2)) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs))  %>% 
  dplyr::rename(Time2 = mins.dec)

shs3 %>% 
 #filter(Time2 > 40) %>% 
  View()

# What are median and mean values for all years combined, by Meet
meet.times <- shs3 %>% 
  group_by(Meet) %>% 
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE))  

shs3 %>% 
  filter(Meet == "Regionals" | 
           Meet == "States") %>% 
  filter(!is.na(Time2),
         Grade == '9') %>% 
  #group_by(Year, Meet) %>% 
  #summarise(Median = median(Time2, na.rm = TRUE),
  #          Mean.Race = mean(Time2, na.rm = TRUE)) 
  View()
  

#meet.times %>% 
#  arrange(Median) %>% 
#  View()

# Count of number of participants by meet
#shs3 %>% 
#  filter(!is.na(Time2)) %>% 
#  group_by(Meet, Grade) %>% 
#  summarise(blah = n()) %>% 
#  arrange(desc(blah)) %>% 
#  View()


# Histogram of all performances on 5k mile course
shs3 %>% 
  filter(Meet != "EndOfSummer"
         #Meet != "SEC3" & Year != "2011"
         ) %>% 
  ggplot(aes(x = Time2 * 60)) +
  geom_histogram(bins = 60, fill = cb.orange, color = cb.blue) + #18
  #geom_vline(aes(xintercept = ifelse(Athlete == sn, HStime, NA)), 
  #           color = "black", 
  #           linetype = "dashed", size = 1) + 
  scale_x_time(labels = time_format("%M:%S")) +
  scale_y_continuous(labels = comma) +
  #labs(title = paste("All",
  #                   event2,
  #                   years),
  #     subtitle = "Dashed Line = Your Athlete",
  #     x = "Time") +
  theme_bw()

#shs3 %>% 
#  filter(!is.na(Time2)) %>% 
#  group_by(Meet, Grade) %>% 
#  summarise(blah = n()) %>% 
#  arrange(desc(blah)) %>% 
#  View()

# Get each individual runner into one row; eg "one row per athlete"

# copy df
shs4 <- shs3

shs4 <- shs4 %>% 
  pivot_wider(names_from = Meet, values_from = Time2) 

shs4 %>% 
  View()


# We want to compare the athletes who ran both Early Bird and Bath in the same year
#  So, we need to remove all those who ran only one of the races in a given year
# Spread the data
#eb.bath <- 
#  shs4 %>% 
#  #spread(key = Meet, value = Time) %>% 
#  group_by(Athlete, Year) %>% 
#  filter(Bath != "NA" & EarlyBird != "NA") %>% 
#  mutate(Diff = Bath - EarlyBird) %>% # negative number means Bath is faster course
#    View()

names(shs4)

score.meets <- c("EarlyBird", "SEC1", "SEC2", "SEC.Champ", "CoachLegends", "Portage")
#score.meets2 <- c("SEC1", "SEC2", "SEC3", "portage2","Coach.Legends", "MegaJambo")

shs4 %>% 
  select(all_of(score.meets), Athlete)

# get score and count of meets among the score.meets group
shs5 <- shs4 %>% 
  group_by(Athlete, Grade) %>% 
  mutate(counted = 6 - sum(is.na(c(Portage, SEC1, SEC2, SEC.Champ, EarlyBird, CoachLegends)))) %>% 
  #mutate(portage2 = Portage * 4/3) %>% 
  rowwise() %>% 
  mutate(score1 = sum(Portage, SEC1, SEC2, SEC.Champ, EarlyBird, CoachLegends, na.rm = TRUE)) %>% 
  #select(score1, counted, everything()) %>% 
  select(score1, counted, Grade, Athlete, Year, all_of(score.meets)) 

str(shs5)

#shs5 %>% 
#  View()

# adjust score to factor in number of meets actually run by individual runner
shs5 <- shs5 %>% 
  mutate(score2 = ifelse(counted == 2, (score1 * 2),
                         ifelse(counted == 3, score1/3 * 4,
                                ifelse(counted == 5, score1 - max(Portage, SEC1, SEC2, SEC.Champ, EarlyBird, CoachLegends, na.rm = TRUE),
                                       ifelse(counted == 6, score1 - max(Portage, SEC1, SEC2, SEC.Champ, EarlyBird, CoachLegends, na.rm = TRUE) - min(Portage, SEC1, SEC2, SEC.Champ, EarlyBird, CoachLegends, na.rm = TRUE),
                                              score1))))) %>% 
  filter(counted > 1) %>% 
  select(score2, score1, everything()) 

# Get one row per athlete, for scores for all grades
#   First, get df by grade level

shs5.10 <- shs5 %>% 
  filter(Grade == '10')

shs5.9 <- shs5 %>% 
  filter(Grade == '9')

shs5.11 <- shs5 %>% 
  filter(Grade == '11')

shs5.12 <- shs5 %>% 
  filter(Grade == '12')

# Do a join to join 2 df at a time
shs.blah1 <- full_join(shs5.9, shs5.10,
                      #shs5.11, shs5.12, 
                      by = "Athlete")

shs.blah2 <- full_join(#shs5.9, shs5.10,
                      shs5.11, shs5.12, 
                      by = "Athlete")


shs.blah3 <- full_join(shs.blah1, shs.blah2,
                      #shs5.11, shs5.12, 
                      by = "Athlete")

# Rename some columns, get the desired result
shs6 <- shs.blah3 %>% 
  rename(Grade12.score = score2.y.y,
         Grade11.score = score2.x.y,
         Grade10.score = score2.y.x,
         Grade9.score = score2.x.x) %>% 
  select(Athlete, Grade9.score, Grade10.score, Grade11.score, Grade12.score) 



# All early bird results 9th grade
ebird.9 <- shs3 %>% 
  filter(Grade== "9",
         Meet == "EarlyBird",
         Time2 > 0)

ebird.9 %>% 
  View()

shs6 %>% 
  View()

# Join (full join) Middle School scores with 9th grade Early Bird Meet results
pred.eb9 <- ebird.9 %>% 
  full_join(sms.scores, by = "Athlete")

pred.eb9 <- pred.eb9 %>% 
  filter(Time2 > 0)

pred.eb9 %>% 
  View()

# Impute missing values from pred.ebd9
pred.eb9.imp <- mice(pred.eb9, m = 5, seed = 157)
eb9.imp <- complete(pred.eb9.imp,3)
eb9.imp %>% 
  View()


ggplot(eb9.imp,aes(x = Grade7.score, y = Time2 * 60
)) + 
  geom_point() +
  #geom_text(aes(label = Athlete),
  #          size = 3) +
  #scale_x_time(labels = time_format("%M:%S"),
  #             name = "MS 7th Grade Time in Minutes") +
  scale_y_time(labels = time_format("%M:%S"),
               name = "HS 8th Grade Time in Minutes") +
  #scale_color_manual(values = cb.six) +
  coord_cartesian(xlim = c(45, 80)) +
  #coord_cartesian(ylim = c(19, 35)) +
  #                xlim = c(48, 70)) +
  
  labs(color = "Early Bird Meet")  +
  theme_bw() 


# Predict for 9th Grade Early Bird Meet
#  Use imputed values
#  |||
#  VVV
set.seed(157)
lm.pred.eb9 <- lm(Time2 ~ Grade7.score + Grade8.score,
                  data = eb9.imp,
                  na.action = na.exclude)

lm.pred.eb9

set.seed(157)
yh.lm.pred.eb9 <- predict(lm.pred.eb9, eb9.imp)

pred.eb9 <- pred.eb9 %>% 
  mutate(lm.pred.eb9 = yh.lm.pred.eb9) 

pred.eb9 %>% 
  View()

names(pred.eb9)

ggplot(pred.eb9,aes(x = (lm.pred.eb9 * 60), y = (Time2 * 60)
)) + 
  geom_point() +
  #geom_text(aes(label = Athlete),
  #          size = 3) +
  scale_x_time(labels = time_format("%M:%S"),
               name = "Predicted Time in Minutes") +
  scale_y_time(labels = time_format("%M:%S"),
               name = "HS 9th Grade Time in Minutes") +
  #scale_color_manual(values = cb.six) +
  #coord_cartesian(xlim = c(45, 80)) +
  #coord_cartesian(ylim = c(19, 35)) +
  #                xlim = c(48, 70)) +
  
  labs(color = "Early Bird Meet")  +
  ggtitle("Early Bird Meet") +
  theme_bw() 

# Next to do's 08/02/2020
#  Training/Test sets
#  MSE, etc
#  Find incoming 9th graders
#  Predict their meets
#  Rinse, Repeat for grades 10, 11, 12
#  Get all predictions for this year, for grades 9 - 12






