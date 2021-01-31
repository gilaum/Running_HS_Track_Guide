# Tom Alig
# Sept 5, 2020
# Saline High School 
# Predict Hudson Mills Sept 9 meet v Northville

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
library(magrittr)
library(mice)
library(randomForest)

source("RunningProjectR/scripts/my_track_scripts.R")
source("smsxc.R")
source("shsxc.R")

# Read in the data
cb <- read.csv("cass.benton.xc.csv")

# Copy the df
cb2 <- cb
names(cb2)

# Get Time field to be in numeric decimal format
cb2 <- cb2 %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = round((minute + second), digits = 2)) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs))  %>% 
  dplyr::rename(Time2 = mins.dec)

cb2 %>% 
  View()

# change some Course names
cb2 <- cb2 %>%
  mutate(Course = recode(Course, 'Cass Benton' = 'CassBenton', 
                         'Hudson Mills' = 'HudsonMills',
                         'Huron Meadows' = 'HuronMeadows')) 

# copy df
cb3 <- cb2

# Compare Schools
cb3 %>% 
  select(-Grade) %>% 
  group_by(School, Course, Year) %>% 
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE)) %>% 
  #summarise(count = n()) %>% 
  filter(School == "Saline" | School == "Ann Arbor Pioneer") %>% 
  View()


# Get each individual runner into one row; eg "one row per athlete"

# First, set up the new df
blah <- cb3 %>% 
  select(-Grade) %>% 
  group_by(Athlete, Course, Year) %>%
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE)) 

names(blah)

# Define Courses of Interest
c1 <- "HuronMeadows"
c2 <- "HudsonMills"

# Now, get one row per athlete
blah2 <- blah %>% 
  filter(Course == c1 | Course == c2) %>% 
  group_by(Athlete, Year) %>%
  pivot_wider(names_from = Course, values_from = Median) %>% 
  fill(c1, c2) 

blah2 %>% 
  View()

names(blah2)

str(blah2$CassBenton)
str(blah2$HudsonMills)
str(blah2)

blah2 %>% 
  filter(HuronMeadows != "NA" & HudsonMills != "NA") %>% 
  filter(Year == '2019') %>% 
  #filter(`c1` != "NA" & `c2` != "NA") %>% 
  #filter(c1 != "NA" & c2 != "NA") %>% 
  #filter(!is.na(c1) & !is.na(c2)) %>% 
  #View()
  ggplot(aes(y = HuronMeadows * 60, x = HudsonMills * 60)) +
  geom_point() +
  scale_x_time(labels = time_format("%M:%S"),
               name = c2) +
  scale_y_time(labels = time_format("%M:%S"),
               name = c1) +
  #coord_cartesian(xlim = c(17*60, 22*60),
  #                ylim = c(17*60, 22*60)) +
  #coord_cartesian(ylim = c(17*60, 22*60)) +
  geom_text(aes(label = round(HuronMeadows, 2)),
            size = 3) +
  ggtitle(paste(c1, "and", c2, "Girls HS XC")) +
  geom_smooth() +
  theme_bw() 

  


#################################################
################################################
######## Discarded Code
####### |||||||||||||||
####### VVVVVVVVVVVVVVV

#cb3 <- 
cb3 %>% 
  select(-Grade) %>% 
  #dplyr::group_by(Athlete) %>% 
  group_by(Athlete) %>% 
  #View()
  mutate(row = row_number()) %>%
  pivot_wider(names_from = Course, values_from = Time2) %>% 
  #rowwise() %>% 
  select(-row) %>% 
  View()


cb3 %>% 
  select(-Grade) %>% 
  group_by(Athlete, Course, Year) %>% 
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE)) %>% 
  #summarise(count = n()) %>% 
  filter(Course == "Cass Benton" | Course == "Hudson Mills") %>% 
  #ungroup() %>% 
  #filter((Athlete == Athlete & Course == "Cass Benton") & (Athlete == Athlete & Course == "Hudson Mills")) %>% 
  pivot_wider(names_from = Course, values_from = Median) %>% 
  group_by(Athlete, Year) %>% 
  View()
ggplot(aes(x = Athlete, y = Median, 
           color = Course)) +
  geom_point()


cb3 %>% 
  select(-Grade) %>% 
  group_by(Athlete, Course, Year) %>% 
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Course == "Cass Benton" | Course == "Hudson Mills") %>% 
  group_by(Course) %>%
  pivot_wider(names_from = Course, values_from = Median) %>% 
  group_by(Athlete, Year) %>% 
  #filter(`Cass Benton` != "NULL" & `Hudson Mills` != "NULL") %>% 
  #mutate(`Cass Benton`) = as.numeric.factor(`Cass Benton`) %>% 
  View()
ggplot(aes(x = `Cass Benton`, y = `Hudson Mills`)) +
  geom_point()

