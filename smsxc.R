# Tom Alig
# June 9, 2020
# Saline Middle School xc times, 2010-2019

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
source("RunningProjectR/scripts/my_track_scripts.R")


# Read in the data
sms <- read.csv("msxc.saline.csv")
str(sms)

# Copy the df
sms2 <- sms
dim(sms2)

names(sms2)

sms2 <- sms2 %>% 
  filter(Grade == "7" | Grade == "8") 

#sms2 <- time.to.decimal4(sms2, "Huron", "Huron2")
#sms2 <- time.to.decimal4(sms2, 'Huron', Huron2)
#sms2 <- time.to.decimal4(sms2, SEC1, blah)
#sms2 <- time.to.decimal5(sms2, Huron)

sms2 %>% 
  View()

# Get all time in one column
sms2.long <- sms2 %>% 
  filter(Grade == "7" | Grade == "8") %>% 
  pivot_longer(cols = SEC1:MegaJambo,
               names_to = "Meet",
               values_to = "Time")


# Get Time field to be in Time format
sms3 <- sms2.long %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = round((minute + second), digits = 2)) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs))  %>% 
  dplyr::rename(Time2 = mins.dec)

sms3 %>% 
  View()

str(sms3)

# Histogram of all performances on 2 mile course
sms3 %>% 
  filter(Meet != "Portage",
         Meet != "SEC3" & Year != "2011") %>% 
  ggplot(aes(x = Time2 * 60)) +
  geom_histogram(bins = 16, fill = cb.orange, color = cb.blue) + #18
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


sms3 %>% 
  filter(#Meet != "Portage",
         #(Meet != "SEC3" | Year != "2011"),
         Athlete == "Lydia Alig" | Athlete == "Elaina Alig") %>% 
  ggplot(aes(x = Meet, y = Time2 * 60, na.rm = TRUE,
           color = Athlete ,
           #size = Athlete == sn,  
           shape = Athlete
))+
  geom_point() +
  #geom_point(position = 'jitter') + 
 # scale_x_discrete(limits = c('Gr7', 'Gr8')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time In Minutes") +
    scale_color_manual(values = c(cb.blue, cb.orange)) +
  #scale_color_manual(values = c("black", cb.blue, cb.orange, cb.purple ),
  #                   limits = c(sn, "D1", "D2", "D3")
  #) +
  #scale_size_manual(values = c(2, 5)) +
  #scale_shape_manual(values = shapes4,
  #                   limits = c(sn, "D1", "D2", "D3")) +
  guides(size = FALSE,
         shape = guide_legend(title = NULL),
         color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(axis.text=element_text(size=11.5)) +
  ggtitle("7th and 8th Grade SMS Girls XC Times 2011-2019")
  
sms3 %>% 
  filter(!is.na(Time2)) %>% 
  group_by(Meet, Grade) %>% 
  summarise(blah = n()) %>% 
  arrange(desc(blah)) %>% 
  View()

# Get each individual runner into one row; eg "one row per athlete"

# copy df
sms4 <- sms3

sms4 <- sms4 %>% 
  pivot_wider(names_from = Meet, values_from = Time2) 

sms4 %>% 
  View()

score.meets <- c("Portage", "SEC1", "SEC2", "SEC3", "Coach.Legends", "MegaJambo")
score.meets2 <- c("SEC1", "SEC2", "SEC3", "portage2","Coach.Legends", "MegaJambo")

sms4 %>% 
  select(all_of(score.meets), Athlete)

# In 2011, the SEC3 meet resulted in very low times. Let's eliminate this meet from calculations
sms5 <- sms4 %>% 
  mutate(SEC3b = ifelse(Year == 2011 & SEC3 > 0, NA , SEC3)) %>% 
  select(-SEC3) %>% 
  mutate(SEC3 = SEC3b) %>% 
  select (-SEC3b) 

# get score and count of meets among the score.meets group
sms5 <- sms5 %>% 
  group_by(Athlete, Grade) %>% 
  mutate(counted = 6 - sum(is.na(c(Portage, SEC1, SEC2, SEC3, MegaJambo, Coach.Legends)))) %>% 
  mutate(portage2 = Portage * 4/3) %>% 
  rowwise() %>% 
  mutate(score1 = sum(portage2, SEC1, MegaJambo, SEC3, SEC2, Coach.Legends, na.rm = TRUE)) %>% 
  #select(score1, counted, everything()) %>% 
  select(score1, counted, Grade, Athlete, Year, all_of(score.meets2)) 

str(sms5)

# adjust score to factor in number of meets actually run by individual runner
sms5 <- sms5 %>% 
  mutate(score2 = ifelse(counted == 2, (score1 * 2),
                         ifelse(counted == 3, score1/3 * 4,
                                ifelse(counted == 5, score1 - max(portage2, SEC1, MegaJambo, SEC3, SEC2, Coach.Legends, na.rm = TRUE),
                                       ifelse(counted == 6, score1 - max(portage2, SEC1, MegaJambo, SEC3, SEC2, Coach.Legends, na.rm = TRUE) - min(portage2, SEC1, MegaJambo, SEC3, SEC2, Coach.Legends, na.rm = TRUE),
                                                                       score1))))) %>% 
  filter(counted > 1) %>% 
  select(score2, score1, everything()) 

sms5 %>% 
  View()

names(sms5)

sms.scores <- sms5 %>% 
  select(score2, Grade, Athlete) %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>%
  dplyr::rename(Grade8.score = `8`,
                Grade7.score = `7`) %>% 
  select(Athlete, Grade7.score, Grade8.score)

sms.scores %>% 
  View()

