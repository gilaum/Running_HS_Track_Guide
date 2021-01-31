# Tom Alig
# Sept 19, 2020
# Saline High School 
# Compare Pioneer HS course

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
library(magrittr)
#library(mice)
#library(randomForest)

#source("RunningProjectR/scripts/my_track_scripts.R")
#source("smsxc.R")
#source("shsxc.R")

# Read in the data
shs <- read.csv("shs.xc.2020.csv")
pioneer <- read.csv("Pioneer.csv")

# Copy the df
shs2 <- shs
names(shs2)
p2 <- pioneer
names(p2)

# Get Time field to be in numeric decimal format
p2 <- p2 %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = round((minute + second), digits = 2)) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs))  %>% 
  dplyr::rename(Time2 = mins.dec)

# change some Course names
p2 <- p2 %>%
  mutate(Course = recode(Course, 'Pioneer' = 'pioneer', 
                         'Hudson Mills' = 'HudsonMills')) %>% 
  filter(Time2 < 23.5)

# copy df
p3 <- p2
p3 %>% View()

# Compare Schools
p3 %>% 
  select(-Grade) %>% 
  #group_by(School, Course) %>% 
  group_by(Course) %>% 
  #filter(Time2 < 23.5) %>% 
  #summarise(n = n())
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE)) %>% 
  #summarise(count = n()) %>% 
  #filter(School == "Saline" | School == "Ann Arbor Pioneer") %>% 
  View()


# Get each individual runner into one row; eg "one row per athlete"

# First, set up the new df
blah <- p3 %>% 
  select(-Grade) %>% 
  group_by(Athlete, Course) %>%
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE)) 

names(blah)
blah %>% 
  View()

# Define Courses of Interest
c1 <- "pioneer"
c2 <- "HudsonMills"

# Now, get one row per athlete
blah2 <- blah %>% 
  filter(Course == c1 | Course == c2) %>% 
  group_by(Athlete) %>%
  pivot_wider(names_from = Course, values_from = Median) %>% 
  fill(c1, c2) 

blah2 %>% 
  View()

names(blah2)

# Compare Schools
blah2 %>% 
  filter(pioneer != "NA" & HudsonMills != "NA") %>% 
  #select(-Grade) %>% 
  group_by(pioneer) %>% 
  #summarise(n = n())
  summarise(pion.med = median(pioneer, na.rm = TRUE),
          pion.mean = mean(pioneer, na.rm = TRUE)) %>% 
  #summarise(count = n()) %>% 
  #filter(School == "Saline" | School == "Ann Arbor Pioneer") %>% 
  View()


blah3 <- blah2 %>% 
  filter(pioneer != "NA" & HudsonMills != "NA")

blah3 %>% 
  View()

median(blah3$pioneer, na.rm = TRUE)
mean(blah3$pioneer, na.rm = TRUE)
median(blah3$HudsonMills, na.rm = TRUE)
mean(blah3$HudsonMills, na.rm = TRUE)


mygraph <- blah2 %>% 
  filter(pioneer != "NA" & HudsonMills != "NA") %>% 
  ggplot(aes(y = pioneer * 60, x = HudsonMills * 60)) +
  geom_point() +
  scale_y_time(labels = time_format("%M:%S"),
               name = c1) +
  scale_x_time(labels = time_format("%M:%S"),
               name = c2) +
  ggtitle(paste(c1, "and", c2, "Girls HS XC")) +
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  theme_bw() 



lj.df <- left_join(p3, blah3, by = "Athlete")
lj.df %>% View()
names(lj.df)

lj.df2 <- lj.df %>% 
  mutate(hm2 = HudsonMills,
         p2 = pioneer) %>% 
  select(-c(HudsonMills, pioneer, Grade, Mean.Race))

names(lj.df2)
lj.df2 %>% 
  View()

lj.df3 <- lj.df2 %>% 
  distinct(Athlete, .keep_all = TRUE) %>% 
  mutate(hm3 = ifelse(School == 'Saline', Time2, hm2),
         p3 = ifelse(School == 'Saline', Time2, p2)) %>% 
  filter(School == 'Saline') %>% 
  mutate(HudsonMills = hm3,
         pioneer = p3) 
  

cb.orange <- "#E69F00"
cb.purple <- "#CC79A7"

mg2 <- mygraph + geom_point(data = lj.df3, 
                     color = cb.orange,
                     size = 5) 


lj.df4 <- lj.df2 %>% 
  distinct(Athlete, .keep_all = TRUE) %>% 
  filter(School == 'Ann Arbor Pioneer') %>% 
  mutate(HudsonMills = hm2,
         pioneer = p2) 


mg3 <- mg2 + geom_point(data = lj.df4,
                color = cb.purple,
                size = 5)

mg3 + coord_flip()
mg3

mygraph + coord_flip()

##   DISCARDED CODE
lj.df2 %>% 
  ifelse(Course %in% c('HudsonMills') & is.na(hm2),
         hm2 = Time2,
         p2 = Time2) %>% 
  View()


  #ifelse((Course %in% "HudsonMills" & !is.na(Time2)), 
  ifelse(Course == "HudsonMills", 
         hm2 = Time2,
         p2 = 99) %>% 
  #select(-(c(Grade, Course, Time2))) %>% 
  View()

  
  ggplot(aes(y = p3 * 60, x = hm3 * 60),
         color = School,
         size = School) +
    geom_point() +
    scale_y_time(labels = time_format("%M:%S"),
                 name = c1) +
    scale_x_time(labels = time_format("%M:%S"),
                 name = c2) +
    #coord_cartesian(xlim = c(17*60, 22*60),
    #                ylim = c(17*60, 22*60)) +
    #coord_cartesian(ylim = c(17*60, 22*60)) +
    #geom_text(aes(label = round(pioneer, 2)),
    #          size = 3) +
    ggtitle(paste(c1, "and", c2, "Girls HS XC")) +
    geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
    theme_bw() 
  
  