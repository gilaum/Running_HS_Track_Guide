# Tom Alig
# Oct 13, 2020
# Saline High School 
# Compare Courses
#  Courses included are:
#    Cass Benton
#    Hudson Mills
#    Huron Meadows  
#    Grand Rapids Riverside Park
#    Indian Creek Park in Temp Bedford area
#    Lake Erie Metropark at Gibraltar Carlson
#    MIS (State Meet)
#    Pioneer High School
#    Portage


library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
library(magrittr)

source("RunningProjectR/scripts/my_track_scripts.R")

# Read in the data
mydata <- read.csv("hsxc181920.csv")

# Copy the df
md2 <- mydata
names(md2)

# Get Time field to be in numeric decimal format
md2 <- md2 %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = round((minute + second), digits = 2)) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs))  %>% 
  dplyr::rename(Time2 = mins.dec)

# copy df
md3 <- md2
md3 %>% View()
names(md3)

# Compare Schools
md3 %>% 
  #group_by(School, Course) %>% 
  group_by(Course) %>% 
  filter(Time2 < 22.5,
         Year == 2020) %>% 
  #summarise(n = n())
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE)) %>% 
  #summarise(count = n()) %>% 
  #filter(School == "Saline" | School == "Ann Arbor Pioneer") %>% 
  View()


# Get each individual runner into one row; eg "one row per athlete", per course
# First, set up the new df
blah <- md3 %>% 
  filter(Time2 < 22.25) %>% 
  group_by(Athlete, Course, Year, School) %>%
  summarise(Median = median(Time2, na.rm = TRUE),
            Mean.Race = mean(Time2, na.rm = TRUE))

names(blah)

# Now, get one row per athlete
blah2 <- blah %>% 
  select(-Mean.Race) %>% 
  group_by(Athlete, School) %>%
  pivot_wider(names_from = c(Course,Year), values_from = Median) 

blah2 %>% 
  View()

names(blah2)

# copy the df
blah3 <- blah2
dim(blah3)
unique(blah$Course)
names(blah3)

cb.orange <- "#E69F00"
cb.purple <- "#CC79A7"


blah3 %>% 
  select(Athlete, School, `Hudson Mills_2020`, `Indian Creek_2020`) %>% 
  filter(!is.na(`Hudson Mills_2020`) & !is.na(`Indian Creek_2020`)) %>% 
  #group_by(School, Course) %>% 
  #group_by(School) %>% 
  #filter(Time2 < 22.5) %>% 
  #summarise(Mean.Pio = mean(Pioneer_2020, na.rm = TRUE),
  #          Mean.IC = mean(`Indian Creek_2020`, na.rm = TRUE)) %>% 
  mutate(diff = `Hudson Mills_2020` - `Indian Creek_2020`) %>% 
  #View()
  ggplot(aes(y = `Hudson Mills_2020` * 60, x = `Indian Creek_2020` * 60,
             color = School)) +
  geom_point(size = 5) +
  #scale_color_manual(values = c(cb.purple, cb.orange)) +
  scale_y_time(labels = time_format("%M:%S"),
               name = "Hudson Mills Course") +
  scale_x_time(labels = time_format("%M:%S"),
               name = "Indian Creek Course") +
  coord_cartesian(xlim = c(17.5*60, 22.5*60),
                                  ylim = c(17.5*60, 22.5*60)) +
                  #coord_cartesian(ylim = c(17*60, 22*60)) +
  #ggtitle(paste(c1, "and", c2, "Girls HS XC")) +
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  theme_bw() 

# Define Courses of Interest
#c1 <- "GR Riverside_2020"
#c2 <- blah3$`Cass Benton_2020`


blah33 <- blah3 %>% 
  select(Athlete, School, starts_with("GR"), starts_with("Hudson"))

mean((blah33[,3]), na.rm = TRUE)
mean(blah33$`Indian Creek_2020`, na.rm = TRUE)
class(blah33)
names(blah33)

mean(blah33[,3:5], na.rm = TRUE)
  #group_by(School) %>% 
  View()
  #filter(starts_with("GR") < 22.5 & starts_with("Hudson") < 22.5) %>% 
  #summarise(n = n())
  #summarise(across(`GR Riverside_2020`:`GR Riverside_2018`, mean, na.rm= TRUE)) %>% 
  summarise_at(vars(`GR Riverside_2020`:`GR Riverside_2018`), mean, na.rm = TRUE) %>% 
  View()
  summarise(Median.c1 = median[,3:5], na.rm = TRUE,
            Mean.c1 = mean[,3:5], na.rm = TRUE,
            Median.c2 = median[,6:8], na.rm = TRUE,
            Mean.c2 = mean[,6:8], na.rm = TRUE,
            diff = Mean.c2 - Mean.c1,
            diff.median = Median.c2 - Median.c1) %>% 
  View()

names(blah3)
    
blah3 %>% 
  mutate(c1 = `GR Riverside_2019`,
         c2 = `Portage_2019`) %>% 
  filter(c1 != "NA" & c2 != "NA") %>% 
  select(Athlete, School, c1, c2) %>% 
    #View()
    #group_by(School) %>% 
  group_by(Athlete) %>% 
    filter(c1 < 21.5 & c2 < 21.5) %>% 
  #summarise_all(mean, na.rm = TRUE)
    #summarise(n = n())
    summarise(#Median.c1 = median(c1, na.rm = TRUE),
              Mean.c1 = round(mean(c1, na.rm = TRUE), 4),
              #Median.c2 = median(c2, na.rm = TRUE),
              Mean.c2 = mean(c2, na.rm = TRUE),
              diff = Mean.c2 - Mean.c1#,
              #diff.median = Median.c2 - Median.c1
              ) %>% 
  summarise(mean.course1 = round(mean(Mean.c1),digits = 4),
            mean.course2 = mean(Mean.c2))
    View()


blah3 %>%
  select(Athlete, School, `Huron Meadows_2020`) %>% 
  View()
    
blah3 %>% 
  group_by(School) %>% 
  summarise_at(vars(`MIS_2018`: `Huron Meadows_2019`), funs(mean, median), na.rm = TRUE) %>% 
  View()
  #summarise_at(vars(`MIS_2018`: `Huron Meadows_2019`),mean, na.rm = TRUE) 

blahblah3 <- as.tibble(blah3)

blahblah3 %>% 
  #group_by(School) %>% 
  summarise_at(vars(`MIS_2018`: `Huron Meadows_2019`), funs(mean, median), na.rm = TRUE) %>% 
  View()
  summarise(my.mean = mean(`MIS_2018`, na.rm = TRUE))




# Stopped here 10/14/20

###########################
###########################
###########################


mygraph <- blah2 %>% 
  filter(ICP != "NA" & HM != "NA") %>% 
  ggplot(aes(y = ICP * 60, x = HM * 60)) +
  geom_point() +
  scale_y_time(labels = time_format("%M:%S"),
               name = c1) +
  scale_x_time(labels = time_format("%M:%S"),
               name = c2) +
  ggtitle(paste(c1, "and", c2, "Girls HS XC")) +
  geom_smooth(method = 'lm', formula = y~x, se = FALSE) +
  theme_bw() 

mygraph

lj.df <- left_join(ic3, blah3, by = "Athlete")
lj.df %>% View()
names(lj.df)

lj.df2 <- lj.df %>% 
  mutate(hm2 = HM,
         ic2 = ICP) %>% 
  select(-c(HM, ICP, X, Mean.Race, Course))

names(lj.df2)
lj.df2 %>% 
  View()

lj.df3 <- lj.df2 %>% 
  distinct(Athlete, .keep_all = TRUE) %>% 
  #mutate(hm3 = ifelse(School == 'Saline', Time2, hm2),
  #       ic3 = ifelse(School == 'Saline', Time2, ic2)) %>% 
  filter(School == 'Saline') %>% 
  mutate(HM = hm2,
         ICP = ic2) 

lj.df3 %>% View()

cb.orange <- "#E69F00"
cb.purple <- "#CC79A7"

mg2 <- mygraph + geom_point(data = lj.df3, 
                            color = cb.orange,
                            size = 5) 


mg2
names(blah2)
names(lj.df3)

lj.df4 <- lj.df2 %>% 
  distinct(Athlete, .keep_all = TRUE) %>% 
  filter(School == 'Ann Arbor Pioneer') %>% 
  mutate(HM = hm2,
         ICP = ic2) 


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

