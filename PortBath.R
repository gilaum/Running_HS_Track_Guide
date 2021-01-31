# Tom Alig
# Oct 11, 2019
# Are Bath times or Portage times faster?

#library(rvest)
#library(plyr)
library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
source("RunningProjectR/scripts/my_track_scripts.R")

# Took the girls xc data from 2017, 2018, 2019 to do the analysis

# Read in the data
port.bath <- read.csv("Portage Bath Compare.csv")

head(port.bath)
str(port.bath)

port.bath2 <- port.bath

port.bath3 <- port.bath2 %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = round((minute + second), digits = 2)) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs))  %>% 
  dplyr::rename(Time = mins.dec)

head(port.bath3)
str(port.bath3)
(port.bath3[2,6] - port.bath3[6,6] )*60

# What are median and mean values for all years combined, by Meet
port.bath3 %>% 
  group_by(Meet) %>% 
  summarise(Median = median(Time, na.rm = TRUE),
            Mean.Race = mean(Time, na.rm = TRUE))  


# We want to compare the athletes who ran both Portage and Bath in the same year
#  So, we need to remove all those who ran only one of the races in a given year

# Spread the data
data.spread <- port.bath3 %>% 
  spread(key = Meet, value = Time) %>% 
  group_by(Athlete, Year) %>% 
  filter(Bath != "NA" & Portage != "NA") %>% 
  mutate(Diff = Bath - Portage)

data.spread %>% 
  View()
  
# By year, what are the median and mean values?
p1 <- data.spread %>% 
  group_by(Year) %>% 
  summarise(BathMed = round(median(Bath, na.rm = TRUE), digits = 2),
            PortMed = round(median(Portage, na.rm = TRUE), digits = 2),
            BathMean = round(mean(Bath, na.rm = TRUE), digits = 2),
            PortMean = round(mean(Portage, na.rm = TRUE), digits = 2)) %>% 
  mutate(DecDiffMed = BathMed - PortMed) %>% 
  mutate(SecDiffMedian = DecDiffMed * 60) %>% 
  mutate(DecDiffMean = BathMean - PortMean) %>% 
  mutate(SecsDiffMean = DecDiffMean * 60) 
p1

sprintf("%02d:%02d",x%/%60,x%%60)

p1 %>% 
  mutate(newtimeblah = BathMed * 60) %>% 
  mutate(newtime2 = sprintf("%02g:%02g", newtimeblah%/%60,newtimeblah%%60)) %>% 
  select(newtime2, everything()) %>% 
  View()

ggplot(data = p1, aes(x = Year, y = c(PortMed, BathMed))) +
  geom_bar(stat = "identity")

p1 %>% 
  mutate(time2 = ms(PortMed)) %>% 
  select(time2, everything())

p1 %>% 
  gather(BathMed, BathMean, PortMed, PortMean, key = "Meet", value = "time") %>% 
  select(-c(DecDiffMed, SecDiffMedian, DecDiffMean, SecsDiffMean)) %>% 
  mutate(newtimeblah = time * 60) %>%
  mutate(time2 = sprintf("%02g:%02g", newtimeblah%/%60,newtimeblah%%60)) %>% 
  ggplot(aes(x = Year, y = time, fill = Meet)) +#, color  = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = time2),
            position = position_dodge(0.9),
            vjust = 1.5) +
  ylab("Time in Minutes") +
  ggtitle("Bath and Portage Comparison, MI HS XC, 2017-19")
  #scale_y_discrete(limits = c("0", "21"))
  #scale_y_time(labels = date_format("%M:%S"))

# Are the differences statistically significant?
#  Need to use the data in the original (aka "long") form
#   But only use the Athletes who peformed in both races
#    So, we need to "gather" the data.spread df

data.gather <- data.spread %>% 
  gather(Bath, Portage, key = "Meet", value = "Time") 
    
# Now, filter for year, so that we can run stat analyses on each year
# For 2017
data.gather17 <- data.gather %>% 
  filter(Year == "2017")# %>% 
  View()

m17 <- lm(Time ~ Meet , data = data.gather17)
summary(m17)

# For 2018
data.gather18 <- data.gather %>% 
  filter(Year == "2018")# %>% 
  View()

m18 <- lm(Time ~ Meet , data = data.gather18)
summary(m18)

# For 2019
data.gather19 <- data.gather %>% 
  filter(Year == "2019")

m19 <- lm(Time ~ Meet , data = data.gather19)
summary(m19)

# Overall, regardless of year

m <- lm(Time ~ Meet , data = data.gather)
summary(m)


####
##    FINDINGS
####

# Statistically significant different times in 2017
# NOT Statistically significant different times in 2018, 2019
# NOT Statistically significant different times when NOT filtering by Year

# By school, what are the median values?
data.spread %>% 
  group_by(School) %>% 
  summarise(BathMedian = round(median(Bath, na.rm = TRUE), digits = 2),
            PortageMedian = median(Portage, na.rm = TRUE)) %>% 
  mutate(DecDiff = BathMedian - PortageMedian) %>% 
  mutate(SecondsDiff = DecDiff * 60)


########################################

#  Discarded Code
#  |||
#  VVV
port.bath3 <- time.to.format(port.bath3, 
                             port.bath2$time,
                             "time")

port.bath2$Time[1:33]
port.bath$Time[1:33]
port.bath %>% 
  mutate(blah = as.numeric(as.character(Time))) %>% 
  mutate(blah2 = ms(blah))

str(port.bath)

port.bath2 <- port.bath2 %>% 
  mutate(Time = y) %>% 
  select(-y)

str(port.bath2)

port.bath4 <- port.bath3 %>% 
  mutate(newtime = ms(Time))

port.bath4 %>% 
  group_by(Meet) %>% 
  summarise(Median = median(newtime, na.rm = TRUE))

head(port.bath4)

port.bath3 %>% 
  dplyr::count(Athlete, Year) %>% 
  filter(n > 1) %>% 
  select(everything())



port.bath3 %>% 
  group_by(Meet, Year) %>% 
  summarise(Median = median(Time, na.rm = TRUE))

