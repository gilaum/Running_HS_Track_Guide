# Tom Alig
# June 9, 2019
# This takes the data from combine-coll-hs.R and begins the analysis

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
library(ggridges)
source("RunningProjectR/scripts/my_track_scripts.R")

#event <- "800m"
#event2 <- "Girls High School 800 meters"

# Read in the data
comb.800m.c <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs-data-for-coll-athletes_girls_800m_one-row-per-athlete_2011-19.csv")
hs.800.w.comb.800m.long2 <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs-and-college-comb_girls_800m_one-row-per-athlete-per-year_2011-19.csv")
hs.800.one.row <- read.csv("RunningProjectR/data-after_raw/2019-12-31_high-school-girls_800-meters_combined_2011-19.csv")
hs.800.one.row.all.years <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs-and-college-comb_girls_800m_one-row-per-athlete_all-years-one-row_2011-19.csv")

# Rename the data
hs.data.for.coll.ath <- comb.800m.c
all.hs.and.coll.data.long <- hs.800.w.comb.800m.long2

# Get the time data to be in time format
# First, long data, college times
all.hs.and.coll.data.long <- time.to.format(all.hs.and.coll.data.long, 
                                            all.hs.and.coll.data.long$Coll.Time,
                                            "Coll.Time")
all.hs.and.coll.data.long <- all.hs.and.coll.data.long %>% 
  dplyr::rename(Coll.Time = y)

# Second, long data, hs times
all.hs.and.coll.data.long <- time.to.format(all.hs.and.coll.data.long, 
                                            all.hs.and.coll.data.long$HStime,
                                            "HStime")
all.hs.and.coll.data.long <- all.hs.and.coll.data.long %>% 
  dplyr::rename(HStime = y)

# Third, short data, college times
hs.data.for.coll.ath <- time.to.format(hs.data.for.coll.ath, 
                                            hs.data.for.coll.ath$Coll.Time,
                                            "Coll.Time")
hs.data.for.coll.ath <- hs.data.for.coll.ath %>% 
  dplyr::rename(Coll.Time = y)

# Fourth, short data, hs times
hs.data.for.coll.ath <- time.to.format(hs.data.for.coll.ath, 
                                       hs.data.for.coll.ath$Gr9,
                                       "Gr9")
hs.data.for.coll.ath <- hs.data.for.coll.ath %>% 
  dplyr::rename(Gr9 = y)

hs.data.for.coll.ath <- time.to.format(hs.data.for.coll.ath, 
                                       hs.data.for.coll.ath$Gr10,
                                       "Gr10")
hs.data.for.coll.ath <- hs.data.for.coll.ath %>% 
  dplyr::rename(Gr10 = y)

hs.data.for.coll.ath <- time.to.format(hs.data.for.coll.ath, 
                                       hs.data.for.coll.ath$Gr11,
                                       "Gr11")
hs.data.for.coll.ath <- hs.data.for.coll.ath %>% 
  dplyr::rename(Gr11 = y)

hs.data.for.coll.ath <- time.to.format(hs.data.for.coll.ath, 
                                       hs.data.for.coll.ath$Gr12,
                                       "Gr12")
hs.data.for.coll.ath <- hs.data.for.coll.ath %>% 
  dplyr::rename(Gr12 = y)


names(all.hs.and.coll.data.long)
names(hs.data.for.coll.ath)
# View the df
all.hs.and.coll.data.long %>% 
  View()

hs.data.for.coll.ath %>% 
  View()

##########################################################
##########################################################

# Begin the analysis

#  ||||
#  vvvv  

##########################################################
# Set parameters for ggplot shapes, Athlete, College Division, etc
# ||||||
# VVVVVV

# <these were moved to "my_track_scripts.R" in Jan 2020>
#shapes <- c(19, 8) 
#shapes4 <- c(16, 17, 15, 19)
#sn <- "Lydia Alig"
#cdiv <- "D2"
#hsteam <- "Saline"
#years <- "2011 - 2019"

#grade.2019.20 <- 10
#current.school.year.grade <- grade.2019.20
#last.year.grade <- current.school.year.grade - 1

#sn.current.grade <- paste("Gr", current.school.year.grade, sep = "")
#sn.ly.grade <- paste("Gr", last.year.grade, sep = "")

# colors for main use
# colorblind blue
#cb.blue <- "#0072B2"
#cb.orange <- "#E69F00"
#cb.purple <- "#CC79A7"
#cb.six <- c("#000000",  
#            cb.blue, 
#            cb.orange, cb.purple,
#            "#56B4E9", "#009E73" )

#slowest.time <- "2:45"
#floor.perf <- as.hms(2.75*60)
#floor.perf

ath.best.last.year <- hs.800.one.row %>% 
  filter(Athlete == sn,
         Grade == last.year.grade) %>% 
  select(Time)

ath.best.last.year

(ath.best.last.year * 60)

#  ^^^^^^
#  ||||||
# / set parameters
#####################################################
#####################################################
#####################################################

# Now, Graph how the Athlete has performed as compared to college parameter(s)
#   set above

scatterplot.hs.perf2 <- all.hs.and.coll.data.long %>% 
  filter(!is.na(College.Division) |
           Athlete == sn) %>%
  mutate(Col.Div = ifelse(Athlete == sn, sn, College.Division),
         Col.Div = ifelse(Col.Div == "1", "D1", ifelse(Col.Div == "2",
                                                       "D2", 
                                                       ifelse(Col.Div == "3", "D3",
                                                              Col.Div)))) %>% 
  replace_na(list(College.All.Conf = 0)) %>% 
  ggplot(aes(x = Grade, y = HStime, na.rm = TRUE, 
             color = Col.Div,
             size = Athlete == sn,  
             shape = Col.Div
             ))+
  geom_point(position = 'jitter') + 
  scale_x_discrete(limits = c('Gr9', 'Gr10', 'Gr11', 'Gr12')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time In Minutes") +
  scale_color_manual(values = c("black", cb.blue, cb.orange, cb.purple ),
                     limits = c(sn, "D1", "D2", "D3")
  ) +
  scale_size_manual(values = c(2, 5)) +
  scale_shape_manual(values = shapes4,
                     limits = c(sn, "D1", "D2", "D3")) +
  guides(size = FALSE,
         shape = guide_legend(title = NULL),
         color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(axis.text=element_text(size=11.5)) +
  ggtitle(paste(sn,
                " & College Athlete ",
                event2,
                " Performances",
                sep = ""))

set.seed(1106)
scatterplot.allconf.aa <-  all.hs.and.coll.data.long %>% 
  filter((College.All.Conf == 1) |
           College.AA == 1 |
           Athlete == sn) %>%
  mutate(Col.Div = ifelse(Athlete == sn, sn, College.Division),
         Col.Div = ifelse(Col.Div == "1", "D1", ifelse(Col.Div == "2",
                                                       "D2", 
                                                       ifelse(Col.Div == "3", "D3",
                                                              Col.Div)))) %>% 
  ggplot(aes(x = Grade, y = HStime, na.rm = TRUE, 
             color = Col.Div,
             size = Athlete == sn,  
             shape = Col.Div
  ))+
  geom_point(position = 'jitter') + #+ #size = 2.5) + #
  scale_x_discrete(limits = c('Gr9', 'Gr10', 'Gr11', 'Gr12')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time In Minutes") +
  scale_color_manual(values = c("black", cb.blue, cb.orange, cb.purple ),
                     limits = c(sn, "D1", "D2", "D3")
                     ) +
  scale_size_manual(values = c(2, 5)) +
  scale_shape_manual(values = shapes4,
                     limits = c(sn, "D1", "D2", "D3")) +
  guides(size = FALSE,
         shape = guide_legend(title = NULL),
         color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(axis.text=element_text(size=11.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste(
    "College All-Conf & All-American ",
    event2,
    " Performances",
    sep = ""))


##########################################################

scatterplot.hs.perf <- hs.data.for.coll.ath %>% 
  group_by(College.Division) %>% 
  #View()
  filter(!is.na(College.Division)) %>% 
  #View()
  gather(key = "grade", value = "HStime", Gr9:Gr12) %>% 
  select(College.Division, grade, HStime) %>% 
  ggplot(aes(x = grade, y = HStime, na.rm = TRUE,
             color = College.Division,
             shape = College.Division,
             #shape = grade,
             group = College.Division)) +
  geom_point(position = 'jitter',
            size = 2.5) +
  scale_color_manual(values=c(cb.blue, cb.orange, cb.purple)) +
  scale_x_discrete(limits = c('Gr9', 'Gr10',
                              'Gr11', 'Gr12')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time") +
  xlab("High School Grade") +
  theme_bw() +
  theme(axis.text=element_text(size=11.5)) +
  ggtitle(paste("High School",
                event,
                "Times for College Runners")) 

##########################################################
hs.data.for.coll.ath %>%
  View


# Table format median high school times for college runners
mynewdf <-   hs.data.for.coll.ath %>% 
    group_by(College.Division) %>% 
    filter(!is.na(College.Division)) %>% 
    summarise(Gr9 = as.hms(median(Gr9, na.rm = TRUE)),
              Gr10 = as.hms(median(Gr10, na.rm = TRUE)),
              Gr11 = as.hms(median(Gr11, na.rm = TRUE)),
              Gr12 = as.hms(median(Gr12, na.rm = TRUE)))
  
mynewdf    
newdf2 <- as.data.frame(t((mynewdf)))#, stringsAsFactors = FALSE))
newdf2
str(newdf2)
names(newdf2)  
newdf3 <- newdf2[2:5,]
newdf3
  
# table format median high school times for all high school runners (under 2:45)  
all.hs.median <- all.hs.and.coll.data.long %>% 
  mutate(Grade = as.character(Grade)) %>% 
  filter(is.na(College.Division)) %>% 
  group_by(Grade) %>% 
  summarise(median.all.hs = as.hms(median(HStime, na.rm = TRUE)))
#change the Gr9 to Gr09, so it sorts properly
all.hs.median[4,"Grade"] <- "Gr09"
# arrange so Gr09 is first row
all.hs.median <- all.hs.median %>% 
  arrange(Grade)
all.hs.median

all.hs.and.coll.data.long %>% 
  View()


# For the high school team
hs.team.median <- all.hs.and.coll.data.long %>% 
  filter(Team == hsteam) %>% 
  mutate(Grade = as.character(Grade)) %>% 
  group_by(Team) %>%
  group_by(Grade) %>%
  summarise(median.hs.team = as.hms(median(HStime, na.rm = TRUE))) 
#change the Gr9 to Gr09, so it sorts properly
hs.team.median[4,"Grade"] <- "Gr09"
# arrange so Gr09 is first row
hs.team.median <- hs.team.median %>% 
  arrange(Grade)

# Create df for the data we want to graph   
chart1_df <- data.frame(c(newdf3,
                          all.hs.median,
                          hs.team.median))  

chart1_df <- chart1_df %>% 
  rename(median.D1 = "V1",
         median.D2 = "V2",
         median.D3 = "V3") %>% 
  select(-c(Grade.1))

chart1_df

# Get the time data to be in time format
chart1_df <- time.to.format(chart1_df, 
                            chart1_df$median.D1,
                            "median.D1")
chart1_df <- chart1_df %>% 
  rename(median.D1 = y)

chart1_df <- time.to.format(chart1_df, 
                            chart1_df$median.D2,
                            "median.D2")
chart1_df <- chart1_df %>% 
  rename(median.D2 = y)

chart1_df <- time.to.format(chart1_df, 
                            chart1_df$median.D3,
                            "median.D3")
chart1_df <- chart1_df %>% 
  rename(median.D3 = y)

chart1_df <- chart1_df %>% 
  select(Grade, median.hs.team, median.all.hs, median.D1,
         median.D2, median.D3)

chart1_df

# Athlete's times by hs grade
athlete.data <- all.hs.and.coll.data.long %>% 
  filter(Athlete == sn) %>%  
  group_by(Grade) %>% 
  select(HStime)

athlete.data[3,2]

# Add athlete data to df with the median data
chart1.1_df <- as.data.frame(c(chart1_df, athlete.data))
chart1.1_df <- chart1.1_df %>% 
  select(-Grade.1) %>% 
  rename(AthleteTime = HStime) %>% 
  select(Grade, AthleteTime, everything())

# Chart Athlete data as compared to median data
med.hs.line.plot <- chart1.1_df %>% 
  gather(key = "Type", value = "HSmedian", AthleteTime:median.D3) %>% 
  select(Grade, Type, HSmedian) %>% 
  ggplot(aes(x = Grade, y = HSmedian, na.rm = TRUE,
             color = Type,
             size = Type == "AthleteTime",
             group = Type
             )) +
  geom_point(size = 3) + 
  geom_line(size = 2) +
  scale_x_discrete(limits = c('Gr09', 'Gr10',
                              'Gr11', 'Gr12'),
                   labels = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time") +
  scale_colour_manual(values = cb.six,
                      limits = c("AthleteTime", 
                                 "median.D1", 
                                 "median.D2", "median.D3",
                                 "median.all.hs",
                                 "median.hs.team"
                                  ),
                      labels = c(sn, 
                                   "D1", "D2", "D3", "All HS Athletes",
                                  hsteam)
                      ) +
  scale_size_manual(values = c(1, 3)) +
  guides(size = FALSE,
         group = guide_legend(title = NULL)) +
  theme_bw() +
  ggtitle(paste(sn,
                "and Median High School",
                event,
                "Times"))

################################################
# Inter Quartile Range
# Table format median high school times for college runners

all.hs.and.coll.data.long %>% 
  View()

# All high schoolers
boxplot.by.grade <- all.hs.and.coll.data.long %>% 
  ggplot(aes(x = Grade, y = HStime, 
             fill = Grade,
             na.rm = TRUE)) +
  geom_boxplot(alpha = 0.3) +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  ggtitle("Girls HS 800m under 2:45, 2011 - 2019, Boxplot Distribution")

all.hs.and.coll.data.long %>% 
  ggplot(aes(x = Grade, y = HStime, 
             fill = Grade,
             na.rm = TRUE)) +
  geom_boxplot(alpha = 0.3) +
  geom_point(aes(x = "Gr9", y = 151)) +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  ggtitle("Girls HS 800m under 2:45, 2011 - 2019, Boxplot Distribution")



# D1, D2, D3
# D1
boxplotting(all.hs.and.coll.data.long, 
            all.hs.and.coll.data.long$College.Division,
            "D1")
# D2
boxplotting(all.hs.and.coll.data.long, 
            all.hs.and.coll.data.long$College.Division,
            "D2")

# D3
boxplotting(all.hs.and.coll.data.long, 
            all.hs.and.coll.data.long$College.Division,
            "D3")

all.hs.and.coll.data.long %>% 
  #filter(!is.na(College.Division)) %>%
  ggplot(aes(x = Grade, y = HStime, 
             fill = Grade,
             na.rm = TRUE)) +
  geom_boxplot(alpha = 0.3)+
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) + 
  facet_wrap(~College.Division, nrow = 4)


facet.boxplots <- all.hs.and.coll.data.long %>% 
  mutate(group99 = ifelse(Team == hsteam, 
                     hsteam, #College.Division)) %>% 
                     paste("", College.Division, sep = ""))) %>% 
  filter(!is.na(College.Division) | Team == hsteam) %>% 
  ggplot(aes(x = Grade, y = HStime, 
             fill = Grade,
            # size = Athlete == sn,
             na.rm = TRUE)) +
  geom_boxplot(alpha = 0.3)+
  coord_flip() +
  scale_x_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) + 
  theme_bw() +
  theme(axis.text=element_text(size=11.5)) +
  theme(legend.position = "none") +
  ggtitle(paste("High School",
                event,
                "Performances")) +
  facet_wrap(~group99, nrow = 4)


class(all.hs.and.coll.data.long)
str(all.hs.and.coll.data.long$HStime)

# Group (Cut) Data and get Heat Map

breaks <-  seq(2, 2.74, by = 0.0833333)
#breaks2 <- fct_explicit_na(breaks)
labels <- c("2:00 - 2:05", 
            "2:05.01 - 2:10", 
                     "2:10.01 - 2:15", 
                     "2:15.01 - 2:20",
                     "2:20.01 - 2:25",
                     "2:25.01 - 2:30", 
                     "2:30.01 - 2:35",
                     "2:35.01 - 2:40",
                     "2:40.01 - 2:45")

ggrade <- c("Gr9", "Gr10", "Gr11", "Gr12")
#ggrade <- c(9, 10, 11, 12)
ggrade

# Preparation for Heat Map Highlighting and Heat Map Rectangles
#  Need to identify the correct rectangle to be highlighted
#  Creating columns "Gr9rect" (for "Grade 9 rectangle"), etc, helps
#   us ID the correct rectangle
prepa <- all.hs.and.coll.data.long %>% 
  mutate(HStime2 = as.numeric(all.hs.and.coll.data.long$HStime/60)) %>% 
  group_by(Grade, gr = cut(HStime2, breaks= breaks)) %>% 
  mutate(gr2 = ifelse(Athlete == sn, gr, 0)) %>% 
  filter(Athlete == sn) %>% 
  mutate(Gr9rect = ifelse(Grade == "Gr9", gr2, 0),
         Gr10rect = ifelse(Grade == "Gr10", gr2 ,0),
         Gr11rect = ifelse(Grade == "Gr11", gr2 ,0),
         Gr12rect = ifelse(Grade == "Gr12", gr2 ,0))

# Create variable for the Heat Map ID number (x axis)
Gr9hmrect <- prepa$Gr9rect[1]
Gr10hmrect <- prepa$Gr10rect[2]
Gr11hmrect <- prepa$Gr11rect[3]
Gr12hmrect <- prepa$Gr12rect[4]

# Create gg_rect ID's based on correct variable
#  Note: For y-axis, Grade 9 will always have ymin be "0 + 0.5". The
#   first 0 represents the y-axis starting point. Grade 10 ymin will always be
#    "1 + 0.5", and so on for grade 11 and 12.
Gr9ggrect <- aes(xmin = Gr9hmrect - 0.5, xmax = Gr9hmrect + 0.5, 
             ymin = 0 +0.5, ymax = 1 +0.5)

Gr10ggrect <- aes(xmin = Gr10hmrect - 0.5, xmax = Gr10hmrect + 0.5, 
             ymin = 1 +0.5, ymax = 2 +0.5)

Gr11ggrect <- aes(xmin = Gr11hmrect - 0.5, xmax = Gr11hmrect + 0.5, 
                  ymin = 2 +0.5, ymax = 3 +0.5)

Gr12ggrect <- aes(xmin = Gr12hmrect - 0.5, xmax = Gr12hmrect + 0.5, 
                  ymin = 3 +0.5, ymax = 4 +0.5)

heatmap2 <- all.hs.and.coll.data.long %>% 
  mutate(HStime2 = as.numeric(all.hs.and.coll.data.long$HStime/60)) %>% 
  filter(HStime != "NA") %>% 
  group_by(Grade, gr = cut(HStime2, breaks= breaks)) %>% 
  mutate(gr2 = ifelse(Athlete == sn, gr, 0)) %>% 
  #View()
  tally() %>% 
  mutate(pct_by_grade = n/sum(n)) %>% 
  ggplot(aes(x = gr, y = Grade, fill = pct_by_grade)) +
  geom_tile() +
  scale_x_discrete(labels = labels,
                   name = "Time") +
  geom_rect(size = 1, fill = cb.orange, colour = "black",
            Gr9ggrect) +
  geom_rect(size = 1, fill = cb.orange, colour = "black",
            Gr10ggrect) +
  geom_rect(size = 1, fill = cb.orange, colour = "black",
            Gr11ggrect) +
  geom_rect(size = 1, fill = cb.orange, colour = "black",
            Gr12ggrect) +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  scale_y_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  theme_minimal() +
  geom_text(aes(label=percent(pct_by_grade))) +
  theme(axis.text.x = element_text(angle = 30, 
                                   hjust = 1,
                                   vjust = 1)) +
  ggtitle(paste(event2,
                "Time Distributions",
                years)) +
  guides(fill = FALSE) 


all.hs.and.coll.data.long %>% 
  filter(Team == hsteam) %>% 
  View()

names(hs.800.one.row)

# Histogram of last year's grade performance for athlete, combined with 
#  all performances for grade x over time
Hist.Last.Year <- all.hs.and.coll.data.long %>% 
  filter(HStime != "NA") %>% 
  filter(Grade == paste("Gr", last.year.grade, sep = "")) %>% 
  ggplot(aes(x = HStime)) +
  geom_histogram(bins = (((2.75-2)*60))/3.09,
                         fill = cb.orange, 
                         color = cb.blue) +  #14
  geom_vline(aes(xintercept = ifelse(Athlete == sn, HStime, NA)), 
             color = "black", 
             linetype = "dashed", size = 1) + 
  #scale_x_continuous(limits = c(2, 2.75)) +
  scale_x_time(labels = time_format("%M:%S")) +
  scale_y_continuous(labels = comma) +
  labs(title = paste("All ",
                     "Grade ", 
                     last.year.grade,
                     " Girls HS 800m 2011 - 2019",
                     sep = ""),
       subtitle = "Dashed Line = Your Athlete",
       x = "Time") +
  theme_bw() 

#for (i in ggrade) {
#  mydata_id <- subset(all.hs.and.coll.data.long, Grade == i)
#  p <- ggplot(mydata_id, aes(x = HStime)) +
#    geom_histogram(bins = 11, fill = "#F0E442", color = "#0072B2") +
#    geom_vline(aes(xintercept = ifelse(Athlete == sn, HStime, NA)), 
#               color = "#0072B2", 
#               linetype = "dashed", size = 1) + 
#    scale_x_time(labels = time_format("%M:%S")) +
#    scale_y_continuous(labels = comma) +
#    ggtitle(i)
#  png(paste("plot_", i, ".png", sep = ""))
#  print(p)
#  dev.off()
#}

#all.hs.and.coll.data.long %>%
#  View()

# Histogram for all HS 800m 2011- 2019
Histogram.All <- all.hs.and.coll.data.long %>% 
  filter(HStime != "NA") %>% 
  ggplot(aes(x = HStime)) +
  geom_histogram(bins = 18, fill = cb.orange, color = cb.blue) + #18
  geom_vline(aes(xintercept = ifelse(Athlete == sn, HStime, NA)), 
             color = "black", 
             linetype = "dashed", size = 1) + 
  scale_x_time(labels = time_format("%M:%S")) +
  scale_y_continuous(labels = comma) +
  labs(title = paste("All",
                     event2,
                     years),
       subtitle = "Dashed Line = Your Athlete",
       x = "Time") +
  theme_bw()


# Histogram for all HS 800m 2011- 2019
# First, define the breaks
vvalues <- c(2, 2.25, 2.4166667, 2.583333)
vvalues2 <- 2.75

#for (i in vvalues) {
#  mydata_id <- subset(hs.800.one.row, Time < i)
#  p <- ggplot(mydata_id, aes(x = year)) +
#    geom_bar(fill = "#F0E442", color = "#0072B2") +
#    scale_x_continuous(breaks = seq(2011, 2019, 1)) +
#    scale_y_continuous(labels = comma) +
#    ggtitle(i)
#  png(paste("plot_", i, ".png", sep = ""))
#  print(p)
#  dev.off()
#}

num.of.participants <- hs.800.one.row %>% 
  #filter(Time != "NA") %>% 
  filter(Time < 2.76) %>% 
 # group_by(year) %>% 
  ggplot(aes(x = year)) +
  geom_bar(fill = cb.orange, color = cb.blue) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(labels = comma) +
  ggtitle(paste("Count of",
                event2,
                "under",
                slowest.time,
                ",",
                years)) +
  theme_bw()


hs.800.one.row %>% 
  group_by(year) %>% 
  summarise(median.time = median(Time))


#detach(package:plyr)


median.mean.barplot <- hs.800.one.row %>% 
  group_by(year) %>% 
  summarise(median.time = (median(Time)),
            mean.time = mean(Time)) %>% 
  mutate(med2 = round(median.time * 60, 2),
         mean2 = round(mean.time * 60, 2)) %>% 
  gather(med2, mean2, key = "type", value = "timess") %>% 
  mutate(timess2 = as_hms(timess)) %>%
  ggplot(aes(x = year, y = timess, fill = type)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_hline(aes(yintercept = as.numeric(unlist(ath.best.last.year * 60))),
             linetype = 'dashed',
             color = "black",
             size = 2) +
  scale_fill_manual(values = c(cb.blue, cb.orange),
                     labels = c("Median", "Mean")) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  coord_cartesian(ylim = c(120, 165)) +
  scale_y_time(labels = time_format("%M:%S")) +
  theme_bw() +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(fill = "") +
  xlab("Year") +
  ylab("Time") +
  labs(title = paste("Mean and Median", 
                     event2,
                     "under",
                     slowest.time, 
                     ",",
                     years),
       caption = "Dashed Line = Your Athlete")
  
# All Grade 9 HS times
blah <- all.hs.and.coll.data.long %>% 
  mutate(HStime2 = as.numeric(all.hs.and.coll.data.long$HStime/60)) %>% 
  filter(Grade == "Gr9" & HStime != "NA") %>% 
  #View()
  group_by(gr = cut(HStime2, breaks= seq(2, 2.75, by = 0.15)))  %>% 
  summarise(n= n()) %>%
  arrange(as.numeric(gr))

blah

class(blah)
str(blah)

# Add new column
blah$Grade <- c(9, 9, 9, 9, 9)
str(blah)
blah$gr <- lubridate::hms(blah$gr)
blah


ggplot(blah, aes(x = gr, y = Grade, fill = n)) +
  geom_tile() +
  scale_x_discrete(labels = c("2:00 - 2:09", "2:09.01 - 2:18", 
                              "2:18.01 - 2:27", 
                              "2:27.01 - 2:36",
                              "2:36.01 - 2:45"),
               #breaks = as.hms("00:00:10"),
               name = "Time In Minutes") +
  #scale_y_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  scale_fill_distiller(palette = "Reds", direction = +1) +
  ggtitle("All HS 9th Grade 800m Time Distributions, 2011 - 2019") +
  theme_minimal()

all.hs.and.coll.data.long %>% 
  View()



hs.800.one.row.all.years <- time.to.format(hs.800.one.row.all.years, 
                                          hs.800.one.row.all.years$Gr9.x, 
                                          "Gr9.x")
hs.800.one.row.all.years <- hs.800.one.row.all.years %>% 
  rename(Gr9 = y)

hs.800.one.row.all.years <- time.to.format(hs.800.one.row.all.years, 
                                          hs.800.one.row.all.years$Gr10.x, "Gr10.x")
hs.800.one.row.all.years <- hs.800.one.row.all.years %>% 
  rename(Gr10 = y)

hs.800.one.row.all.years <- time.to.format(hs.800.one.row.all.years, 
                                          hs.800.one.row.all.years$Gr11.x, "Gr11.x")
hs.800.one.row.all.years <- hs.800.one.row.all.years %>% 
  rename(Gr11 = y)

hs.800.one.row.all.years <- time.to.format(hs.800.one.row.all.years, 
                                          hs.800.one.row.all.years$Gr12.x, "Gr12.x")
hs.800.one.row.all.years <- hs.800.one.row.all.years %>% 
  rename(Gr12 = y)

# Re-arrange the columns
hs.800.one.row.all.years <- hs.800.one.row.all.years %>% 
  select(Athlete, State, Gr9, Gr10, Gr11, Gr12, everything())



# Find improvements year to year
#  9th grade to 10th grade
#  negative number = improvement
#  positive number = worse

improve <- hs.800.one.row.all.years %>% 
  mutate(Gr9b = as.numeric(hs.800.one.row.all.years$Gr9/60),
         Gr10b = as.numeric(hs.800.one.row.all.years$Gr10/60),
         Gr11b = as.numeric(hs.800.one.row.all.years$Gr11/60),
         Gr12b = as.numeric(hs.800.one.row.all.years$Gr12/60)) %>% 
  #filter(Gr9 != "NA" & Gr10 != "NA") %>% 
  #View()
  mutate(improve.9.to.10 = Gr10b - Gr9b,
         improve.9.to.11 = Gr11b - Gr9b,
         improve.9.to.12 = Gr12b - Gr9b,
         improve.10.to.11 = Gr11b - Gr10b,
         improve.10.to.12 = Gr12b - Gr10b,
         improve.11.to.12 = Gr12b - Gr11b)

improve %>% 
  View()

#write.csv(improve, 
#          file = "RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_improve_one-row-per-athlete_2011-19.csv")


str(improve)
#improve.bins <- c(2, 2.25, 2.4166667, 2.583333)

improve2 <- improve %>% 
  gather(improve.9.to.10, improve.9.to.11, improve.9.to.12,
         improve.10.to.11, improve.10.to.12, improve.11.to.12,
         key = "improve.grade", value = "improve.time")
  #View()

#write.csv(improve2, 
 #         file = "RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_improve_one-row-per-athlete-per-year_2011-19.csv")

improve.stats <- improve %>% 
  select(improve.9.to.10:improve.11.to.12) %>% 
  summarize_all(list(mean = mean, median = median, sd = sd,
                     IQR = IQR),
                na.rm = TRUE) 
dim(improve.stats)
improve.stats

#improve2 %>% 
  #View()
#  filter(Gr9b > 2.5) %>% 
  #filter(!is.na(Gr9b)) %>% 
#  ggplot(aes(x = improve.time * 60)) +
#  geom_histogram()
             

mean.x <- (improve.stats$improve.9.to.10_mean * 60)
mean.x
improve2 %>% 
  #filter(improve.grade == "improve.9.to.10") %>% 
  #summarize_all(list(mean = mean(improve2$improve.time))) %>% 
  #summarise(mean = mean(improve2$improve.time * 60)) %>% 
  ggplot(aes(x = improve.time * 60)) +
  geom_histogram(bins = 20, fill = cb.orange, color = cb.blue) +
  geom_vline(aes(xintercept = mean.x), 
             color = cb.blue, 
             linetype = "dashed", size = 1) + 
  xlab("Improvement in Seconds (Negative Number = Improvement)") +
  scale_x_continuous(breaks = seq(-20,20,5)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Girls HS 800m 2011 - 2019, Improvements all grades",
       subtitle = "Dashed Line = Average Improvement Grade 9 to Grade 10")

# Improvements for each grade
improve.dist <- ggplot(improve2, aes(x = improve.time * 60, y = improve.grade)) +
  #geom_density_ridges(stat="binline", bins=20) +
  geom_density_ridges(fill = cb.orange,
                      color = cb.blue) +
  theme_ridges(font_size = 13, grid = TRUE) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20)) +
  #scale_y_discrete(limits = c("improve.9.to.10",
   #                           "improve.9.to.11")) +
  theme(axis.title.y = element_blank()) +
  labs(title = "HS Girls 800m Improvements Year to Year",
       x = "Improvement [seconds]; Negative = Improvement")
  
  



boxplot(improve$improve.9.to.10)
plot(improve$improve.9.to.10)
barplot(improve$improve.9.to.10)







####

#all.hs.and.coll.data.long %>% 
#  mutate(HStime2 = as.numeric(all.hs.and.coll.data.long$HStime/60)) %>% 
#  filter((Grade == "Gr9" | Grade == "Gr10") & HStime != "NA") %>% 
#  group_by(Athlete) %>% 
#  summarize(median.imp = HStime2 & Grade == "Gr9")
#  View()

#hs.800.one.row %>% 
#  View()

#########################
#########################
#  Discarded Code
#  ||||||
#  VVVVVV

#########################

  
# /discarded code
#######################################
