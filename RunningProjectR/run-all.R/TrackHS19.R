# Tom Alig
# April 7, 2019
# This takes the raw data .csv files and transforms them into 
#   workable R format

library(rvest)
library(plyr)
library(tidyverse)
library(lubridate)
library(scales)
library(hms)
source("RunningProjectR/scripts/my_track_scripts.R")

###########################################################
# Key variables for this file

# Event
event.txform <- "400-meters"
gender.txform <- "girls"
last.year.txfform <- "2019"
years.txform <- "2011-2019"
q.std.event <- qualify.std.400.girls

# Qualifying standard for data processing purposes only
qualify.std.400.girls <- 1 + (10/60)
qualify.std.800.girls <- 2.75


###########################################################
##
#     GET FILE NAME(S) DYNAMICALLY, AND NAME THE VARIABLE

# Be able to retrieve the raw data files dynamically
flist.track <- list.files(path = "RunningProjectR/data-raw",
                       #pattern = "high-school_girls_800-meters",
                       pattern = paste("high-school_girls_",
                                       event.txform, sep = ""),
                       full.names = T)

flist.track

######################################

# Merge the files from raw data
ldf <- lapply(flist.track , read.csv)
df.final <- do.call("rbind", ldf)
df.final <- as_tibble(df.final)

# Re-name the combined file
my.800m <- df.final
dim(my.800m)
my.800m %>% 
  View

# Run functions
my.800m <- change.default.col.names(my.800m)
my.800m <- delete.rows(my.800m)
my.800m <- get.date.format(my.800m)
my.800m <- grade.to.numeric(my.800m)

# Use this for most events
#my.800m <- time.to.decimal(my.800m)

# Use this for when events have some times both less than one minute
#  and some times greater than one minute

my.800m <- my.800m %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(blah = ifelse(minute == 60, 60, minute/60)) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = ifelse(minute == 60, blah + second, 
                             paste(blah, ".", second, sep = "")))
  

# Filter to get Grade > 5, and < 20, for data validation purposes
my.800m <- my.800m %>% 
  filter(Grade <= 20 &
           Grade > 5)

# Get total.secs to be numeric
my.800m$total.secs <- as.numeric(as.character(my.800m$total.secs))

# Function for time format
my.800m <- my.800m %>% 
  mutate(time2 = total.secs/60)

#my.800m <- mins.secs(my.800m)

dim(my.800m)
# Filter to get qualifying time for data set purposes
my.800m <- my.800m %>% 
  filter(time2 <= q.std.event)

filename <- paste("RunningProjectR/data-after_raw/",
                  last.year.txfform,
                  "-12-31_high-school-",
                  gender.txform,
                  "_",
                  event.txform,
                  "_combined_",
                  years.txform,
                  ".csv",
                  sep = "")

filename
# Write file to .csv
write.csv(my.800m, filename)
          #"RunningProjectR/data-after_raw/2019-12-31_high-school-girls_800-meters_combined_2011-19.csv")


########################################################################
## Discarded Code Below
#   |||||
#   vvvvv


# Add variable for Conference        
my.800m2 <- my.800m2 %>% 
  mutate(conf = Team) 

my.800m2 %>% 
  arrange(time2)

my.800m2 %>% 
  spread(key = Athlete, value = year)

# Make copy of the data
eight.2018.b <- eight.2018
eight.2018.b <- as.tibble(eight.2018.b)
eight.2018.b

# Change column names to get what we want
#  Create a function for this since will be using it many times
eight.2018.b <- change.default.col.names(eight.2018.b)
eight.2018.b 

# Change "Grade" to numeric for ease of manipulation later
eight.2018.c <- grade.to.numeric(eight.2018.b)

# Get minutes, seconds in decimal format (Goal is to get mins and seconds into format
#   that can be manipulated)
eight.2018.d <- time.to.decimal(eight.2018.c)
eight.2018.d

# Add year into the data frame
eight.2018.e <- add.year(eight.2018.d)
eight.2018.e

# Next, delete unneeded rows
eight.2018.f <- delete.rows(eight.2018.e)
eight.2018.f

# Filter for those who meet criteria
#  For girls 800, filter for all times at or under 2:45
eight.2018.g <- eight.2018.f %>% 
  filter(Time <= 2.75)

# Filter to put time into mins/secs, and for grade > 8
eight.2018.g <- mins.secs(grade.filter(eight.2018.g))
eight.2018.g

# Add variable for Conference        
eight.2018.h <- eight.2018.g2 %>% 
  mutate(conf = Team)

eight.2018.h

# Filter for Saline's conference                
SalineConference <- c("Ann Arbor Pioneer",
                      "Ann Arbor Huron",
                      "Ann Arbor Skyline",
                      "Saline",
                      "Monroe",
                      "Temperance Bedford")


# Graph all results. Code for grade.
eight.2018.h %>% 
  filter(Team == 'Saline') %>% 
  ggplot(aes(x = Grade, y = time2, na.rm = TRUE,
                     color = Grade)) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(9, 12, 1))
  
# Scatterplot By Grade
eight.2018.h %>% 
  filter(Grade == '9') %>%
  #mutate(time2 = seconds_to_period(Time)) %>% 
  ggplot(aes(x = time2 * 60, na.rm = TRUE)) +
  geom_histogram(#binwidth = 5, 
                   closed = "left",
                   boundary = .01,
                   fill = "gold", 
                   color = "blue") +
  scale_x_time(labels = date_format("%M:%S"),
               name = "Time In Minutes")

#Boxplot
eight.2018.h %>% 
  #mutate(conf = ifelse(conf %in% SalineConference, 
   #                    'SEC-R', conf)) %>% 
  #filter(conf == 'SEC-R') %>% 
  ggplot(mapping = aes(x = Grade, y = time2, group = 1)) +
  geom_boxplot(width = 0.5) +
  scale_x_continuous(breaks = c(9, 10, 11, 12)) 
  #scale_y_continuous(breaks = c(2, 2.5, 2.55, 2.60, 2.65, 3))

# Min, Max, Median by Grade  
eight.2018.h %>% 
  group_by(Grade, na.rm = TRUE) %>% 
  dplyr::summarize(median = median(time2, na.rm = TRUE),
                   min = min(time2, na.rm = TRUE),
                   max = max(time2, na.rm = TRUE)) %>% 
  select(-na.rm)

names(eight.2018.h)

# Gets count by athlete name. Want to make sure we don't have duplicates
#   Do need to check common names to verify each one is unique
eight.2018.g %>% 
  select(Athlete, Team) %>% 
  dplyr::count(Athlete, Team) %>% 
  arrange(desc(n)) %>% 
  filter(n > 2) 


eight.2018.g %>% 
  mutate(conf = Team, 
         conf = ifelse(conf %in% SalineConference, 
                       'SEC-R', conf)) %>% 
  filter(Team == 'Saline') %>% 
  View()




# Plot Saline Conference 800 runners
eight.2018.h %>% 
  mutate(conf = ifelse(conf %in% SalineConference, 
                       'SEC-R', conf)) %>% 
  filter(conf == 'SEC-R') %>% 
  ggplot(aes(x = Grade, y = time2, na.rm = TRUE,
           shape = Team)) +
  geom_point(size = 2.5, position = 'jitter') +
  scale_x_continuous(breaks = seq(9, 12, 1)) 

# Saline runners in the state  of MI
set.seed(3900)
eight.2018.h %>% 
  filter(State == 'MI') %>%
  ggplot(aes(x = Grade, y = time2, na.rm = TRUE,
             color = Team == "Saline",
             size = Team == 'Saline',
             fill = Team == 'Saline',
             shape = Team == 'Saline')) +
  geom_point(position = 'jitter') + #size = 2.5) +
  scale_size_manual(values = c(1.5, 4)) +
  scale_color_manual(values = c('orange', 'blue')) +
  scale_shape_manual(values = c(1, 21)) +
  scale_fill_manual(values = c(NA, 'blue'),
                    guide = guide_legend(override.aes = list(shape = 21))) +
  scale_x_continuous(breaks = seq(9, 12, 1)) +
  ggtitle("2018 Girls 800m, state of Michigan and Saline HS")
  

# Median, min, max by month
eight.2018.h %>% 
  group_by(month) %>% 
  count(month)# %>% 
  dplyr::summarize(median = median(time2),  
                   min = min(time2),
                   max = max(time2)) %>% 
  arrange(median)
 

    
  


eight.2018.g
# To print to pdf document
first <- paste0("The runner is in ", eight.2018[2,2], "th grade.")
second <- paste0("She ran the ", eight.2018[2,9], " meet." )
pdf('out14.pdf',width=8,height=11)
plot(NA, xlim=c(0,15), ylim=c(0,5), bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
text(1,2,first, pos=4)
text(1,1,second, pos=4)
#text(1,2,c, pos=4)
#text(1,1,d, pos=4)
points(rep(1,2),1:2, pch=15)
dev.off()










# Change column names, 
# and delete column 4,
# and change columns as appropriate to NOT be character

# Col 1 = "Rank" [numeric]
# Col 2 = 'Grade' [numeric]
# Col 3 = keep as is
# Col 4 = delete
# Col 5 = 'Time' [convert to time] AND [delete 'PR' as appropriate]
# Col 6 = 'State' [keep as character]
# Col 7 = 'Date' [convert to date] AND add year
# Col 8 = keep as is

# Random number generator without repeating same number
#   sample(1:50, 10, replace = FALSE)
#      where 50 is the highest random number, and 10 is the 
#        number of integers we want

###########################
######

# Code from other stuff
# Change Time to be Formal Class 'period' 

allmscc2 <- allmscc %>% 
  mutate(time = lubridate::ms(Time))

# Get minutes, seconds in decimal format (Goal is to get mins and seconds into format
#   that can be manipulated)
allmscc3 <- allmscc2.5 %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = minute + second) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs)) %>% 
  dplyr::rename(Time = mins.dec)


# Change "Grade" to numeric for ease of manipulation later
allmscc2.5 <- allmscc2 %>% 
  mutate(grade22 = as.numeric(levels(Grade))[Grade]) %>% 
  mutate(testgrade = grade22 + 3) %>% 
  select(-Grade, -testgrade) %>% 
  dplyr::rename(Grade = grade22) 

##### / End code from other stuff
##########################

# Then, do same for 2017, 2016, etc
# Then merge them all into one df or tibble
# Then gather and spread as necessary

#########################################
#########################################



#########################################

#Reading the HTML code from the website
webpage <- read_html(url)


#Specifying the url for desired website to be scraped
url <- 'https://www.athletic.net/TrackAndField/Division/Event.aspx?DivID=89402&Event=22'

page <- read_html(url) #Creates an html document from URL
table <- html_table(page, fill = TRUE) #Parses tables into data frames

pages_3to5 <- c('https://www.athletic.net/TrackAndField/Division/Event.aspx?DivID=89402&Event=22') %>% 
  paste0(3:5)%>%
  paste0(c("?ie=UTF8&pageNumber=")) %>%
  paste0(3:5) %>%
  paste0(c("&pageSize=10&sortBy=recent"))
pages_3to5
table2 <- html_table(pages_3to5, fill = TRUE)

class(page)
class(pages_3to5)


pdf(file = 'output1.pdf')
plot(output1)
dev.off()

pdf(file = tf <- tempfile(fileext = ".pdf"))
print(output1)
#paste0("The runner is in ", eight.2018[2,2], "th grade.")
dev.off()
shell.exec(tf)


#plot(g)
dev.off()
shell.exec(tf)

#file.remove("test1.pdf") #cleanup
dev.off()

require(rmarkdown)
my_text <- output1
cat(my_text, sep="  \n", file = "my_text.Rmd")
render("my_text.Rmd", pdf_document())
file.remove("my_text.Rmd") #cleanup

# Delete "PR" at end of Race Time
testing <- eight.2018[2,5]
result <- gsub("PR", "", testing)
str(result)

# Converting to date format
testdate <- eight.2018[2,8]
testdate
str(testdate)

test2 <- as_date(testdate)#, "%b%d")
test2

eight.2018.e %>% 
  filter(Athlete != c('Athlete',
                      "Qualifying Standard",
                      "Automatic Qualifying Standard"),
         !grepl("View Next", State)) %>% 
  View()


eight.2018.f <- eight.2018.e %>% 
  filter(Athlete != c('Athlete',
                      "Qualifying Standard",
                      "Automatic Qualifying Standard"))
eight.2018.g <- filter(eight.2018.f, !grepl("View Next", State))


use.me <- grepl(filename, flist.track)
use.me
zz <- ifelse(use.me == 'TRUE', name.of.event[[2]][3], print("not here"))
ifelse(zz == name.of.event[[2]][3], read.csv(name.of.event[[2]][3], "not this file"))
eight.2018 <- read.csv(use.me)


# This works
eight.2018.g %>% 
  mutate(conf = Team, 
         conf = ifelse(conf == 'Saline', 
                       'SEC-R', 0)) %>% 
  filter(Team == 'Saline')

# This works
eight.2018.g %>% 
  mutate(conf = Team, 
         conf = ifelse((conf == 'Saline' |
                          conf == 'Ann Arbor Pioneer'),
                       'SEC-R', 0)) %>% 
  filter(conf == 'SEC-R')
#`filter(Team == 'Ann Arbor Pioneer'| Team == 'Saline')


#mutate(g = ifelse(condition1, 2, ifelse(condition2, 3, g))


hhh <- read.csv('projectR/data-raw/2018-12-31_high-school_girls_800-meters_2018.csv')
hhh <- as.tibble(hhh)
hhh

hhh2 <- hhh %>% 
  mutate(year4 = year)
hhh2

csvfilename2 <- scrape.to.csv(hhh2, 'girls', '800-meters', '2018')
write.csv(hhh2, file = csvfilename2)


#source("C://Users//TomAlig06.15//Documents//R//Running//hs-track-800m//toms_scripts_for_running//my_functions")

# Scrape web page for data using function
# Web page from athletic.net
# 2017 outdoor girls 800 meters
fetchAll <- function(page) {
  url <- paste0('https://www.athletic.net/TrackAndField/Division/Event.aspx?DivID=80153&Event=22', "&page=", page)
  url2 <- read_html(url)
  url3 <- url2%>%html_nodes("table")%>%.[1]%>%html_table(fill = TRUE)
  url4 <- data.frame(url3)
}

allData <- ldply(0:302, fetchAll, .progress="text")

year.raw <- 2018

allData2 <- allData
allData2 <- as.tibble(allData2)
allData3 <- allData2 %>% 
  mutate(year = year.raw)
allData3

#download as raw data
blah <- scrape.to.csv(allData3, 'girls', '800-meters', year.raw)
#blah <- scrape.to.csv(allData3, 'girls', '800-meters', '2017')
blah
write.csv(allData3, file = blah)


# Dynamically name the file for name of event and for YearMonth
name.of.event <- flist.track %>% 
  str_split(c("_", "/"))

name.of.event

name.of.event2 <- name.of.event[[1]][5] %>% 
  str_split("\\.")

name.of.event2

filename <- paste(name.of.event[[1]][3], name.of.event[[1]][4], name.of.event2[[1]][1],
                  sep = "_")

filename

#         / END NAMING CONVENTION
##

##    /Discarded Code

##########################################
