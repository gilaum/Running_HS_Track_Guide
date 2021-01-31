# Tom Alig
# April 7, 2019
# Attempt to web scrape HS girls 800 meter track times

library(rvest)
library(plyr)
library(tidyverse)
library(lubridate)
library(scales)
library(hms)
source("RunningProjectR/scripts/my_track_scripts.R")

# First, scrape the data
# Then, save as .csv with correct naming protocol

# Scrape web page for data using function
# Web page from athletic.net

# Boys or Girls?
gender.scrape <- "girls"

# Event
event.scrape <- "1600-meters"

# What year is this from?
year.raw <- 2011

fetchAll <- function(page) {
  url <- paste0('https://www.athletic.net/TrackAndField/Division/Event.aspx?DivID=28139&Event=53', "&page=", page)
  url2 <- read_html(url)
  url3 <- url2%>%html_nodes("table")%>%.[1]%>%html_table(fill = TRUE)
  url4 <- data.frame(url3)
}

allData <- ldply(0:302, fetchAll, .progress="text") #302
#tail(allData)



# Add the year to the df
allData2 <- allData
allData2 <- as_tibble(allData2)
allData3 <- allData2 %>% 
  mutate(year = year.raw)
allData3
tail(allData3)
# save the raw data as .csv
#blah <- scrape.to.csv(allData3, 'girls', '800-meters', year.raw)
blah <- scrape.to.csv(allData3, gender.scrape, event.scrape, year.raw)

ll <- length(blah)
blah[ll]
write.csv(allData3, file = blah[ll])

