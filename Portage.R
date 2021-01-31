# Tom Alig
# Oct 5, 2019
# Attempt to web scrape HS girls 2018 Bath XC times

library(rvest)
library(plyr)
library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(RCurl)
library(XML)
library(RSelenium)
source("RunningProjectR/scripts/my_track_scripts.R")

# First, scrape the data
# Then, save as .csv with correct naming protocol

# Scrape web page for data using function
# Web page from athletic.net

# start up local selenium server 
rD <- rsDriver()
remDr <- rD$client



base_url <- 'https://www.athletic.net/CrossCountry/meet/157015/results/654408'
webpage <- read_html(base_url)
dim(webpage)
webpage$node

#main_page %>% html_nodes(".sx-price-large") %>% html_text()
scrape_school <- webpage %>% 
  html_nodes(".mw-225") %>% 
  html_text()

scrape_school















webpage %>% 
  html_nodes('.div') %>%    # select enclosing table node
  html_text()

#artist <- html_nodes(webpage, ".chart-row__artist")
artist <- html_nodes(webpage, ".row+ div")
artist <- as.character(html_text(artist))

head(artist)



  fetchAll <- function(page) {
  url <- ('https://www.athletic.net/CrossCountry/meet/144339/results/606036/')
  url2 <- read_html(url)
  url3 <- url2%>%html_nodes("table")%>%.[1]%>%html_table(fill = TRUE)
  url4 <- data.frame(url3)
}

X <- read.csv("https://www.athletic.net/CrossCountry/meet/144339/results/606036",
              header = FALSE)

head(X[1:25,])

URL <- "https://www.athletic.net/CrossCountry/meet/144339/results/606036"
x <- getURL(URL)

head(x)


allData <- ldply(0:2,fetchAll, .progress="text")

# What year is this from?
year.raw <- 2018
head(allData)

a <- readLines('https://www.athletic.net/CrossCountry/meet/144339/results/606036')
b <- read.table(text=a, header = FALSE)



url <- ('https://www.athletic.net/CrossCountry/meet/144339/results/606036')
urldata <- getURL(url)
aa <- readHTMLTable(urldata)#, stringsAsFactors = FALSE)
head(aa)
str(aa)
aa


str(a)
a[16:25]

goodlines = '<a href="/title[^>]*>(.*)</a>.*$|^ *\\$'
try = grep(goodlines,a,value=TRUE)
try[1:10]



lego_movie <- read_html("https://www.athletic.net/CrossCountry/meet/144339/results/606036")
head(lego_movie)
str(lego_movie)
rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating
lego_movie$node
lego_movie$doc


# Add the year to the df
allData2 <- allData
allData2 <- as.tibble(allData2)
allData3 <- allData2 %>% 
  mutate(year = year.raw)
allData3

# save the raw data as .csv
blah <- scrape.to.csv(allData3, 'girls', '800-meters', year.raw)
#blah <- scrape.to.csv(allData3, 'girls', '800-meters', '2017')
ll <- length(blah)
blah[ll]
write.csv(allData3, file = blah[ll])
