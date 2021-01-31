# Tom Alig
# May 13, 2019
# This takes the after raw data .csv files, as well as the raw data
#    college .csv files, merges them, and begins to perform  data analyses

#library(rvest)
library(plyr)
library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
source("RunningProjectR/scripts/my_track_scripts.R")

###########################################################
# Key variables for this file
# ||||||
# VVVVVV

# Event
event.combine <- "400-meters"
gender.combine <- "girls"
last.year.combine <- "2019"
years.combine <- "2011-2019"

# ^^^^^^
# ||||||
# /key variables for this file
###########################################################

# Be able to retrieve the raw data files dynamically
flist.track.comb <- list.files(path = "RunningProjectR/data-after_raw",
                          #pattern = "high-school-girls_800-meters",
                          pattern = paste("_high-school-",
                                          gender.combine,
                                          "_",
                                          event.combine, sep = ""),
                          full.names = T)

flist.track.comb

flist.college <- list.files(path = "RunningProjectR/data-raw",
                            pattern = "_college_",
                            full.names = T)

flist.college

######################
# Merge the college and hs df together using inner join

# Dynamically name file to read in
read.in.this <- paste("RunningProjectR/data-after_raw/",
                      last.year.combine,
                      "-12-31_high-school-",
                      gender.combine,
                      "_",
                      event.combine,
                      "_combined_",
                      years.combine,
                      ".csv",
                      sep = "")

read.in.this

# First, read in the data
hs.800 <- read.csv(read.in.this)
# Now read in the college data
coll.800 <- read.csv("RunningProjectR/data-raw/2018-12-31_college_women_800-meters_2018.csv")

hs.800 <- as_tibble(hs.800)
hs.800 %>% 
  View()

coll.800 <- as_tibble(coll.800)
coll.800 %>% 
  View()

# Format names of Athlete to be First Last (as opposed to First LAST)
coll.800$Athlete <- tolower(coll.800$Athlete)
coll.800$Athlete <- toTitleCase(coll.800$Athlete)

hs.800.one.row <- hs.800 %>% 
  #select(-c(Meet, Team, X.1, X, year, month, Time, yeardaymonth)) %>% 
  select(-c(Meet, X.1, X, year, month, Time, yeardaymonth)) %>%  # This line keeps 'Team' as a variable
  #select(-c(Meet, X.1, X, Time, yeardaymonth)) %>% 
  distinct(Athlete, Grade, State, .keep_all = TRUE) %>% 
  spread(key = Grade, value = time2) %>% 
  group_by(Athlete, State)# 

names(hs.800.one.row)

# Delete 6, 7, 8 grade times
#  Change remaining column names to not be numbers
hs.800.one.row <- hs.800.one.row %>% 
  select(-c(`6`, `7`, `8`)) %>% 
  dplyr::rename(Gr9 = `9`,
         Gr10 = `10`,
         Gr11 = `11`,
         Gr12 = `12`)

hs.800.one.row %>% 
  #filter(Athlete == "Emma Call") %>% 
  View()

# Write to csv the file with one row per athlete

# Dynamically name file to read in
read.in.this.2 <- paste("RunningProjectR/data-output/",
                      last.year.combine,
                      "-12-31_high-school-",
                      gender.combine,
                      "_",
                      event.combine,
                      "_one.row.per.athlete_",
                      years.combine,
                      ".csv",
                      sep = "")

read.in.this.2
write.csv(hs.800.one.row, file = read.in.this.2)
#write.csv(hs.800.one.row, file = "RunningProjectR/data-output/2019-12-31_high-school-girls_800m_one.row.per.athlete_2011-19.csv")

###########################
# Read in the hs data (same data with write to csv just above)
#hs.800.one.row <- read.csv("RunningProjectR/data-output/2019-12-31_high-school-girls_800m_one.row.per.athlete_2011-19.csv")
dim(hs.800.one.row)
names(hs.800.one.row)
names(coll.800)
coll.800 %>% 
  View()

# Merge hs and college df for this track and field event
comb.800m <- merge(x = hs.800.one.row, y = coll.800, by = "Athlete")
comb.800m <- as_tibble(comb.800m)                   
comb.800m
comb.800m %>% 
  View()

# Function to get hs times into time format, and then rename columns appropriately
comb.800m <- time.to.format(comb.800m, comb.800m$Gr9, "Gr9")
comb.800m <- comb.800m %>% 
  dplyr::rename(Gr9 = y)
comb.800m <- time.to.format(comb.800m, comb.800m$Gr10, "Gr10")
comb.800m <- comb.800m %>% 
  dplyr::rename(Gr10 = y)
comb.800m <- time.to.format(comb.800m, comb.800m$Gr11, "Gr11")
comb.800m <- comb.800m %>% 
  dplyr::rename(Gr11 = y)
comb.800m <- time.to.format(comb.800m, comb.800m$Gr12, "Gr12")
comb.800m <- comb.800m %>% 
  dplyr::rename(Gr12 = y)

# Works to get Coll Time into time format
comb.800m <- comb.800m %>% 
  mutate(newtime = ms(Coll.Time) + 120) %>% 
  mutate(newtime2 = as.hms(newtime)) %>%
  select(-c(newtime, Coll.Time)) %>% 
  dplyr::rename(Coll.Time = newtime2) %>% 
  select(Athlete, State, Coll.Time, everything())

comb.800m.b <- comb.800m
# Check for duplicate Athlete names
comb.800m.b %>% 
  dplyr::count(Athlete, Name.of.College) %>% 
  filter(n > 1) 


# Find entries with common names, de-duplicate them based on best educated guess
#   Get the closest hs time to college time for given duplicated Athlete name
comb.800m.c <- comb.800m.b %>% 
  group_by(Athlete, State, Team) %>% 
  #group_by(Athlete, Team) %>% 
  mutate(minhs = min(Gr12, Gr11, Gr10, Gr9, na.rm = TRUE)) %>% 
  mutate(minhs2 = minhs/60) %>% 
  group_by(Athlete) %>% 
  filter(minhs2 == min(minhs2)) %>% 
  select(-c(minhs, minhs2))  

comb.800m.c %>% 
  View()

dim(comb.800m.c)

# Filter out these athletes because they are not the same person hs to college
comb.800m.c <- comb.800m.c %>% 
  filter(!(Athlete == "Nicole Montgomery")) %>% 
  filter(!(Athlete =="Mallory King")) %>% 
  filter(!(Athlete =="Brittany Parker")) 

dim(comb.800m.c)


# Check for duplicate Athlete names
#  Should be zero
comb.800m.c %>% 
  dplyr::count(Athlete, Name.of.College) %>% 
  filter(n > 1) 
 
read.in.this.3 <- paste("RunningProjectR/data-after_raw/",
                        last.year.combine,
                        "-12-31_hs-data-for-coll-athletes_",
                        gender.combine,
                        "_",
                        event.combine,
                        "_one.row.per.athlete_",
                        years.combine,
                        ".csv",
                        sep = "")

read.in.this.3
write.csv(comb.800m.c, file = read.in.this.3)
#write.csv(comb.800m.c, file = "RunningProjectR/data-after_raw/2019-12-31_hs-data-for-coll-athletes_girls_800m_one-row-per-athlete_2011-19.csv")

# Now need to get data to be one row per grade, per athlete (make data long,
#  not wide)
comb.800m.long <- comb.800m.c %>% 
  gather(Grade, HStime, Gr12:Gr9)

comb.800m.long %>% 
  View()

names(comb.800m.c)
names(hs.800.one.row)
# Left-join the long college data with the hs data for this event
hs.800.w.comb.800m.long <- left_join(x = hs.800.one.row, y = comb.800m.c, 
                                 by = c("Athlete", "State", "Team"))
                                        
hs.800.w.comb.800m.long <- as_tibble(hs.800.w.comb.800m.long)
dim(hs.800.w.comb.800m.long)
names(hs.800.w.comb.800m.long)
hs.800.w.comb.800m.long %>% 
  View()

# Eliminate unnecessary columns
hs.800.w.comb.800m.long <- hs.800.w.comb.800m.long %>% 
  select(-c(Gr9.y, Gr10.y, Gr11.y, Gr12.y)) 

read.in.this.4 <- paste("RunningProjectR/data-after_raw/",
                        last.year.combine,
                        "-12-31_hs-and-college-comb_",
                        gender.combine,
                        "_",
                        event.combine,
                        "_one-row-per-athlete_all-years-one-row_",
                        years.combine,
                        ".csv",
                        sep = "")

read.in.this.4
write.csv(hs.800.w.comb.800m.long, read.in.this.4)
#write.csv(hs.800.w.comb.800m.long, 
#          file = "RunningProjectR/data-after_raw/2019-12-31_hs-and-college-comb_girls_800m_one-row-per-athlete_all-years-one-row_2011-19.csv")


# Get structure of grade level times to be hms
hs.800.w.comb.800m.long <- time.to.format(hs.800.w.comb.800m.long, 
                                           hs.800.w.comb.800m.long$Gr9.x, 
                                           "Gr9.x")
hs.800.w.comb.800m.long <- hs.800.w.comb.800m.long %>% 
  rename(Gr9 = y)

hs.800.w.comb.800m.long <- time.to.format(hs.800.w.comb.800m.long, 
                                          hs.800.w.comb.800m.long$Gr10.x, "Gr10.x")
hs.800.w.comb.800m.long <- hs.800.w.comb.800m.long %>% 
  rename(Gr10 = y)

hs.800.w.comb.800m.long <- time.to.format(hs.800.w.comb.800m.long, 
                                          hs.800.w.comb.800m.long$Gr11.x, "Gr11.x")
hs.800.w.comb.800m.long <- hs.800.w.comb.800m.long %>% 
  rename(Gr11 = y)

hs.800.w.comb.800m.long <- time.to.format(hs.800.w.comb.800m.long, 
                                          hs.800.w.comb.800m.long$Gr12.x, "Gr12.x")
hs.800.w.comb.800m.long <- hs.800.w.comb.800m.long %>% 
  rename(Gr12 = y)

# Re-arrange the columns
hs.800.w.comb.800m.long <- hs.800.w.comb.800m.long %>% 
  select(Athlete, State, Gr9, Gr10, Gr11, Gr12, everything())

hs.800.w.comb.800m.long %>% 
  View()


# Now need to get data to be one row per grade, per athlete (make data long,
#  not wide)
hs.800.w.comb.800m.long2 <- hs.800.w.comb.800m.long %>% 
  gather(Grade, HStime, Gr9:Gr12)# 
dim(hs.800.w.comb.800m.long2)

hs.800.w.comb.800m.long2 %>% 
  View()

read.in.this.5 <- paste("RunningProjectR/data-after_raw/",
                        last.year.combine,
                        "-12-31_hs-and-college-comb_",
                        gender.combine,
                        "_",
                        event.combine,
                        "_one-row-per-athlete-per-year_",
                        years.combine,
                        ".csv",
                        sep = "")

read.in.this.5

write.csv(hs.800.w.comb.800m.long2, read.in.this.5)
#write.csv(hs.800.w.comb.800m.long2, 
#          file = "RunningProjectR/data-after_raw/2019-12-31_hs-and-college-comb_girls_800m_one-row-per-athlete-per-year_2011-19.csv")

str(hs.800.w.comb.800m.long2$College.All.Conf)


# /end  
############################################################

############################################################
############################################################
# Deleted Code

#reshape(v.names = "time2", timevar = "Grade", idvar = "Athlete",
#        direction = 'wide') %>% 
#group_by(Athlete) %>%
#summarize(n())
#aggregate(hs.800$Time, by = hs.800['Athlete'], FUN = median) %>% 
#gather(key = Grade, value = Time) %>% 
# ddply(.(Athlete), summarize,
#      gr9 = paste(unique(`9`), collapse = ","),
#     gr10 = paste(unique(`10`), collapse = ","),
#    gr11 = paste(unique(`11`), collapse = ","),
#   gr12 = paste(unique(`12`), collapse = ",")) %>% 
#group_by(Athlete) %>%
#gather(temp, time2) %>% 
#unite(temp1, Grade, temp) %>% 
#spread(temp1, time2) %>% 
#lapply(hs.800b) %>% 
#do.call('rbind', hs.800b$Athlete) %>% 



hs.800b <- structure(list(hs.800))
str(hs.800b)


# Next, get one row per athlete in each df
hs.800.one.row <- hs.800 %>% 
  #head(50000) %>% 
  select(-c(Meet, Team, X.1, X, year, month, Time)) %>% 
  distinct(Athlete, Grade, State, .keep_all = TRUE) %>% 
  spread(key = Grade, value = time2) %>% 
  group_by(Athlete) %>%
  summarise_all(funs(first(na.omit(.))))

#hs.800b <- hs.800 %>% head(5500)
#hs.800b
#blahblah <- setDT(hs.800b)[, lapply(.SD, function(x) toString(na.omit(x))), by = Athlete]
#blahblah %>% 
#  View()

time.to.decimal.part2(comb.800m, comb.800m$Coll.Time)

names(comb.800m)
comb.800m %>% 
  mutate(newtime = ms(Coll.Time)) %>% 
  mutate(newtime2 = newtime * 2) %>% 
  mutate(newtime3 = newtime - 1) %>% 
  mutate(newtime4 = newtime + 120) %>% 
  #mutate(time2 = as.hms(Time * 60)) %>% 
  mutate(time5 = hms::as.hms(newtime4))

comb.800m %>% 
  time.to.decimal.part2("Gr9", Gr9)# %>% 
View()

time.to.decimal.part2(comb.800m, comb.800m$Gr9)

comb.800m %>% 
  #mutate(time = lubridate::ms(Coll.Time))# %>% 
  #time.to.decimal(comb.800m, comb.800m$Coll.Time) %>% 
  #time.to.decimal(comb.800m$Gr9, na.rm = TRUE) %>% 
  View()

comb.800.b <- comb.800m
comb.800.b$Coll.Time2 <- timeagain(comb.800.b$Coll.Time)
str(comb.800.b)
comb.800.b %>% 
  View


function(){
  sapply(strsplit(comb.800.b$Coll.Time,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )
}

comb.800.b <- time.to.decimal(comb.800m, c(comb.800m$Coll.Time, comb.800m$Gr9))
comb.800.b %>% 
  View()

coll.800zzz %>% 
  mutate(time2 = as.hms(mins.dec * 60)) %>% 
  select(-Coll.Time) %>% 
  rename(Coll.Time = time2) %>% 
  View()

# Does NOT work to get Gr9 into time format
comb.800m %>% 
  mutate(newtime = ifelse(is.na(Gr9), 
                          as.character("NA"), 
                          ms(Gr9) + 120)) %>% 
  select(newtime, everything()) %>% 
  View()
mutate(newtime2 = as.hms(newtime)) %>%
  #select(-newtime) %>% 
  rename(Gr9b = newtime2) %>% 
  arrange(Gr9b, everything())



comb.800m %>% 
  mutate(newtime = ms(Coll.Time) + 120) %>% 
  mutate(newtime2 = as.hms(newtime)) %>%
  select(-newtime) %>% 
  rename(Coll.Time2 = newtime2) %>% 
  mutate(newtime9 = ms(Gr9) + 120) %>% 
  mutate(newtime99 = as.hms(newtime9)) %>%
  select(-newtime9) %>% 
  rename(Gr9b = newtime99) %>% 
  View()

comb.800m %>% 
  mutate(blah = sub(".*:", "", Gr9)) %>% 
  mutate(blah2 = as.numeric(as.character(blah))) %>% 
  mutate(blah3 = (blah2/60) + 2) %>% 
  mutate(blah4 = ms(blah3)) %>% 
  mutate(blah5 = as.hms(blah3 * 60)) %>%
  select(blah5, blah4, blah3, blah2, blah, everything())# %>% 
View()

#mutate(row_id = row_number()) %>%     # create a row identifier
#nest(-id)# %>%                # nest rest of columns apart from id and row id
#mutate(data = ([,c(Gr9, Gr10, Gr11, Gr12)])) %>% 
#mutate(min_val = map(c(Gr9, Gr10, Gr11, Gr12), min)) %>%      # get min and max
#   max_val = map(data, max)) %>%
#unnest()  %>%   


#mutate(minhs = apply(comb.800m[,c(Gr12, Gr11)], 1, FUN=min)) %>% 
#mutate(minhs = min(Gr12, Gr11, Gr10, Gr9, na.rm = TRUE)) %>% 
#mutate(minhs = min(!is.na(Gr12))) %>% 

# Get each individual athlete from hs data into one row; eg "one row per athlete"
hs.800 %>% 
  #head(50000) %>% 
  select(-c(Meet, Team, X.1, X, year, month, Time, yeardaymonth)) %>% 
  filter(Athlete == "Emma Smith") %>% 
  distinct(Athlete, Grade, State, .keep_all = TRUE) %>% 
  spread(key = Grade, value = time2) %>% 
  group_by(Athlete, State) %>%
  #dplyr::count(Athlete) %>% 
  #arrange(desc(n)) %>% 
  #filter(n > 1) #
  View()
dplyr::summarise_all(funs(first(na.omit(.)))) 


# Graph   
comb.800m.long %>% 
  ggplot(aes(x = Grade, y = HStime, na.rm = TRUE, 
             color = College.All.Conf == '1')) +
  geom_point(size = 2.5) + #, position = 'jitter') +
  scale_x_discrete(limits = c('Gr9', 'Gr10', 'Gr11', 'Gr12')) +
  scale_color_manual(values = c('orange', 'blue'))


# / Discarded Code
###