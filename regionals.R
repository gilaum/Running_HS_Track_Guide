# Tom Alig
# October 29, 2020
# Saline High School 
# Predict Regional Meet at Lake Erie

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

# Only use prior meet if can be applied to most if not all seasons
prior.meet1 <- 'Regionals'
prior.meet2 <- 'SEC.Champ'
covid.meet1 <- 'I.Creek20'
covid.meet2 <- 'GR.Riverside'
this.meet <- "States"
other.meet <- "PROM"
factor1 <- 0.65

shs3 %>% 
  #mutate(imp.portage = Portage) %>% 
  View()

# For incoming 9th graders
# All this.meet results 9th grade
this.meet.9 <- shs3 %>% 
  filter(Grade== "9",
         Meet == this.meet | Meet == prior.meet1 | Meet == prior.meet2,
         #Meet == prior.meet1 | Meet == this.meet,
         Time2 > 0)

this.meet.9 %>% 
  View()

# Join (full join) Middle School scores with 9th grade this.meet results
pred.hm9 <- this.meet.9 %>% 
  full_join(sms.scores, by = "Athlete")

names(pred.hm9)

pred.hm9 %>% 
  View()

# select the Meet
pred.hm9.b <- pred.hm9 %>% 
  filter(Meet == this.meet | Meet == prior.meet1 | Meet == prior.meet2) 

# Filter out those Athletes who do NOT have any score for 7th grade AND 8th grade
pred.hm9.c <- pred.hm9.b %>% 
  filter(!is.na(Grade7.score) | (!is.na(Grade8.score)))

pred.hm9.c %>% 
  View()

# Duplicate the df
pred.hm9.d <- pred.hm9.c

# Get each individual runner into one row; eg "one row per athlete", per course
# First, set up the new df
pred.hm9.d <- pred.hm9.d %>% 
  select(-Year) %>% 
  group_by(Athlete) %>%
  pivot_wider(names_from = Meet, values_from = Time2) %>% 
  filter(States > 0)
  #filter(PROM > 0 | Regionals > 0) %>% 
  #mutate(Lake.Erie = ifelse(!is.na(PROM), PROM, Regionals)) %>% 
  #select(-c(PROM, Regionals))

pred.hm9.d %>% 
  View()

str(pred.hm9.d)

# Impute missing values from pred.ebd9
pred.hm9.imp <- mice(pred.hm9.d, m = 15, seed = 157)
hm9.imp <- complete(pred.hm9.imp,median(1:15))
#hm9.imp <- complete(pred.hm9.imp)
hm9.imp %>% 
  View()

# duplicate the df
hm9.modeldata <- hm9.imp


# Get training data and test data
# Split data into 70% train, 30% test
smp_size <- floor(0.70 * nrow(hm9.modeldata))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(hm9.modeldata)), size = smp_size)

train.hm9 <- hm9.modeldata[train_ind, ]
test.hm9 <- hm9.modeldata[-train_ind, ]

train.hm9 %>% 
  View()
# /split data

#####
dim(train.hm9)
dim(test.hm9)

# Linear Regression
set.seed(157)
lm.pred.hm9 <- lm(States ~ #Grade7.score + 
                    Grade8.score + SEC.Champ + Regionals,
                  data = train.hm9)

lm.pred.hm9
summary(lm.pred.hm9) # Adj R Sq is 0.9306; pvalue is great, RSE 0.7969

# predictions
yh.lm.pred.hm9 <- predict(lm.pred.hm9, train.hm9)
yh.lm.pred.hm9


# Create new df to capture predictions for training data in one df
newtraindf  <-  train.hm9 %>% 
  mutate(meet.pred = yh.lm.pred.hm9)

# Calculate MSE for predictions on training data
mse.func(newtraindf$States, newtraindf$meet.pred)   # MSE 0.4366

# Run models on TEST data
test.hm9.meet <- predict(lm.pred.hm9, newdata = test.hm9)

# Create new df to capture predictions for test data in one df
newtestdf  <-  test.hm9 %>% 
  mutate(meet.pred = test.hm9.meet)

# Calculate MSE for predictions on test data
mse.func(newtestdf$States, newtestdf$meet.pred)   # MSE 0.3876


# Now, predict for this.meet for Grade 9 for 2020
#   First, get list of 2020 freshman and their 8th grade scores and 7th/8th grade scores
hm9.2020 <- sms5 %>% 
  select(Athlete, Grade, score2, Year) %>% 
  filter(Grade == '7' & Year == '2018' | Grade  == '8' & Year == '2019')

# Now, get one Athlete per row
hm9.2020 <- hm9.2020 %>% 
  select(score2, Grade, Athlete) %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>%
  dplyr::rename(Grade8.score = `8`,
                Grade7.score = `7`)

# In this case, we know the only two current 8th graders are Lucy an Laney
hm9.2020.bb <- hm9.2020 %>% 
  filter(Athlete == "Lucy Woehlke" | Athlete == "Elaina Alig")

hm9.2020.bb <- hm9.2020.bb%>% 
  mutate(Athlete = ifelse(Athlete == "Elaina Alig", "Laney Alig", "Lucy Woehlke"))

hm9.2020.bb %>% 
  View()

blah <- shs3 %>% 
  filter(Athlete == "Lucy Woehlke" | Athlete == "Laney Alig",
         Meet == "Regionals" | Meet == "I.Creek20") %>% 
  select(-Year) %>% 
  pivot_wider(names_from = Meet, values_from = Time2) 

mynewjoin <- left_join(hm9.2020.bb, blah, by = "Athlete")

# COVID in 2020 has messed with data. Not running on same courses at same time of year as in past
#  due to large invites being cancelled etc. 
# GR Riverside is roughly equiv to Portage, based on 2019 data
# Set Add 38 seconds to Indian Creek time to make it equivalent to Hudson Mills in past
hm9.2020.bb.imp <- mynewjoin %>% 
  select(-Grade) %>% 
  mutate(#Portage = GR.Riverside,
         SEC.Champ = I.Creek20 + 38/60) %>% 
  #select(-c(GR.Riverside, I.Creek20))
  select(-I.Creek20)

hm9.2020.bb.imp

# predictions
hm9.both.2020 <- predict(lm.pred.hm9, newdata = hm9.2020.bb.imp)
hm9.both.2020

# Create new df to capture predictions for 2020 freshmen
hm9.2020.pred <- hm9.2020.bb.imp %>% 
  mutate(pred.lm.hmead.2020 = hm9.both.2020)  %>%
  #mutate(Gr7and8.pred = hm9.both.2020)  %>% 
  #mutate(pred.lm.eb.2020 = Gr7and8.pred) %>% 
  select(Athlete, pred.lm.hmead.2020) %>% 
  mutate(run_time = as_hms(pred.lm.hmead.2020 * 60))

hm9.2020.pred %>% 
  View()



##############################################################

# For incoming 10th graders

# First, get historical 10th graders
this.meet.10 <- shs3 %>% 
  select(-Year) %>% 
  filter(Grade== "10",
         Meet == this.meet |  Meet ==  prior.meet1 | Meet == prior.meet2,
         Time2 > 0) %>% 
  pivot_wider(names_from = Grade, values_from = Time2) %>%
  mutate(Gr10.thismeet.time = `10`,
         grade10meet = Meet) %>% 
  select(-c('10', Meet))

this.meet.10 %>% 
  View()

# filter out last year's 9th graders, because we only want those who have completed the race
#   in the 10th grade, so we can model the data
this.meet.9b <- this.meet.9 %>% 
  filter(Year != '2019')

this.meet.9b <- this.meet.9b %>% 
  select(-c(Year, Grade)) %>% 
  mutate(gr9.thismeet.time = Time2,
         grade9meet = Meet) %>% 
  select(-c(Meet, Time2)) %>% 
  filter(gr9.thismeet.time < 23) 

# join together 9th grade this meet runners w 10th grade this meet runners
this.meet.10and9 <- left_join(this.meet.9b, this.meet.10,
                          by = 'Athlete')

this.meet.10and9 %>% 
  View()

#this.meet.10and9 %>% 
#  filter(!is.na(Meet.y),
#         Gr10.thismeet.time < 22.5) %>% 
#  mutate(gr10meet = "Lake.Erie",
#         gr9time = Time2) %>% 
 # mutate(gr9meet = ifelse(Meet.x == "PROM", "Lake.Erie",
#                          ifelse(Meet.x == "Regionals", "Lake.Erie",
 #                                Meet.x))) %>% 
#  select(-c(Meet.y, Meet.x, Time2, Year)) %>% 
#  View()

# pick the columns we want
#this.meet.10and9 <- this.meet.10and9 %>% 
#  select(Athlete, Time2, Gr10.thismeet.time) %>% 
#  rename(Gr9.thismeet.time = Time2)

# Add in 7th and 8th grade scores for those in the this.meet.10and9 df
this.meet.10and9b <- this.meet.10and9%>% 
  left_join(sms.scores, by  = "Athlete")

# Add in 9th grade scores for those in the this.meet.10and9 df
#  We don't add in 10th grade scores because we are trying to predict 10th grade this meet result
#   At the time of the 10th grade this meet, we don't know the athlete's total 10th grade score!
#    Also, we will filter out those with over 50% NA's
this.meet.10and9c <- this.meet.10and9b %>% 
  left_join(shs6, by = 'Athlete') %>% 
  select(-c(Grade10.score, Grade11.score, Grade12.score)) %>% 
  rowwise() %>% 
  mutate(num.of.na = sum(is.na(c(gr9.thismeet.time, Gr10.thismeet.time, 
                                 Grade7.score, Grade8.score, Grade9.score)))) %>% 
  filter(num.of.na < 3) %>% 
  select(-num.of.na) 

this.meet.10and9c %>% 
  View()

this.meet.10and9c %>% 
  #pivot_wider(names_from = grade9meet, values_from = gr9.thismeet.time) %>% 
  pivot_wider(names_from = c(grade9meet, grade10meet), 
              values_from = c(gr9.thismeet.time, Gr10.thismeet.time)) %>% 
  View()

pred.hm10.imp <- mice(this.meet.10and9c, m = 5, seed = 157)
hm10.imp <- complete(pred.hm10.imp,median(1:5))
hm10.imp %>% 
  View()

# Get so we have data for modeling purposes
#  Duplicate df
hm10.modeldata <- hm10.imp

hm10.modeldata %>% 
  View()

# Get training data and test data
# Split data into 70% train, 30% test
smp_size <- floor(0.70 * nrow(hm10.modeldata))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(hm10.modeldata)), size = smp_size)

train.hm10 <- hm10.modeldata[train_ind, ]
test.hm10 <- hm10.modeldata[-train_ind, ]

train.hm10 %>% 
  View()

names(train.hm10)

# First, linear modeling
set.seed(157)
lm.pred.hm10 <- lm(Gr10.hmead.time ~ Grade7.score + Grade8.score + Grade9.score +
                     Gr9.hmead.time,
                   data = train.hm10,
                   na.action = na.exclude)

lm.pred.hm10
summary(lm.pred.hm10) # Adj R Sq is 0.7598; pvalue is great, RSE 1.955

# predictions
yh.lm.pred.hm10 <- predict(lm.pred.hm10, train.hm10)
yh.lm.pred.hm10

# Add the predictions into the df
newtraindf.10  <-  train.hm10 %>% 
  mutate(Gr10.lm.eb.pred = yh.lm.pred.hm10)

# Calculate MSE for predictions on training data
mse.func(newtraindf.10$Gr10.hmead.time, newtraindf.10$Gr10.lm.eb.pred)  # MSE 3.61092

# Run models on test data
test.hm10.lm <- predict(lm.pred.hm10, newdata = test.hm10)

# Create new df to capture predictions for test data in one df
newtestdf.10  <-  test.hm10 %>% 
  mutate(Gr10.lm.eb.pred = test.hm10.lm)

# Calculate MSE for predictions on test data
mse.func(newtestdf.10$Gr10.hmead.time, newtestdf.10$Gr10.lm.eb.pred)  # MSE 3.30

# Now do random forest
set.seed(157)
rf.pred.hm10 <- randomForest(Gr10.hmead.time ~ Grade7.score + Grade8.score + Grade9.score +
                               Gr9.hmead.time,
                             data = train.hm10, importance = TRUE,
                             na.action = na.exclude)

yh.rf.pred.hm10 <- predict(rf.pred.hm10, data = train.hm10)
yh.rf.pred.hm10 %>% 
  View()

rf.pred.hm10
importance(rf.pred.hm10)
varImpPlot(rf.pred.hm10)
summary(rf.pred.hm10)

#yh.rf.pred.hm10 <- predict(rf.pred.hm10, data = train.hm10)
#yh.rf.pred.hm10 %>% 
#  View()

train.hm10 <- train.hm10 %>% 
  mutate(rfpred10 = yh.rf.pred.hm10) 

names(train.hm10)
train.hm10 %>% 
  View()

mse.func(train.hm10$rfpred10, train.hm10$Gr10.hmead.time) # MSE 8.36991

# Go with linear model due to MSE


# Now, predict for this.meet for Grade 10 for 2020
#   First, get list of 2020 SOPHS and their 8th grade scores and 7th/8th grade scores
hm10.2020 <- sms5 %>% 
  select(Athlete, Grade, score2, Year) %>% 
  filter(Grade == '7' & Year == '2017' | Grade  == '8' & Year == '2018')

# Now, get one Athlete per row
hm10.2020 <- hm10.2020 %>% 
  select(score2, Grade, Athlete) %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>%
  dplyr::rename(Grade8.score = `8`,
                Grade7.score = `7`)

hm10.2020 %>% 
  View()

# Get 9th grade scores
hmead.pred10 <- hm10.2020 %>% 
  left_join(shs6, by = 'Athlete') %>% 
  select(-c(Grade10.score, Grade11.score, Grade12.score)) 
#filter(!is.na(Grade9.score))

hmead.pred10

# Get 9th grade this.meet performance
hmead.thismeet.10 <- shs3 %>% 
  select(-Year) %>% 
  filter(Grade== "9",
         Meet == this.meet,
         Time2 > 0) %>% 
  pivot_wider(names_from = Grade, values_from = Time2) %>%
  mutate(Gr9.hmead.time = `9`) %>% 
  #select(-c(`9`, `10`)) %>% 
  select(-'9')

# Join
# Get 9th grade scores
hmead.pred10.b <- hmead.pred10 %>% 
  left_join(hmead.thismeet.10, by = 'Athlete') %>% 
  select(-Meet) 

hmead.pred10.b %>% 
  View()

names(hmead.pred10.b)

# Eliminate those runners who are missing 3 values
hmead.pred10.c <- hmead.pred10.b %>% 
  rowwise() %>% 
  mutate(num.of.na = sum(is.na(c(Gr9.hmead.time, 
                                 Grade7.score, Grade8.score, Grade9.score)))) %>% 
  filter(num.of.na < 3) %>% 
  select(-num.of.na) 



# Now, impute the missing values
pred.hm10.imp <- mice(hmead.pred10.c, m = 5, seed = 157)
#pred.hm10.imp$imp$Grade8.score %>% 
#  View()
hm10.imp <- complete(pred.hm10.imp,median(1:5))
hm10.imp %>% 
  View()


# predictions
hm10.both.2020 <- predict(lm.pred.hm10, newdata = hm10.imp)
str(hm10.both.2020)
hm10.both.2020 %>% 
  View()


newblah <- cbind(hm10.imp, hm10.both.2020)

# Create new df to capture predictions for 2020 freshmen
#hm10.2020.pred <- 
newblah %>% 
  #mutate(pred.lm.hmead.2020 = hm10.both.2020)  %>%
  rename(pred.lm.hmead.2020 = hm10.both.2020)  %>%
  #View()
  #mutate(Gr7and8.pred = hm10.both.2020)  %>% 
  #mutate(pred.lm.eb.2020 = Gr7and8.pred) %>% 
  select(Athlete, pred.lm.hmead.2020) %>% 
  mutate(run_time = as_hms(pred.lm.hmead.2020 * 60)) %>% 
  View()

hm10.2020.pred %>% 
  View()















###############################################
#   DISCARDED CODE
# Predictions for 10th graders
hm10.both.2020 <- predict(lm.pred.hm10, newdata = hm10.imp)
hm10.both.2020


# Create new df to capture predictions for 2020 sophomores

hm10.2020.pred <- hm10.imp %>% 
  mutate(pred.lm.hmead.2020 = hm10.both.2020)  %>%
  #mutate(Gr7and8.pred = hm10.both.2020)  %>% 
  #mutate(pred.lm.eb.2020 = Gr7and8.pred) %>% 
  select(Athlete, pred.lm.hmead.2020) %>% 
  mutate(run_time = as_hms(pred.lm.hmead.2020 * 60))

hm10.2020.pred %>% 
  View()




# 8/3/20 stopping point
#   Get MSE for test data
#   Predict for Gr 10
#   Median v '3'
#   For predictor variable, ie grade 10 early bird race, select only those who have run the race in grade 10?
# ie don't impute for grade 10 early bird race result?
# Move on to Gr 11



# Now, predict for Early Bird for Grade 10 for 2020
#   First, get list of 2020 soph and their 9th grade scores and 7th/8th grade scores
eb10.2020 <- shs5 %>% 
  select(Athlete, Grade, score2, Year, EarlyBird) %>% 
  filter(Grade == '9' & Year == '2019')

# Now, get one Athlete per row
eb10.2020 <- eb10.2020 %>% 
  select(score2, Grade, Athlete, EarlyBird) %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>% 
  dplyr::rename(Grade9.score = `9`)

eb10.2020 %>% 
  View()


# Stopping here on 6.30.20. Need to get all the predictors into the df (eb10.2020)
#  for 2020 sophomores. ie need to get Grade 7 score, Grade 8 score,
#    Grade 9 score, and Grade 9 Early Bird race time.

eb10.2020b <- sms5 %>% 
  select(Athlete, Grade, score2, Year) %>% 
  filter(Grade == '7' & Year == '2017' | Grade  == '8' & Year == '2018')

# Now, get one Athlete per row
eb10.2020b <- eb10.2020b %>% 
  select(score2, Grade, Athlete) %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>%
  dplyr::rename(Grade8.score = `8`,
                Grade7.score = `7`)

# Join (full join) Middle School scores with 9th grade Early Bird Meet results
newdf.eb10 <- eb10.2020 %>% 
  full_join(eb10.2020b, by = "Athlete") %>% 
  mutate(Gr9.hmead.time = EarlyBird) %>% 
  select(-EarlyBird)

names(newdf.eb10)

newdf.eb10 %>% 
  View()

# predictions
eb10.lm.2020 <- predict(lm.pred.eb10, newdata = newdf.eb10)

eb10.lm.2020


# Create new df to capture predictions for 2020 sophomores
eb10.2020.pred <- newdf.eb10 %>% 
  mutate(Gr10.lm.pred = eb10.lm.2020)  %>% 
  select(Athlete, Gr10.lm.pred) %>% 
  mutate(run_time = as_hms(Gr10.lm.pred * 60))

eb10.2020.pred %>% 
  View()

