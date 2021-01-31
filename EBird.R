# Tom Alig
# June 18, 2020
# Saline High School 
# Predict Early Bird XC Meet

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

# For incoming 9th graders
# All early bird results 9th grade
ebird.9 <- shs3 %>% 
  filter(Grade== "9",
         Meet == "EarlyBird",
         Time2 > 0)

ebird.9 %>% 
  View()

# Join (full join) Middle School scores with 9th grade Early Bird Meet results
pred.eb9 <- ebird.9 %>% 
  full_join(sms.scores, by = "Athlete")

names(pred.eb9)

pred.eb9 %>% 
  View()

# select the Meet
pred.eb9 <- pred.eb9 %>% 
  filter(Meet == "EarlyBird") 

# Impute missing values from pred.ebd9
pred.eb9.imp <- mice(pred.eb9, m = 5, seed = 157)
eb9.imp <- complete(pred.eb9.imp,3)
eb9.imp %>% 
  View()

# duplicate the df
eb9.modeldata <- eb9.imp

# Get training data and test data
# Split data into 70% train, 30% test
smp_size <- floor(0.70 * nrow(eb9.modeldata))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(eb9.modeldata)), size = smp_size)

train.eb9 <- eb9.modeldata[train_ind, ]
test.eb9 <- eb9.modeldata[-train_ind, ]

train.eb9 %>% 
  View()
# /split data

#####
dim(train.eb9)
dim(test.eb9)

# First, model for both 7th grade score and 8th grade score
set.seed(157)
lm.pred.eb9 <- lm(Time2 ~ Grade7.score + Grade8.score,
                  data = train.eb9)

lm.pred.eb9
summary(lm.pred.eb9) # Adj R Sq is 0.767; pvalue is great, RSE 2.51

# predictions
yh.lm.pred.eb9 <- predict(lm.pred.eb9, train.eb9)
yh.lm.pred.eb9


# Now model for 7th grade score only
set.seed(157)
lm.pred.eb9.only7 <- lm(Time2 ~ Grade7.score,
                  data = train.eb9)

lm.pred.eb9.only7
summary(lm.pred.eb9.only7) # Adj R Sq is 0.6821; pvalue is great; RSE 2.932

# predictions
yh.lm.pred.eb9.only7 <- predict(lm.pred.eb9.only7, train.eb9)
yh.lm.pred.eb9.only7


# Now model for 8th grade score only
set.seed(157)
lm.pred.eb9.only8 <- lm(Time2 ~ Grade8.score,
                        data = train.eb9)

lm.pred.eb9.only8
summary(lm.pred.eb9.only8) # Adj R Sq is 0.6929; pvalue is great; RSE 2.882

# predictions
yh.lm.pred.eb9.only8 <- predict(lm.pred.eb9.only8, train.eb9)
yh.lm.pred.eb9.only8

# Create new df to capture predictions for training data in one df
newtraindf  <-  train.eb9 %>% 
  mutate(Gr7.pred = yh.lm.pred.eb9.only7,
         Gr8.pred = yh.lm.pred.eb9.only8,
         Gr7and8.pred = yh.lm.pred.eb9)
  
# Calculate MSE for predictions on training data
mse.func(newtraindf$Time2, newtraindf$Gr7.pred)  # MSE 8.38
mse.func(newtraindf$Time2, newtraindf$Gr8.pred)  # MSE 8.09
mse.func(newtraindf$Time2, newtraindf$Gr7and8.pred)   # MSE 6.06

# Run models on test data
test.eb9.7 <- predict(lm.pred.eb9.only7, newdata = test.eb9)
test.eb9.8 <- predict(lm.pred.eb9.only8, newdata = test.eb9)
test.eb9.both <- predict(lm.pred.eb9, newdata = test.eb9)

# Create new df to capture predictions for test data in one df
newtestdf  <-  test.eb9 %>% 
  mutate(Gr7.pred = test.eb9.7,
         Gr8.pred = test.eb9.8,
         Gr7and8.pred = test.eb9.both)

# Calculate MSE for predictions on test data
mse.func(newtestdf$Time2, newtestdf$Gr7.pred)  # MSE 7.91
mse.func(newtestdf$Time2, newtestdf$Gr8.pred)  # MSE 3.05
mse.func(newtestdf$Time2, newtestdf$Gr7and8.pred)   # MSE 5.16

# We see that MSE for 8th grade scores is wayyy different between training and test data

# What to do?
#  Because there are only one or two possible predictors, and because our training and test data
#    sets are relatively low sample size, let's run the models over the full data set


eb9.modeldata %>% 
  View()

# Predictions on full data set
all.eb9.7 <- predict(lm.pred.eb9.only7, newdata = eb9.modeldata)
all.eb9.8 <- predict(lm.pred.eb9.only8, newdata = eb9.modeldata)
all.eb9.both <- predict(lm.pred.eb9, newdata = eb9.modeldata)

# Create new df to capture predictions for ALL data in one df
new.eb9.modeldata <- eb9.modeldata %>% 
  mutate(Gr7.pred = all.eb9.7,
         Gr8.pred = all.eb9.8,
         Gr7and8.pred = all.eb9.both)

# Calculate MSE for predictions on ALL data  
mse.func(new.eb9.modeldata$Time2, new.eb9.modeldata$Gr7.pred)  # MSE 8.23
mse.func(new.eb9.modeldata$Time2, new.eb9.modeldata$Gr8.pred)  # MSE 6.55
mse.func(new.eb9.modeldata$Time2, new.eb9.modeldata$Gr7and8.pred)   # MSE 5.78

# We see that predictions using 8th grade data only is slightly worse MSE than using both 
#   7th and 8th grade data

# Going back to the training data, the residuals look much better using both 7th and 8th grade data

# Deciding to use model with both 7th and 8th data as model of choice

sms5 %>% 
  View()


# Now, predict for Early Bird for Grade 9 for 2020
#   First, get list of 2020 freshman and their 8th grade scores and 7th/8th grade scores
eb9.2020 <- sms5 %>% 
  select(Athlete, Grade, score2, Year) %>% 
  filter(Grade == '7' & Year == '2018' | Grade  == '8' & Year == '2019')

# Now, get one Athlete per row
eb9.2020 <- eb9.2020 %>% 
  select(score2, Grade, Athlete) %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>%
  dplyr::rename(Grade8.score = `8`,
                Grade7.score = `7`)

eb9.2020 %>% 
  View()

# Impute missing values from pred.ebd9
eb9.2020.imp <- mice(eb9.2020, m = 5, seed = 157)
eb9.2020.imp.b <- complete(eb9.2020.imp,3)

eb9.2020.imp.b %>% 
  View()

# predictions
eb9.both.2020 <- predict(lm.pred.eb9, newdata = eb9.2020.imp.b)

# Create new df to capture predictions for 2020 freshmen
eb9.2020.pred <- eb9.2020.imp.b %>% 
  mutate(pred.lm.eb.2020 = eb9.both.2020)  %>%
  #mutate(Gr7and8.pred = eb9.both.2020)  %>% 
  #mutate(pred.lm.eb.2020 = Gr7and8.pred) %>% 
  select(Athlete, pred.lm.eb.2020) %>% 
  mutate(run_time = as_hms(pred.lm.eb.2020 * 60))

eb9.2020.pred %>% 
  View()



##############################################################

# For incoming 10th graders

# All early bird results 10th grade
#ebird.10 <- shs3 %>% 
#  filter(Grade== "10",
#         Meet == "EarlyBird",
#         Time2 > 0)


ebird.10 <- shs3 %>% 
  select(-Year) %>% 
  filter(Grade== "10",
         #Grade== "10" | Grade == "9",
         Meet == "EarlyBird",
         Time2 > 0) %>% 
  pivot_wider(names_from = Grade, values_from = Time2) %>%
  mutate(#Gr9.eb.time = `9`,
         Gr10.eb.time = `10`) %>% 
  #select(-c(`9`, `10`)) %>% 
  select(-'10')

# filter out last year's 9th graders, because we only want those who have completed the race
#   in the 10th grade, so we can model the data
ebird.9b <- ebird.9 %>% 
  filter(Year != '2019')

# join together 9th grade early bird runners w 10th grade early bird runners
ebird.10and9 <- full_join(ebird.9b, ebird.10,
                          by = 'Athlete')

ebird.10and9 %>% 
  View()

# pick the columns we want
ebird.10and9 <- ebird.10and9 %>% 
  select(Athlete, Time2, Gr10.eb.time) %>% 
  rename(Gr9.eb.time = Time2)

# Add in 7th and 8th grade scores for those in the ebird.10and9 df
ebird.10and9 <- ebird.10and9%>% 
  left_join(sms.scores, by  = "Athlete")

# Add in 9th grade scores for those in the ebird.10and9 df
#  We don't add in 10th grade scores because we are trying to predict 10th grade early bird meet result
#   At the time of the 10th grade ebird meet, we don't know the athlete's total 10th grade score!
#    Also, we will filter out those with over 50% NA's
ebird.10and9 <- ebird.10and9 %>% 
  left_join(shs6, by = 'Athlete') %>% 
  select(-c(Grade10.score, Grade11.score, Grade12.score)) %>% 
  rowwise() %>% 
  mutate(num.of.na = sum(is.na(c(Gr9.eb.time, Gr10.eb.time, 
                                    Grade7.score, Grade8.score, Grade9.score)))) %>% 
  filter(num.of.na < 3) %>% 
  select(-num.of.na) 

pred.eb10.imp <- mice(ebird.10and9, m = 5, seed = 157)
pred.eb10.imp$imp$Grade9.score %>% 
  View()
eb10.imp <- complete(pred.eb10.imp,median(1:5))
eb10.imp %>% 
  View()

# Get so we have data for modeling purposes
#  Duplicate df
eb10.modeldata <- eb10.imp

eb10.modeldata %>% 
  View()

# Get training data and test data
# Split data into 70% train, 30% test
smp_size <- floor(0.70 * nrow(eb10.modeldata))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(eb10.modeldata)), size = smp_size)

train.eb10 <- eb10.modeldata[train_ind, ]
test.eb10 <- eb10.modeldata[-train_ind, ]

train.eb10 %>% 
  View()

names(train.eb10)

# First, linear modeling
set.seed(157)
lm.pred.eb10 <- lm(Gr10.eb.time ~ Grade7.score + Grade8.score + Grade9.score +
                     Gr9.eb.time,
                  data = train.eb10,
                  na.action = na.exclude)

lm.pred.eb10
summary(lm.pred.eb10) # Adj R Sq is 0.7974; pvalue is great, RSE 1.476

# predictions
yh.lm.pred.eb10 <- predict(lm.pred.eb10, train.eb10)
yh.lm.pred.eb10

# Add the predictions into the df
newtraindf.10  <-  train.eb10 %>% 
  mutate(Gr10.lm.eb.pred = yh.lm.pred.eb10)

# Calculate MSE for predictions on training data
mse.func(newtraindf.10$Gr10.eb.time, newtraindf.10$Gr10.lm.eb.pred)  # MSE 2.012005

# Run models on test data
test.eb10.lm <- predict(lm.pred.eb10, newdata = test.eb10)

# Create new df to capture predictions for test data in one df
newtestdf.10  <-  test.eb10 %>% 
  mutate(Gr10.lm.eb.pred = test.eb10.lm)

# Calculate MSE for predictions on test data
mse.func(newtestdf.10$Gr10.eb.time, newtestdf.10$Gr10.lm.eb.pred)  # MSE 4.789849

# Now do random forest
set.seed(157)
rf.pred.eb10 <- randomForest(Gr10.eb.time ~ Grade7.score + Grade8.score + Grade9.score +
                               Gr9.eb.time,
                             data = train.eb10, importance = TRUE,
                             na.action = na.exclude)
                           
yh.rf.pred.eb10 <- predict(rf.pred.eb10, data = train.eb10)
yh.rf.pred.eb10 %>% 
  View()

rf.pred.eb10
importance(rf.pred.eb10)
varImpPlot(rf.pred.eb10)
summary(rf.pred.eb10)

#yh.rf.pred.eb10 <- predict(rf.pred.eb10, data = train.eb10)
#yh.rf.pred.eb10 %>% 
#  View()

train.eb10 <- train.eb10 %>% 
  mutate(rfpred10 = yh.rf.pred.eb10) 

names(train.eb10)
train.eb10 %>% 
  View()

mse.func(train.eb10$rfpred10, train.eb10$Gr10.eb.time) # MSE 3.330413

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
  mutate(Gr9.eb.time = EarlyBird) %>% 
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

