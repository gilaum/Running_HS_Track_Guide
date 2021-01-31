# Tom Alig
# Nov 26, 2019
# Predictions
# This file helped determine which model is best
# DO NOT use this file in production

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
#library(lm)
source("RunningProjectR/scripts/my_track_scripts.R")

# Read in the data
train99.df <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_predictions_random-forest_2011-19.csv")

# The train99.df data set was the training data from the random forest
#  R file. We will now use this same training data to determine a 
#   linear regression model.

# Rename the dataframe
train.lm <- train99.df

#########################
# Predict for 10th Grade
#  |||
#  VVV
set.seed(157)
lm.pred.10b <- lm(Gr10b ~ Gr9b + State,
                            data = train.lm,
                            na.action = na.exclude)

yh.lm.pred.10b <- predict(lm.pred.10b, data = train.lm)
yh.lm.pred.10b %>% 
  View()

lm.pred.10b
summary(lm.pred.10b)

yh.lm.pred.10b <- predict(lm.pred.10b, data = train.lm)
yh.lm.pred.10b %>% 
  View()

train.lm <- train.lm %>% 
  mutate(lmpred10 = yh.lm.pred.10b) 

train.lm %>% 
  View()

mse.func(train.lm$lmpred10, train.lm$Gr10b)

# /predict for 10th grade
#########################
# Predict for 11th Grade
#  |||
#  VVV

set.seed(157)
lm.pred.11 <- lm(Gr11b ~ Gr9b + Gr10b + State,
                           data = train.lm,
                           na.action = na.exclude)

set.seed(157)
lm.pred.11a <- lm(Gr11b ~ Gr10b + State,
                            data = train.lm, 
                            na.action = na.exclude)

set.seed(157)
lm.pred.11b <- lm(Gr11b ~ Gr9b + State,
                            data = train.lm, 
                            na.action = na.exclude)

yh.lm.pred.11 <- predict(lm.pred.11, data = train.lm)
yh.lm.pred.11a <- predict(lm.pred.11a, data = train.lm)
yh.lm.pred.11b <- predict(lm.pred.11b, data = train.lm)

train.lm88 <- train.lm %>% 
  mutate(lm11 = yh.lm.pred.11,
         lm11a = yh.lm.pred.11a,
         lm11b = yh.lm.pred.11b)# %>% 

names(train.lm88)

# Get the averages of each of the grade 11 predictions
colnames.11 <- c("lm11", "lm11a", "lm11b")
blah <- rowMeans(train.lm88[,colnames.11], na.rm = TRUE)

train.lm88 <- train.lm88 %>% 
  mutate(alllm11 = blah)# %>% 
#View()

# Find the MSE's for each of our Grade 11 predictions
mse.func(train.lm88$lm11, train.lm88$Gr11b)
mse.func(train.lm88$lm11a, train.lm88$Gr11b)
mse.func(train.lm88$lm11b, train.lm88$Gr11b)
mse.func(train.lm88$alllm11, train.lm88$Gr11b)

train.lm88 <- train.lm88 %>% 
  mutate(lmpred11 = ifelse(!is.na(lm11) & lm11 >0, lm11,
                           ifelse(!is.na(lm11a) & lm11a > 0, lm11a,
                                  ifelse(!is.na(lm11b) & lm11b > 0, lm11b,
                                         alllm11))))

# Find that the MSE for lmpred11 is the best of all worlds
mse.func(train.lm88$lmpred11, train.lm88$Gr11b)

# Eliminate the columns for Grade 11 (except the final lmpred11 column)
train.lm88 <- train.lm88 %>% 
  select(-c(lm11, lm11a, lm11b, alllm11))

# / Predict for 11th grade
#########################
#########################
# Predict for 12th grade
#  |||
#  VVV

set.seed(157)
lm.pred.12 <- lm(Gr12b ~ Gr9b + Gr10b + Gr11b + State,
                           data = train.lm,
                           na.action = na.exclude)

set.seed(157)
lm.pred.12a <- lm(Gr12b ~ Gr10b + Gr11b + State,
                            data = train.lm,
                            na.action = na.exclude)

#yh.lm.pred.12a <- predict(lm.pred.12a, data = train.lm)
#sum(sapply(yh.lm.pred.12a, function(x) sum(!is.na(x))))

set.seed(157)
lm.pred.12b <- lm(Gr12b ~ Gr9b + Gr11b + State,
                            data = train.lm, 
                            na.action = na.exclude)

#yh.lm.pred.12b <- predict(lm.pred.12b, data = train.lm)
#sum(sapply(yh.lm.pred.12b, function(x) sum(!is.na(x))))

set.seed(157)
lm.pred.12c <- lm(Gr12b ~ Gr11b + State,
                            data = train.lm, 
                            na.action = na.exclude)
#lm.summary(yh.lm.pred.12c, lm.pred.12c, train.lm)

set.seed(157)
lm.pred.12d <- lm(Gr12b ~ Gr10b + Gr9b + State,
                            data = train.lm, 
                            na.action = na.exclude)
#lm.summary(yh.lm.pred.12d, lm.pred.12d, train.lm)

set.seed(157)
lm.pred.12e <- lm(Gr12b ~ Gr9b + State,
                            data = train.lm, 
                            na.action = na.exclude)

set.seed(157)
lm.pred.12f <- lm(Gr12b ~ Gr10b + State,
                            data = train.lm, 
                            na.action = na.exclude)
#lm.summary(yh.lm.pred.12f, lm.pred.12f, train.lm)

yh.lm.pred.12 <- predict(lm.pred.12, data = train.lm)
yh.lm.pred.12a <- predict(lm.pred.12a, data = train.lm)
yh.lm.pred.12b <- predict(lm.pred.12b, data = train.lm)
yh.lm.pred.12c <- predict(lm.pred.12c, data = train.lm)
yh.lm.pred.12d <- predict(lm.pred.12d, data = train.lm)
yh.lm.pred.12e <- predict(lm.pred.12e, data = train.lm)
yh.lm.pred.12f <- predict(lm.pred.12f, data = train.lm)

train.lm99 <- train.lm88 %>% 
  mutate(lm12 = yh.lm.pred.12,
         lm12a = yh.lm.pred.12a,
         lm12b = yh.lm.pred.12b,
         lm12c = yh.lm.pred.12c,
         lm12d = yh.lm.pred.12d,
         lm12e = yh.lm.pred.12e,
         lm12f = yh.lm.pred.12f)# %>% 

colnames.12 <- c("lm12", "lm12a", "lm12b", "lm12c",
                 "lm12d", "lm12e", "lm12f")
blah12 <- rowMeans(train.lm99[,colnames.12], na.rm = TRUE)

train.lm99 <- train.lm99 %>% 
  mutate(alllm12 = blah12)# %>% 
#View()

mse.func(train.lm99$lm12, train.lm99$Gr12b)
mse.func(train.lm99$lm12a, train.lm99$Gr12b)
mse.func(train.lm99$lm12b, train.lm99$Gr12b)
mse.func(train.lm99$lm12c, train.lm99$Gr12b)
mse.func(train.lm99$lm12d, train.lm99$Gr12b)
mse.func(train.lm99$lm12e, train.lm99$Gr12b)
mse.func(train.lm99$lm12f, train.lm99$Gr12b)
mse.func(train.lm99$alllm12, train.lm99$Gr12b)

train.lm99 <- train.lm99 %>% 
  mutate(lmpred12 = ifelse(!is.na(lm12) & lm12 >0, lm12,
                           ifelse(!is.na(lm12a) & lm12a > 0, lm12a,
                                  ifelse(!is.na(lm12b) & lm12b > 0, lm12b,
                                         ifelse(!is.na(lm12c) & lm12c > 0, lm12c,
                                                alllm12)))))

mse.func(train.lm99$lmpred12, train.lm99$Gr12b)

train.lm99 <- train.lm99 %>% 
  select(-c(colnames.12, alllm12))# %>% 
#View()

train.lm99 %>% 
  View()

names(train.lm99)

train.lm99 <- train.lm99 %>% 
  mutate(mean10 = apply(select(.,rfpred10, lmpred10),1,mean)) %>% 
  mutate(mean11 = apply(select(.,rfpred11, lmpred11),1,mean)) %>% 
  mutate(mean12 = apply(select(.,rfpred12, lmpred12),1,mean)) %>% 
  select(mean10, rfpred10, lmpred10, 
         mean11, rfpred11, lmpred11, everything())

mse.func(train.lm99$mean10, train.lm99$Gr10b)
mse.func(train.lm99$mean11, train.lm99$Gr11b)
mse.func(train.lm99$mean12, train.lm99$Gr12b)

#write.csv(train.lm99, 
 #         file = "RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_predictions_linear-regression_2011-19.csv")



# Run 10th grade predictions on test data
set.seed(157)
testdata.10 <- predict(lm.pred.10b, test)
#testdata.10 %>% 
#  View()

length(testdata.10)
actuals_preds <- data.frame(cbind(actuals=test$Gr10b, predicteds=testdata.10))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy

mse.func(actuals_preds$predicteds, actuals_preds$actuals)

set.seed(157)
alldata.10 <- predict(lm.pred.10b, improve.df)
#length(alldata.10)

actuals_preds <- data.frame(cbind(actuals=improve.df$Gr10b, predicteds=alldata.10))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy

mse.func(actuals_preds$predicteds, actuals_preds$actuals)


# MSE was calculated for LM, RF, and mean of (LM, RF)
# MSE is lowest for LM.
# Our prediction model will therefore be the LM models

# First, copy the overall data and call the new df "predict.df"
predict.df <- improve.df
#names(predict.df)

# Get df of predictions for grades 10, 11, 12
predict10 <- predict(lm.pred.10b, improve.df)
predict.df <- cbind(predict.df, predict10)
#names(predict.df)


# For grade 11:
lm.predict11 <- predict(lm.pred.11, improve.df)
lm.predict11a <- predict(lm.pred.11a, improve.df)
lm.predict11b <- predict(lm.pred.11b, improve.df)

# Develop column names for predictions for grade 11
improve.df <- improve.df %>% 
  mutate(lm11 = lm.predict11,
         lm11a = lm.predict11a,
         lm11b = lm.predict11b)# %>% 

# Average the 3 predictions above:
# Get the averages of each of the grade 11 predictions
colnames.11 <- c("lm11", "lm11a", "lm11b")
blah <- rowMeans(improve.df[,colnames.11], na.rm = TRUE)

improve.df <- improve.df %>% 
  mutate(alllm11 = blah)

# Create the prediction based on the model
improve.df <- improve.df %>% 
  mutate(predict11 = ifelse(!is.na(lm11) & lm11 >0, lm11,
                           ifelse(!is.na(lm11a) & lm11a > 0, lm11a,
                                  ifelse(!is.na(lm11b) & lm11b > 0, lm11b,
                                         alllm11))))

predict.df <- cbind(predict.df, improve.df$predict11)
predict.df <- predict.df %>% 
  mutate(predict11 = `improve.df$predict11`) %>% 
  select(-`improve.df$predict11`)

# Eliminate the columns for Grade 11 (except the final predict11 column)
improve.df <- improve.df %>% 
  select(-c(lm11, lm11a, lm11b, alllm11))

# Now predict for Grade 12
lm.predict12 <- predict(lm.pred.12, improve.df)
lm.predict12a <- predict(lm.pred.12a, improve.df)
lm.predict12b <- predict(lm.pred.12b, improve.df)
lm.predict12c <- predict(lm.pred.12c, improve.df)
lm.predict12d <- predict(lm.pred.12d, improve.df)
lm.predict12e <- predict(lm.pred.12e, improve.df)
lm.predict12f <- predict(lm.pred.12f, improve.df)

# lm.predict12 and lm.predict12d have "BC" in State, whereas the training
#   data did not have "BC" in State.
#    #trying to add BC to the model:

lm.pred.12$xlevels
lm.pred.12$xlevels[["State"]] <- union(lm.pred.12$xlevels[["State"]], levels(improve.df[["State"]]))
lm.predict12 <- predict(lm.pred.12, improve.df)
lm.predict12

lm.pred.12d$xlevels[["State"]] <- union(lm.pred.12d$xlevels[["State"]], levels(improve.df[["State"]]))
lm.predict12d <- predict(lm.pred.12d, improve.df)

# Develop column names for predictions for grade 12
improve.df <- improve.df %>% 
  mutate(lm12 = lm.predict12,
         lm12a = lm.predict12a,
         lm12b = lm.predict12b,
         lm12c = lm.predict12c,
         lm12d = lm.predict12d,
         lm12e = lm.predict12e,
         lm12f = lm.predict12f)# %>% 

# Average the predictions above:
# Get the averages of each of the grade 12 predictions
colnames.12 <- c("lm12", "lm12a", "lm12b", "lm12c",
                 "lm12d", "lm12e", "lm12f")
blah12 <- rowMeans(improve.df[,colnames.12], na.rm = TRUE)

improve.df <- improve.df %>% 
  mutate(alllm12 = blah12)

improve.df <- improve.df %>% 
  mutate(predict12 = ifelse(!is.na(lm12) & lm12 >0, lm12,
                           ifelse(!is.na(lm12a) & lm12a > 0, lm12a,
                                  ifelse(!is.na(lm12b) & lm12b > 0, lm12b,
                                         ifelse(!is.na(lm12c) & lm12c > 0, lm12c,
                                                alllm12)))))

predict.df <- cbind(predict.df, improve.df$predict12)

names(predict.df)

predict.df <- predict.df %>% 
  mutate(predict12 = `improve.df$predict12`) %>% 
  select(-`improve.df$predict12`)

ath.prediction <- predict.df %>% 
  filter(Athlete == sn) %>%
  mutate(ten = round(predict10, 3),
         eleven = round(predict11, 3),
         twelve = round(predict12, 3)) %>% 
 # View()
  summarise("10th grade prediction" = as.hms(ten * 60), 
            "11th grade prediction" = as.hms(eleven * 60), 
            "12th grade prediction" = as.hms(twelve * 60))



