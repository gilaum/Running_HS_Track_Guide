# Tom Alig
# Nov 26, 2019
# Predictions

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(lm)
library(caret)
source("RunningProjectR/scripts/my_track_scripts.R")

# Read in the data
train.kf <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_predictions_linear-regression_2011-19.csv")

# The train99.df data set was the training data from the linear regressions
#  R file. We will now use this same training data to determine a 
#   k folds cross-validation model.

#########################

# Identify the k-folds:
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

train.kfx <- train.kf %>% 
  filter(Gr10b > 0 & Gr9b > 0) 

# Predict for 10th Grade
#  |||
#  VVV
set.seed(157)
kf.pred.10b <- train(Gr10b ~ Gr9b + State,
                  data = train.kfx, 
                  #na.action = na.exclude,
                  method = "lm",
                  trControl = train.control)
                  #controls = ctree_control(maxsurrogate = 2))
                  
print(kf.pred.10b)
yh.kf.pred.10b <- predict(kf.pred.10b, data = train.kf)
yh.kf.pred.10b %>% 
  View()

kf.pred.10b
summary(kf.pred.10b)

yh.kf.pred.10b <- predict(kf.pred.10b, data = train.kf)
yh.kf.pred.10b %>% 
  View()

train.kfx <- train.kfx %>% 
  mutate(kfpred10 = yh.kf.pred.10b) 

train.kf %>% 
  View()

mse.func(train.kfx$kfpred10, train.kfx$Gr10b)

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

write.csv(train.lm99, 
          file = "RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_predictions_linear-regression_2011-19.csv")
