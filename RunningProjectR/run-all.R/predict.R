# Tom Alig
# Nov 26, 2019
# Predictions

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(randomForest)
source("RunningProjectR/scripts/my_track_scripts.R")

# Read in the data
improve.df <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_improve_one-row-per-athlete_2011-19.csv")
str(improve.df)
names(improve.df)

# Lumps states with small counts together
#   Necessary because random forest limits categorical values to a 
#    max of 53
improve.df <- improve.df %>% 
  mutate(State = fct_lump(State, n = 52)) 

# Split data into 70% train, 30% test
smp_size <- floor(0.70 * nrow(improve.df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(improve.df)), size = smp_size)

train <- improve.df[train_ind, ]
test <- improve.df[-train_ind, ]
# /split data

#####
dim(train)
dim(test)

#########################
# Predict for 10th Grade
#  |||
#  VVV
set.seed(157)
rf.pred.10b <- randomForest(Gr10b ~ Gr9b + State,
                           data = train, importance = TRUE,
                           na.action = na.exclude)

yh.rf.pred.10b <- predict(rf.pred.10b, data = train)
yh.rf.pred.10b %>% 
  View()

rf.pred.10b
importance(rf.pred.10b)
varImpPlot(rf.pred.10b)
summary(rf.pred.10b)

yh.rf.pred.10b <- predict(rf.pred.10b, data = train)
yh.rf.pred.10b %>% 
  View()

train <- train %>% 
  mutate(rfpred10 = yh.rf.pred.10b) 

names(train)

mse.func(train$rfpred10, train$Gr10b)

# /predict for 10th grade
#########################
# Predict for 11th Grade
#  |||
#  VVV

set.seed(157)
rf.pred.11 <- randomForest(Gr11b ~ Gr9b + Gr10b + State,
                           data = train, importance = TRUE,
                           na.action = na.exclude)

set.seed(157)
rf.pred.11a <- randomForest(Gr11b ~ Gr10b + State,
                            data = train, importance = TRUE,
                            na.action = na.exclude)

set.seed(157)
rf.pred.11b <- randomForest(Gr11b ~ Gr9b + State,
                            data = train, importance = TRUE,
                            na.action = na.exclude)

yh.rf.pred.11 <- predict(rf.pred.11, data = train)
yh.rf.pred.11a <- predict(rf.pred.11a, data = train)
yh.rf.pred.11b <- predict(rf.pred.11b, data = train)

train88 <- train %>% 
  mutate(rf11 = yh.rf.pred.11,
         rf11a = yh.rf.pred.11a,
         rf11b = yh.rf.pred.11b)# %>% 

names(train88)

# Get the averages of each of the grade 11 predictions
colnames.11 <- c("rf11", "rf11a", "rf11b")
blah <- rowMeans(train88[,colnames.11], na.rm = TRUE)

train88 <- train88 %>% 
  mutate(allrf11 = blah)# %>% 
#View()

# Find the MSE's for each of our Grade 11 predictions
mse.func(train88$rf11, train88$Gr11b)
mse.func(train88$rf11a, train88$Gr11b)
mse.func(train88$rf11b, train88$Gr11b)
mse.func(train88$allrf11, train88$Gr11b)

train88 <- train88 %>% 
  mutate(rfpred11 = ifelse(!is.na(rf11) & rf11 >0, rf11,
                           ifelse(!is.na(rf11a) & rf11a > 0, rf11a,
                                  ifelse(!is.na(rf11b) & rf11b > 0, rf11b,
                                                allrf11))))

# Find that the MSE for rfpred11 is the best of all worlds
mse.func(train88$rfpred11, train88$Gr11b)

# Eliminate the columns for Grade 11 (except the final rfpred11 column)
train88 <- train88 %>% 
  select(-c(rf11, rf11a, rf11b, allrf11))

# / Predict for 11th grade
#########################
#########################
# Predict for 12th grade
#  |||
#  VVV

set.seed(157)
rf.pred.12 <- randomForest(Gr12b ~ Gr9b + Gr10b + Gr11b + State,
                           data = train, importance = TRUE,
                           na.action = na.exclude)

set.seed(157)
rf.pred.12a <- randomForest(Gr12b ~ Gr10b + Gr11b + State,
                           data = train, importance = TRUE,
                           na.action = na.exclude)

yh.rf.pred.12a <- predict(rf.pred.12a, data = train)
sum(sapply(yh.rf.pred.12a, function(x) sum(!is.na(x))))

set.seed(157)
rf.pred.12b <- randomForest(Gr12b ~ Gr9b + Gr11b + State,
                            data = train, importance = TRUE,
                            na.action = na.exclude)

yh.rf.pred.12b <- predict(rf.pred.12b, data = train)
sum(sapply(yh.rf.pred.12b, function(x) sum(!is.na(x))))

set.seed(157)
rf.pred.12c <- randomForest(Gr12b ~ Gr11b + State,
                            data = train, importance = TRUE,
                            na.action = na.exclude)
rf.summary(yh.rf.pred.12c, rf.pred.12c, train)

set.seed(157)
rf.pred.12d <- randomForest(Gr12b ~ Gr10b + Gr9b + State,
                            data = train, importance = TRUE,
                            na.action = na.exclude)
rf.summary(yh.rf.pred.12d, rf.pred.12d, train)

set.seed(157)
rf.pred.12e <- randomForest(Gr12b ~ Gr9b + State,
                            data = train, importance = TRUE,
                            na.action = na.exclude)

set.seed(157)
rf.pred.12f <- randomForest(Gr12b ~ Gr10b + State,
                            data = train, importance = TRUE,
                            na.action = na.exclude)
rf.summary(yh.rf.pred.12f, rf.pred.12f, train)

yh.rf.pred.12 <- predict(rf.pred.12, data = train)
yh.rf.pred.12a <- predict(rf.pred.12a, data = train)
yh.rf.pred.12b <- predict(rf.pred.12b, data = train)
yh.rf.pred.12c <- predict(rf.pred.12c, data = train)
yh.rf.pred.12d <- predict(rf.pred.12d, data = train)
yh.rf.pred.12e <- predict(rf.pred.12e, data = train)
yh.rf.pred.12f <- predict(rf.pred.12f, data = train)

train99 <- train88 %>% 
  mutate(rf12 = yh.rf.pred.12,
         rf12a = yh.rf.pred.12a,
         rf12b = yh.rf.pred.12b,
         rf12c = yh.rf.pred.12c,
         rf12d = yh.rf.pred.12d,
         rf12e = yh.rf.pred.12e,
         rf12f = yh.rf.pred.12f)# %>% 

colnames.12 <- c("rf12", "rf12a", "rf12b", "rf12c",
                 "rf12d", "rf12e", "rf12f")
blah12 <- rowMeans(train99[,colnames.12], na.rm = TRUE)

train99 <- train99 %>% 
  mutate(allrf12 = blah12)# %>% 
  #View()

mse.func(train99$rf12, train99$Gr12b)
mse.func(train99$rf12a, train99$Gr12b)
mse.func(train99$rf12b, train99$Gr12b)
mse.func(train99$rf12c, train99$Gr12b)
mse.func(train99$rf12d, train99$Gr12b)
mse.func(train99$rf12e, train99$Gr12b)
mse.func(train99$rf12f, train99$Gr12b)
mse.func(train99$allrf12, train99$Gr12b)

train99 <- train99 %>% 
  mutate(rfpred12 = ifelse(!is.na(rf12) & rf12 >0, rf12,
                         ifelse(!is.na(rf12a) & rf12a > 0, rf12a,
                                ifelse(!is.na(rf12b) & rf12b > 0, rf12b,
                                       ifelse(!is.na(rf12c) & rf12c > 0, rf12c,
                                              allrf12)))))
  
mse.func(train99$rfpred12, train99$Gr12b)

train99 <- train99 %>% 
  select(-c(colnames.12, allrf12))# %>% 
  #View()

write.csv(train99, 
          file = "RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_predictions_random-forest_2011-19.csv")

# /predict for 12th grade
##############################################
##############################################
# Discarded Code
# |||||||
# VVVVVVV

train %>% 
  count(State, sort = TRUE) %>% 
  View()

blahblah <- bind_rows(yh.rf.improve.12, train12) #train12 dim 4173 x 29
dim(train12)
names(blahblah)
dim(blahblah)
blahblah[1,1:5]

yh.rf.improve.12$Prediction <- yh.rf.improve.12
class(yh.rf.improve.12)
yh.rf.improve.12.df <- as.data.frame(yh.rf.improve.12)
dim(yh.rf.improve.12.df)
names(yh.rf.improve.12.df[28:35])



yh.rf.improve.12.df[333,3700]
blahblah2 <- bind_cols(yh.rf.improve.12.df, train12)
dim(blahblah2)

mean((yh.rf.improve.12 - train12$Gr12b)^2)
length(yh.rf.improve.12)
dim(yh.rf.improve.12) # 4173 x 1


#blahblah %>% 
# View()

# Eliminate NA's
train10 <- train %>% 
  filter(!is.na(improve.9.to.10))

# Random Forest
set.seed(157)
rf.improve <- randomForest(Gr10b ~ Gr9b + State,
                           data = train10, importance = TRUE
                           #mtry = 2
)
rf.improve
importance(rf.improve)
varImpPlot(rf.improve)
summary(rf.improve)

set.seed(157)
rf.pred.10 <- randomForest(Gr10b ~ Gr9b + State,
                           data = train, importance = TRUE,
                           na.action = na.omit)


train12 <- train %>% 
  filter(!is.na(improve.9.to.12), 
         !is.na(improve.10.to.12), 
         !is.na(improve.11.to.12))
dim(train12)

lm12 <- lm(Gr12b ~ Gr9b + Gr10b + Gr11b + State,
           data = train)

lm12
summary(lm12)

newblah <- predict(rf.pred.12e, newdata = test)
sum(sapply(newblah, function(x) sum(!is.na(x))))

rf.pred.12ee <- randomForest(Gr12b ~ Gr9b,
                             data = train, importance = TRUE,
                             na.action = na.omit)

rf.summary(yh.rf.pred.12ee, rf.pred.12ee, train)

rf.pred.12
importance(rf.pred.12)

yh.rf.pred.12 <- predict(rf.pred.12, data = train)
length(yh.rf.pred.12)
sum(sapply(yh.rf.pred.12, function(x) sum(is.na(x))))
sum(sapply(yh.rf.pred.12, function(x) sum(!is.na(x))))
(sapply(train, function(x) sum(is.na(x))))

train %>% 
  mutate(rf12 = yh.rf.pred.12) %>% 
  View()

set.seed(157)
rf.pred.122 <- randomForest(Gr12b ~ Gr9b + Gr10b + Gr11b,
                            data = train, importance = TRUE,
                            na.action = na.exclude)

newblah2 <- predict(rf.pred.122, newdata = test)
sum(sapply(newblah2, function(x) sum(!is.na(x))))

blah <- rowMeans(train88[,31:33], na.rm = TRUE)
blah[1:50]


sum(sapply(test$Gr9b, function(x) sum(!is.na(x))))

