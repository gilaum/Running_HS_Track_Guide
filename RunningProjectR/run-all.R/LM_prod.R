# Tom Alig
# Nov 26, 2019
# Predictions
# USE this one for PRODUCTION

# MSE was calculated for LM, RF, and mean of (LM, RF)
# MSE is lowest for LM.
# Our prediction model will therefore be the LM models

library(tidyverse)
library(lubridate)
library(scales)
library(hms)

source("RunningProjectR/scripts/my_track_scripts.R")

# Read in the all data
lm.df <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_improve_one-row-per-athlete_2011-19.csv")

# Copy the dataframe
lm.df2 <- lm.df

# Lumps states with small counts together
#   Necessary because linear regression in R limits categorical values to a 
#    max of 53
lm.df2 <- lm.df2 %>% 
  mutate(State = fct_lump(State, n = 52)) 
#########################
# Predict for 10th Grade
#  |||
#  VVV
set.seed(157)
lm.pred.10b <- lm(Gr10b ~ Gr9b + State,
                  data = lm.df2,
                  na.action = na.exclude)

set.seed(157)
yh.lm.pred.10b <- predict(lm.pred.10b, lm.df2)

lm.df2 <- lm.df2 %>% 
  mutate(lmpred10 = yh.lm.pred.10b) 

lm.df2 %>% 
  View()
# /predict for 10th grade
#########################
# Predict for 11th Grade
#  |||
#  VVV

# copy the df
lm.df11 <- lm.df2

# Use Grades 9 and 10 and the State as predictors
set.seed(157)
lm.pred.11 <- lm(Gr11b ~ Gr9b + Gr10b + State,
                 data = lm.df11,
                 na.action = na.exclude)

# Use State and only Grade 10 as predictors
set.seed(157)
lm.pred.11a <- lm(Gr11b ~ Gr10b + State,
                  data = lm.df11, 
                  na.action = na.exclude)

# Use State and only Grade 9 as predictors
set.seed(157)
lm.pred.11b <- lm(Gr11b ~ Gr9b + State,
                  data = lm.df11, 
                  na.action = na.exclude)

yh.lm.pred.11 <- predict(lm.pred.11, lm.df11)
yh.lm.pred.11a <- predict(lm.pred.11a, lm.df11)
yh.lm.pred.11b <- predict(lm.pred.11b, lm.df11)

lm.df11 <- lm.df11 %>% 
  mutate(lm11 = yh.lm.pred.11,
         lm11a = yh.lm.pred.11a,
         lm11b = yh.lm.pred.11b)# %>% 

# Get the averages of each of the grade 11 predictions
colnames.11 <- c("lm11", "lm11a", "lm11b")
blah <- rowMeans(lm.df11[,colnames.11], na.rm = TRUE)

lm.df11 <- lm.df11 %>% 
  mutate(alllm11 = blah) #

lm.df11 <- lm.df11 %>% 
  mutate(lmpred11 = ifelse(!is.na(lm11) & lm11 >0, lm11,
                           ifelse(!is.na(lm11a) & lm11a > 0, lm11a,
                                  ifelse(!is.na(lm11b) & lm11b > 0, lm11b,
                                         alllm11))))

# Eliminate the columns for Grade 11 (except the final lmpred11 column)
lm.df11 <- lm.df11 %>% 
  select(-c(lm11, lm11a, lm11b, alllm11))

names(lm.df11)

# / Predict for 11th grade
#########################
#########################
# Predict for 12th grade
#  |||
#  VVV

# copy the df
lm.df12 <- lm.df11

# Use State and Grades 9, 10, and 11 as predictors
set.seed(157)
lm.pred.12 <- lm(Gr12b ~ Gr9b + Gr10b + Gr11b + State,
                 data = lm.df12,
                 na.action = na.exclude)

# Use State and Grades 10 and 11 as predictors (no 9)
set.seed(157)
lm.pred.12a <- lm(Gr12b ~ Gr10b + Gr11b + State,
                  data = lm.df12,
                  na.action = na.exclude)

# Use State and Grades 9 and 11 as predictors (no 10)
set.seed(157)
lm.pred.12b <- lm(Gr12b ~ Gr9b + Gr11b + State,
                  data = lm.df12, 
                  na.action = na.exclude)

# Use State and Grade 11 as predictors (no 9 or 10)
set.seed(157)
lm.pred.12c <- lm(Gr12b ~ Gr11b + State,
                  data = lm.df12, 
                  na.action = na.exclude)

# Use State and Grades 9 and 10 as predictors (no 11)
set.seed(157)
lm.pred.12d <- lm(Gr12b ~ Gr10b + Gr9b + State,
                  data = lm.df12, 
                  na.action = na.exclude)

# Use State and Grade 9 as predictors (no 10 or 11)
set.seed(157)
lm.pred.12e <- lm(Gr12b ~ Gr9b + State,
                  data = lm.df12, 
                  na.action = na.exclude)

# Use State and Grade 10 as predictors (no 9 or 11)
set.seed(157)
lm.pred.12f <- lm(Gr12b ~ Gr10b + State,
                  data = lm.df12, 
                  na.action = na.exclude)

lm.predict12 <- predict(lm.pred.12, lm.df12)
lm.predict12a <- predict(lm.pred.12a, lm.df12)
lm.predict12b <- predict(lm.pred.12b, lm.df12)
lm.predict12c <- predict(lm.pred.12c, lm.df12)
lm.predict12d <- predict(lm.pred.12d, lm.df12)
lm.predict12e <- predict(lm.pred.12e, lm.df12)
lm.predict12f <- predict(lm.pred.12f, lm.df12)

# lm.predict12 and lm.predict12d have "BC" in State, whereas the model 
#   data did not have "BC" in State.
#    add BC to the model:

#lm.pred.12$xlevels
lm.pred.12$xlevels[["State"]] <- union(lm.pred.12$xlevels[["State"]], levels(lm.df12[["State"]]))
lm.predict12 <- predict(lm.pred.12, lm.df12)
#lm.predict12

lm.pred.12d$xlevels[["State"]] <- union(lm.pred.12d$xlevels[["State"]], levels(lm.df12[["State"]]))
lm.predict12d <- predict(lm.pred.12d, lm.df12)


# Develop column names for predictions for grade 12
lm.df12 <- lm.df12 %>% 
  mutate(lm12 = lm.predict12,
         lm12a = lm.predict12a,
         lm12b = lm.predict12b,
         lm12c = lm.predict12c,
         lm12d = lm.predict12d,
         lm12e = lm.predict12e,
         lm12f = lm.predict12f)# %>% 

colnames.12 <- c("lm12", "lm12a", "lm12b", "lm12c",
                 "lm12d", "lm12e", "lm12f")
blah12 <- rowMeans(lm.df12[,colnames.12], na.rm = TRUE)

lm.df12 <- lm.df12 %>% 
  mutate(alllm12 = blah12)# %>% 

lm.df12 <- lm.df12 %>% 
  mutate(lmpred12 = ifelse(!is.na(lm12) & lm12 >0, lm12,
                           ifelse(!is.na(lm12a) & lm12a > 0, lm12a,
                                  ifelse(!is.na(lm12b) & lm12b > 0, lm12b,
                                         ifelse(!is.na(lm12c) & lm12c > 0, lm12c,
                                                alllm12)))))


lm.df12 <- lm.df12 %>% 
  select(-c(colnames.12, alllm12))# %>% 


mmy.pred <- lm.df12 %>% 
  filter(Athlete == sn) %>% 
  mutate(Gr10pred = round(as.hms(lmpred10 * 60),2)) %>%
  
  mutate(Gr10pred2 = as.hms(Gr10pred)) %>% 
  #View()
  #mutate(Gr10pred = as.hms(lmpred10 * 60)) %>%
  mutate(Gr11pred = round(as.hms(lmpred11 * 60),2)) %>%
  mutate(Gr11pred2 = as.hms(Gr11pred)) %>% 
  mutate(Gr12pred = round(as.hms(lmpred12 * 60),2)) %>%
  mutate(Gr12pred2 = as.hms(Gr12pred)) %>% 
  select(Gr10pred2, Gr11pred2, Gr12pred2)

current.year.pred <- ifelse(current.school.year.grade == 9, "na", 
                            ifelse(current.school.year.grade == 10, 
                                   as.hms(mmy.pred$Gr10pred2),
                                ifelse(current.school.year.grade == 11, 
                                       as.hms(mmy.pred$Gr11pred2), 
                                       as.hms(mmy.pred$Gr12pred2))))

current.year.pred

current.year.pred2 <- as.hms(current.year.pred)

current.year.pred2

mmy.pred[2]

#  Write to File
write.csv(lm.df12, 
         file = "RunningProjectR/data-output/2019-12-31_hs_girls_800m_predictions_linear-regression_2011-19.csv")







