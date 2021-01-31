# Tom Alig
# Nov 26, 2019
# Logistic Regression

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(aod)
#library(lm)
#library(caret)
source("RunningProjectR/scripts/my_track_scripts.R")

# Read in the data
train.logreg <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_predictions_linear-regression_2011-19.csv")

#########################

# Identify whether a runner has run better than 2:30 in the 10th grade
#  This is a simple yes/no
#  code of 1 for has broken 2:30 in 10th grade
#  code of 0 for has not broken 2:30

# Remember, this is the training set
names(train.logreg)
dim(train.logreg)
train.logreg <- train.logreg %>% 
  mutate(faster.than.2.5.grade10 = ifelse(Gr10b < 2.5, 1, 0)) %>% 
  select(faster.than.2.5.grade10, everything()) 

table(train.logreg$faster.than.2.5.grade10)
# Of those who have run as 10th graders, 28.63% have run faster than 2:30

# Logistic Regression Model

# Predict for 10th Grade
#  |||
#  VVV
set.seed(157)
glm.pred.10bb <- glm(faster.than.2.5.grade10 ~ Gr9b + State,
                    data = train.logreg,
                    family = "binomial",
                    na.action = na.exclude)

summary(glm.pred.10bb)
confint.default(glm.pred.10bb)

wald.test(b = coef(glm.pred.10bb), Sigma = vcov(glm.pred.10bb), Terms = 2)

train.logreg$RankP <- predict(glm.pred.10bb, type = 'response')
train.logreg %>% 
  select(RankP, everything()) %>% 
  View()

newdata3 <- cbind(train.logreg, predict(glm.pred.10bb, data = train.logreg, type = "link", se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

newdata3 %>% 
  select(PredictedProb, everything()) %>% 
  View()

ggplot(newdata3, aes(x = Gr9b, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL)) +
              #alpha = 0.2) + 
  #geom_line() +
  geom_point()

# Find the p-value
with(glm.pred.10bb, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

logLik(glm.pred.10bb)
summary(glm.pred.10bb) #AIC = 13572

# View table of those with likelihood of > 50% of running better than 2:30
#  36% are projected to have better than 50% likelihood
newdata3 %>% 
  count(PredictedProb > 0.5, sort = TRUE)

times <- (c(2.16667, 2.25, 2.3333, 2.416667, 2.5, 2.5833333, 2.666667))
  
train.logreg <- train.logreg %>% 
  mutate(new10cola = ifelse(Gr10b < times[1], 1, 0),
           new10colb = ifelse(Gr10b < times[2], 1, 0),
           new10colc = ifelse(Gr10b < times[3], 1, 0),
           new10cold = ifelse(Gr10b < times[4], 1, 0),
           new10cole = ifelse(Gr10b < times[5], 1, 0),
           new10colf = ifelse(Gr10b < times[6], 1, 0),
           new10colg = ifelse(Gr10b < times[7], 1, 0))
  
times[5]

# Time < 2:10
df.1 <- glmx.func2(glm.pred.10.test.a, train.logreg$new10cola, 
                     #train.logreg$Gr10b, 
                     train.logreg$Gr9b, 
                     train.logreg$State,
                     train.logreg,
                     new.pred.10.test.a,
                     probpred10.a)
  
df.ab <- data.frame(matrix(unlist(df.1), 
                             ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.210.gr10 <- df.aab
  
df.1 <- glmx.func2(glm.pred.10.test.a, train.logreg$new10colb, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.10.test.a,
                   probpred10.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.215.gr10 <- df.aab

df.1 <- glmx.func2(glm.pred.10.test.a, train.logreg$new10colc, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.10.test.a,
                   probpred10.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.220.gr10 <- df.aab

df.1 <- glmx.func2(glm.pred.10.test.a, train.logreg$new10cold, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.10.test.a,
                   probpred10.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.225.gr10 <- df.aab

df.1 <- glmx.func2(glm.pred.10.test.a, train.logreg$new10cole, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.10.test.a,
                   probpred10.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.230.gr10 <- df.aab

df.1 <- glmx.func2(glm.pred.10.test.a, train.logreg$new10colf, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.10.test.a,
                   probpred10.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.235.gr10 <- df.aab

df.1 <- glmx.func2(glm.pred.10.test.a, train.logreg$new10colg, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.10.test.a,
                   probpred10.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.240.gr10 <- df.aab

# /predict for 10th grade
#########################
# Predict for 11th Grade
#  |||
#  VVV

# Identify whether a runner has run better than 2:30 in the 11th grade
#  This is a simple yes/no
#  code of 1 for has broken 2:30 in 10th grade
#  code of 0 for has not broken 2:30

# Remember, this is the training set
names(train.logreg)

train.logreg <- train.logreg %>% 
  mutate(faster.than.2.5.grade11 = ifelse(Gr11b < 2.5, 1, 0)) %>% 
  select(faster.than.2.5.grade11, everything()) 

table(train.logreg$faster.than.2.5.grade11)
# Of those who have run as 11th graders, 34.28% have run faster than 2:30

# Logistic Regression Model
# model building based on grades 9 and 10

####

# Try functions etc
train.logreg <- train.logreg %>% 
  mutate(new11cola = ifelse(Gr11b < times[1], 1, 0),
         new11colb = ifelse(Gr11b < times[2], 1, 0),
         new11colc = ifelse(Gr11b < times[3], 1, 0),
         new11cold = ifelse(Gr11b < times[4], 1, 0),
         new11cole = ifelse(Gr11b < times[5], 1, 0),
         new11colf = ifelse(Gr11b < times[6], 1, 0),
         new11colg = ifelse(Gr11b < times[7], 1, 0))

times[5]
names(train.logreg)
#################################################
# Grade 11 predictions
## |||||||||
## VVVVVVVVV

# First, use both grades 9 and 10 as predictors
sum(train.logreg$new11colc, na.rm = TRUE)
#  Time < 2:20
df <- glmx.func3(glm.pred.11.test.cc, train.logreg$new11colc, 
          train.logreg$Gr10b, 
          train.logreg$Gr9b, 
          train.logreg$State,
          train.logreg,
          new.pred.11.test.c,
          probpred11.test)

df.a <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df)), byrow = TRUE))
df.aa <- t(df.a)
sum(df.aa, na.rm = TRUE)
train.logreg$prob.220.gr11 <- df.aa
sum(train.logreg$prob.220.gr11, na.rm = TRUE)

#train.logreg$prob.2.33.gr11 <- df[[1]][52]

# Time < 2:10
df.1 <- glmx.func3(glm.pred.11.test.a, train.logreg$new11cola, 
                 train.logreg$Gr10b, 
                 train.logreg$Gr9b, 
                 train.logreg$State,
                 train.logreg,
                 new.pred.11.test.a,
                 probpred11.a)

df.a1 <- data.frame(matrix(unlist(df.1), 
                          ncol = max(lengths(df.1)), byrow = TRUE))
df.aa1 <- t(df.a1)

train.logreg$prob.210.gr11 <- df.aa1


# Time < 2:15
df.2 <- glmx.func3(glm.pred.11.test.b, train.logreg$new11colb, 
                   train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.b,
                   probpred11.b)

df.a2 <- data.frame(matrix(unlist(df.2), 
                           ncol = max(lengths(df.2)), byrow = TRUE))
df.aa2 <- t(df.a2)

train.logreg$prob.215.gr11 <- df.aa2

# Time < 2:25
df.4 <- glmx.func3(glm.pred.11.test.d, train.logreg$new11cold, 
                   train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.d,
                   probpred11.d)

df.a4 <- data.frame(matrix(unlist(df.4), 
                           ncol = max(lengths(df.4)), byrow = TRUE))
df.aa4 <- t(df.a4)

train.logreg$prob.225.gr11 <- df.aa4


# Time < 2:30
df.5 <- glmx.func3(glm.pred.11.test.e, train.logreg$new11cole, 
                   train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.e,
                   probpred11.e)

df.a5 <- data.frame(matrix(unlist(df.5), 
                           ncol = max(lengths(df.5)), byrow = TRUE))
df.aa5 <- t(df.a5)

train.logreg$prob.230.gr11 <- df.aa5

# Time < 2:35
df.6 <- glmx.func3(glm.pred.11.test.f, train.logreg$new11colf, 
                   train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.f,
                   probpred11.f)

df.a6 <- data.frame(matrix(unlist(df.6), 
                           ncol = max(lengths(df.6)), byrow = TRUE))
df.aa6 <- t(df.a6)

train.logreg$prob.235.gr11 <- df.aa6

# Time < 2:40
df.7 <- glmx.func3(glm.pred.11.test.g, train.logreg$new11colg, 
                   train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.g,
                   probpred11.g)

df.a7 <- data.frame(matrix(unlist(df.7), 
                           ncol = max(lengths(df.7)), byrow = TRUE))
df.aa7 <- t(df.a7)

train.logreg$prob.240.gr11 <- df.aa7

# Now, use Grade 9 only as a predictor for Grade 11

# This is where I left off on 12/15/19
# Next step is to create a new function that will take only 2 variables
# Then re-run grade 11 under two scenarios: Grade 9 only; and Grade 10 only
#  Then take the mean of the predicted likelihoods
#   There has got to be a way to do this with loops, lapply, functions, etc

#  Time < 2:20
df <- glmx.func2(glm.pred.11.test.cc, train.logreg$new11colc, 
                 train.logreg$Gr9b, 
                 train.logreg$State,
                 train.logreg,
                 new.pred.11.test.c,
                 probpred11.test)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.220.gr11.only9 <- df.aaa

# Time < 2:10
df.1 <- glmx.func2(glm.pred.11.test.a, train.logreg$new11cola, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.a,
                   probpred11.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.210.gr11.only9 <- df.aab


# Time < 2:15
df.2 <- glmx.func2(glm.pred.11.test.b, train.logreg$new11colb, 
                   #train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.b,
                   probpred11.b)

df.bb <- data.frame(matrix(unlist(df.2), 
                           ncol = max(lengths(df.2)), byrow = TRUE))
df.bbb <- t(df.bb)
train.logreg$prob.215.gr11.only9 <- df.bbb

# Time < 2:25
df.4 <- glmx.func2(glm.pred.11.test.d, train.logreg$new11cold, 
                  # train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.d,
                   probpred11.d)

df.dd <- data.frame(matrix(unlist(df.4), 
                           ncol = max(lengths(df.4)), byrow = TRUE))
df.ddd <- t(df.dd)
train.logreg$prob.225.gr11.only9 <- df.ddd


# Time < 2:30
df.5 <- glmx.func2(glm.pred.11.test.e, train.logreg$new11cole, 
                  # train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.e,
                   probpred11.e)

df.ee <- data.frame(matrix(unlist(df.5), 
                           ncol = max(lengths(df.5)), byrow = TRUE))
df.eee <- t(df.ee)
train.logreg$prob.230.gr11.only9 <- df.eee

# Time < 2:35
df.6 <- glmx.func2(glm.pred.11.test.f, train.logreg$new11colf, 
                  # train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.f,
                   probpred11.f)

df.ff <- data.frame(matrix(unlist(df.6), 
                           ncol = max(lengths(df.6)), byrow = TRUE))
df.fff <- t(df.ff)
train.logreg$prob.235.gr11.only9 <- df.fff

# Time < 2:40
df.7 <- glmx.func2(glm.pred.11.test.g, train.logreg$new11colg, 
                  # train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.g,
                   probpred11.g)

df.gg <- data.frame(matrix(unlist(df.7), 
                           ncol = max(lengths(df.7)), byrow = TRUE))
df.ggg <- t(df.gg)
train.logreg$prob.240.gr11.only9 <- df.ggg

names(train.logreg)
train.logreg %>% 
  #mutate(gr11prob210 = ifelse(Gr10b > 0 & Gr9b > 0, prob.210.gr11,
  #                             ifelse(is.na(Gr10b), prob.210.gr11.only9,
  #                                    "blah"))) %>% 
  mutate(gr11prob210 = ifelse(is.na(Gr10b), 
         prob.210.gr11.only9, prob.210.gr11)) %>% 
  select(gr11prob210, prob.210.gr11, prob.210.gr11.only9, Gr10b, Gr9b,
         everything()) %>% 
  filter(!is.na(gr11prob210)) %>% 
  View()

# Now, use Grade 10 only as a predictor for Grade 11

#  Time < 2:20
df <- glmx.func2(glm.pred.11.test.cc, train.logreg$new11colc, 
                 train.logreg$Gr10b, 
                 train.logreg$State,
                 train.logreg,
                 new.pred.11.test.c,
                 probpred11.test)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.220.gr11.only10 <- df.aaa

# Time < 2:10
df.1 <- glmx.func2(glm.pred.11.test.a, train.logreg$new11cola, 
                   train.logreg$Gr10b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.a,
                   probpred11.a)

df.ab <- data.frame(matrix(unlist(df.1), 
                           ncol = max(lengths(df.1)), byrow = TRUE))
df.aab <- t(df.ab)
train.logreg$prob.210.gr11.only10 <- df.aab


# Time < 2:15
df.2 <- glmx.func2(glm.pred.11.test.b, train.logreg$new11colb, 
                   train.logreg$Gr10b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.b,
                   probpred11.b)

df.bb <- data.frame(matrix(unlist(df.2), 
                           ncol = max(lengths(df.2)), byrow = TRUE))
df.bbb <- t(df.bb)
train.logreg$prob.215.gr11.only10 <- df.bbb

# Time < 2:25
df.4 <- glmx.func2(glm.pred.11.test.d, train.logreg$new11cold, 
                    train.logreg$Gr10b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.d,
                   probpred11.d)

df.dd <- data.frame(matrix(unlist(df.4), 
                           ncol = max(lengths(df.4)), byrow = TRUE))
df.ddd <- t(df.dd)
train.logreg$prob.225.gr11.only10 <- df.ddd


# Time < 2:30
df.5 <- glmx.func2(glm.pred.11.test.e, train.logreg$new11cole, 
                   train.logreg$Gr10b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.e,
                   probpred11.e)

df.ee <- data.frame(matrix(unlist(df.5), 
                           ncol = max(lengths(df.5)), byrow = TRUE))
df.eee <- t(df.ee)
train.logreg$prob.230.gr11.only10 <- df.eee

# Time < 2:35
df.6 <- glmx.func2(glm.pred.11.test.f, train.logreg$new11colf, 
                   train.logreg$Gr10b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.f,
                   probpred11.f)

df.ff <- data.frame(matrix(unlist(df.6), 
                           ncol = max(lengths(df.6)), byrow = TRUE))
df.fff <- t(df.ff)
train.logreg$prob.235.gr11.only10 <- df.fff

# Time < 2:40
df.7 <- glmx.func2(glm.pred.11.test.g, train.logreg$new11colg, 
                   train.logreg$Gr10b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.g,
                   probpred11.g)

df.gg <- data.frame(matrix(unlist(df.7), 
                           ncol = max(lengths(df.7)), byrow = TRUE))
df.ggg <- t(df.gg)
train.logreg$prob.240.gr11.only10 <- df.ggg

#  This is where I left off on 12/18/19. 
#   Next, need to run a for loop to get gr11prob215, 220, etc
#    Then need to get prob for Grade 12

names(train.logreg)
train.logreg %>% 
  mutate(gr11prob210 = ifelse(is.na(Gr10b), 
                              prob.210.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.210.gr11.only10,
                                     prob.210.gr11))) %>% 
  select(gr11prob210, prob.210.gr11, prob.210.gr11.only9, 
         prob.210.gr11.only10, Gr10b, Gr9b,
         everything()) %>% 
  filter(!is.na(gr11prob210)) %>% 
  View()

#  ^^^^^^^
#  |||||||
#  Gr 11 predictions
#######################################
#################################################
# Grade 12 predictions
## |||||||||
## VVVVVVVVV

# First, use grades 9, 10, and 11 as predictors
# First, predict for 2:10


# Identify whether a runner has run better than 2:30 in the 12th grade
#  This is a simple yes/no
#  code of 1 for has broken 2:30 in 10th grade
#  code of 0 for has not broken 2:30

# Remember, this is the training set
names(train.logreg)

train.logreg <- train.logreg %>% 
  mutate(faster.than.2.5.grade12 = ifelse(Gr12b < 2.5, 1, 0)) %>% 
  select(faster.than.2.5.grade12, everything()) 

table(train.logreg$faster.than.2.5.grade12)
# Of those who have run as 12th graders, 39.6% have run faster than 2:30

train.logreg <- train.logreg %>% 
  mutate(new12cola = ifelse(Gr12b < times[1], 1, 0),
         new12colb = ifelse(Gr12b < times[2], 1, 0),
         new12colc = ifelse(Gr12b < times[3], 1, 0),
         new12cold = ifelse(Gr12b < times[4], 1, 0),
         new12cole = ifelse(Gr12b < times[5], 1, 0),
         new12colf = ifelse(Gr12b < times[6], 1, 0),
         new12colg = ifelse(Gr12b < times[7], 1, 0))

# Logistic Regression Model

#  Likelihood of 2:10 or better
# Use Grade 11 as only predictor
df.a <- glmx.func2(glm.pred.12.test.a, train.logreg$new12cola, 
                   train.logreg$Gr11b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.12.test.a,
                   probpred12.a)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df.a)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.210.gr12.only11 <- df.aaa


# Use Grade 10 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12cola, 
                 train.logreg$Gr10b, 
                 #train.logreg$Gr9b, 
                 train.logreg$State,
                 train.logreg,
                 new.pred.12.test.ab,
                 probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                          ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.210.gr12.only10 <- df.aac

# Use Grade 9 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12cola, 
                    #train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.210.gr12.only9 <- df.aac

# Use Grades 9 and 10 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cola, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.210.gr12.9and10 <- df.aac

# Use Grades 9 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cola, 
                    train.logreg$Gr11b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.210.gr12.9and11 <- df.aac

# Use Grades 10 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cola, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.210.gr12.10and11 <- df.aac

# Use Grades 9, 10, and 11 as predictors
df.ab <- glmx.func4(glm.pred.12.test.ab, train.logreg$new12cola, 
                    train.logreg$Gr9b, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.210.gr12.9and10and11 <- df.aac


#  Likelihood of 2:15 or better
# Use Grade 11 as only predictor
df.a <- glmx.func2(glm.pred.12.test.a, train.logreg$new12colb, 
                   train.logreg$Gr11b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.12.test.a,
                   probpred12.a)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df.a)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.215.gr12.only11 <- df.aaa


# Use Grade 10 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colb, 
                    train.logreg$Gr10b, 
                    #train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.215.gr12.only10 <- df.aac

# Use Grade 9 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colb, 
                    #train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.215.gr12.only9 <- df.aac

# Use Grades 9 and 10 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colb, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.215.gr12.9and10 <- df.aac

# Use Grades 9 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colb, 
                    train.logreg$Gr11b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.215.gr12.9and11 <- df.aac

# Use Grades 10 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colb, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.215.gr12.10and11 <- df.aac

# Use Grades 9, 10, and 11 as predictors
df.ab <- glmx.func4(glm.pred.12.test.ab, train.logreg$new12colb, 
                    train.logreg$Gr9b, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.215.gr12.9and10and11 <- df.aac

#  Likelihood of 2:20 or better
# Use Grade 11 as only predictor
df.a <- glmx.func2(glm.pred.12.test.a, train.logreg$new12colc, 
                   train.logreg$Gr11b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.12.test.a,
                   probpred12.a)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df.a)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.220.gr12.only11 <- df.aaa


# Use Grade 10 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colc, 
                    train.logreg$Gr10b, 
                    #train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.220.gr12.only10 <- df.aac

# Use Grade 9 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colc, 
                    #train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.220.gr12.only9 <- df.aac

# Use Grades 9 and 10 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colc, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.220.gr12.9and10 <- df.aac

# Use Grades 9 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colc, 
                    train.logreg$Gr11b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.220.gr12.9and11 <- df.aac

# Use Grades 10 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colc, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.220.gr12.10and11 <- df.aac

# Use Grades 9, 10, and 11 as predictors
df.ab <- glmx.func4(glm.pred.12.test.ab, train.logreg$new12colc, 
                    train.logreg$Gr9b, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.220.gr12.9and10and11 <- df.aac


#  Likelihood of 2:25 or better
# Use Grade 11 as only predictor
df.a <- glmx.func2(glm.pred.12.test.a, train.logreg$new12cold, 
                   train.logreg$Gr11b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.12.test.a,
                   probpred12.a)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df.a)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.225.gr12.only11 <- df.aaa


# Use Grade 10 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12cold, 
                    train.logreg$Gr10b, 
                    #train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.225.gr12.only10 <- df.aac

# Use Grade 9 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12cold, 
                    #train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.225.gr12.only9 <- df.aac

# Use Grades 9 and 10 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cold, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.225.gr12.9and10 <- df.aac

# Use Grades 9 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cold, 
                    train.logreg$Gr11b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.225.gr12.9and11 <- df.aac

# Use Grades 10 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cold, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.225.gr12.10and11 <- df.aac

# Use Grades 9, 10, and 11 as predictors
df.ab <- glmx.func4(glm.pred.12.test.ab, train.logreg$new12cold, 
                    train.logreg$Gr9b, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.225.gr12.9and10and11 <- df.aac

#  Likelihood of 2:30 or better
# Use Grade 11 as only predictor
df.a <- glmx.func2(glm.pred.12.test.a, train.logreg$new12cole, 
                   train.logreg$Gr11b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.12.test.a,
                   probpred12.a)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df.a)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.230.gr12.only11 <- df.aaa


# Use Grade 10 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12cole, 
                    train.logreg$Gr10b, 
                    #train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.230.gr12.only10 <- df.aac

# Use Grade 9 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12cole, 
                    #train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.230.gr12.only9 <- df.aac

# Use Grades 9 and 10 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cole, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.230.gr12.9and10 <- df.aac

# Use Grades 9 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cole, 
                    train.logreg$Gr11b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.230.gr12.9and11 <- df.aac

# Use Grades 10 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12cole, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.230.gr12.10and11 <- df.aac

# Use Grades 9, 10, and 11 as predictors
df.ab <- glmx.func4(glm.pred.12.test.ab, train.logreg$new12cole, 
                    train.logreg$Gr9b, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.230.gr12.9and10and11 <- df.aac


#  Likelihood of 2:35 or better
# Use Grade 11 as only predictor
df.a <- glmx.func2(glm.pred.12.test.a, train.logreg$new12colf, 
                   train.logreg$Gr11b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.12.test.a,
                   probpred12.a)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df.a)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.235.gr12.only11 <- df.aaa


# Use Grade 10 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colf, 
                    train.logreg$Gr10b, 
                    #train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.235.gr12.only10 <- df.aac

# Use Grade 9 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colf, 
                    #train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.235.gr12.only9 <- df.aac

# Use Grades 9 and 10 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colf, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.235.gr12.9and10 <- df.aac

# Use Grades 9 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colf, 
                    train.logreg$Gr11b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.235.gr12.9and11 <- df.aac

# Use Grades 10 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colf, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.235.gr12.10and11 <- df.aac

# Use Grades 9, 10, and 11 as predictors
df.ab <- glmx.func4(glm.pred.12.test.ab, train.logreg$new12colf, 
                    train.logreg$Gr9b, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.235.gr12.9and10and11 <- df.aac

#  Likelihood of 2:40 or better
# Use Grade 11 as only predictor
df.a <- glmx.func2(glm.pred.12.test.a, train.logreg$new12colg, 
                   train.logreg$Gr11b, 
                   #train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.12.test.a,
                   probpred12.a)

df.aa <- data.frame(matrix(unlist(df), 
                           ncol = max(lengths(df.a)), byrow = TRUE))
df.aaa <- t(df.aa)
train.logreg$prob.240.gr12.only11 <- df.aaa


# Use Grade 10 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colg, 
                    train.logreg$Gr10b, 
                    #train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.240.gr12.only10 <- df.aac

# Use Grade 9 as only predictor
df.ab <- glmx.func2(glm.pred.12.test.ab, train.logreg$new12colg, 
                    #train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.240.gr12.only9 <- df.aac

# Use Grades 9 and 10 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colg, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.240.gr12.9and10 <- df.aac

# Use Grades 9 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colg, 
                    train.logreg$Gr11b, 
                    train.logreg$Gr9b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.240.gr12.9and11 <- df.aac

# Use Grades 10 and 11 as predictors
df.ab <- glmx.func3(glm.pred.12.test.ab, train.logreg$new12colg, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.240.gr12.10and11 <- df.aac

# Use Grades 9, 10, and 11 as predictors
df.ab <- glmx.func4(glm.pred.12.test.ab, train.logreg$new12colg, 
                    train.logreg$Gr9b, 
                    train.logreg$Gr10b, 
                    train.logreg$Gr11b, 
                    train.logreg$State,
                    train.logreg,
                    new.pred.12.test.ab,
                    probpred12.ab)

df.aab <- data.frame(matrix(unlist(df.ab), 
                            ncol = max(lengths(df.ab)), byrow = TRUE))
df.aac <- t(df.aab)
train.logreg$prob.240.gr12.9and10and11 <- df.aac

names(train.logreg)


# Obtain the likelihoods for each grade, and each time
# First, copy the df, then run the mutate function
# Finally, delete the unnecessary columns
names(train.logreg)

train.logreg <- train.logreg %>% 
  select(-c(prob.210.gr10.only9, prob.215.gr10.only9, prob.220.gr10.only9,
            prob.225.gr10.only9, prob.230.gr10.only9,
            prob.235.gr10.only9, prob.240.gr10.only9))
  
train.logreg2 <- train.logreg

# For Grade 11:
train.logreg2 <- train.logreg2 %>% 
  mutate(gr11prob210 = ifelse(is.na(Gr10b), 
                              prob.210.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.210.gr11.only10,
                                     prob.210.gr11))) %>%
  mutate(gr11prob215 = ifelse(is.na(Gr10b), 
                              prob.215.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.215.gr11.only10,
                                     prob.215.gr11))) %>%
  mutate(gr11prob220 = ifelse(is.na(Gr10b), 
                              prob.220.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.220.gr11.only10,
                                     prob.220.gr11))) %>%
  mutate(gr11prob225 = ifelse(is.na(Gr10b), 
                              prob.225.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.225.gr11.only10,
                                     prob.225.gr11))) %>%
  mutate(gr11prob230 = ifelse(is.na(Gr10b), 
                              prob.230.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.230.gr11.only10,
                                     prob.230.gr11))) %>%
  mutate(gr11prob235 = ifelse(is.na(Gr10b), 
                              prob.235.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.235.gr11.only10,
                                     prob.235.gr11))) %>%
  mutate(gr11prob240 = ifelse(is.na(Gr10b), 
                              prob.240.gr11.only9, 
                              ifelse(is.na(Gr9b), prob.240.gr11.only10,
                                     prob.240.gr11))) %>%
  select(gr11prob210, prob.210.gr11, prob.210.gr11.only9, 
         prob.210.gr11.only10, Gr10b, Gr9b,
         everything())


# For Grade 12:
train.logreg2 <- train.logreg2 %>% 
  mutate(gr12prob210 = ifelse(is.na(Gr11b) & is.na(Gr10b), 
                              prob.210.gr12.only9,
                              ifelse(is.na(Gr11b) & is.na(Gr9b),
                                     prob.210.gr12.only10,
                                     ifelse(is.na(Gr10b) & is.na(Gr9b),
                                            prob.210.gr12.only11,
                              ifelse(is.na(Gr9b), prob.210.gr12.10and11,
                                     ifelse(is.na(Gr10b), prob.210.gr12.9and11,
                                            ifelse(is.na(Gr11b), prob.210.gr12.9and10,
                                     prob.210.gr12.9and10and11))))))) %>% 
  mutate(gr12prob215 = ifelse(is.na(Gr11b) & is.na(Gr10b), 
                              prob.215.gr12.only9,
                              ifelse(is.na(Gr11b) & is.na(Gr9b),
                                     prob.215.gr12.only10,
                                     ifelse(is.na(Gr10b) & is.na(Gr9b),
                                            prob.215.gr12.only11,
                                            ifelse(is.na(Gr9b), prob.215.gr12.10and11,
                                                   ifelse(is.na(Gr10b), prob.215.gr12.9and11,
                                                          ifelse(is.na(Gr11b), prob.215.gr12.9and10,
                                                                 prob.215.gr12.9and10and11))))))) %>% 
  mutate(gr12prob220 = ifelse(is.na(Gr11b) & is.na(Gr10b), 
                              prob.220.gr12.only9,
                              ifelse(is.na(Gr11b) & is.na(Gr9b),
                                     prob.220.gr12.only10,
                                     ifelse(is.na(Gr10b) & is.na(Gr9b),
                                            prob.220.gr12.only11,
                                            ifelse(is.na(Gr9b), prob.220.gr12.10and11,
                                                   ifelse(is.na(Gr10b), prob.220.gr12.9and11,
                                                          ifelse(is.na(Gr11b), prob.220.gr12.9and10,
                                                                 prob.220.gr12.9and10and11))))))) %>% 
  mutate(gr12prob225 = ifelse(is.na(Gr11b) & is.na(Gr10b), 
                              prob.225.gr12.only9,
                              ifelse(is.na(Gr11b) & is.na(Gr9b),
                                     prob.225.gr12.only10,
                                     ifelse(is.na(Gr10b) & is.na(Gr9b),
                                            prob.225.gr12.only11,
                                            ifelse(is.na(Gr9b), prob.225.gr12.10and11,
                                                   ifelse(is.na(Gr10b), prob.225.gr12.9and11,
                                                          ifelse(is.na(Gr11b), prob.225.gr12.9and10,
                                                                 prob.225.gr12.9and10and11))))))) %>% 
  mutate(gr12prob230 = ifelse(is.na(Gr11b) & is.na(Gr10b), 
                              prob.230.gr12.only9,
                              ifelse(is.na(Gr11b) & is.na(Gr9b),
                                     prob.230.gr12.only10,
                                     ifelse(is.na(Gr10b) & is.na(Gr9b),
                                            prob.230.gr12.only11,
                                            ifelse(is.na(Gr9b), prob.230.gr12.10and11,
                                                   ifelse(is.na(Gr10b), prob.230.gr12.9and11,
                                                          ifelse(is.na(Gr11b), prob.230.gr12.9and10,
                                                                 prob.230.gr12.9and10and11))))))) %>% 
  mutate(gr12prob235 = ifelse(is.na(Gr11b) & is.na(Gr10b), 
                              prob.235.gr12.only9,
                              ifelse(is.na(Gr11b) & is.na(Gr9b),
                                     prob.235.gr12.only10,
                                     ifelse(is.na(Gr10b) & is.na(Gr9b),
                                            prob.235.gr12.only11,
                                            ifelse(is.na(Gr9b), prob.235.gr12.10and11,
                                                   ifelse(is.na(Gr10b), prob.235.gr12.9and11,
                                                          ifelse(is.na(Gr11b), prob.235.gr12.9and10,
                                                                 prob.235.gr12.9and10and11))))))) %>% 
  mutate(gr12prob240 = ifelse(is.na(Gr11b) & is.na(Gr10b), 
                              prob.240.gr12.only9,
                              ifelse(is.na(Gr11b) & is.na(Gr9b),
                                     prob.240.gr12.only10,
                                     ifelse(is.na(Gr10b) & is.na(Gr9b),
                                            prob.240.gr12.only11,
                                            ifelse(is.na(Gr9b), prob.240.gr12.10and11,
                                                   ifelse(is.na(Gr10b), prob.240.gr12.9and11,
                                                          ifelse(is.na(Gr11b), prob.240.gr12.9and10,
                                                                 prob.240.gr12.9and10and11))))))) %>% 
  select(gr12prob210, gr12prob215,gr12prob220,gr12prob225,
         gr12prob230, gr12prob235, gr12prob240,
         Gr12b,
         Gr11b, Gr10b, Gr9b,
         everything()) 


names(train.logreg2)

# duplicate df, then eliminate columns
train.logreg3 <- train.logreg2

train.logreg3 <- train.logreg3 %>% 
  rename(gr10prob210 = prob.210.gr10,
         gr10prob215 = prob.215.gr10,
         gr10prob220 = prob.220.gr10,
         gr10prob225 = prob.225.gr10,
         gr10prob230 = prob.230.gr10,
         gr10prob235 = prob.235.gr10,
         gr10prob240 = prob.240.gr10) %>% 
  select(-starts_with('improve')) %>% 
  select(-starts_with('new1')) %>% 
  select(-starts_with('prob.2')) %>% 
  select(-RankP)
  
names(train.logreg3)

train.logreg3 %>% 
  select(rfpred10, lmpred10, rfpred11, lmpred11,
         rfpred12, lmpred12, Gr9b, Gr10b, Gr11b, Gr12b,
         everything()) %>% 
  View()

##################################
######################################
## Discarded Code
## |||||||
## VVVVVVV



times

train.logreg4 <- train.logreg

multipetal <- function(df, n) {
  varname <- paste("faster.than.", n , sep="")
  df[[varname]] <- ifelse(df$Gr11b < n, 1, 0)
  #df
}

train.logreg5 <- multipetal(train.logreg4, times)

train.logreg5 %>% 
  View()

train.logreg6 <- train.logreg
for(i in times) {
  train.logreg6 <- train.logreg6 %>% 
    add_column(newname = paste("faster.than", i, sep = "")) %>% 
    mutate(train.logreg6[,newname] <- ifelse(Gr11b < i, 1, 0))
}

train.logreg6 %>% 
  View()

gr11times <- train.logreg6$Gr11b[1:20]
gr11times
crapx <- 0
for(i in times) {
  #crapx[i] <- ifelse(gr11times[8] < i, 1, 0)
  crapx[i] <- ifelse(gr11times < i, 1, 0)
  print(crapx[i])
  #print(crapx[,i])
  #print(gr11times)
}
print(i)


gr11times <- train.logreg6$Gr11b[1:20]
gr11times
crapx <- 0
for(i in times) {
  for (j in gr11times) {
    #crapx[i] <- ifelse(gr11times[8] < i, 1, 0)
    crapx[i] <- ifelse(gr11times[j] < i, 1, 0)
    print(crapx[i])
    #print(crapx[,i])
    #print(gr11times)
  }
}
print(i)
crapx[i]

gr11times[1:9]
length(gr11times)







newblahdf <- data.frame(cbind(gr11times, crapx[i]))
newblahdf

crapx[[2.25]]
gr11times[8]
crap[[8]][2]
length(crap)

newblah <- ifelse(gr11times < times, 1, 0)

lapply(gr11times, newblah)




faster.than[[3]][17]
length(faster.than)
times[[3]][1]

df.new <- cbind(df, ObJeCt[1:10])
names(df.new) <- c(names(df), paste("St", 1:10, sep = ""))

glmx.func(glm.pred.11.test.cc, train.logreg$new11colc, 
          train.logreg$Gr10b, 
          train.logreg$Gr9b, 
          train.logreg$State,
          train.logreg)

glmx.func(glm.pred.11.test.e, train.logreg$new11cole, 
          train.logreg$Gr10b, 
          train.logreg$Gr9b, 
          train.logreg$State,
          train.logreg)

head(glm.pred.11.test)

train.logreg$probpred11.test
names(train.logreg)
train.logreg$new11colc[1:20]
summary(glm.pred.11.test.cc)
#summary(neww)

pred.func2(new.pred.11.test.c, 
           train.logreg, 
           glm.pred.11.test.cc, 
           probpred11.test)

# Time < 2:40
df.7 <- glmx.func3(glm.pred.11.test.g, train.logreg$new11colg, 
                   train.logreg$Gr10b, 
                   train.logreg$Gr9b, 
                   train.logreg$State,
                   train.logreg,
                   new.pred.11.test.g,
                   probpred11.g)

df.77 <- data.frame(matrix(unlist(df.7), 
                           ncol = max(lengths(df.7)), byrow = TRUE))
df.777 <- t(df.77)

train.logreg$prob.240.gr11 <- df.777


train.logreg %>% 
  filter(Gr10b > 0 & Gr9b > 0) %>% 
  mutate(gr11prob210 = prob.2.17.gr11) %>% 
  select(gr11prob210, prob.2.17.gr11, prob.210.gr11, Gr10b, Gr9b,
         everything()) %>% 
  View()

set.seed(157)
glm.pred.11bb <- glm(faster.than.2.5.grade11 ~ Gr9b + Gr10b + State,
                     data = train.logreg,
                     family = "binomial",
                     na.action = na.exclude)

summary(glm.pred.11bb)
confint.default(glm.pred.11bb)

train.logreg$RankP11 <- predict(glm.pred.11bb, type = 'response')
train.logreg %>% 
  select(RankP11, everything()) %>% 
  View()

newdata3.11 <- cbind(train.logreg, predict(glm.pred.11bb, data = train.logreg, type = "link", se = TRUE))
newdata3.11 <- within(newdata3.11, {
  PredictedProb11 <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

newdata3.11 %>% 
  select(PredictedProb11, everything()) %>% 
  View()

ggplot(newdata3.11, aes(x = Gr10b, y = PredictedProb11)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL), #) +
              alpha = 0.2) + 
  #geom_line() +
  geom_point()

# Find the p-value
with(glm.pred.11bb, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

logLik(glm.pred.11bb)
summary(glm.pred.11bb) #AIC = 6916.9

####
# model building predicting grade 11 based on grade 10 only
set.seed(157)
glm.pred.11bb.a <- glm(faster.than.2.5.grade11 ~ Gr10b + State,
                       data = train.logreg,
                       family = "binomial",
                       na.action = na.exclude)

summary(glm.pred.11bb.a)
confint.default(glm.pred.11bb.a)

train.logreg$RankP11a <- predict(glm.pred.11bb.a, type = 'response')
train.logreg %>% 
  select(RankP11a, everything()) %>% 
  View()

newdata3.11a <- cbind(train.logreg, predict(glm.pred.11bb.a, data = train.logreg, type = "link", se = TRUE))
newdata3.11a <- within(newdata3.11a, {
  PredictedProb11a <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

newdata3.11a %>% 
  select(PredictedProb11a, everything()) %>% 
  View()

ggplot(newdata3.11a, aes(x = Gr10b, y = PredictedProb11a)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL), #) +
              alpha = 0.2) + 
  geom_line() 

ggplot(newdata3.11a, aes(x = Gr9b, y = PredictedProb11a)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL), #) +
              alpha = 0.2) + 
  #geom_line() 
  geom_point()

# Find the p-value
with(glm.pred.11bb.a, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

logLik(glm.pred.11bb.a)
summary(glm.pred.11bb.a) #AIC = 14715

####
# model building predicting grade 11 based on grade 9 only
set.seed(157)
glm.pred.11bb.b <- glm(faster.than.2.5.grade11 ~ Gr9b + State,
                       data = train.logreg,
                       family = "binomial",
                       na.action = na.exclude)

summary(glm.pred.11bb.b)
confint.default(glm.pred.11bb.b)

train.logreg$RankP11b <- predict(glm.pred.11bb.b, type = 'response')
train.logreg %>% 
  select(RankP11b, everything()) %>% 
  View()

newdata3.11b <- cbind(train.logreg, predict(glm.pred.11bb.b, data = train.logreg, type = "link", se = TRUE))
newdata3.11b <- within(newdata3.11b, {
  PredictedProb11b <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

newdata3.11b %>% 
  select(PredictedProb11b, everything()) %>% 
  View()

ggplot(newdata3.11b, aes(x = Gr10b, y = PredictedProb11b)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL), #) +
              alpha = 0.2) + 
  geom_line() 

ggplot(newdata3.11b, aes(x = Gr9b, y = PredictedProb11b)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL), #) +
              alpha = 0.2) + 
  #geom_line() 
  geom_point()

# Find the p-value
with(glm.pred.11bb.b, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

logLik(glm.pred.11bb.b)
summary(glm.pred.11bb.b) #AIC = 10528


#
#train.logreg %>% 
#  mutate(blahblah = ifelse(is.na(Gr10b), prob.210.gr11, ifelse(is.na(Gr9b), prob.210.gr11,
#         prob.2.17.gr11))) %>% 
#  select(blahblah, prob.2.17.gr11, prob.210.gr11, Gr10b, Gr9b,
#         everything()) %>% 
#  View()

