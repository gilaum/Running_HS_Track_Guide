# Tom Alig
# Nov 26, 2019
# Logistic Regression

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(aod)
library(gridExtra)
library(grid)
#library(lm)
#library(caret)
source("RunningProjectR/scripts/my_track_scripts.R")

# Read in the data
#train.logreg <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_predictions_linear-regression_2011-19.csv")

glm.df <- read.csv("RunningProjectR/data-after_raw/2019-12-31_hs_girls_800m_improve_one-row-per-athlete_2011-19.csv")
#########################
# copy the df
glm.df2 <- glm.df
# Lumps states with small counts together
#   Necessary because linear regression in R limits categorical values to a 
#    max of 53
glm.df2 <- glm.df2 %>% 
  mutate(State = fct_lump(State, n = 50)) 

# Breaks for times for this event
times <- (c(2.16667, 2.25, 2.3333, 2.416667, 2.5, 2.5833333, 2.666667))

# Identify whether a runner has run better than 2:30 in the 10th grade
#  This is a simple yes/no
#  code of 1 for has broken 2:30 in 10th grade
#  code of 0 for has not broken 2:30

names(glm.df2)
dim(glm.df2)
glm.df2 <- glm.df2 %>% 
  mutate(faster.than.230.grade10 = ifelse(Gr10b < 2.5, 1, 0)) 

table(glm.df2$faster.than.230.grade10)
# Of those who have run as 10th graders, 28.4% have run faster than 2:30

# Now do this for all times of interest
glm.df2 <- glm.df2 %>% 
  mutate(faster.than.210.grade10 = ifelse(Gr10b < 2.16667, 1, 0),
         faster.than.215.grade10 = ifelse(Gr10b < 2.25, 1, 0),
         faster.than.220.grade10 = ifelse(Gr10b < 2.3333, 1, 0),
         faster.than.225.grade10 = ifelse(Gr10b < 2.416667, 1, 0),
         faster.than.235.grade10 = ifelse(Gr10b < 2.5833333, 1, 0),
         faster.than.240.grade10 = ifelse(Gr10b < 2.6666667, 1, 0))

# Logistic Regression Model

names(glm.df2)

# Predict for 10th Grade
#  |||
#  VVV

df.10.210 <- glm.func.10(newdf, glm.df2$faster.than.210.grade10, 
            glm.df2$Gr9b, 
            glm.df2$State, 
            glm.df2, pprob.gr10.2.10)

df.10.215 <- glm.func.10(newdf, glm.df2$faster.than.215.grade10, 
                         glm.df2$Gr9b, 
                         glm.df2$State, 
                         glm.df2, pprob.gr10.2.15)

df.10.220 <- glm.func.10(newdf, glm.df2$faster.than.220.grade10, 
                   glm.df2$Gr9b, 
                   glm.df2$State, 
                   glm.df2, pprob.gr10.2.20)

df.10.225 <- glm.func.10(newdf, glm.df2$faster.than.225.grade10, 
                         glm.df2$Gr9b, 
                         glm.df2$State, 
                         glm.df2, pprob.gr10.2.25)

df.10.230 <- glm.func.10(newdf, glm.df2$faster.than.230.grade10, 
            glm.df2$Gr9b, 
            glm.df2$State, 
            glm.df2, pprob.gr10.2.30)

df.10.235 <- glm.func.10(newdf, glm.df2$faster.than.235.grade10, 
                         glm.df2$Gr9b, 
                         glm.df2$State, 
                         glm.df2, pprob.gr10.2.35)

df.10.240 <- glm.func.10(newdf, glm.df2$faster.than.240.grade10, 
                         glm.df2$Gr9b, 
                         glm.df2$State, 
                         glm.df2, pprob.gr10.2.40)

Gr10prob <- cbind(df.10.210, df.10.215$prdprob, df.10.220$prdprob,
                  df.10.225$prdprob, df.10.230$prdprob,
                  df.10.235$prdprob, df.10.240$prdprob)


Gr10prob <- Gr10prob %>% 
  mutate(gr10prob210 = prdprob,
         gr10prob215 = `df.10.215$prdprob`,
       gr10prob220 = `df.10.220$prdprob`,
       gr10prob225 = `df.10.225$prdprob`,
       gr10prob230 = `df.10.230$prdprob`,
       gr10prob235 = `df.10.235$prdprob`,
       gr10prob240 = `df.10.240$prdprob`) %>% 
  select(-c(`df.10.215$prdprob`, `df.10.220$prdprob`,
            `df.10.225$prdprob`, `df.10.230$prdprob`,
            `df.10.235$prdprob`, `df.10.240$prdprob`))

names(Gr10prob)

#  ^^^^^^^
#  |||||||
# /predict for 10th grade
#################################################
#################################################
# Predict for 11th Grade
#  |||
#  VVV

# copy the df
glm11 <- Gr10prob

glm11 <- glm11 %>% 
  mutate(faster.than.210.grade11 = ifelse(Gr11b < 2.16667, 1, 0),
         faster.than.215.grade11 = ifelse(Gr11b < 2.25, 1, 0),
         faster.than.220.grade11 = ifelse(Gr11b < 2.3333, 1, 0),
         faster.than.225.grade11 = ifelse(Gr11b < 2.416667, 1, 0),
         faster.than.230.grade11 = ifelse(Gr11b < 2.5, 1, 0),
         faster.than.235.grade11 = ifelse(Gr11b < 2.5833333, 1, 0),
         faster.than.240.grade11 = ifelse(Gr11b < 2.6666667, 1, 0))

table(glm11$faster.than.230.grade11)
# Of those who have run as 11th graders, 34.3% have run faster than 2:30

glm11 <- glm11 %>% 
  select(-c(fit, se.fit, residual.scale, UL, LL, prdprob))

names(glm11)

# Logistic Regression Model
# model building based on grades 9 and 10

# First, use both grades 9 and 10 as predictors

df.11.210 <- glm.func.11(newdf2, glm11$faster.than.210.grade11, 
                         glm11$Gr9b,
                         glm11$Gr10b,
                         glm11$State, 
                         glm11, pprob.gr11.2.10)

df.11.215 <- glm.func.11(newdf2, glm11$faster.than.215.grade11, 
                         glm11$Gr9b,
                         glm11$Gr10b,
                         glm11$State, 
                         glm11, pprob.gr11.2.15)


df.11.220 <- glm.func.11(newdf2, glm11$faster.than.220.grade11, 
                         glm11$Gr9b,
                         glm11$Gr10b,
                         glm11$State, 
                         glm11, pprob.gr11.2.20)

df.11.225 <- glm.func.11(newdf2, glm11$faster.than.225.grade11, 
                         glm11$Gr9b,
                         glm11$Gr10b,
                         glm11$State, 
                         glm11, pprob.gr11.2.25)

df.11.230 <- glm.func.11(newdf2, glm11$faster.than.230.grade11, 
                         glm11$Gr9b,
                         glm11$Gr10b,
                         glm11$State, 
                         glm11, pprob.gr11.2.30)

df.11.235 <- glm.func.11(newdf2, glm11$faster.than.235.grade11, 
                         glm11$Gr9b,
                         glm11$Gr10b,
                         glm11$State, 
                         glm11, pprob.gr11.2.10)

df.11.240 <- glm.func.11(newdf2, glm11$faster.than.240.grade11, 
                         glm11$Gr9b,
                         glm11$Gr10b,
                         glm11$State, 
                         glm11, pprob.gr11.2.10)

Gr11prob <- cbind(df.11.210, df.11.215$prdprob, df.11.220$prdprob,
                  df.11.225$prdprob, df.11.230$prdprob,
                  df.11.235$prdprob, df.11.240$prdprob)

names(Gr11prob)

####
# Now predict Grade 11 based on Grade 9 only (no grade 10, because some run the
#   event in Grade 9, but not Grade 10, and then again in Grade 11)
#    Use glm.func.10 because this function calls for use of two variables
#      as predictors (in this case will be Grade 9 and State)

df.11.210b <- glm.func.10(newdf, glm11$faster.than.210.grade11, 
                         glm11$Gr9b, 
                         glm11$State, 
                         glm11, pprob.gr11.2.10)

df.11.215b <- glm.func.10(newdf, glm11$faster.than.215.grade11, 
                         glm11$Gr9b, 
                         glm11$State, 
                         glm11, pprob.gr11.2.15)

df.11.220b <- glm.func.10(newdf, glm11$faster.than.220.grade11, 
                         glm11$Gr9b, 
                         glm11$State, 
                         glm11, pprob.gr11.2.20)

df.11.225b <- glm.func.10(newdf, glm11$faster.than.225.grade11, 
                         glm11$Gr9b, 
                         glm11$State, 
                         glm11, pprob.gr11.2.25)

df.11.230b <- glm.func.10(newdf, glm11$faster.than.230.grade11, 
                         glm11$Gr9b, 
                         glm11$State, 
                         glm11, pprob.gr11.2.30)

df.11.235b <- glm.func.10(newdf, glm11$faster.than.235.grade11, 
                         glm11$Gr9b, 
                         glm11$State, 
                         glm11, pprob.gr11.2.35)

df.11.240b <- glm.func.10(newdf, glm11$faster.than.240.grade11, 
                         glm11$Gr9b, 
                         glm11$State, 
                         glm11, pprob.gr11.2.40)

####
# Now predict Grade 11 based on Grade 10 only (no grade 9, because some run the
#   event in Grade 10, but not Grade 9, and then again in Grade 11)
#    Use glm.func.10 because this function calls for use of two variables
#      as predictors (in this case will be Grade 10 and State)

df.11.210c <- glm.func.10(newdf, glm11$faster.than.210.grade11, 
                          glm11$Gr10b, 
                          glm11$State, 
                          glm11, pprob.gr11.2.10)

df.11.215c <- glm.func.10(newdf, glm11$faster.than.215.grade11, 
                          glm11$Gr10b, 
                          glm11$State, 
                          glm11, pprob.gr11.2.15)

df.11.220c <- glm.func.10(newdf, glm11$faster.than.220.grade11, 
                          glm11$Gr10b, 
                          glm11$State, 
                          glm11, pprob.gr11.2.20)

df.11.225c <- glm.func.10(newdf, glm11$faster.than.225.grade11, 
                          glm11$Gr10b, 
                          glm11$State, 
                          glm11, pprob.gr11.2.25)

df.11.230c <- glm.func.10(newdf, glm11$faster.than.230.grade11, 
                          glm11$Gr10b, 
                          glm11$State, 
                          glm11, pprob.gr11.2.30)

df.11.235c <- glm.func.10(newdf, glm11$faster.than.235.grade11, 
                          glm11$Gr10b, 
                          glm11$State, 
                          glm11, pprob.gr11.2.35)

df.11.240c <- glm.func.10(newdf, glm11$faster.than.240.grade11, 
                          glm11$Gr10b, 
                          glm11$State, 
                          glm11, pprob.gr11.2.40)

# copy glm11 df
glm11b <- glm11
names(glm11b)

glm11b <- cbind(glm11b, df.11.210$prdprob, df.11.215$prdprob, df.11.220$prdprob,
                df.11.225$prdprob, df.11.230$prdprob,
                df.11.235$prdprob, df.11.240$prdprob,
                df.11.210b$prdprob, df.11.215b$prdprob, df.11.220b$prdprob,
                df.11.225b$prdprob, df.11.230b$prdprob,
                df.11.235b$prdprob, df.11.240b$prdprob,
                df.11.210c$prdprob, df.11.215c$prdprob, df.11.220c$prdprob,
                df.11.225c$prdprob, df.11.230c$prdprob,
                df.11.235c$prdprob, df.11.240c$prdprob)

names(glm11b)

glm11b <- glm11b %>% 
  mutate(gr11prob210 = ifelse(is.na(Gr10b), `df.11.210b$prdprob`,
                              ifelse(is.na(Gr9b), `df.11.210c$prdprob`,
                                     `df.11.210$prdprob`)),
         gr11prob215 = ifelse(is.na(Gr10b), `df.11.215b$prdprob`,
                              ifelse(is.na(Gr9b), `df.11.215c$prdprob`,
                                     `df.11.215$prdprob`)),
         gr11prob220 = ifelse(is.na(Gr10b), `df.11.220b$prdprob`,
                              ifelse(is.na(Gr9b), `df.11.220c$prdprob`,
                                     `df.11.220$prdprob`)),
         gr11prob225 = ifelse(is.na(Gr10b), `df.11.225b$prdprob`,
                              ifelse(is.na(Gr9b), `df.11.225c$prdprob`,
                                     `df.11.225$prdprob`)),
         gr11prob230 = ifelse(is.na(Gr10b), `df.11.230b$prdprob`,
                              ifelse(is.na(Gr9b), `df.11.230c$prdprob`,
                                     `df.11.230$prdprob`)),
         gr11prob235 = ifelse(is.na(Gr10b), `df.11.235b$prdprob`,
                              ifelse(is.na(Gr9b), `df.11.235c$prdprob`,
                                     `df.11.235$prdprob`)),
         gr11prob240 = ifelse(is.na(Gr10b), `df.11.240b$prdprob`,
                              ifelse(is.na(Gr9b), `df.11.240c$prdprob`,
                                     `df.11.240$prdprob`)))

# copy df
glm11c <- glm11b

# drop columns
glm11c <- glm11c[-c(51:71)]
names(glm11c)

glm11c %>% 
  select(gr10prob225, gr11prob225, Athlete, Gr9b, Gr10b, Gr11b,
         everything()) %>% 
  filter(Team == "Saline") %>% 
  View()

#  ^^^^^^^
#  |||||||
#  Gr 11 predictions
#######################################
#################################################
# Grade 12 predictions
## |||||||||
## VVVVVVVVV

# Identify whether a runner has run better than 2:30 in the 12th grade
#  This is a simple yes/no
#  code of 1 for has broken 2:30 in 10th grade
#  code of 0 for has not broken 2:30
glm12 <- glm11c

glm12 <- glm12 %>% 
  mutate(faster.than.210.grade12 = ifelse(Gr12b < 2.16667, 1, 0),
         faster.than.215.grade12 = ifelse(Gr12b < 2.25, 1, 0),
         faster.than.220.grade12 = ifelse(Gr12b < 2.3333, 1, 0),
         faster.than.225.grade12 = ifelse(Gr12b < 2.416667, 1, 0),
         faster.than.230.grade12 = ifelse(Gr12b < 2.5, 1, 0),
         faster.than.235.grade12 = ifelse(Gr12b < 2.5833333, 1, 0),
         faster.than.240.grade12 = ifelse(Gr12b < 2.6666667, 1, 0))

table(glm12$faster.than.230.grade12)
# Of those who have run as 12th graders, 39.5% have run faster than 2:30

names(glm12)

#glm11 %>% 
#  filter(State == "BC") %>% 
#  View()
# glm12 has "BC" in State, whereas other glm's in this file 
#    did not have "BC" in State.
#    #trying to add BC to the model:

#glm12$xlevels
#glm12$xlevels[["State"]] <- union(glm12$xlevels[["State"]], levels(glm11c[["State"]]))
#lm.predict12 <- predict(glm12, glm.df2)
#lm.predict12

glm12$faster.than.210.grade12 %>% 
  View()

glm12 %>% 
  count(State) %>% 
  View()



# Logistic Regression Model

# model building based on grades 9 and 10
# First, use both grades 9 and 10 as predictors

df.12.210 <- glm.func.11(newdf3, glm12$faster.than.210.grade12, 
                         glm12$Gr9b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)

df.12.215 <- glm.func.11(newdf2, glm12$faster.than.215.grade12, 
                         glm12$Gr9b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.15)


df.12.220 <- glm.func.11(newdf2, glm12$faster.than.220.grade12, 
                         glm12$Gr9b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.20)

df.12.225 <- glm.func.11(newdf2, glm12$faster.than.225.grade12, 
                         glm12$Gr9b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.25)

df.12.230 <- glm.func.11(newdf2, glm12$faster.than.230.grade12, 
                         glm12$Gr9b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.30)

df.12.235 <- glm.func.11(newdf2, glm12$faster.than.235.grade12, 
                         glm12$Gr9b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)

df.12.240 <- glm.func.11(newdf2, glm12$faster.than.240.grade12, 
                         glm12$Gr9b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)




# Now, use both grades 9 and 11 as predictors

df.12.210b <- glm.func.11(newdf3, glm12$faster.than.210.grade12, 
                         glm12$Gr9b,
                         glm12$Gr11b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)

df.12.215b <- glm.func.11(newdf2, glm12$faster.than.215.grade12, 
                         glm12$Gr9b,
                         glm12$Gr11b,
                         glm12$State, 
                         glm12, pprob.gr12.2.15)


df.12.220b <- glm.func.11(newdf2, glm12$faster.than.220.grade12, 
                         glm12$Gr9b,
                         glm12$Gr11b,
                         glm12$State, 
                         glm12, pprob.gr12.2.20)

df.12.225b <- glm.func.11(newdf2, glm12$faster.than.225.grade12, 
                         glm12$Gr9b,
                         glm12$Gr11b,
                         glm12$State, 
                         glm12, pprob.gr12.2.25)

df.12.230b <- glm.func.11(newdf2, glm12$faster.than.230.grade12, 
                         glm12$Gr9b,
                         glm12$Gr11b,
                         glm12$State, 
                         glm12, pprob.gr12.2.30)

df.12.235b <- glm.func.11(newdf2, glm12$faster.than.235.grade12, 
                         glm12$Gr9b,
                         glm12$Gr11b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)

df.12.240b <- glm.func.11(newdf2, glm12$faster.than.240.grade12, 
                         glm12$Gr9b,
                         glm12$Gr11b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)


# Now, use both grades 10 and 11 as predictors

df.12.210c <- glm.func.11(newdf3, glm12$faster.than.210.grade12, 
                         glm12$Gr11b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)

df.12.215c <- glm.func.11(newdf2, glm12$faster.than.215.grade12, 
                         glm12$Gr11b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.15)


df.12.220c <- glm.func.11(newdf2, glm12$faster.than.220.grade12, 
                         glm12$Gr11b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.20)

df.12.225c <- glm.func.11(newdf2, glm12$faster.than.225.grade12, 
                         glm12$Gr11b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.25)

df.12.230c <- glm.func.11(newdf2, glm12$faster.than.230.grade12, 
                         glm12$Gr11b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.30)

df.12.235c <- glm.func.11(newdf2, glm12$faster.than.235.grade12, 
                         glm12$Gr11b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)

df.12.240c <- glm.func.11(newdf2, glm12$faster.than.240.grade12, 
                         glm12$Gr11b,
                         glm12$Gr10b,
                         glm12$State, 
                         glm12, pprob.gr12.2.10)

# Now, Grade 10 only as predictor
df.12.210d <- glm.func.10(newdf, glm12$faster.than.210.grade12, 
                          glm12$Gr10b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.10)

df.12.215d <- glm.func.10(newdf, glm12$faster.than.215.grade12, 
                          glm12$Gr10b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.15)

df.12.220d <- glm.func.10(newdf, glm12$faster.than.220.grade12, 
                          glm12$Gr10b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.20)

df.12.225d <- glm.func.10(newdf, glm12$faster.than.225.grade12, 
                          glm12$Gr10b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.25)

df.12.230d <- glm.func.10(newdf, glm12$faster.than.230.grade12, 
                          glm12$Gr10b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.30)

df.12.235d <- glm.func.10(newdf, glm12$faster.than.235.grade12, 
                          glm12$Gr10b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.35)

df.12.240d <- glm.func.10(newdf, glm12$faster.than.240.grade12, 
                          glm12$Gr10b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.40)

###
# Now, Grade 11 only as predictor
df.12.210e <- glm.func.10(newdf, glm12$faster.than.210.grade12, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.10)

df.12.215e <- glm.func.10(newdf, glm12$faster.than.215.grade12, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.15)

df.12.220e <- glm.func.10(newdf, glm12$faster.than.220.grade12, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.20)

df.12.225e <- glm.func.10(newdf, glm12$faster.than.225.grade12, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.25)

df.12.230e <- glm.func.10(newdf, glm12$faster.than.230.grade12, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.30)

df.12.235e <- glm.func.10(newdf, glm12$faster.than.235.grade12, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.35)

df.12.240e <- glm.func.10(newdf, glm12$faster.than.240.grade12, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.40)


# Now, Grade 9 only as predictor
df.12.210f <- glm.func.10(newdf, glm12$faster.than.210.grade12, 
                          glm12$Gr9b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.10)

df.12.215f <- glm.func.10(newdf, glm12$faster.than.215.grade12, 
                          glm12$Gr9b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.15)

df.12.220f <- glm.func.10(newdf, glm12$faster.than.220.grade12, 
                          glm12$Gr9b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.20)

df.12.225f <- glm.func.10(newdf, glm12$faster.than.225.grade12, 
                          glm12$Gr9b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.25)

df.12.230f <- glm.func.10(newdf, glm12$faster.than.230.grade12, 
                          glm12$Gr9b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.30)

df.12.235f <- glm.func.10(newdf, glm12$faster.than.235.grade12, 
                          glm12$Gr9b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.35)

df.12.240f <- glm.func.10(newdf, glm12$faster.than.240.grade12, 
                          glm12$Gr9b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.40)

# Now, Grade 9, 10, and 11 only as predictor
df.12.210g <- glm.func.12(newdf, glm12$faster.than.210.grade12, 
                          glm12$Gr9b, 
                          glm12$Gr10b, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.10)

df.12.215g <- glm.func.12(newdf, glm12$faster.than.215.grade12, 
                          glm12$Gr9b, 
                          glm12$Gr10b, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.15)

df.12.220g <- glm.func.12(newdf, glm12$faster.than.220.grade12, 
                          glm12$Gr9b, 
                          glm12$Gr10b, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.20)

df.12.225g <- glm.func.12(newdf, glm12$faster.than.225.grade12, 
                          glm12$Gr9b, 
                          glm12$Gr10b, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.25)

df.12.230g <- glm.func.12(newdf, glm12$faster.than.230.grade12, 
                          glm12$Gr9b, 
                          glm12$Gr10b, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.30)

df.12.235g <- glm.func.12(newdf, glm12$faster.than.235.grade12, 
                          glm12$Gr9b, 
                          glm12$Gr10b, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.35)

df.12.240g <- glm.func.12(newdf, glm12$faster.than.240.grade12, 
                          glm12$Gr9b, 
                          glm12$Gr10b, 
                          glm12$Gr11b, 
                          glm12$State, 
                          glm12, pprob.gr12.2.40)

# copy the df
glm12b <- glm12
names(glm12b)

glm12b <- cbind(glm12b, df.12.210$prdprob, df.12.215$prdprob, df.12.220$prdprob,
                df.12.225$prdprob, df.12.230$prdprob,
                df.12.235$prdprob, df.12.240$prdprob,
                df.12.210b$prdprob, df.12.215b$prdprob, df.12.220b$prdprob,
                df.12.225b$prdprob, df.12.230b$prdprob,
                df.12.235b$prdprob, df.12.240b$prdprob,
                df.12.210c$prdprob, df.12.215c$prdprob, df.12.220c$prdprob,
                df.12.225c$prdprob, df.12.230c$prdprob,
                df.12.235c$prdprob, df.12.240c$prdprob,
                df.12.210d$prdprob, df.12.215d$prdprob, df.12.220d$prdprob,
                df.12.225d$prdprob, df.12.230d$prdprob,
                df.12.235d$prdprob, df.12.240d$prdprob,
                df.12.210e$prdprob, df.12.215e$prdprob, df.12.220e$prdprob,
                df.12.225e$prdprob, df.12.230e$prdprob,
                df.12.235e$prdprob, df.12.240e$prdprob,
                df.12.210f$prdprob, df.12.215f$prdprob, df.12.220f$prdprob,
                df.12.225f$prdprob, df.12.230f$prdprob,
                df.12.235f$prdprob, df.12.240f$prdprob,
                df.12.210g$prdprob, df.12.215g$prdprob, df.12.220g$prdprob,
                df.12.225g$prdprob, df.12.230g$prdprob,
                df.12.235g$prdprob, df.12.240g$prdprob)

names(glm12b)
# copy the df
glm12c <- glm12b

glm12c <- glm12c %>% 
  mutate(gr12prob210 = ifelse(!is.na(Gr11b) & !is.na(Gr10b) & !is.na(Gr9b), `df.12.210g$prdprob`,
                              ifelse(!is.na(Gr10b) & !is.na(Gr11b), `df.12.210c$prdprob`,
                                     ifelse(!is.na(Gr9b) & !is.na(Gr11b), `df.12.210b$prdprob`, 
                                            ifelse(!is.na(Gr9b) & !is.na(Gr10b), `df.12.210$prdprob`,
                                                   ifelse(!is.na(Gr11b), `df.12.210e$prdprob`, 
                                                          ifelse(!is.na(Gr10b), `df.12.210d$prdprob`,
                                     `df.12.210f$prdprob`)))))),
         gr12prob215 = ifelse(!is.na(Gr11b) & !is.na(Gr10b) & !is.na(Gr9b), `df.12.215g$prdprob`,
                             ifelse(!is.na(Gr10b) & !is.na(Gr11b), `df.12.215c$prdprob`,
                                    ifelse(!is.na(Gr9b) & !is.na(Gr11b), `df.12.215b$prdprob`, 
                                           ifelse(!is.na(Gr9b) & !is.na(Gr10b), `df.12.215$prdprob`,
                                                  ifelse(!is.na(Gr11b), `df.12.215e$prdprob`, 
                                                         ifelse(!is.na(Gr10b), `df.12.215d$prdprob`,
                                                                `df.12.215f$prdprob`)))))),
         gr12prob220 = ifelse(!is.na(Gr11b) & !is.na(Gr10b) & !is.na(Gr9b), `df.12.220g$prdprob`,
                              ifelse(!is.na(Gr10b) & !is.na(Gr11b), `df.12.220c$prdprob`,
                                     ifelse(!is.na(Gr9b) & !is.na(Gr11b), `df.12.220b$prdprob`, 
                                            ifelse(!is.na(Gr9b) & !is.na(Gr10b), `df.12.220$prdprob`,
                                                   ifelse(!is.na(Gr11b), `df.12.220e$prdprob`, 
                                                          ifelse(!is.na(Gr10b), `df.12.220d$prdprob`,
                                                                 `df.12.220f$prdprob`)))))),
         gr12prob225 = ifelse(!is.na(Gr11b) & !is.na(Gr10b) & !is.na(Gr9b), `df.12.225g$prdprob`,
                              ifelse(!is.na(Gr10b) & !is.na(Gr11b), `df.12.225c$prdprob`,
                                     ifelse(!is.na(Gr9b) & !is.na(Gr11b), `df.12.225b$prdprob`, 
                                            ifelse(!is.na(Gr9b) & !is.na(Gr10b), `df.12.225$prdprob`,
                                                   ifelse(!is.na(Gr11b), `df.12.225e$prdprob`, 
                                                          ifelse(!is.na(Gr10b), `df.12.225d$prdprob`,
                                                                 `df.12.225f$prdprob`)))))),
         gr12prob230 = ifelse(!is.na(Gr11b) & !is.na(Gr10b) & !is.na(Gr9b), `df.12.230g$prdprob`,
                              ifelse(!is.na(Gr10b) & !is.na(Gr11b), `df.12.230c$prdprob`,
                                     ifelse(!is.na(Gr9b) & !is.na(Gr11b), `df.12.230b$prdprob`, 
                                            ifelse(!is.na(Gr9b) & !is.na(Gr10b), `df.12.230$prdprob`,
                                                   ifelse(!is.na(Gr11b), `df.12.230e$prdprob`, 
                                                          ifelse(!is.na(Gr10b), `df.12.230d$prdprob`,
                                                                 `df.12.230f$prdprob`)))))),
         gr12prob235 = ifelse(!is.na(Gr11b) & !is.na(Gr10b) & !is.na(Gr9b), `df.12.235g$prdprob`,
                              ifelse(!is.na(Gr10b) & !is.na(Gr11b), `df.12.235c$prdprob`,
                                     ifelse(!is.na(Gr9b) & !is.na(Gr11b), `df.12.235b$prdprob`, 
                                            ifelse(!is.na(Gr9b) & !is.na(Gr10b), `df.12.235$prdprob`,
                                                   ifelse(!is.na(Gr11b), `df.12.235e$prdprob`, 
                                                          ifelse(!is.na(Gr10b), `df.12.235d$prdprob`,
                                                                 `df.12.235f$prdprob`)))))),
         gr12prob240 = ifelse(!is.na(Gr11b) & !is.na(Gr10b) & !is.na(Gr9b), `df.12.240g$prdprob`,
                              ifelse(!is.na(Gr10b) & !is.na(Gr11b), `df.12.240c$prdprob`,
                                     ifelse(!is.na(Gr9b) & !is.na(Gr11b), `df.12.240b$prdprob`, 
                                            ifelse(!is.na(Gr9b) & !is.na(Gr10b), `df.12.240$prdprob`,
                                                   ifelse(!is.na(Gr11b), `df.12.240e$prdprob`, 
                                                          ifelse(!is.na(Gr10b), `df.12.240d$prdprob`,
                                                                 `df.12.240f$prdprob`))))))) 
 


names(glm12c)

#copy df
glm12d <- glm12c
 
glm12d <- glm12d[-c(65:113)]

names(glm12d)

glm12d %>% 
  select(gr12prob225, gr11prob225, gr10prob225, Athlete, Gr12b, Gr11b, Gr10b,
         Gr9b, 
         everything()) %>% 
  filter(Team == "Saline") %>% 
  View()                                     

#  Write to File
write.csv(glm12d, 
          file = "RunningProjectR/data-output/2019-12-31_hs_girls_800m_probabilities_glm_2011-19.csv")


#############################

# Gauge Plot
df_sub <- glm12d %>% 
  filter(Athlete == "Lydia Alig")

###

gg.gauge_percent <- function(pos,breaks=c(0,30,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#D55E00")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#F0E442")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#009E73")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), 
              size=3, 
              fontface="bold", 
              vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),
                  y=1.1*sin(pi*(1-breaks/100)),
                  label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,
             label=paste(pos,"%"),
             vjust=0,
             size=5,
             fontface="bold")+
    coord_fixed()+
    #ggtitle(df_sub$Athlete, "Probability of Running") +
   # ggtitle(paste("Probability of Running", stuff)) +
    theme_bw()+
    #theme(panel.border=element_rect(fill=NA)) +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank())
}

val.a <- round(df_sub$gr10prob240 * 100, 0)
val.b <- round(df_sub$gr10prob235 * 100, 0)
val.c <- round(df_sub$gr10prob230 * 100, 0)
val.d <- round(df_sub$gr10prob225 * 100, 0)
val.e <- round(df_sub$gr10prob220 * 100, 0)
val.f <- round(df_sub$gr10prob215 * 100, 0)
val.g <- round(df_sub$gr10prob210 * 100, 0)
p1 <- gg.gauge_percent(val.a , breaks=c(0,30,70,100))
p2 <- gg.gauge_percent(val.b , breaks=c(0,30,70,100))
p3 <- gg.gauge_percent(val.c , breaks=c(0,30,70,100))
p4 <- gg.gauge_percent(val.d , breaks=c(0,30,70,100))
p5 <- gg.gauge_percent(val.e , breaks=c(0,30,70,100))
p6 <- gg.gauge_percent(val.f , breaks=c(0,30,70,100))
p7 <- gg.gauge_percent(val.g , breaks=c(0,30,70,100))

bang <- times * 60
bong <- round(as.hms(bang, 2))
as.hms(bong)[7]

my.gages <- grid.arrange(arrangeGrob(p1, top = paste("Probability of", as.hms(bong)[7])),
             arrangeGrob(p2, top = paste("Probability of", as.hms(bong)[6])),
             arrangeGrob(p3, top = paste("Probability of", as.hms(bong)[5])),
             arrangeGrob(p4, top = paste("Probability of", as.hms(bong)[4])),
             arrangeGrob(p5, top = paste("Probability of", as.hms(bong)[3])),
             arrangeGrob(p6, top = paste("Probability of", as.hms(bong)[2])),
             arrangeGrob(p7, top = paste("Probability of", as.hms(bong)[1])),
             #p3, p4, p5, p6, p7, 
             #top = df_sub$Athlete,
             top = "Lydia Alig",
             ncol = 2)
grid.rect(width = .98, height = .98, gp = gpar(lwd = 2, col = "black", fill = NA))
###########
