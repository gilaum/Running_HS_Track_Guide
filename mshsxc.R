# Tom Alig
# Oct 27, 2019
# 8th grade and 9th grade xc times

library(tidyverse)
library(lubridate)
library(scales)
library(hms)
library(reshape2)
library(data.table)
library(Hmisc)
library(tools)
source("RunningProjectR/scripts/my_track_scripts.R")

# Took the girls xc data from 2017, 2018, 2019 to do the analysis

# Read in the data
ms2hs <- read.csv("Grades8.9 xc Saline.csv")
str(ms2hs)

# Copy the df
ms2hs2 <- ms2hs

# Get Time field to be in Time format
ms2hs3 <- ms2hs2 %>% 
  mutate(time = lubridate::ms(Time)) %>% 
  mutate(minute = time$minute *60) %>% 
  mutate(second = time$second) %>% 
  mutate(total.secs = round((minute + second), digits = 2)) %>% 
  mutate(mins.dec = total.secs/60) %>% 
  select(-minute, -second) %>% 
  select(-c(Time, time, total.secs))  %>% 
  dplyr::rename(Time = mins.dec)

str(ms2hs3)

# Get each individual runner into one row; eg "one row per athlete"
ms2hs3 <- ms2hs3 %>% 
  select(-Year) %>% 
  distinct(Runner, Grade, Time, .keep_all = TRUE) %>% 
  spread(key = Grade, value = Time) %>% 
  drop_na()

# Rename columns
colnames(ms2hs3)[2] <- "Grade8"
colnames(ms2hs3)[3] <- "Grade9"

dim(ms2hs3)

ms2hs3 %>% 
  View()

# Plot the data
ggplot(ms2hs3,aes(x = Grade8 * 60, y = Grade9 * 60)) + 
  geom_point() +
  scale_x_time(labels = time_format("%M:%S"),
               name = "MS 8th Grade Time in Minutes") +
  scale_y_time(labels = time_format("%M:%S"),
               name = "HS 9th Grade Time in Minutes") +
  stat_smooth(method = lm)

# 8th and 9th grade density plots
plot(density(ms2hs3$Grade8), 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(ms2hs3$Grade8), 2)))  

plot(density(ms2hs3$Grade9), 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(ms2hs3$Grade9), 2)))  

# Calculate correlation coefficient
round(cor(ms2hs3$Grade9, ms2hs3$Grade8),2)

# Build linear regression model
linearMod <- lm(Grade9 ~ Grade8, data=ms2hs3)  # build linear regression model on full data
print(linearMod)

summary(linearMod)

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(ms2hs3), 0.75*nrow(ms2hs3))  # row indices for training data
trainingData <- ms2hs3[trainingRowIndex, ]  # model training data
testData  <- ms2hs3[-trainingRowIndex, ]   # test data
testData

# Build the model on training data
lmMod <- lm(Grade9 ~ Grade8, data=trainingData)  # build the model
distPred <- predict(lmMod, testData) 

summary (lmMod)
AIC (lmMod)

# Compare predictions to actuals
actuals_preds <- data.frame(cbind(actuals=testData$Grade9, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)
actuals_preds
correlation_accuracy #0.661

# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 96.59%, min_max accuracy
min_max_accuracy

# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 3.48%, mean absolute percentage deviation
mape

df <- ms2hs3 %>% 
  select(-Runner)


###################################################
### Create a 'scree' plot to determine the num of clusters
#####################################################

dev.off()
wssplot <- function(df, nc=15, seed=1234) {
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(df, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(df)

# Scree plot tells us to use 6 clusters
# K Means clustering
set.seed(123)
km.res6 <- kmeans(df, 6, nstart = 25)
print(km.res6)

# Create new data frame of clusters
newdf <- as.data.frame(km.res6$cluster)
newdf


# Add new df of clusters with original df
combdata <- cbind(ms2hs3,newdf)
head(combdata)

combdata %>% 
  View()

str(combdata)

combdata[4]

# Rename column
colnames(combdata)[4] <- "Cluster.Num"
str(combdata)

ggplot(combdata,aes(x = Grade8 * 60, y = Grade9 * 60, 
                    color = factor(Cluster.Num),
                    group = Cluster.Num
       )) + 
  geom_point() +
  geom_text(aes(label = Runner)) +
  scale_x_time(labels = time_format("%M:%S"),
               name = "MS 8th Grade Time in Minutes") +
  scale_y_time(labels = time_format("%M:%S"),
               name = "HS 9th Grade Time in Minutes") +
  scale_color_manual(values = cb.six) +
  labs(color = "Cluster #")  +
  theme_bw() 









