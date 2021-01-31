# Discarded Code




# Discarded Code
# |||||||
# VVVVVVV


# Get each individual runner into one row; eg "one row per athlete"
sms3 <- sms3 %>% 
  select(-Year) %>% 
  distinct(Runner, Grade, Time, .keep_all = TRUE) %>% 
  spread(key = Grade, value = Time) %>% 
  drop_na()

# Rename columns
colnames(sms3)[2] <- "Grade8"
colnames(sms3)[3] <- "Grade9"

dim(sms3)

sms3 %>% 
  View()

# Plot the data
ggplot(sms3,aes(x = Grade8 * 60, y = Grade9 * 60)) + 
  geom_point() +
  scale_x_time(labels = time_format("%M:%S"),
               name = "MS 8th Grade Time in Minutes") +
  scale_y_time(labels = time_format("%M:%S"),
               name = "HS 9th Grade Time in Minutes") +
  stat_smooth(method = lm)

# 8th and 9th grade density plots
plot(density(sms3$Grade8), 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(sms3$Grade8), 2)))  

plot(density(sms3$Grade9), 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(sms3$Grade9), 2)))  

# Calculate correlation coefficient
round(cor(sms3$Grade9, sms3$Grade8),2)

# Build linear regression model
linearMod <- lm(Grade9 ~ Grade8, data=sms3)  # build linear regression model on full data
print(linearMod)

summary(linearMod)

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(sms3), 0.75*nrow(sms3))  # row indices for training data
trainingData <- sms3[trainingRowIndex, ]  # model training data
testData  <- sms3[-trainingRowIndex, ]   # test data
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

df <- sms3 %>% 
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
combdata <- cbind(sms3,newdf)
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






sms4 %>% 
  #group_by(Athlete, Grade) %>% 
  rowwise() %>% 
  mutate(score = sum(c(Portage, SEC1, SEC2, SEC3, MegaJambo, Coach.Legends, na.rm = TRUE))) %>% 
  select(score, everything()) %>% 
  #filter(counted < 4) %>% 
  View()


sms4 %>% 
  group_by(Athlete, Grade) %>% 
  mutate(counted = 6 - sum(is.na(c(Portage, SEC1, SEC2, SEC3, MegaJambo, Coach.Legends)))) %>% 
  #mutate(counted = 3 - sum(is.na(score.meets))) %>% 
  #mutate(bah = sum(score.meets)) %>% 
  select(counted, everything()) %>% 
  filter(counted < 4) %>% 
  View()


sms4 %>% 
  mutate(portage2 = Portage * 4/3) %>% 
  #filter((Meet != "SEC3" | Year != "2011"))
  rowwise() %>% 
  mutate(score1 = sum(portage2, SEC1, MegaJambo, SEC3, SEC2, Coach.Legends, na.rm = TRUE)) %>% 
  #mutate(worst = top_n(2)) %>% 
  select(score1, portage2, Portage, everything()) %>% 
  View()


shs5.9

df.agg <- aggregate(score2 ~ Athlete, shs5, max)
df.max <- merge(df.agg, shs5)

df.max %>% 
  View()


#shs.scores <- 
shs5 %>% 
  #group_by(Athlete) %>% 
  #rowwise() %>% 
  select(score2, Grade, Athlete, score1, Year) %>% 
  #select(score2, Grade, Athlete, score1) %>% 
  #mutate(row = row_number()) %>% 
  #mutate(row_num = 1:n()) %>%
  #group_by(Athlete) %>% 
  #distinct(score2, Athlete, Grade) %>% 
  #distinct(score2, Athlete, .keep_all = TRUE) %>% 
  #distinct(score2, keep.all = TRUE) %>% 
  #distinct(score2, Athlete, Grade) %>% 
  #View()
  pivot_wider(names_from = c(Grade, Year), values_from = c(score1, score2)) %>%
  #pivot_wider(names_from = Grade, values_from = c(score1, score2, counted)) %>%
  select(-starts_with("score1_")) %>% 
  select(order(colnames(.))) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  #mutate(new12 = ifelse(score2_12_2010:score2_12_2019) > 0, score2_12_2010:score2_12_2019, 0) %>% 
  #mutate(mak = do.call(pmax, (.))) %>%
  #mutate(new12 = pmax(score2_12_2010:score2_12_2019, na.rm = TRUE)) %>% 
  #mutate(new12 = ifelse(is.na(score2_12_2010:score2_12_2019)), 0, score2_12_2010:score2_12_2019) %>% 
  #mutate(new12 = if(starts_with("score2_12_") > 0, starts_with("score2_12_"))) %>% 
  #mutate(new12 = ifelse(starts_with("score2_12_") > 0, starts_with("score2_12_"), 0 )) %>% 
  #mutate(new12 = ifelse(starts_with("score2_12_") == "NA", 0, starts_with("score2_12_"))) %>% 
  #unite(newnew, starts_with("score2_12_"), remove = F, sep = " ") %>% 
  View()
dplyr::rename(Grade12.score = starts_with("score2_12_"),
              Grade11.score = starts_with("score2_11_"), 
              Grade10.score = starts_with("score2_10_"),
              Grade9.score = starts_with("score2_9_")) %>% 
  #group_by(Athlete) %>% 
  select(Athlete, Grade9.score, Grade10.score, Grade11.score, Grade12.score) %>% 
  #select(-counted) %>% 
  #ungroup()
  #group_by(Athlete) %>% 
  View()

shs.scores %>% 
  View()

head(shs.scores)


shs5 %>% 
  #filter(score2 == 103) %>% 
  View()

which(duplicated(shs5$score2))

shs5[c(661, 717, 893, 894),]


shs.scores <- shs5 %>% 
  select(score2, Grade, Athlete) %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>%
  #View()
  dplyr::rename(Grade12.score = `12`,
                Grade11.score = `11`,
                Grade10.score = `10`,
                Grade9.score = `9`) %>% 
  select(Athlete, Grade9.score, Grade10.score, Grade11.score, Grade12.score)


shs5 %>% 
  select(Athlete, Grade, score2) %>% 
  #distinct(Athlete, Grade, score2, .keep_all = TRUE) %>% 
  distinct(Athlete, score2, .keep_all = TRUE) %>% 
  spread(key = Grade, value = score2) %>% 
  #spread(key = Grade, value = score2) %>% 
  View()
#group_by(Athlete, Year) %>% 

blah12 <- starts_with("score2_12")
blah11 <- starts_with("score2_11")




df.clus.hs <- shs5 %>% 
  filter(Grade == "9" | Grade == "10") %>% 
  select(score2)

#df.clus.hs %>% 
#  View()

###################################################
### Create a 'scree' plot to determine the num of clusters
#####################################################

#dev.off()
#wssplot <- function(df.clus.hs, nc=15, seed=1234) {
#  wss <- (nrow(df.clus.hs)-1)*sum(apply(df.clus.hs,2,var))
#  for (i in 2:nc) {
#    set.seed(seed)
#    wss[i] <- sum(kmeans(df.clus.hs, centers=i)$withinss)}
#  plot(1:nc, wss, type="b", xlab="Number of Clusters",
#       ylab="Within groups sum of squares")} 

#wssplot(df.clus.hs)

# Scree plot tells us to use 5 clusters
# K Means clustering
#set.seed(123)
#km.res5 <- kmeans(df.clus.hs, 5, nstart = 100)
#print(km.res5)

# Create new data frame of clusters
#newdf <- as.data.frame(km.res5$cluster)
#newdf

# Add new df of clusters with original df
#shs5.910 <- shs5 %>% 
#  filter(Grade == "9" | Grade == "10")

#combdata <- cbind(shs5.910,newdf)

#combdata %>% 
#  View()

#str(combdata)

# Rename column
#colnames(combdata)[13] <- "Cluster.Num"

# To set up the graph, get score 2 into columns for Grade10 and Grade9
#  use pivot_wider to do this
#combdata2 <- combdata %>% 
#  pivot_wider(names_from = Grade, values_from = score2) %>% 
#  mutate(Grade9.score = `9`,
#         Grade10.score = `10`) %>% 
#  select(-c(`9`, `10`)) 

#combdata3 <- combdata %>% 
#  select(score2, Athlete, Grade, Cluster.Num) %>% 
#  pivot_wider(names_from = Grade,
#              values_from = c(Cluster.Num, score2)) 



#combdata2 %>% 
#  View()

#names(combdata2)
#class(combdata3)

#ggplot(combdata3,aes(x = score2_9, y = score2_10, 
#                     color = factor(Cluster.Num_10),
#                     group = Cluster.Num_10,
#                     na.rm = TRUE

#ggplot(combdata2,aes(x = Grade9.score, y = Grade10.score, 
#                     color = factor(Cluster.Num),
#                     group = Cluster.Num,
#                     na.rm = TRUE                     

#)) + 
#  geom_point() +
#  geom_text(aes(label = Athlete),
#            size = 3) +
#scale_x_time(labels = time_format("%M:%S"),
#             name = "MS 7th Grade Time in Minutes") +
#scale_y_time(labels = time_format("%M:%S"),
#             name = "HS 8th Grade Time in Minutes") +
#scale_color_manual(values = cb.six) +
#coord_cartesian(xlim = c(45, 70)) +
#coord_cartesian(ylim = c(48, 70),
#                xlim = c(48, 70)) +

#  labs(color = "Cluster Num 10th")  +
#  theme_bw() 

#combdata3 %>% 
#  View()


df.clus <- sms5 %>% 
  select(score2)

df.clus %>% 
  View()

###################################################
### Create a 'scree' plot to determine the num of clusters
#####################################################

dev.off()
wssplot <- function(df.clus, nc=15, seed=1234) {
  wss <- (nrow(df.clus)-1)*sum(apply(df.clus,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(df.clus, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(df.clus)

# Scree plot tells us to use 5 clusters
# K Means clustering
set.seed(123)
km.res5 <- kmeans(df.clus, 5, nstart = 100)
print(km.res5)

# Create new data frame of clusters
newdf <- as.data.frame(km.res5$cluster)
newdf

# Add new df of clusters with original df
combdata <- cbind(sms5,newdf)

# Rename column
colnames(combdata)[13] <- "Cluster.Num"

# To set up the graph, get score 2 into columns for Grade8 and Grade7
#  use pivot_wider to do this
combdata2 <- combdata %>% 
  pivot_wider(names_from = Grade, values_from = score2) %>% 
  mutate(Grade8.score = `8`,
         Grade7.score = `7`) %>% 
  select(-c(`7`, `8`)) 

combdata3 <- combdata %>% 
  select(score2, Athlete, Grade, Cluster.Num) %>% 
  pivot_wider(names_from = c(Grade),
              values_from = c(score2, Cluster.Num)) 


ggplot(combdata3,aes(x = score2_7, y = score2_8, 
                     color = factor(Cluster.Num_8),
                     group = Cluster.Num_8,
                     na.rm = TRUE
)) + 
  geom_point() +
  geom_text(aes(label = Athlete),
            size = 3) +
  #scale_x_time(labels = time_format("%M:%S"),
  #             name = "MS 7th Grade Time in Minutes") +
  #scale_y_time(labels = time_format("%M:%S"),
  #             name = "HS 8th Grade Time in Minutes") +
  #scale_color_manual(values = cb.six) +
  #coord_cartesian(xlim = c(45, 70)) +
  coord_cartesian(ylim = c(48, 70),
                  xlim = c(48, 70)) +
  
  labs(color = "Cluster Num 8th")  +
  theme_bw() 

combdata3 %>% 
  View()


#################################
################################
###############################

# Get so we have data for modeling purposes
#  Want EB race result, AND at least one of 7th grade and/or 8th grade score
eb9.modeldata <- pred.eb9 %>% 
  filter(Time2 > 0,
         Grade7.score > 0 | Grade8.score > 0)


