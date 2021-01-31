# Scripts for Project "Running"
# April 2019
# Tom Alig


####################
# Girls 800 meters variables
times <- (c(2.16667, 2.25, 2.3333, 2.416667, 2.5, 2.5833333, 2.666667))

event <- "800m"
event2 <- "Girls High School outdoor 800 meters"

slowest.time <- "2:45"
floor.perf <- as_hms(2.75*60)
floor.perf

sn <- "Ella Woehlke"
cdiv <- "D2"
hsteam <- "Saline"
years <- "2011 - 2019"

grade.2019.20 <- 12
current.school.year.grade <- grade.2019.20
last.year.grade <- current.school.year.grade - 1
two.years.ago.grade <- current.school.year.grade - 2

sn.current.grade <- paste("Gr", current.school.year.grade, sep = "")
sn.ly.grade <- paste("Gr", last.year.grade, sep = "")

# colors for main use
# colorblind blue
cb.blue <- "#0072B2"
cb.orange <- "#E69F00"
cb.purple <- "#CC79A7"
cb.six <- c("#000000",  
            cb.blue, 
            cb.orange, cb.purple,
            "#56B4E9", "#009E73" )

shapes <- c(19, 8) 
shapes4 <- c(16, 17, 15, 19)




#ath.best.last.year <- hs.800.one.row %>% 
#  filter(Athlete == sn,
#         Grade == last.year.grade) %>% 
#  select(Time)






###################




# Function to write raw data to file
#  f = file
#  yy = year
scrape.to.csv <- function(f, gender, event, yy) {
  f <- (paste("RunningProjectR/data-raw/",
              #enquo(yy),
              yy,
              '-12-31_high-school_',
              gender,
              '_',
              event,
              '_', 
              yy,
              '.csv', 
              sep = ""))
  
}

#################################################################
#################################################################

# Function to change column names from initial scraping
#  x will be the original file name
change.default.col.names <- function(x) {
  x %>% 
    dplyr::rename(Grade = X2,
                  Athlete = X3,
                  Time = X5,
                  State = X6,
                  Team = X7,
                  Date = X8,
                  Meet = X9,
                  Year = year) %>% 
    select(c(-X1, -X4))
}

#################################################################
#################################################################

# Get into proper date format
#  Get columns for month, monthday, year, and yearmonthday
#   d = df
get.date.format <- function(d){
  d %>% 
    mutate(moday = format(Date),
           moday2 = moday, 
           year2 = Year,
           date2 = Date) %>% 
    unite(moday, Date, Year) %>% 
    unite(moday2, date2, year2) %>% 
    mutate(newdate22 = gsub("_", "\\-", moday)) %>% 
    mutate(newdate222 = gsub("_", "\\-", moday2)) %>% 
    mutate(newdate33 = dmy(newdate22)) %>%
    mutate(newdate333 = mdy(newdate222)) %>%
    mutate(finaldate = ifelse(newdate33 %in% NA, newdate333, newdate33)) %>% 
    mutate(yeardaymonth = as_date(finaldate)) %>% 
    select(-c(moday, moday2, newdate22, newdate222,
              newdate33, newdate333, finaldate)) %>% 
    mutate(year = format(yeardaymonth, '%Y')) %>% 
    mutate(month = months(as.Date(yeardaymonth))) 
}

#################################################################
#################################################################

# Change "Grade" to numeric for ease of manipulation later
grade.to.numeric <- function(g) {
  g %>% 
    mutate(Grade2 = as.numeric(as.character(Grade))) %>% 
    dplyr::select(-Grade) %>% 
    dplyr::rename(Grade = Grade2) 
}

#################################################################
#################################################################

# Get minutes, seconds in decimal format (Goal is to get mins and seconds into format
#   that can be manipulated)
#     t: df
time.to.decimal <- function(t) {
  t %>%
    mutate(time = lubridate::ms(Time)) %>% 
    mutate(minute = time$minute *60) %>% 
    mutate(second = time$second) %>% 
    mutate(total.secs = minute + second) %>% 
    mutate(mins.dec = total.secs/60) %>% 
    select(-minute, -second) %>% 
    select(-c(Time, time, total.secs))  %>% 
    dplyr::rename(Time = mins.dec)
}   
#################################################################
#################################################################

# t: df
# x: new column name
# y: existing column name

time.to.decimal3 <- function(t, x, y) {
  t %>%
    mutate(x = lubridate::ms(y)) %>% 
    mutate(minute = time$minute *60) %>% 
    mutate(second = time$second) %>% 
    mutate(total.secs = minute + second) %>% 
    mutate(mins.dec = total.secs/60) %>% 
    select(-minute, -second) %>% 
    select(-c(y, time, total.secs))  %>% 
    dplyr::rename(x = mins.dec)
}   


time.to.decimal4 <- function(mydf, mycol) {
mydf %>% 
  mutate(time = lubridate::ms(mycol),
         minute = time$minute *60,
         second = time$second,
         total.secs = round((minute + second), digits = 2),
         mins.dec = total.secs/60) %>% 
  select(-c(mycol, time, total.secs, minute, second)) # %>% 
  #dplyr::rename(mynewcol = mins.dec)

}


time.to.decimal5 <- function(mydf, mycol) {
  mydf %>%
    #mutate(time = lubridate::ms(mycol)) %>% 
    mutate(time = parse_time(mydf$mycol, "%H:%M:%S")) %>% 
    mutate(minute = time$minute *60) %>% 
    mutate(second = time$second) %>% 
    mutate(total.secs = minute + second) %>% 
    mutate(mins.dec = total.secs/60) %>% 
    select(-minute, -second) %>% 
    select(-c(mycol, time, total.secs)) # %>% 
    #dplyr::rename(x = mins.dec)
}   


# Once the high school and college data have been merged, the Time
#   for each Athlete in their event gets all out of whack.
#    This function gets the hs times to be in "Time" format
#     x: df
#     y: df$column 
time.to.format <- function(x, y, z) {
  x %>% 
    mutate(blah = sub(".*:", "", y)) %>% 
    mutate(blah2 = as.numeric(as.character(blah))) %>% 
    mutate(blah3 = (blah2/60) + 2) %>% 
    mutate(blah4 = ms(blah3)) %>% 
    mutate(blah5 = as_hms(blah3 * 60)) %>%
    dplyr::rename(y = blah5) %>%
    select(-c(z, blah4, blah3, blah2, blah)) %>% 
    select(y, everything())
  
}

#################################################################
#################################################################

# Get minutes, seconds in decimal format (Goal is to get mins and seconds into format
#   that can be manipulated)
#     t: df
#     d: column with time element in it
time.to.decimal.part2 <- function(t, x, y) {
  t %>% 
    mutate(newtime = ms(x) + 120) %>% 
    mutate(newtime2 = as_hms(newtime)) %>%
    #select(-newtime) %>% 
    #rename(y = newtime2) %>% 
    #summarise(mean = median(newtime2))
    select(newtime2, newtime, everything())# %>% 
    #View()
}   
#################################################################
#################################################################

# Another time function
#  Trying to get time to decimal

timeagain <- 
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }


#################################################################
#################################################################

# Add Year into the data frame
add.year <- function(y) {
 y %>% 
  mutate(Year2 = name.of.event2[[1]][1]) %>%
  mutate(Year = as.numeric(as.character(Year2))) %>% 
  select(-Year2)
}

#################################################################
#################################################################

# Delete un-needed rows
delete.rows <- function(d){
  d %>% 
  filter(Athlete != c('Athlete',
                      "Qualifying Standard",
                      "Automatic Qualifying Standard"),
         !grepl("View Next", State),
         !grepl("Mark", Time))
}

#################################################################
#################################################################

# Get time into mins/secs format
mins.secs <- function(m){
  m %>% 
  mutate(time2 = as_hms(Time * 60)) 
}

#################################################################
#################################################################

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#################################################################
#################################################################

# Filter grade
grade.filter <- function(gg)
gg %>% 
  filter(Grade > 8)

#################################################################
#################################################################

# write to .csv
#  x: name of df
#  y: first year of data in df
#  z: last year of data in df
file.to.csv <- function(x,gender,event, y,z) {
  write.csv(paste(x,
                  ",",
                  "RunningProjectR/data-after_raw/",
                  z,
                  "-12-31_high-school-",
                  enquo(gender),
                  "_",
                  event,
                  "_combined_",
                  y,
                  "-",
                  z,
                  ".csv",
                  sep = ""
                  ))
  
}


##########################################
# Boxplotting
#   df = df
#   var = variable
#   z = group, ie D1 for Division 1, must be in quotes

boxplotting <- function(df, var, z){
df %>% 
  filter(var == z) %>%
  ggplot(aes(x = Grade, y = HStime, 
             fill = Grade,
             na.rm = TRUE)) +
  geom_boxplot(alpha = 0.3)+
  theme(legend.position = "none") +
  coord_flip()+
  scale_x_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12"))

}
########################################
########################################

# Gets count of data points for which a prediction was made
rf.summary <- function(yh, new, dataset){
  yh <- predict(new, data = dataset)
  sum(sapply(yh, function(x) sum(!is.na(x))))
}

# Function to calculate MSE
mse.func <- function(x, y) {
  mean((x - y)^2, na.rm = TRUE)
}

##########################################
# GLM

glmx.func <- function(newdf, col, var1, var2, var3, df) {
  set.seed(157)  
  newdf <- glm(col ~ var1 + var2 + var3,
               data = df,
               family = "binomial",
               na.action = na.exclude)
  #return(newdf)
  summary(newdf)
}

# Ultimately outputs a list of probabilities using logistic regression
#  func3 uses 3 variables
glmx.func3 <- function(newdf, col, var1, var2, var3, df,
                       pred.df, prdprob) {
  set.seed(157)  
  newdf <- glm(col ~ var1 + var2 + var3,
               data = df,
               family = "binomial",
               na.action = na.exclude)
  #return(newdf)
  summary(newdf)
  
  pred.df <- cbind(df, predict(newdf, data = df,
                               type = "link",
                               se = TRUE))
  
  pred.df <- within(pred.df, {
    prdprob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
    })
  list(temp = pred.df$prdprob)
}



# Ultimately outputs a list of probabilities using logistic regression
#  func2 uses 2 variables
glmx.func2 <- function(newdf, col, var1, var2, df,
                       pred.df, prdprob) {
  set.seed(157)  
  newdf <- glm(col ~ var1 + var2,
               data = df,
               family = "binomial",
               na.action = na.exclude)
  #return(newdf)
  summary(newdf)
  
  pred.df <- cbind(df, predict(newdf, data = df,
                               type = "link",
                               se = TRUE))
  
  pred.df <- within(pred.df, {
    prdprob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  list(temp = pred.df$prdprob)
}




# Ultimately outputs a list of probabilities using logistic regression
#  func2 uses 2 variables
glmx.func2.prod <- function(newdf, col, var1, var2, df,
                       pred.df, prdprob) {
  set.seed(157)  
  newdf <- glm(col ~ var1 + var2,
               df,
               family = "binomial",
               na.action = na.exclude)
  #return(newdf)
  summary(newdf)
  
  pred.df <- cbind(df, predict(newdf, data = df,
                               type = "link",
                               se = TRUE))
  
  pred.df <- within(pred.df, {
    prdprob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  list(temp = pred.df$prdprob)
}



# Ultimately outputs a list of probabilities using logistic regression
#  func4 uses 4 variables
glmx.func4 <- function(newdf, col, var1, var2, var3, var4, df,
                       pred.df, prdprob) {
  set.seed(157)  
  newdf <- glm(col ~ var1 + var2 + var3 + var4,
               data = df,
               family = "binomial",
               na.action = na.exclude)
  #return(newdf)
  summary(newdf)
  
  pred.df <- cbind(df, predict(newdf, data = df,
                               type = "link",
                               se = TRUE))
  
  pred.df <- within(pred.df, {
    prdprob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  list(temp = pred.df$prdprob)
}


#  Get into df
getdf.func <- function(dfa, dfb, dfc){
  dfb <- data.frame(matrix(unlist(dfa), 
                             ncol = max(lengths(dfa)), byrow = TRUE))
  
  dfc <- t(dfb)
}


###################################################
###################################################
# Production GLM functions

glm.func.10 <- function(newdf, col, var1, var2, df,
                       prdprob) {
  
  set.seed(157)
  newdf <- glm(col ~ var1 + var2,
               df,
               family = "binomial",
               na.action = na.exclude)
  
  newdata3 <- cbind(df, predict(newdf, 
                                   df, 
                                   type = "link", 
                                   se = TRUE))
  
  newdata3 <- within(newdata3, {
    prdprob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
}


##############################################
glm.func.11 <- function(newdf, col, var1, var2, var3, df,
                        prdprob) {
  
  set.seed(157)
  newdf <- glm(col ~ var1 + var2 + var3,
               df,
               family = "binomial",
               na.action = na.exclude)
  
  newdata3 <- cbind(df, predict(newdf, 
                                df, 
                                type = "link", 
                                se = TRUE))
  
  newdata3 <- within(newdata3, {
    prdprob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
}

##############################################
glm.func.12 <- function(newdf, col, var1, var2, var3, var4, df,
                        prdprob) {
  
  set.seed(157)
  newdf <- glm(col ~ var1 + var2 + var3 + var4,
               df,
               family = "binomial",
               na.action = na.exclude)
  
  newdata3 <- cbind(df, predict(newdf, 
                                df, 
                                type = "link", 
                                se = TRUE))
  
  newdata3 <- within(newdata3, {
    prdprob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
}





#ggplot(newdata3.11b, aes(x = Gr10b, y = PredictedProb11b)) + 
#  geom_ribbon(aes(ymin = LL,
#                  ymax = UL), #) +
#              alpha = 0.2) + 
#  geom_line() 

#ggplot(newdata3.11b, aes(x = Gr9b, y = PredictedProb11b)) + 
#  geom_ribbon(aes(ymin = LL,
#                  ymax = UL), #) +
#              alpha = 0.2) + 
  #geom_line() 
#  geom_point()

# Find the p-value
#with(glm.pred.11bb.b, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#logLik(glm.pred.11bb.b)
#summary(glm.pred.11bb.b) #AIC = 10528


#########################################

#  /Not Quite Working Yet

###########################################
