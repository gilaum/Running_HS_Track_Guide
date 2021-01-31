# Tom Alig
# Dec 28, 2019

# Discarded Code from Running Project

hs.data.for.coll.ath %>% 
  group_by(College.Division) %>% 
  filter(!is.na(College.Division)) %>% 
  summarise(Gr9 = quantile(Gr9, na.rm = TRUE))

all.hs.and.coll.data.long %>% 
  tbl_df() %>%
  nest(-Grade) %>%
  mutate(Quantiles = map(data, ~ quantile(.$HStime, na.rm = TRUE)),
         Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
  unnest(Quantiles)

all.hs.and.coll.data.long %>% 
  filter(is.na(College.Division) & 
           State == "MI") %>% 
  group_by(Grade) %>% 
  summarise(median = median(HStime, na.rm = TRUE),
            fastest = min(HStime, na.rm = TRUE))

all.hs.and.coll.data.long %>% 
  filter(College.Division == "D1") %>%
  ggplot(aes(x = Grade, y = HStime, 
             fill = Grade,
             na.rm = TRUE)) +
  geom_boxplot(alpha = 0.3)+
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12"))

all.hs.and.coll.data.long %>% 
  #mutate(group99 = Team.x == "Saline"
  #                     ) %>% 
  ggplot(aes(x = Grade, y = HStime, 
             fill = Grade,
             na.rm = TRUE)) +
  geom_point(sn)
geom_boxplot(alpha = 0.3)+
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) + 
  facet_wrap(~College.Division, nrow = 5)

for (i in vvalues) {
  ggplot(data= subset(hs.800.one.row, Time == i)) + 
    geom_point(size=3, aes(x=year, y=Time )) +
    ggtitle(i)
  
}

ggplot(data= subset(hs.800.one.row, Time < 2.75)) + 
  geom_point(size=3, aes(x=year, y=Time )) +
  ggtitle("2.75")

ggplot(data= subset(iris, Species == i)) + 
  geom_point(size=3, aes(x=Petal.Length, y=Petal.Width )) +
  ggtitle(i)

for (i in vvalues) {
  hs.800.one.row %>%
    filter(Time < i) %>% 
    ggplot(aes(x = year)) +
    geom_bar(fill = "#F0E442", color = "#0072B2") +
    scale_x_continuous(breaks = seq(2011, 2019, 1)) +
    scale_y_continuous(labels = comma) +
    ggtitle(i)
}
#  ggsave(temp.plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")


improve %>% 
  gather(improve.9.to.10, improve.9.to.11, improve.9.to.12,
         improve.10.to.11, improve.10.to.12, improve.11.to.12,
         key = "improve.grade", value = "improve.time") %>% 
  #View()
  ggplot(aes(x = improve.grade, y = improve.time)) +
  geom_point(position = 'jitter') 

improve %>% 
  #filter(Gr9 != "NA" & Gr10 != "NA") %>% 
  select(improve.9.to.10:improve.11.to.12) %>% 
  #summary()
  #summarise(range9.10 = IQR(improve.9.to.10,na.rm = TRUE))
  ggplot(aes(x = improve.9.to.10, y = improve.9.to.10)) +
  geom_point()    


all.hs.and.coll.data.long %>% 
  mutate(HStime2 = as.numeric(all.hs.and.coll.data.long$HStime/60)) %>% 
  filter(HStime != "NA") %>% 
  group_by(Grade, gr = cut(HStime2, breaks= breaks)) %>% 
  tally() %>% 
  mutate(pct_by_grade = n/sum(n)) %>% 
  ggplot(aes(x = gr, y = Grade, fill = pct_by_grade)) +
  geom_tile() +
  scale_x_discrete(labels = labels,
                   name = "Time In Minutes") +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  scale_y_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  geom_text(aes(label=percent(pct_by_grade))) +
  ggtitle("All Girls HS 800m Time Distributions, 2011 - 2019") +
  guides(fill = FALSE) +
  theme_minimal()

median.mean.graph <- hs.800.one.row %>% 
  group_by(year) %>% 
  summarise(median.time = (median(Time)),
            mean.time = mean(Time)) %>% 
  mutate(med2 = round(median.time * 60, 2),
         mean2 = round(mean.time * 60, 2)) %>% 
  gather(med2, mean2, key = "type", value = "timess") %>% 
  mutate(timess2 = as.hms(timess)) %>%
  mutate(timess3 = round(as.hms(timess, 2))) %>% 
  #ungroup() %>% 
  ggplot(aes(x = year, y = timess, color = type)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c(cb.blue, cb.orange),
                     labels = c("Median", "Mean")) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_time(lim = c(150, 165),
               labels = time_format("%M:%S")) +
  geom_text(aes(label = timess2),
            #options(digits.secs = 2)),
            size = 3.25,
            # face = "bold",
            vjust = 1.5) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(color = "") +
  xlab("Year") +
  ylab("Time") +
  ggtitle("Mean, Median Girls HS 800m under 2:45, 2011 - 2019")

HeatMap800f <- all.hs.and.coll.data.long %>% 
  mutate(HStime2 = as.numeric(all.hs.and.coll.data.long$HStime/60)) %>% 
  filter(HStime != "NA") %>% 
  group_by(Grade, gr = cut(HStime2, breaks= seq(2.08333333, 2.75, by = 0.1666667))) %>% 
  tally() %>% 
  mutate(pct_by_grade = n/sum(n)) %>% 
  ggplot(aes(x = gr, y = Grade, fill = pct_by_grade)) +
  geom_tile() +
  scale_x_discrete(labels = c(
    #"2:00 - 2:10", 
    "2:05.01 - 2:15", 
    #"2:10.01 - 2:20", 
    "2:15.01 - 2:25",
    #"2:20.01 - 2:30",
    "2:25.01 - 2:35", 
    #"2:30.01 - 2:40",
    "2:35.01 - 2:45"),
    #"2:40.01 - 2:45"),
    name = "Time In Minutes") +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  #scale_fill_distiller(low = "#56B4E9", 
  #                     high = "#0072B2", 
  #                     direction = +1) +
  scale_y_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  geom_text(aes(label=percent(pct_by_grade))) +
  ggtitle("All Girls HS 800m Time Distributions, 2011 - 2019") +
  guides(fill = FALSE) +
  theme_minimal()


chart1.1_df %>% 
  gather(key = "Type", value = "HSmedian", AthleteTime:median.D3) %>% 
  select(Grade, Type, HSmedian) %>% 
  ggplot(aes(x = Grade, y = HSmedian, na.rm = TRUE,
             color = Type,
             size = Type == "AthleteTime",
             #label = Type,
             group = Type)) +
  geom_point(size = 2) + #size = 2.5) + #, position = 'jitter') +
  geom_line() +
  #geom_text(aes(label=y)) +
  #geom_text(label=Type, 
  #y=group, 
  # x=rep(1, NROW(df)), 
  #       hjust=1.1, 
  #      size=3.5) +
  guides(size = FALSE) +
  scale_size_manual(values = c(1, 3)) +
  scale_x_discrete(limits = c('Gr09', 'Gr10',
                              'Gr11', 'Gr12')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time In Minutes") +
  ggtitle(paste(sn,
                "and Median High School",
                event,
                "Times"))

# Median High School Times for College Runners
#  graph
med.hs.times.for.coll.athletes <- hs.data.for.coll.ath %>% 
  group_by(College.Division) %>% 
  filter(!is.na(College.Division)) %>% 
  # View()
  summarise(fast_Gr9 = min(Gr9, na.rm = TRUE),
            fast_Gr10 = min(Gr10, na.rm = TRUE),
            fast_Gr11 = min(Gr11, na.rm = TRUE),
            fast_Gr12 = min(Gr12, na.rm = TRUE),
            Gr9 = as.hms(median(Gr9, na.rm = TRUE)),
            Gr10 = as.hms(median(Gr10, na.rm = TRUE)),
            Gr11 = as.hms(median(Gr11, na.rm = TRUE)),
            Gr12 = as.hms(median(Gr12, na.rm = TRUE))) %>% 
  gather(key = "grade", value = "HSmedian", Gr9:Gr12) %>% 
  select(College.Division, grade, HSmedian) %>% 
  #View()
  ggplot(aes(x = grade, y = HSmedian, na.rm = TRUE,
             color = College.Division,
             group = College.Division)) +
  geom_point(size = 3) + #size = 2.5) + #, position = 'jitter') +
  geom_line(size = 2) +
  scale_x_discrete(limits = c('Gr9', 'Gr10',
                              'Gr11', 'Gr12')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time") +
  xlab("High School Grade") +
  scale_color_manual(values=c(cb.blue, cb.orange, cb.purple)) +
  theme_bw() +
  theme(axis.text=element_text(size=11.5)) +
  ggtitle(paste("Median High School",
                event,
                "Times for College Runners"))

all.hs.and.coll.data.long %>% 
  filter((College.All.Conf == 1) |
           College.AA == 1 |
           Athlete == sn) %>%
  mutate(Col.Div = ifelse(Athlete == sn, sn, College.Division)) %>% 
  ggplot(aes(x = Grade, y = HStime, na.rm = TRUE, 
             color = Col.Div,
             size = Athlete == sn,  
             shape = Col.Div
  ))+
  geom_point(position = 'jitter') + #+ #size = 2.5) + #
  scale_x_discrete(limits = c('Gr9', 'Gr10', 'Gr11', 'Gr12')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time In Minutes") +
  scale_color_manual(values = c(cb.blue, cb.orange, cb.purple, "black")) +
  scale_size_manual(values = c(2, 5)) +
  scale_shape_manual(values = shapes4) +
  guides(size = FALSE) +
  theme_bw() +
  theme(axis.text=element_text(size=11.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste(
    "College All-Conf & All-American ",
    event2,
    " Performances",
    sep = ""))

# Graph   
set.seed(1106)
all.hs.and.coll.data.long %>% 
  filter(College.Division == cdiv |
           Athlete == sn) %>%
  replace_na(list(College.All.Conf = 0)) %>% 
  #View()
  ggplot(aes(x = Grade, y = HStime, na.rm = TRUE, 
             color = Athlete == sn,
             size = Athlete == sn,  
             shape = as.factor(College.All.Conf)))+
  geom_point(position = 'jitter') + #+ #size = 2.5) + #
  scale_x_discrete(limits = c('Gr9', 'Gr10', 'Gr11', 'Gr12')) +
  scale_y_time(labels = date_format("%M:%S"),
               name = "Time In Minutes") +
  scale_color_manual(values = c(cb.orange, cb.blue),
                     labels = c("No", "Yes")) +
  scale_size_manual(values = c(2.5, 4.5)) +
  scale_shape_manual(values = shapes,
                     labels = c("No", "Yes")) +
  labs(shape = "College All-Conf",
       color = "My Athlete") +
  guides(size = FALSE) +
  ggtitle(paste(sn,
                " & ",
                cdiv,
                " high school times in 800m",
                sep = ""))


heatmap2 <- all.hs.and.coll.data.long %>% 
  mutate(HStime2 = as.numeric(all.hs.and.coll.data.long$HStime/60)) %>% 
  filter(HStime != "NA") %>% 
  group_by(Grade, gr = cut(HStime2, breaks= breaks)) %>% 
  # View()
  tally() %>% 
  mutate(pct_by_grade = n/sum(n)) %>% 
  ggplot(aes(x = gr, y = Grade, fill = pct_by_grade)) +
  geom_tile() +
  scale_x_discrete(labels = labels,
                   name = "Time") +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  scale_y_discrete(limits = c("Gr9", "Gr10", "Gr11", "Gr12")) +
  theme_minimal() +
  geom_text(aes(label=percent(pct_by_grade))) +
  theme(axis.text.x = element_text(angle = 30, 
                                   hjust = 1,
                                   vjust = 1)) +
  ggtitle(paste(event2,
                "Time Distributions",
                years)) +
  guides(fill = FALSE) 





#annotate(geom="text", 
#         x = 0, 
#         y = as.numeric(unlist(ath.best.last.year * 60)), 
#         label = sn, 
#         colour=cb.purple) +

# geom_text(aes(label = timess2),
#            size = 3.25,
#           vjust = 1.5) +


###########################################
############################################
# Scripts
# ||||||
# VVVVVV

##  Not Quite Working Yet

# Function to scrape from web
scrape.from.web <- function(page) {
  url <- paste0(page)
  url2 <- read_html(url)
  url3 <- url2%>%html_nodes("table")%>%.[1]%>%html_table(fill = TRUE)
  url4 <- data.frame(url3)
}


blkajfklejfkl <- function(x) {
  mutate(moday = format(Date)) %>% 
    unite(moday, Date, Year) %>% 
    mutate(newdate22 = gsub("_", "\\-", moday)) %>% 
    mutate(newdate33 = dmy(newdate22)) %>% 
    mutate(monthday = format(newdate33, '%m/%d')) %>% 
    mutate(year = format(newdate33, '%Y')) %>% 
    mutate(month = months(as.Date(newdate33))) %>% 
    select(-newdate22, -moday) %>% 
    rename(yeardaymonth = newdate33) 
}



Gr10.prob <- glm12d %>% 
  select(c(gr10prob210, gr10prob215, gr10prob220, gr10prob225,
           gr10prob230, gr10prob235, gr10prob240))

glm12d <- glm12d %>% 
  mutate(aim = rep(50, nrow(.)))



df_sub %>% 
  View()

Gr10.prob2 <- c("gr10prob210", "gr10prob215", "gr10prob220", "gr10prob225",
                "gr10prob230", "gr10prob235", "gr10prob240")

names(glm12d)


colour_func_gauge <- function(x){ifelse(x$gr10prob210 > 45, '#800000', 
                                        ifelse(x$gr10prob210 < 30, '#000080', '#008000'))
}


gauge(df_sub$gr10prob220, min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
))

ggplot(glm12d, aes(x = gr10prob220)) + 
  geom_bar(width = 0.85, stat="identity", aes(y = Athlete == "Lydia Alig"), 
           colour = "white", fill = "white") +
  geom_bar(data=df_sub, aes(y = aim), width = 0.85, stat = "identity", colour = "grey68") +
  geom_bar(data=df_sub, width = 0.85, stat = "identity", 
           aes(y = Athlete == "Lydia Alig", fill = gr10prob220), 
           colour = "black") +
  geom_text(data=df_sub, aes(x = gr10prob220, y = 50), label = paste0(round(df_sub$gr10prob220,0),'%'),
            colour = "azure4", vjust = -1) +
  # scale_fill_manual(values = as.factor(colour_func_gauge(df_sub)))# +
  labs(title ='Indication of Distance Compared to Highest',
       subtitle = 'Blue under 60%, Green 60-90%, Red over 90%') +
  xlab("") + ylab("") +
  geom_text(data=df_sub, hjust = 1.02, size = 4, 
            aes(x = gr10prob220, y = 0, label = gr10prob220), angle = 70) +
  theme_classic()+
  coord_polar(theta = "y",start=-pi/2) +
  xlim(0,1) +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "top",
        legend.text.align = 0,
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size=18,
                                            color="#222222"))



df <- tibble(player = c("Player1", "Player2", "Player3", "Player4"),
             value =  sample(2500:5000, 4),
             max = sample(5000:5500, 4))

# scale data: 
df <- df %>% 
  add_row(player = "A", `value` = 0, max=0,.before = 1) %>%
  mutate(value_scale = (value / max)*100,
         value_sc_half = value_scale/2,
         aim = rep(50, nrow(.)))

df %>% 
  View()

