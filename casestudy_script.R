#===============================================
# STEP 1: COLLECT DATA AND INSTALL/LOAD PACKAGES
#===============================================

install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

getwd()
setwd("C:/Users/Tad/Desktop/Original_Dataset")

df1 <- read_csv("202202-divvy-tripdata.csv")
df2 <- read_csv("202203-divvy-tripdata.csv")
df3 <- read_csv("202204-divvy-tripdata.csv")
df4 <- read_csv("202205-divvy-tripdata.csv")
df5 <- read_csv("202206-divvy-tripdata.csv")
df6 <- read_csv("202207-divvy-tripdata.csv")
df7 <- read_csv("202208-divvy-tripdata.csv")
df8 <- read_csv("202209-divvy-tripdata.csv")
df9 <- read_csv("202210-divvy-tripdata.csv")
df10 <- read_csv("202211-divvy-tripdata.csv")
df11 <- read_csv("202212-divvy-tripdata.csv")
df12 <- read_csv("202301-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df5)
colnames(df6)
colnames(df7)
colnames(df8)
colnames(df9)
colnames(df10)
colnames(df11)
colnames(df12)

all_trips <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

colnames(all_trips)
nrow(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

all_tripsv2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_tripsv2 <- all_tripsv2[complete.cases(all_tripsv2[,c("ride_length")]),]
#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

mean(all_tripsv2$ride_length) #straight average (total ride length / rides)
median(all_tripsv2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_tripsv2$ride_length) #longest ride
min(all_tripsv2$ride_length) #shortest ride

summary(all_tripsv2$ride_length)

aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = mean)
aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = median)
aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = max)
aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = min)

aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual + all_tripsv2$day_of_week, FUN = mean)

all_tripsv2$day_of_week <- ordered(all_tripsv2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual + all_tripsv2$day_of_week, FUN = mean)

all_tripsv2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) # sorts

# Let's visualize the number of rides by rider type
all_tripsv2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_tripsv2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================

write.csv(all_tripsv2, "C:/Users/Tad/Desktop/Original_Dataset/casestudy.csv", row.names=FALSE)
