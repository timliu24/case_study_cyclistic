library(tidyverse)
library(lubridate)
library(ggplot2)
#Upload data sets
cyc22_01_df <- read.csv("202201-divvy-tripdata.csv")
cyc22_02_df <- read.csv("202202-divvy-tripdata.csv")
cyc22_03_df <- read.csv("202203-divvy-tripdata.csv")
cyc22_04_df <- read.csv("202204-divvy-tripdata.csv")
cyc22_05_df <- read.csv("202205-divvy-tripdata.csv")
cyc22_06_df <- read.csv("202206-divvy-tripdata.csv")
cyc22_07_df <- read.csv("202207-divvy-tripdata.csv")
cyc22_08_df <- read.csv("202208-divvy-tripdata.csv")
cyc22_09_df <- read.csv("202209-divvy-tripdata.csv")
cyc22_10_df <- read.csv("202210-divvy-tripdata.csv")
cyc22_11_df <- read.csv("202211-divvy-tripdata.csv")
cyc22_12_df <- read.csv("202212-divvy-tripdata.csv")
#Combine individual month's data frames into an annual data frame
all_2022_trips <- bind_rows(cyc22_01_df, cyc22_02_df, cyc22_03_df,
                            cyc22_04_df, cyc22_05_df, cyc22_06_df,
                            cyc22_07_df, cyc22_08_df, cyc22_09_df,
                            cyc22_10_df, cyc22_11_df, cyc22_12_df)
#Remove irrelevant data fields from the data frame
all_2022_trips <- all_2022_trips %>% select(-c(start_lat, start_lng,
                                               end_lat, end_lng))
#Add date, month, day and year fields. 
all_2022_trips$date <- as.Date(all_2022_trips$started_at)
all_2022_trips$month <- format(as.Date(all_2022_trips$date), "%m")
all_2022_trips$day <- format(as.Date(all_2022_trips$date), "%d")
all_2022_trips$year <- format(as.Date(all_2022_trips$date), "%Y")
all_2022_trips$day_of_week <- format(as.Date(all_2022_trips$date), "%A")
#Calculate the length of each ride
all_2022_trips$ride_length <- difftime(all_2022_trips$ended_at, 
                                       all_2022_trips$started_at)
#Convert factor to numeric
is.factor(all_2022_trips$ride_length)
all_2022_trips$ride_length <- as.numeric(as.character(all_2022_trips$ride_length))
is.numeric(all_2022_trips$ride_length)
#Clean up bad data
all_2022_trips_v2 <- all_2022_trips[!(all_2022_trips$ride_length<0),]
#Calculate mean, median, max and min
summary(all_2022_trips_v2$ride_length)
#Compare members and casual riders
aggregate(all_2022_trips_v2$ride_length ~ all_2022_trips_v2$member_casual,
          FUN = mean)
aggregate(all_2022_trips_v2$ride_length ~ all_2022_trips_v2$member_casual,
          FUN = median)
aggregate(all_2022_trips_v2$ride_length ~ all_2022_trips_v2$member_casual,
          FUN = max)
aggregate(all_2022_trips_v2$ride_length ~ all_2022_trips_v2$member_casual,
          FUN = min)
#Arrange the order of the week of the day and average ride time 
#by each day for members vs casual riders
all_2022_trips_v2$day_of_week <- ordered(all_2022_trips_v2$day_of_week,
                                         levels=c("Sunday", "Monday", "Tuesday",
                                                  "Wednesday", "Thursday",
                                                  "Friday", "Saturday"))
aggregate(all_2022_trips_v2$ride_length ~ all_2022_trips_v2$member_casual
          + all_2022_trips_v2$day_of_week, FUN = mean)
#Analyze ridership data by type and weekday
all_2022_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)
#Graphic of number of rides by rider type
all_2022_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
#Graphic of average ride duration by rider type
all_2022_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#Export to csv on Windows 11
df <- aggregate(all_2022_trips_v2$ride_length ~ all_2022_trips_v2$member_casual +
                  all_2022_trips_v2$day_of_week, FUN = mean)
write.csv(df, "C:\\Users\\*username*\\OneDrive\\Documents\\case_study_cyclistic\\avg_ride_length.csv", row.names=FALSE)