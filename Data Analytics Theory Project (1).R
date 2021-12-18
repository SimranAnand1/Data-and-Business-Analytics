# Data Science part

library(sqldf)

library("tidyverse")
library("janitor")
library("dplyr")
library("skimr")
library("here")
library("lubridate")
library("tidyr")
library("ggplot2")
library("readr")
library("magrittr")

activity <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/dailyActivity_merged.csv")
calories <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/dailyCalories_merged.csv")
hourly_calories <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/hourlyCalories_merged.csv")
intensities <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/dailyIntensities_merged.csv")
heartrate<- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/heartrate_seconds_merged.csv")
sleep <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/sleepDay_merged.csv")
weight <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/weightLogInfo_merged.csv")
daily_steps <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/dailySteps_merged.csv")
hourly_steps <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/hourlySteps_merged.csv")
hourly_intensities <- read.csv("C:/Users/simmu/Dropbox/PC/Downloads/hourlyIntensities_merged.csv")

head(activity,3)
glimpse(activity)
colnames(activity)
head(calories,3)
colnames(calories)



# intensities
intensities$ActivityDay=as.POSIXct(intensities$ActivityDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
#calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
##hourly_calories
hourly_calories$ActivityHour=as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p",tz = Sys.timezone())
##hourly_steps
hourly_steps$ActivityHour=as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p",tz = Sys.timezone())
##hourly_intensities
hourly_intensities$ActivityHour=as.POSIXct(hourly_intensities$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p",tz = Sys.timezone())


class(hourly_calories$ActivityHour)
class(hourly_intensities$ActivityHour)
class(hourly_steps$ActivityHour)
class(sleep$SleepDay)
class(activity$ActivityDate)
class(intensities$ActivityDay)

n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

#Removing data frames
nrow(activity)
nrow(activity[duplicated(activity),])

activity <- activity %>% filter(TotalSteps !=0)
activity <- activity %>%  filter(TotalDistance !=0)

sleep_day_new <- sleep %>% 
  separate(SleepDay, c("Date", "Time"), " ")
nrow(sleep_day_new)
nrow(sleep_day_new[duplicated(sleep_day_new),])
sleep_day_new <- unique(sleep_day_new)
nrow(sleep_day_new)

daily_activity2 <- activity %>%
   select (Id, ActivityDate, Calories)
head(daily_activity2)



#sql_check <- sqldf('SELECT * FROM daily_activity2 INTERSECT SELECT * FROM calories')
ncol(daily_activity2)
ncol(calories)
#head(sql_check,3)



n_unique(hourly_calories)
n_unique(hourly_steps)
n_unique(hourly_intensities)
n_unique(sleep)
n_unique(activity)

sum(duplicated(calories))
sum(duplicated(intensities))
sum(duplicated(sleep))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))
sum(duplicated(hourly_calories))

sum(is.na(hourly_calories))
sum(is.na(daily_steps))
sum(is.na(intensities))
sum(is.na(sleep))
sum(is.na(activity))
sum(is.na(hourly_steps))

#merging data

merged_data <- merge(sleep, activity, by=c('Id', 'date'))
head(merged_data)
hourlies_df <- hourly_steps %>% 
  left_join(hourly_calories, by = c("Id", "ActivityHour")) %>% 
  left_join(hourly_intensities, by = c("Id", "ActivityHour")) %>% 
  separate(ActivityHour, sep = " ", into = c("date","time")) %>% 
  mutate(day = format(ymd(date), format = '%a')) %>% 
  mutate(time = format(parse_date_time(as.character(time), "HMS"), format = "%H:%M")) %>% 
  mutate(date = as.POSIXct(date))
head(hourlies_df,3)
