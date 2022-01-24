
#downloading packages to use
install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("stringr")
install.packages("janitor")
install.packages("skimr")
install.packages("RColorBrewer")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(janitor)
library(skimr)
library(RColorBrewer)

#uploading all daily csv files
daily_activity <- read_csv("Desktop/dailyActivity_merged.csv")
daily_calories <- read_csv("Desktop/dailyCalories_merged.csv")
daily_intensities <- read_csv("Desktop/dailyIntensities_merged.csv")
daily_steps <- read_csv("Desktop/dailySteps_merged.csv")
sleep_day <- read_csv("Desktop/sleepDay_merged.csv")
weight_log <- read_csv("Desktop/weightLogInfo_merged.csv")

#view headers for all files
head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(daily_steps)
head(sleep_day)
head(weight_log)

##Data Cleaning
skim(sleep_day)
View(sleep_day)
sleep_day_1 <-
  sleep_day %>% 
  separate(SleepDay, c("SleepDate", "SleepTime","AM/PM"), sep="([ ])") %>%
  unite(SleepTimes, 3:4, sep=" ", remove=FALSE)%>%
  mutate(SleepDate= as.Date(SleepDate, "%m/%d/%Y"))%>%
  mutate(SleepTime= parse_time(SleepTimes, "%H:%M:%S %p"))%>%
  distinct()
new_sleep_day<- subset(sleep_day_1, select = -c(3,5))
View(new_sleep_day)
head(new_sleep_day)

skim(weight_log)
View(weight_log)
weight_log_1<-
  weight_log %>% separate(Date, c("LogDate", "LogTime","AM/PM"), sep="([ ])")%>%
  unite(LogTimes, 3:4, sep=" ", remove=FALSE)%>%
  mutate(LogDate= as.Date(LogDate, "%m/%d/%Y"))%>%
  mutate(LogTime= parse_time(LogTimes, "%H:%M:%S %p"))%>%
  distinct()
weight_log_1= weight_log_1[,!sapply(weight_log_1, function(x) mean(is.na(x)))>0.5]
new_weight_log<- subset(weight_log_1, select = -c(3,5))
View(new_weight_log)
head(new_weight_log)

colnames(daily_activity)[2] <-"ActivityDay"
colnames(daily_steps)[3] <- "TotalSteps"
#After checking their headers, I can see that dates for sleep_day and weight_log are in character format not date format so I fix this. I also check for duplicates and missing information. I found 3 duplicate rows in the sleep_day dataset and deleted them. I also found that 65 of 67 rows are empty in the Fat column under the weight_log dataset so decided to get rid of the column completely by using a formula that gets rid of columns where more than 50% of entries are empty/NA. I also clean up the names in daily_activity and daily_steps to make columns match up when merging sets of data together.

##Data Prep
#let's see how many unique participants are there for each activity
n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_steps$Id)
n_distinct(new_sleep_day$Id)
n_distinct(new_weight_log$Id)
#I found that most people use their Fitbit device to track daily activities(33), intensities(33), calories(33), steps(33), and sleep(24). We can see that our activities data is just a merge of our intensities, calories, and steps data. We should merge these together to make sure no data was lost in on dataset into the other. With this in mind we only need to focus on analyzing the daily activities and sleep categories.

#merging everything together
m0 <- merge(daily_activity,daily_intensities,
            by=c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes",
                 "FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", 
                 "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance"))
daily_data <- merge(m0, daily_steps,
                    by= c("Id","ActivityDay", "TotalSteps"))
View(daily_data)

#I check our new dataframe for duplicate data and find that there's no duplicate data since we still have 940 rows. I can also see that ActivityDay is not in date format so I fix that.

daily_data%>% distinct() %>% View
daily_data<-
  daily_data %>% mutate(ActivityDate= as.Date(ActivityDay, "%m/%d/%Y"))
new_daily_data<- subset(daily_data, select = -c(2))
View(new_daily_data)
str(new_daily_data)
new_daily_data<-rename(new_daily_data, ActivityDay = "ActivityDate")

new_daily_data %>%
  summarise(Activity_Participants= n_distinct(new_daily_data$Id))

##Analysis
#Now our data is all clean and formatted the way we want it. Lets analyze our data.
#plot number of activities per day
new_daily_data%>%
  count(ActivityDay, sort=TRUE) %>%
  ggplot()+geom_col(mapping=aes(x=ActivityDay,y=n),fill="yellowgreen",color="darkgreen")+labs(title= "Number of Activities per Day")

#The graph shows us that at least 12 people have missed an activity throughout the 31 days of this study. It also shows us that users are less dedicated as time goes on. Lets see how many people have gone without their fitbit assuming that 1440 sedentary minutes is equal to 24 sedentary hours.
fitbit_off <- new_daily_data %>% 
  filter(SedentaryMinutes == 1440) %>% 
  group_by(Id) %>% 
  summarise(count = n()) %>% 
  print()
#The output shows us that 17 users went at LEAST one day without using their fitbit. We want to see at which intensities our users are active while accounting for the number of users who had their fitbit off but still counted as Sedentary Active and skewing our data.
user_activity <- new_daily_data %>% 
  filter(SedentaryMinutes != 1440) %>% 
  group_by(Id) %>% 
  summarize(total_very_active_mins = sum(VeryActiveMinutes),
            total_fairly_active_mins = sum(FairlyActiveMinutes),
            total_lightly_active_mins = sum(LightlyActiveMinutes),
            total_sendentary_mins = sum(SedentaryMinutes),
            total_mins = sum(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes),
            percent_very_active = (total_very_active_mins/total_mins)*100,
            percent_fairly_active = (total_fairly_active_mins/total_mins)*100,
            percent_lightly_active = (total_lightly_active_mins/total_mins)*100,
            percent_sendentary_active = (total_sendentary_mins/total_mins)*100) # Calculate percentages of minutes by intensity
user_activity <- user_activity %>% 
  mutate(intensity =
           case_when(percent_very_active > mean(percent_very_active) ~ "Very Active",
                     percent_fairly_active > mean(percent_fairly_active) ~ "Fairly Active",
                     percent_lightly_active > mean(percent_lightly_active) ~ "Lightly Active",
                     percent_sendentary_active > mean(percent_sendentary_active) ~ "Sendentary Active"))
ds_activity <- user_activity %>% 
  group_by(intensity) %>% 
  summarise(count = n())
ggplot(ds_activity, aes(x = intensity, y = count, fill = intensity)) +
  geom_histogram(stat = "identity", color= "black") +
  ylab("Number of Users") +
  xlab("Type of Users by Intensity") +
  labs(title = "Amount of Users by Intensity") +
  theme(legend.position = "none")
#The graph shows us that 'Very Active' accounts for the highest number of users. Let's see how active our users are by checking total average steps per day.
new_daily_steps <- new_daily_data %>% 
  group_by(Id) %>% 
  summarise(avg_daily_steps = mean(TotalSteps))
ggplot(new_daily_steps, aes(x = avg_daily_steps)) +
  geom_histogram(bins = 8, fill = "dodgerblue", color = "blue4") +
  ylab("Number of Users") +
  xlab("Average Steps Taken Daily") +
  labs(title = "Amount of Users Average Steps Daily") 
#avg. steps per day
new_daily_data%>%
  group_by(ActivityDay) %>%
  summarise_at(vars(TotalSteps), list(AvgSteps = mean))%>%
  ggplot()+geom_col(mapping = aes(x=ActivityDay, y=AvgSteps),fill="hotpink2", color="violetred4")+ labs(title="Average Steps per Day")
#Users tend to take 5,000 to 10,000 steps daily on average. As we see in the second graph, this stays pretty consistent over time as well. This is not a high number since the CDC recommends that a person takes 10,000 steps per day. This makes sense since most users probably purchased a Fitbit with the desire to become more active. Let's check with the information that we have, how fit the users are.

body_weight <- new_weight_log %>% 
  group_by(Id) %>% 
  summarise(avg_bmi = mean(BMI))
ggplot(body_weight, aes(x =avg_bmi)) +
  geom_histogram(bins = 8, fill = "lavenderblush", color = "lightpink1") +
  ylab("Number of Users") +
  xlab("Average BMI per User") +
  labs(title = "Average User BMI Count") 
#Though only 8 people logged their weights, 5 of those 8 fall within a BMI higher than 25 which, according to the CDC, is considered overweight. One person has a BMI over 30 which is considered obese. 

#most active days of the week
new_daily_data$DayOfWeek<-weekdays(new_daily_data$ActivityDay)            
new_daily_data%>%
  count(DayOfWeek,sort=TRUE)%>%
  ggplot()+geom_col(mapping= aes(x=DayOfWeek, y=n),fill="springgreen3" ,color="darkgreen")+labs(title="Number of Activities by Day of Week")
#Tuesday, Wednesday and Thursday are the days of the week with the most activity. The graph shows that people are less active on the weekends.

#whats the relationship between steps and calories
ggplot(data=new_daily_data, aes(x=TotalSteps, y=Calories))+geom_point()+
  geom_smooth() + labs(title="Total Steps vs Calories")
#we see a positive correlation between total steps and calories. The more active they are, the more calories they are burning.

#how many users logged their sleep per day
new_sleep_day%>%
  count(SleepDate, sort=TRUE) %>%
  ggplot()+geom_col(aes(x=SleepDate,y=n),fill="royalblue", color="midnightblue")+labs(title= "Number of Sleep Records per day")
#We can see that their are sudden drops in sleep records within the week. These days fall on Mondays. Not only do Mondays account for the lowest activity but also lowest sleep records.

skim(new_sleep_day)
#The average TotalMinutesAsleep equates to about 7 hours a night.

#average sleep
new_sleep_day%>%
  group_by(SleepDate) %>%
  summarise_at(vars(TotalMinutesAsleep), list(AvgSleep = mean))%>%
  ggplot()+geom_col(mapping = aes(x=SleepDate, y=AvgSleep/60),fill="goldenrod1", color="darkorange")+ labs(title="Average Sleep per Night")
#CDC recommends at least 7 hours of sleep per night for those over the age of 18. The younger you are, the more sleep is recommended.

##relationship between minutes asleep and time in bed
ggplot(data=new_sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+ geom_point()+
  geom_smooth()+labs(title="Minutes Asleep vs. Time in Bed in Minutes")
#As expected, there is an almost completely linear trend between minutes asleep and time in bed. To help users improve their sleep schedules, the company should consider using notification to go to sleep.

#average time in bed but awake
new_sleep_day%>%
  mutate(InBedNoSleep=TotalTimeInBed-TotalMinutesAsleep)%>%
  group_by(SleepDate) %>%
  summarise_at(vars(InBedNoSleep), list(IbnsMean = mean))%>%
  ggplot()+geom_col(mapping = aes(x=SleepDate, y=IbnsMean),fill="lightblue1", color="steelblue4")+ labs(title="Average minutes in bed awake per night")
#on average, it took the participants 40 minutes to fall asleep after laying in bed. Going off the previous idea, the company should consider setting the notification to go to sleep to 40 minutes before the user plans on actually sleeping.

#Lets see if being active helps or hurts when trying to get a good nights sleep.
##intensities data
new_daily_data$ActiveIntensity <- (daily_intensities$VeryActiveMinutes/60)
ggplot(data=new_daily_data, aes(x=ActivityDay, y=ActiveIntensity))+ geom_histogram(stat= "identity", fill="plum3")+ theme(axis.text.x = element_text(angle=90))+labs(title="Total 
Very Active Intensity vs Date")
#When comparing the two graphs, we can see that there is no visible correlation between intensity and time in bed awake. 
