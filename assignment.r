library(utils)  # to get unzip
unzip("activity.zip")

activity <- read.csv("activity.csv")
head(activity)

library(dplyr)
activity_summary <- activity %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE)) 

library(graphics)
hist(activity_summary$total_steps, main = "Histogram of Steps per day", xlab = "Total steps per day")

activity_summary2 <- activity_summary %>% 
  summarise(mean = mean(total_steps, na.rm = TRUE), median = median(total_steps, na.rm = TRUE))

activity_summary3 <- activity %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm=TRUE))
plot(x = activity_summary3$interval, y = activity_summary3$avg_steps, 
     type="l", xlab = "Interval", ylab = "Steps average", 
     main="Average of steps per interval across all days")

activity_max <- activity_summary3 %>% summarise(max =max(avg_steps, na.rm = TRUE))
max_interval <- activity_summary3 %>% filter(activity_summary3$avg_steps == activity_max$max)

number_of_missing_data <- sum(is.na(activity$steps))

complete <- activity

for (i in 1:nrow(complete)) {
  if (is.na(complete$steps[i])) {
    complete$steps[i] = activity_summary3$avg_steps[i]
  }
}

complete_activity_summary <- complete %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE)) # group by day and sum the steps
hist(complete_activity_summary$total_steps, main = "Histogram of Steps per day", xlab = "Total steps per day") # Change main title and x axis label

complete_activity_summary2 <- complete_activity_summary %>% 
  summarise(mean = mean(total_steps, na.rm = TRUE), median = median(total_steps, na.rm = TRUE))

complete_wdays <- complete %>% mutate(Is.Weekday = 
                                        ifelse(
                                          (weekdays(as.POSIXct(date)) == "Saturday") | (weekdays(as.POSIXct(date)) == "Sunday"), 
                                               FALSE, TRUE))

complete_summary_weekdays <- complete_wdays %>% filter(Is.Weekday == TRUE) %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE)) 

complete_summary_weekends <- complete_wdays %>% filter(Is.Weekday == FALSE) %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE)) 

hist(complete_summary_weekdays$total_steps, main = "Histogram of Steps on Weekdays", xlab = "Total steps per day")

hist(complete_summary_weekends$total_steps, main = "Histogram of Steps on weekends", xlab = "Total steps per day")
