# Reproducible Research Peer Assessment Project 1

# Section 1: Loading and preprocessing the data.

activity <- read.csv("activity.csv")

# Convert date factor variable to POSIXct for downstream analysis.
library(lubridate)
activity$lubridate <- ymd(activity$date)

# Section 2: What is the mean total number of steps taken per day?

# Ignore NA's
complete <- complete.cases(activity)
comp_activity <- activity[complete, ]

# Magrittr necessary for proper implementation in knitr.
library(dplyr)
library(magrittr)

result1 <-
comp_activity %>%
    group_by(lubridate) %>%
    summarize(sum_steps = sum(steps)
    )
    
# Make png file.
png(file = "plot1.png", width = 480, height =480)

# Make histogram of total steps.
hist(result1$sum_steps, xlab = "Total steps/day", col = 2, bg = "white", main = "Total steps per day (na.rm)", ylim = c(0,40))

# Turn device off.
dev.off()

# Calculate the mean and median steps per day
totalSteps <- result1$sum_steps
mean_steps <- mean(totalSteps)
median_steps <- median(totalSteps)
summary(totalSteps)

# Section 3: What is the average daily activity pattern?

result2 <-
comp_activity %>%
    group_by(interval) %>%
    summarise(avg_interval = mean(steps))

# Make png file.
png(file = "plot2.png", width = 480, height =480)
              
# Make histogram of total steps.
plot(result2$interval, result2$avg_interval, xlab = "Interval", ylab = "Average steps", col = 2, bg = "white", main = "", type = "l")
              
# Turn device off.	
dev.off()               
 
# Determine interval with the highest average number of steps.
 max_interval <- subset(result2, avg_interval == max(result2$avg_interval))
 max_interval$interval
 
# Section 4: Imputing missing values.
 
# Calculate total NAs.
totalNA <- sum(is.na(activity))
 
# Devise a strategy to impute missing values.
# I will use the average for the 5 minute interval.
# i.e. result2$avg_interval data will be used.
 
# Create a new data set.
# Add average interval variable to activity data frame.
result3 <-
activity %>%
    group_by(interval) %>%
    mutate(avg_steps = mean(steps, na.rm = TRUE))
 
result4 <- transform(result3, combined = ifelse(is.na(result3$steps), avg_steps, steps))

# Make histogram using data.frame with imputed data.

result5 <-
    result4 %>%
    group_by(lubridate) %>%
    summarize(sum_steps = sum(combined)
    )

# Make png file.
png(file = "plot3.png", width = 480, height =480)

# Make histogram of total steps.
hist(result5$sum_steps, xlab = "Total steps/day", col = 2, bg = "white", main = "Total steps per day (Imputed)", ylim = c(0,40))

# Turn device off.	
dev.off()

# Calculate the mean and median steps per day from imputed data.
totalStepsImp <- result5$sum_steps
mean_stepsImp <- mean(totalStepsImp)
median_stepsImp <- median(totalStepsImp)
summary(totalStepsImp)

# Section 5: Are there differences between weekdays and weekend?

result4$day <- weekdays(result4$lubridate)
week.code <- c(weekday = "Monday", weekday = "Tuesday", weekday = "Wednesday", weekday = "Thursday", weekday = "Friday", weekend = "Saturday", weekend = "Sunday")
result4$week <- as.factor(names(week.code)[match(result4$day, week.code)])

result6 <-
    result4 %>%
    group_by(week, interval) %>%
    summarize(last = mean(combined))
 
# Make the panel plot.

library(ggplot2)
qplot(interval, last, data = result6, geom = "line", facets = week~., ylab = "No. of steps")

# Reference to odd behavior of median calculation
# http://stackoverflow.com/questions/5902183/odd-behavior-with-median