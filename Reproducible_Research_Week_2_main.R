# load libaray
library(ggplot2)
library(Hmisc)
#load data
data <- read.csv("activity.csv")
# check out the data details
head(data)
dim(data)
summary(data)

## What is mean total number of steps taken per day?

# 1.  Calculate the total number of steps taken per day
SumStepsByDay <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
# 2. If you do not understand the difference between a histogram and a barplot, 
#    research the difference between them. \
#    Make a histogram of the total number of steps taken each day
qplot(SumStepsByDay, binwidth=1000, xlab="Sum steps by day", ylab="Frequecy")
# 3. Calculate and report the mean and median of the total number of steps taken per day
meanStep(SumStepsByDay, na.rm=TRUE)
medianStep(SumStepsByDay, na.rm=TRUE)

## What is the average daily activity pattern?
# 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of 
#    the 5-minute interval (x-axis) and the average number of steps taken, 
#     averaged across all days (y-axis)
avgs <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=meanStep, na.rm=TRUE)
ggplot(data=avgs, aes(x=interval, y=steps)) + geom_line() +
  xlab("5-minute interval") +
  ylab("Avg number of steps")

# 2. Which 5-minute interval, on average across all the days in the dataset, 
#     contains the maximum number of steps?
avgs[which.max(avgs$steps),]

## Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
missing <- is.na(data$steps)
N_missing <- sum(missing)


# 2. Devise a strategy for filling in all of the missing values in the dataset...

# -> Use mean interval steps from Mean Steps for that interval with missing values.

# 3. Create a new dataset that is equal to the original dataset but with the ...
new_data <- data
new_data$steps <- impute(new_data$steps, fun=mean)

# 4. Make a histogram of the total number of steps taken each day and...
New_SumStepsByDay <- tapply(new_data$steps, new_data$date, sum)
qplot(New_SumStepsByDay, binwidth=1000,xlab="Sum steps by day", ylab="Frequecy")
mean(New_SumStepsByDay)
median(New_SumStepsByDay)

## Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels...
new_data$dateType <-  ifelse(as.POSIXlt(new_data$date)$wday %in% c(0,6), 'weekend', 'weekday')
head(new_data,15)
# 2. Make a panel plot containing a time series plot ...
new_avgs <- aggregate(steps ~ interval + dateType, data=new_data, mean)
ggplot(new_avgs, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(dateType ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")