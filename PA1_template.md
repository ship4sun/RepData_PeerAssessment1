---
output:
  html_document:
    theme: united
---
Title : PA1_template

Loading and preprocessing the data
===========================================
1. Loading the data

```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format="%Y-%m-%d")
```

2. Preparing the process

```r
library(knitr)
library(ggplot2)
library(dplyr)
```


What is mean total number of steps taken per day?
===========================================
1. Make a histogram of the total number of steps taken each day


```r
dailysteps <- aggregate(data$steps, by=data["date"], FUN=sum)
colnames(dailysteps) <- c("date", "total_steps")
qplot(total_steps, data=dailysteps, binwidth=1000, xlab="Total number of steps taken per day")
```

![plot of chunk histogram](figure/histogram-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
dailymean <- mean(dailysteps$total_steps, na.rm=TRUE)
dailymedian <- median(dailysteps$total_steps, na.rm=TRUE)
```

The mean total number of steps taken per day is '10766.19', 
and the median total number of steps taken per day is '10765'.


What is the average daily activity pattern?
===========================================
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalsteps <- aggregate(data$steps, by=data["interval"],mean,na.rm=TRUE)
colnames(intervalsteps) <- c("interval", "averageStps")
qplot(interval, averageStps, data=intervalsteps, xlab="interval", ylab="Average number of steps taken per day")+geom_line()
```

![plot of chunk average daily activity pattern](figure/average daily activity pattern-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximum <- max(intervalsteps$averageStps)
maxInterval <- intervalsteps[intervalsteps$averageStps==maximum, "interval"]
```

The 5 minute interval that contains the most number of steps on average is 'r maxInterval'.


Imputing missing values
===========================================
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing <- is.na(data$steps)
count <- which(missing==TRUE)
TotalMissing <- length(count)
```

The total number of missing values in the dataset is 'r TotalMissing'.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
intervalsteps <- aggregate(data$steps, by=data["interval"],mean,na.rm=TRUE)
colnames(intervalsteps) <- c("interval", "averageStps")
newData <- read.csv("activity.csv")
newData <- mutate(newData, steps=ifelse(is.na(steps),intervalsteps$averageStps, newData$steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newDailysteps <- aggregate(newData$steps, by=newData["date"], FUN=sum)
colnames(newDailysteps) <- c("date", "total_steps")
qplot(total_steps, data=newDailysteps, binwidth=1000, xlab="Total number of steps taken per day")
```

![plot of chunk histogram2](figure/histogram2-1.png) 

```r
dailymean <- mean(dailysteps$total_steps, na.rm=TRUE)
dailymedian <- median(dailysteps$total_steps, na.rm=TRUE)
newdailymean <- mean(newDailysteps$total_steps, na.rm=TRUE)
newdailymedian <- median(newDailysteps$total_steps, na.rm=TRUE)
```

The mean total number of steps taken per day is '10766.19', 
and the median total number of steps taken per day is '10765'.
After replacing the missing values with the mean of interval steps, 
the mean total number of steps taken per day becomes '10766.19', 
and the median total number of steps taken per day becomes '10766.19'.


Are there differences in activity patterns between weekdays and weekends?
===========================================


```r
newData <- mutate(newData, Days=ifelse(as.POSIXlt(data$date)$wday %in% c(0,6),"weekend","weekday"))
newData$Days <- as.factor(newData$Days)
newintervalsteps <- aggregate(newData$steps, by=newData[c("interval", "Days")],mean,na.rm=TRUE)
colnames(newintervalsteps) <- c("interval", "Days", "averageStps")
ggplot(newintervalsteps, aes(x=interval, y=averageStps))+facet_grid(.~Days)+geom_line()
```

![plot of chunk weekdays](figure/weekdays-1.png) 





