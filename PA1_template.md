---
title: "PA1_template.Rmd"
author: "Dipika"
date: "Sunday, May 17, 2015"
output: html_document
---

##Loading and preprocessing the data
  
The code needed to do the following steps:
  1.  Load the data (i.e. read.csv())

  2.  Process/transform the data (if necessary) into a format suitable for your analysis



```r
dir <- paste(getwd(),"/",sep="")
path1 <- "activity.csv"
x <- read.csv(file.path(dir,path1), header = TRUE, sep=",", stringsAsFactor= FALSE)
x$date <- as.Date(x$date,"%Y-%m-%d")
head(x)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

##Calculate mean of total number of steps taken per day

1. Calculate the total number of steps taken per day

```r
TotalNoOfSteps <- aggregate(steps ~ date, data = x, sum, na.rm = TRUE)
head(TotalNoOfSteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
2.  Make a histogram of the total number of steps taken each day

```r
library(lattice)
hist(TotalNoOfSteps$steps, main = "Total steps Taken by day", xlab = "Days",ylab = "steps", col = "blue")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
3. Calculate the mean and median of the total number of steps taken per day

```r
print("mean :" )
```

```
## [1] "mean :"
```

```r
mean(TotalNoOfSteps$steps)
```

```
## [1] 10766.19
```

```r
print("median :")
```

```
## [1] "median :"
```

```r
median(TotalNoOfSteps$steps)
```

```
## [1] 10765
```
##The average daily activity pattern
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgTaken <- tapply(x$steps, x$interval, mean, na.rm = TRUE)
plot(row.names(avgTaken), avgTaken, type = "l", xlab = "Interval", 
     ylab = "Average Number of steps", main = "Average number of steps taken by interval", 
     col = "blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
max <- which.max(avgTaken)
names(max)
```

```
## [1] "835"
```
##Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NACount <- sum(is.na(x))
NACount
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
avgSteps <- aggregate(steps ~ interval, data = x, FUN = mean)
filler <- numeric()
for (i in 1:nrow(x)) {
  row1 <- x[i, ]
  if (is.na(row1$steps)) {
    steps <- subset(avgSteps, interval == row1$interval)$steps
  } else {
    steps <- row1$steps
  }
  filler <- c(filler, steps)
}
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newx <- x
newx$steps <- filler
```
4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
- Answer : Mean remains the same after imputing missing values but median changes slightly.

```r
TotalNoOfSteps2 <- aggregate(steps ~ date, data = newx, sum, na.rm = TRUE)
hist(TotalNoOfSteps2$steps, main = "Total steps Taken by day", xlab = "Days",ylab = "steps", col = "blue")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(TotalNoOfSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(TotalNoOfSteps2$steps)
```

```
## [1] 10766.19
```
- Answer : Mean remains the same after imputing missing values but median changes slightly.  

##Differences in activity patterns between weekdays and weekends
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
day <- weekdays(x$date)
dayfact <- vector()
for (i in 1:nrow(x)) {
  if (day[i] == "Saturday") {
    dayfact[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    dayfact[i] <- "Weekend"
  } else {
    dayfact[i] <- "Weekday"
  }
}
x$dayfact <- dayfact
x$dayfact <- factor(x$dayfact)
head(x$dayfact,10)
```

```
##  [1] Weekday Weekday Weekday Weekday Weekday Weekday Weekday Weekday
##  [9] Weekday Weekday
## Levels: Weekday Weekend
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
TotalstepsByDay <- aggregate(steps ~ interval + dayfact, data = x, mean)
names(TotalstepsByDay) <- c("interval", "dayfact", "steps")

xyplot(steps ~ interval | dayfact, TotalstepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps Taken")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
