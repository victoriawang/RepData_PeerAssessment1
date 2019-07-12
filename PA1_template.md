---
title: "PA1_template"
output: 
  html_document: 
    keep_md: yes
---



## Loading and Preprocessing the Data


```r
data1 <- read.csv("C:/Victoria/Python files/Johns Hopkins Data Science Specialization/Reproducible Research Course Project 1/activity.csv")
```

## What is the mean total number of steps taken per day?

Shows the 1) the total steps taken per day (stepseachday), 2) a histogram of the steps taken each day, and 3) the mean and median of the steps taken per day


```r
not_na_data <- data1[!is.na(data1$steps),]
totalsteps <- sum(not_na_data$steps)

stepseachday <- NULL
for (i in 1:53) {
    sumsteps <- 0
    for (j in 1:288) {
        sumsteps <- sumsteps + not_na_data[(i-1) * 288 + j,]$steps
    }
    stepseachday <- cbind(stepseachday, sumsteps)
}

stepseachday
```

```
##      sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps
## [1,]      126    11352    12116    13294    15420    11015    12811
##      sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps
## [1,]     9900    10304    17382    12426    15098    10139    15084
##      sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps
## [1,]    13452    10056    11829    10395     8821    13460     8918
##      sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps
## [1,]     8355     2492     6778    10119    11458     5018     9819
##      sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps
## [1,]    15414    10600    10571    10439     8334    12883     3219
##      sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps
## [1,]    12608    10765     7336       41     5441    14339    15110
##      sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps sumsteps
## [1,]     8841     4472    12787    20427    21194    14478    11834
##      sumsteps sumsteps sumsteps sumsteps
## [1,]    11162    13646    10183     7047
```

```r
hist(stepseachday)
```

![](PA1_template_files/figure-html/meansteps-1.png)<!-- -->

```r
meansteps <- mean(stepseachday)
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps <- median(stepseachday)
mediansteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Shows 1) a time-series plot of the average number of steps taken during each 5-minute interval of a day and 2) the interval that contains the maximum average number of steps


```r
intervalsum <- not_na_data[1:288,]$steps
for (i in 2:53) {
    for (j in 1:288) {
        intervalsum[j] <- intervalsum[j] + not_na_data[(i-1) * 288 + j,]$steps
    }
}
intervalavg <- intervalsum/53
plot(1:288, intervalavg, type = 'l', main = "Time Series plot of Avg Steps Taken per 5-min Interval in a Day")
```

![](PA1_template_files/figure-html/dailypattern-1.png)<!-- -->

```r
maximumsteps <- max(intervalavg)

intervalmax <- 0
for (i in 1:15264) {
    if (intervalavg[i] == maximumsteps) {
        intervalmax <- i
        break
    }
}
intervalmax
```

```
## [1] 104
```

## Imputing Missing Values

1) Calculates the number of missing values (missingvals)
2) Devises a strategy for filling in the missing values (by inserting the average steps per 5-min interval calculated using the non-missing data)
3) Fills in the values
4) Makes a histogram of the new data set and gives its mean and median


```r
missingvals <- dim(data1[is.na(data1$steps),])[1]
missingvals
```

```
## [1] 2304
```

```r
data2 <- data1
for (i in 1:17568) {
    if (is.na(data1[i,]$steps)) {
        data2[i,]$steps <- meansteps/288
    }
}

stepseachday2 <- NULL
for (i in 1:61) {
    sumsteps2 <- 0
    for (j in 1:288) {
        sumsteps2 <- sumsteps2 + data2[(i-1) * 288 + j,]$steps
    }
    stepseachday2 <- cbind(stepseachday2, sumsteps2)
}

hist(stepseachday2)
```

![](PA1_template_files/figure-html/imputing-1.png)<!-- -->

```r
meansteps2 <- mean(stepseachday2)
meansteps2
```

```
## [1] 10766.19
```

```r
mediansteps2 <- median(stepseachday2)
mediansteps2
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1) Adds in a variable that determines whether the day is a weekday or weekend
2) Creates two time-series plots that shows the average number per steps per 5-min interval for the weekday and the weekend


```r
numweekdays288 <- 0
numweekends288 <- 0

data2$weekdayornot <- 0
weekdayofdata <- weekdays(as.POSIXct(data2$date))
for (i in 1:17568) {
    if (weekdayofdata[i] == "Sunday" | weekdayofdata[i] == "Saturday") {
        data2[i,]$weekdayornot <- "weekend" 
        numweekends288 <- numweekends288 + 1
    }
    else {
        data2[i,]$weekdayornot <- "weekday"
        numweekdays288 <- numweekdays288 + 1
    }
}

numweekends <- numweekends288/288
numweekdays <- numweekdays288/288

weekdayintsum <- data2[1:288,]$steps
weekendintsum <- rep(0,288)
for (i in 2:61) {
    for (j in 1:288) {
        if (data2[(i-1)*288+j,]$weekdayornot == "weekday") {
            weekdayintsum[j] <- weekdayintsum[j] + data2[(i-1) * 288 + j,]$steps
        }
        else {
            weekendintsum[j] <- weekendintsum[j] + data2[(i-1) * 288 + j,]$steps
        }
    }
}
weekdayintavg <- weekdayintsum/numweekdays
weekendintavg <- weekendintsum/numweekends
par(mfrow = c(2,1), mar = c(2,4,2,4))
plot(1:288, weekdayintavg, type = 'l', main = "Time Series plot of Avg Steps Taken per 5-min Interval in a Weekday")
plot(1:288, weekendintavg, type = 'l', main = "Time Series plot of Avg Steps Taken per 5-min Interval in a Weekend")
```

![](PA1_template_files/figure-html/differences-1.png)<!-- -->

