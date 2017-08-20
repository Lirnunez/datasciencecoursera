# Reproducible Research: Peer Graded Assignment 1
Ivan Rodriguez

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up.

This assignment makes use of data from a personal activity monitoring device

- This device collects data at 5 minute intervals through out the day
- The dataset consists of two months (October and November, 2012) of data from an anonymous individual
- Included are the number of steps taken in 5 minute intervals each day

The data for this assignment can be downloaded from the course web site:

- Dataset: Activity monitoring data

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

## Loading required libraries

```r
    library(dplyr)
    library(ggplot2)
```

1. Code for reading in the dataset

```r
    temp <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
    unzip(temp, "activity.csv")
    activity_data <- read.csv("activity.csv",header=T)
    unlink(temp)
```

2. Histogram of the total number of steps taken each day

```r
    total.steps.per.day <- aggregate(steps ~ date, data = activity_data, FUN = sum, na.rm = TRUE)
    hist(total.steps.per.day$steps, main="Total Steps per Day",  xlab="Number of Steps per Day", 
        ylab = "Interval", col="blue", breaks=50)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3. Mean and median number of steps taken each day

```r
    mean.steps <- mean(total.steps.per.day$steps)
    mean.steps
```

```
## [1] 10766.19
```

```r
    median.steps <- median(total.steps.per.day$steps)
    median.steps
```

```
## [1] 10765
```

4. Time series plot of the average number of steps taken

```r
    five.min <- aggregate(steps ~ interval, data = activity_data, FUN = mean, na.rm = TRUE)
    plot(x = five.min$interval, y = five.min$steps, type = "l", col = "black", xlab = "5-minute Intervals",
        ylab = "Average Steps Taken ~ Days", main = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

5. The 5-minute interval that, on average, contains the maximum number of steps

```r
    max.steps <- five.min$interval[which.max(five.min$steps)]
    max.steps
```

```
## [1] 835
```

6. Code to describe and show a strategy for imputing missing data

```r
    impute.na <- sum(is.na(activity_data$steps))
    impute.na
```

```
## [1] 2304
```

```r
    activity_data_2 <- activity_data
    NA_in_dataset <- is.na(activity_data_2$steps)
    avg_interval <- tapply(activity_data_2$steps, activity_data_2$interval, mean, na.rm=TRUE, simplify = TRUE)
    activity_data_2$steps[NA_in_dataset] <- avg_interval[as.character(activity_data_2$interval[NA_in_dataset])]
    sum(is.na(activity_data_2))
```

```
## [1] 0
```

7. Histogram of the total number of steps taken each day after missing values are imputed

```r
    total.steps.per.day.2 <- aggregate(steps ~ date, data = activity_data_2, FUN = sum, na.rm = TRUE)
    hist(total.steps.per.day.2$steps, main = "Total Steps per Day (no_NA)", xlab = "Number of Steps per Day", 
        ylab = "Interval", col="red", breaks=50)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

8. Calculate and report the mean and median total number of steps taken per day after imputing NAs
What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
    summary(total.steps.per.day.2)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
    summary(total.steps.per.day)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```
## Mean and median values are almost identical, but the quantiles are different

9. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekend

```r
    activity_data_2$date <- as.Date(activity_data_2$date, "%Y-%m-%d")
    activity_data_2<- activity_data_2%>% mutate(typeofday= ifelse(weekdays(activity_data_2$date)=="Saturday"| 
      weekdays(activity_data_2$date)=="Sunday", "Weekend", "Weekday"))
    five.min.2<- aggregate(steps ~ interval, data = activity_data_2, FUN = mean, na.rm = TRUE)
    ggplot(activity_data_2, aes(x =interval , y=steps, color=typeofday)) + geom_line() +
    labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
    facet_wrap(~ typeofday, ncol = 1, nrow=2)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

    
