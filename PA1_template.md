---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
rawdata <- read.csv("activity.csv") 

#transfer interval format into 4-digit format.
rawdata$interval <- format(rawdata$interval,width=4) 
rawdata$interval <- chartr(" ","0",rawdata$interval)
head(rawdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01     0000
## 2    NA 2012-10-01     0005
## 3    NA 2012-10-01     0010
## 4    NA 2012-10-01     0015
## 5    NA 2012-10-01     0020
## 6    NA 2012-10-01     0025
```

## What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day, and ignore NA values.

```r
#ignore missing values
nona <- rawdata[complete.cases(rawdata),]
daystep <- aggregate(steps~date,nona,sum)
head(daystep)
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

2.Make a histogram of the total number of steps taken each day

```r
hist(daystep$steps, breaks = 10,
     xlab="Total steps",
     main="Figure 1: Histgram of Total Steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3.Calculate and report the mean and median of the total number of steps taken per day

```r
meandaystep <- mean(daystep$steps)
mediandaystep <- median(daystep$steps)
```

The mean of total steps per day is 1.0766189 &times; 10<sup>4</sup>, the median is 10765.

## What is the average daily activity pattern?

1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalstep <- aggregate(steps~interval,rawdata,mean)
plot(intervalstep$interval,intervalstep$steps,
     type = "l",
     main = "Figure 2: Plot of Intervals and the Average Steps Across All Day",
     xlab = "5-minute interval",
     ylab = "The average interval steps across all day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxinterval <- intervalstep[intervalstep$steps==max(intervalstep$steps),1]
```

The interval that contains the maximum number of steps is 0835 interval.

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset.

```r
na <- rawdata[rawdata$steps %in% NA,]
narow <- nrow(na)
```

There are 2304 missing values in the dataset.

2.To impute NA values, we use means for each interval to replace the NA values depending on the time interval they refer to. And the new dataset we created is named "imputed".

```r
imputed <- rawdata #create new dataset
#replace the NA with interval means
for (i in 1:nrow(imputed)) {
        if (is.na(imputed[i,1])==TRUE) {
                current <- imputed[i,3]
                imputed[i,1] <- intervalstep[intervalstep$interval==current,2]
        }
}
```

3.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
newdaystep <- aggregate(steps~date,imputed,sum)
newmean <- mean(newdaystep$steps)
newmedian <- median(newdaystep$steps)
hist(newdaystep$steps, 
     breaks = 10, 
     xlab = "Steps taken each day without NA", 
     main = "Figure 3: Histogram of the Total Steps Taken per Day without NA")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

The mean of total steps per day imputed NA is 1.0766189 &times; 10<sup>4</sup>, the median is 1.0766189 &times; 10<sup>4</sup>.

**-Do these values differ from the estimates from the first part of the assignment?**   
Yes.the median is different while mean is the same.

**-What is the impact of imputing missing data on the estimates of the total daily number of steps?**  
Because we choose the interval mean to fill the NA, it do not impact on mean of total steps per day. But for median, we add some records and the total number of records is changed, accordingly the position of middle record is changed, that's why median changed.


## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.

```r
Sys.setlocale("LC_ALL","English") #Setting locale
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
imputed$date <- as.Date(imputed$date,"%Y-%m-%d")
imputed$week <- ifelse(weekdays(imputed$date) %in% c("Saturday","Sunday"),"weekend","weekday")
imputed$week <- as.factor(imputed$week)
head(imputed)
```

```
##       steps       date interval    week
## 1 1.7169811 2012-10-01     0000 weekday
## 2 0.3396226 2012-10-01     0005 weekday
## 3 0.1320755 2012-10-01     0010 weekday
## 4 0.1509434 2012-10-01     0015 weekday
## 5 0.0754717 2012-10-01     0020 weekday
## 6 2.0943396 2012-10-01     0025 weekday
```

2.Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
newimputed <- aggregate(steps~week+interval,imputed,sum)
library(ggplot2)
ggplot(newimputed,aes(interval,steps,group=week)) + 
        geom_line() + 
        facet_grid(week~.) + 
        ggtitle("Figure 4: The Differences in Activity Patterns Between Weekdays and Weekends")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


