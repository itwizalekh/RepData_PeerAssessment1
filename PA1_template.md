Submission for Peer Assesment 1: Reproducible Research
======================================================

## Loading and preprocessing data

Loading the dataset:


```r
rm(list = as.character(ls()))
setwd("C:\\Misc\\Git\\RepData_PeerAssessment1")

activity_raw <- read.csv(unz("activity.zip", filename = "activity.csv"))
```


Cleaning the dataset (removing records with NAs in steps).


```r
activity_cleaned <- activity_raw[!is.na(activity_raw$steps), ]
```


## What is mean total number of steps taken per day?

Aggregating to find daily activity:

```r
daily_activity <- aggregate(steps ~ date, data = activity_cleaned, sum)
```


Plotting histogram:

```r
hist(daily_activity$steps, col = "Green", breaks = 20)
rug(daily_activity$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Mean and median of the number of steps take per day:

```r
mean1 <- mean(daily_activity$steps)
```


```
## [1] 10766
```


```r
median1 <- median(daily_activity$steps)
```


```
## [1] 10765
```


## What is the average daily activity pattern?

### Calculating average daily activity, and re-coding intervals for better plotting:

```r
interval_activity <- aggregate(steps ~ interval, data = activity_cleaned, mean)

# re-coding interval consistently for better plotting
interval_activity$i <- seq(0:(dim(interval_activity)[1] - 1))
interval_activity$interval_recoded <- interval_activity$i * 5
```


Plotting averaged 5-minute interval:

```r
plot(x = interval_activity$interval_recoded, y = interval_activity$steps, type = "l", 
    xlab = "5-minute interval ", ylab = "average number of steps taken")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


Finding the 5-minute interval with maximum number of steps:

```r
bestInterval <- interval_activity[interval_activity$steps == max(interval_activity$steps), 
    ]$interval_recoded

# Calculating time at the beginning of this interval
from_hr <- as.integer(bestInterval/60)
from_min <- bestInterval - as.integer(bestInterval/60) * 60
from <- paste(from_hr, from_min, sep = ":")

# Calculating time at the end of this interval
to_hr <- as.integer((bestInterval + 5)/60)
to_min <- (bestInterval + 5) - as.integer((bestInterval + 4)/60) * 60
to <- paste(to_hr, to_min, sep = ":")
```


Maximum number of steps were taken from 8:40 hours (inclusive) to 8:45 hours (excluded).  
  

## Imputing missing values

### Calculating total number of missing values

Calculating total number of missing values:

```r
cnt_na <- table(is.na(activity_raw$steps))[["TRUE"]]
```

2304 of 17568 values in the dataset are missing.

Creating new data set by replacing NAs with mean value for respective interval:


```r
activity_imputed <- activity_raw

for (i in seq(1:dim(activity_imputed)[1])) {
    if (is.na(activity_imputed[i, ]$steps)) {
        activity_imputed[i, ]$steps <- interval_activity[interval_activity$interval == 
            activity_imputed[i, ]$interval, ]$steps
    }
}
```


Aggregating daily activity from the imputed dataset:


```r
daily_activity_imputed <- aggregate(steps ~ date, data = activity_imputed, sum)
```


Plotting histogram for number of steps taken (from imputed dataset):

```r
hist(daily_activity_imputed$steps, col = "Green", breaks = 20)
rug(daily_activity_imputed$steps)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 


Mean and median of the number of steps take per day:

```r
mean2 <- mean(daily_activity_imputed$steps)
```


```
## [1] 10766
```


```r
median2 <- median(daily_activity_imputed$steps)
```


```
## [1] 10766
```


Mean increased by 0

Adding factor variable distinguishing weekdays from weekends:

```r
day_indicator <- weekdays(as.Date(activity_imputed$date, "%Y-%m-%d"))
day_indicator[day_indicator == "Sunday" | day_indicator == "Saturday"] <- "weekend"
day_indicator[day_indicator != "weekend"] <- "weekday"
activity_imputed$weekday_type <- as.factor(day_indicator)
```


Carving-out seperate datasets for weekday and weekend activity:

```r
data_wd <- activity_imputed[activity_imputed$weekday_type == "weekday", ]
interval_activity_weekday <- aggregate(steps ~ interval, data = data_wd, mean)
data_we <- activity_imputed[activity_imputed$weekday_type == "weekend", ]
interval_activity_weekend <- aggregate(steps ~ interval, data = data_we, mean)
```



Re-coding interval in weekday and weekend datasets for better plotting:

```r
interval_activity_weekday$i <- seq(1:dim(interval_activity_weekday)[1])
interval_activity_weekday$interval_recoded <- interval_activity_weekday$i * 
    5

interval_activity_weekend$i <- seq(1:dim(interval_activity_weekend)[1])
interval_activity_weekend$interval_recoded <- interval_activity_weekend$i * 
    5
```


Plotting

```r
par(mfrow = c(2, 1))

plot(x = interval_activity_weekend$interval_recoded, y = interval_activity_weekend$steps, 
    type = "l", xlab = "weekend intervals", ylab = "Number of Steps")
plot(x = interval_activity_weekday$interval_recoded, y = interval_activity_weekday$steps, 
    type = "l", xlab = "weekday intervals", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23.png) 

