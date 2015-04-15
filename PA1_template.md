# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load **activity.csv**


```r
library(dplyr)
library(ggplot2)
```


```r
dataset <- read.csv('activity.csv', colClasses=c('integer', 'Date', 'integer'))

head(dataset)
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

## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day.


```r
steps_per_day <- dataset %>% group_by(date) %>% summarise(steps=sum(steps))

ggplot(data=steps_per_day, aes(x=date, y=steps)) + geom_bar(stat="identity") + labs(title="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Calculate and report the **mean** and **median** total number of steps taken per day.

Mean:

```r
mean(steps_per_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

Median:

```r
median(steps_per_day$steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_steps_per_interval <- dataset %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarise(steps=mean(steps))

plot(x=average_steps_per_interval$interval, y=average_steps_per_interval$steps, type="l", xlab="interval", ylab="steps", main="Mean steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average_steps_per_interval$interval[which.max(average_steps_per_interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
nrow(dataset %>% filter(is.na(steps)))
```

```
## [1] 2304
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Fill ``na`` with the mean value. Uses the impute function from the Hmisc package:


```r
mean_steps <- mean(dataset$steps, na.rm=TRUE)
dataset_filled_na <- dataset
dataset_filled_na$steps[is.na(dataset_filled_na$steps)] <- mean_steps
head(dataset_filled_na)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

Make a histogram of the total number of steps taken each day:


```r
steps_per_day_filled_na <- dataset_filled_na %>% group_by(date) %>% summarise(steps=sum(steps))

ggplot(data=steps_per_day_filled_na, aes(x=date, y=steps)) + geom_bar(stat="identity") + labs(title="Total number of steps taken per day (filled na)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Calculate and report the **mean** and **median** total number of steps taken per day:

Mean:

```r
mean(steps_per_day_filled_na$steps)
```

```
## [1] 10766.19
```

Difference with first part of the assignment:


```r
abs(mean(steps_per_day_filled_na$steps) - mean(steps_per_day$steps, na.rm=TRUE))
```

```
## [1] 0
```

Median:

```r
median(steps_per_day_filled_na$steps)
```

```
## [1] 10766.19
```

Difference with first part of the assignment:


```r
abs(median(steps_per_day_filled_na$steps) - median(steps_per_day$steps, na.rm=TRUE))
```

```
## [1] 1.188679
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?

Maximum difference:


```r
difference_na <- steps_per_day
difference_na$steps_filled_na <- steps_per_day_filled_na$steps
difference_na <- mutate(difference_na, difference = steps - steps_filled_na)
max(difference_na$difference, na.rm=TRUE)
```

```
## [1] 0
```

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dataset_filled_na <- mutate(
  dataset_filled_na,
  day_type = ifelse(
    weekdays(date, abbreviate=TRUE) %in% c('Sat', 'Sun'),
    "weekend",
    "weekday"
  )
)
dataset_filled_na$day_type <- factor(dataset_filled_na$day_type)
```

Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
steps_weekdays <- dataset_filled_na %>% filter(day_type == "weekday") %>% group_by(interval) %>% summarise(steps=mean(steps))

steps_weekend <- dataset_filled_na %>% filter(day_type == "weekend") %>% group_by(interval) %>% summarise(steps=mean(steps))

par(mfrow=c(2,1)) 

plot(x=steps_weekend$interval, y=steps_weekend$steps, type="l", xlab="interval", ylab="steps", main="Weekend")

plot(x=steps_weekdays$interval, y=steps_weekdays$steps, type="l", xlab="interval", ylab="steps", main="Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 
