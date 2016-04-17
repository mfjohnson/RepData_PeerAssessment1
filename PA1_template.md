# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data



```r
unzip(zipfile="activity.zip")
activity_data <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?




```r
#library(ggplot2)
#library(dplyr)
steps_data <- aggregate(steps ~ date, data=activity_data, sum, na.rm = TRUE)
ggplot(steps_data, aes(x=steps)) +geom_histogram(col="blue", fill="blue") 
```

![](PA1_template_files/figure-html/analyzeStepMeans-1.png) 

```r
mean_steps <- mean(steps_data$steps, na.rm=TRUE)
median_steps <- median(steps_data$steps, na.rm=TRUE)
```



|             | Mean  | Median |
|-------------|-------|--------|
| Daily Steps | 10,766 | 10,765 |

## What is the average daily activity pattern?

```r
dailyStepsByInterval <- activity_data %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(steps = mean(steps))
ggplot(dailyStepsByInterval, aes(x=interval, y=steps)) + geom_line() + xlab("5 minute interval")+ylab("Average number of steps taken")
```

![](PA1_template_files/figure-html/averageDailyActivityPattern-1.png) 

```r
maxSteps <- dailyStepsByInterval[which.max(dailyStepsByInterval$steps),]
```

On average across all the days in the dataset the 5-minute interval, contains the maximum number of steps?

| Max Interval         | Max Number of steps |
|----------------------|---------------------|
|835 | 206.1698113  |



## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missingStepValues <- is.na(activity_data$steps)
missingCounts <- table(missingStepValues)
```

|                     | Count |
|---------------------|-------|
| Missing values (NA) | 2304 |
| Values Present | 15264 |

Replace missing (NA) values with the 5-minute interval mean.


```r
fillMissingSteps <- function(steps, interval) {
  filledVal <- NA
  if (!is.na(steps))
    filledVal <- c(steps)
  else
    filledVal <- (dailyStepsByInterval[dailyStepsByInterval$interval == interval, "steps"])
  return(filledVal)
}

mutateActivityData <- activity_data
mutateActivityData$adjSteps <- mapply(fillMissingSteps, mutateActivityData$steps, mutateActivityData$interval)
mutateActivityData$StepDate <- as.Date(mutateActivityData$date)
adj_total_steps <- tapply(mutateActivityData$steps, mutateActivityData$date, FUN=sum)

qplot(adj_total_steps, binwidth=1000, xlab="Total number of steps taken each day (adj for missing values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean_steps <- mean(adj_total_steps, na.rm=T)
median_steps <- median(adj_total_steps, na.rm=T)
```

|             | Mean  | Median |
|-------------|-------|--------|
| Daily Steps | 10,766 | 10,765 |


## Are there differences in activity patterns between weekdays and weekends?

```r
mutateActivityData$dayType <- ifelse(grepl("S(at|un)", weekdays(mutateActivityData$StepDate)), "Weekend", "Weekday")

averages <- aggregate(steps ~ interval + dayType, data = mutateActivityData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(dayType ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

