---
title: "Activity Monitoring"
author: "Don Thapa"
date: "February 26, 2016"
output: html_document
---

This report looks at some activity monitoring data collected at 5 minute interval  
througout the day for an anonymous individual during the months of October and November 2012.

#### Loading and preprocessing the data

```r
setwd('~/Documents/RepData_PeerAssessment1/')
activity <- read.table(unz('activity.zip', 'activity.csv'), 
                       header = TRUE, sep = ",")
library(lubridate)
activity$date <- ymd(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

#### What is the mean total number of steps taken per day

```r
library(dplyr)
meanSteps <- group_by(activity, date) %>% 
    summarise(mean(steps, na.rm = TRUE))
names(meanSteps) <- c("date", "meanSteps")
library(ggplot2)
ggplot(meanSteps, aes(meanSteps)) + 
    geom_histogram(col = "blue", fill = "orange", binwidth = 10) + 
    geom_vline(aes(xintercept = mean(activity$steps, na.rm = TRUE)), 
               col = "black", size = 1.5) +
    labs(title = "histogram of average steps per day", x = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
meanSteps <- mean(activity$steps, na.rm = TRUE)
medianSteps <- median(activity$steps, na.rm = TRUE)
summaryActivity <- summary(activity$steps)
```

The mean and median steps taken per day is **37.3825996** and **0** respectively.

#### What is the average daily activity pattern?

```r
by_interval <- group_by(activity, interval) %>% 
    summarise(mean(steps, na.rm = TRUE))
names(by_interval) <- c("interval", "meanSteps")
maxMeanSteps <- by_interval[which.max(by_interval$meanSteps), ]
ggplot(by_interval, aes(interval, meanSteps)) + 
    geom_line(col = "blue") + 
    geom_point(alpha = .5, col = "red") +
    geom_text(data = maxMeanSteps, 
              aes(label = paste("highest at", round(interval, 1), ",", 
                                round(meanSteps, 1))), 
              size = 5, col = "purple") + 
    geom_smooth(method = "lm", col = "orange") +
    geom_hline(aes(yintercept = mean(activity$steps, na.rm = TRUE)), 
               col = "lightblue") + 
    geom_point(data = maxMeanSteps, aes(interval, meanSteps), 
               alpha = .4, size = 8, col = "orange") +
    labs(title = "average steps per 5 min interval", 
         y = "mean steps", x = "5 min intervals")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The interval with the highest mean steps of **206.1698113** is at **835**

#### Imputing missing values

```r
missingValues <- sum(!complete.cases(activity))
```
The data contains **2304** missing values

A simple approach to imputating missing values is adopted where missing steps are  
simply replaced by the mean steps calculated above **37.3825996**


```r
activity$steps[is.na(activity$steps)] = meanSteps
meanSteps_imputed <- group_by(activity, date) %>% 
    summarise(mean(steps))
names(meanSteps_imputed) <- c("date", "meanSteps")
ggplot(meanSteps_imputed, aes(meanSteps)) + 
    geom_histogram(col = "blue", fill = "orange", binwidth = 10) + 
    labs(title = "average steps per day", x = "steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
meanStepsImputed <- mean(activity$steps)
medianStepsImputed <- median(activity$steps)
```

The mean steps and median steps of the imputed data are now **37.3825996** and **0** respectively.  
These two statistics remained same before and after imputation, but it is clear from  
the following boxplot, that imputation has decreased the interquartile range.


```r
boxplot(summaryActivity, summary(activity$steps))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

#### Are there differences in activity patterns between weekdays and weekends?

```r
library(chron)
activity$weekend <- is.weekend(activity$date)
activity$day <- "weekday"
activity$day[activity$weekend == T] <- "weekend"
meanSteps <- group_by(activity, date, day) %>% 
    summarise(mean(steps, na.rm = TRUE))
names(meanSteps) <- c("date", "day", "meanSteps")
ggplot(meanSteps) + 
    geom_histogram(aes(meanSteps, fill = day), col = "blue", 
                   binwidth = 5, position="identity") + 
    facet_grid(. ~ day) +
    labs(title = "average steps per day", x = "steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Let us plot an overlay of the two histograms as well.

```r
ggplot(meanSteps) + 
    geom_histogram(aes(meanSteps, fill = day), col = "blue", 
                   binwidth = 5, position="identity") + 
    labs(title = "average steps per day", x = "steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Lets look at the summary next.

```r
weekendSteps <- subset(activity, weekend == T)$steps
weekdaySteps <- subset(activity, weekend == F)$steps
summary(weekendSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   42.37   37.38  785.00
```

```r
summary(weekdaySteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   35.61   37.38  806.00
```

So it does seem like there is some distinction between weekend and weekday mean steps - ***42.3664013*** for weekend vs ***35.6105812*** for weekdays. 

#### Conclusion
Lets validate what we saw above using a two sided t-test next using unequal variance.


```r
t.test.result <- t.test(weekendSteps, weekdaySteps)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  weekendSteps and weekdaySteps
## t = 3.7053, df = 7834.382, p-value = 0.0002126
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   3.181666 10.329974
## sample estimates:
## mean of x mean of y 
##  42.36640  35.61058
```

Given that the 95% confidence interval of the differences between the two subsets is **3.181666, 10.3299743** (so doesn't include 0) and  
the t-statistic is **3.7052711** which is much higher than the 95% confidence t-statistic for a **7834.3824839** degrees of freedom of around  
**1.9602668** and the associated p-value of the t test is **2.1261899 &times; 10<sup>-4</sup>**, we can statistically conclude that the individual walked on  
average more on the weekends than weekdays.
