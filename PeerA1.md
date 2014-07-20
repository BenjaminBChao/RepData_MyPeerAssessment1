---
title: "Peer1Assessment"
author: "Benjamin Chao"
date: "July 20, 2014"
output: html_document
---

# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
### Boys and Girls, let's load the dataset to R Studio!
```{r loaddata}
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
```{r}
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```

## What is the average daily activity pattern?
```{r}
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

Here, we are going to find out which 5-minute interal, on average across all the days in the dataset, contains the meximun number of steps! 
```{r}
averages[which.max(averages$steps),]
```

## Imputing missing values

Unfortunately, we have several missing values (coded as `NA`) in the dataset and  that may introduce some bias into our data calculations and summaries.

```{r how_many_missing}
missing <- is.na(data$steps)
# How many missing
table(missing)
```

All of the missing values are filled in with mean value for that 5-minute
interval.

```{r}
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```
Ok, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.

```{r}
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)
```

Mean & Median values are now higher after imputing missing data The reason is
that in the original data, there were some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are now removed from histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First thing first, let's find the day of the week for each measurement in the dataset. In this part, we use the dataset with the filled-in values.


```{r}
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```