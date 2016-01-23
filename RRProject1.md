---
title: "RR Project 1"
author: "Rich Huebner"
date: "January 22, 2016"
output: html_document
---

# Introduction
This project uses data from a personal activity monitoring device. The data contains information about the number of steps a person took at 5-minute intervals throughout the day. Data was collected anonymously during October & November, 2012.

This document outlines all of the procedures used to determine the results of the questions in the assignment.

```{r, echo=TRUE}
library(knitr)
opts_chunk$set(echo=TRUE)
```

First, load the libraries.
```{r}
library(ggplot2)
library(dplyr)
library(knitr)
library(lubridate)
```

Change to my local working directory. I have everything on a D: under various directories. Change this to your own.
```{r}
setwd("D:\\data\\Rprojects\\RResearch_Project1")
```

Read data set first and tidy up the date data
```{r}
data <- read.csv("activity.csv", header=TRUE, sep=",")
data$date <- ymd(data$date)
```

## Question 1: What is the mean total number of steps taken per day?
It is OK to ignore missing values in the data set.

1. Calculate the total number of steps taken per day.

```r
steps <- data %>%
     filter(!is.na(steps)) %>%
     group_by(date) %>%
     summarize(steps = sum(steps))
```

2. Make a histogram of the total number of steps taken per day.

```{r}
ggplot(steps, aes(x=steps)) + 
     geom_histogram(fill="navyblue") + labs(x="# of Steps", y="Frequency")
![plot of chunk unnamed-chunk-1](unnamed-chunk-1.png)
```



3. Calculate and report on the mean and median of the total number of steps taken per day.

```{r}
avg_steps <- mean(total_steps$steps, na.rm=TRUE)
median_steps <- median(total_steps$steps, na.rm=TRUE)

avg_steps
median_steps
```





## Question 2: What is the average daily activity pattern?

Create a time series plot (i.e. type = "l) of the 5-minute interval (x-axis) and the average number
of steps taken, averaged across all days. 

1. First create the interval.

```r
interval <- data %>%
          filter(!is.na(steps)) %>%
          group_by(interval) %>%
          summarize(steps = mean(steps))
```

2. Create the plot now.

```r
ggplot(interval, aes(x=interval, y=steps)) +
     geom_line(color="navyblue")

```

3. Which 5-minue interval that, on average, contains the maximum number of steps

```{r}
i <- interval[which.max(interval$steps), ]
i
```




## Question 3: Imputing missing values.
Calculate and report the total number of missing values (coded as NA). The presense of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the data set. ()

```r
sum(is.na(data$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. 

```r
imputed_data <- data
na_values <- is.na(imputed_data$steps)
avg_int <- tapply(imputed_data$steps, imputed_data$interval, mean, na.rm=TRUE, simplify=TRUE)

```


3. Create a new dataset that is equal to the original data set but with the missing data filled in.

### I use the average number of steps within the same 5-minute interval to impute the missing values.



### Next, get rid of the NAs.
imputed_data$steps[na_values] <- avg_int[as.character(imputed_data$interval[na_values])]

### and check to make sure that all of the NAs are gone now.
sum(is.na(imputed_data$steps))

### 4. Make a histogram of the total number of steps taken each day and calculate and report the mean
### and median total number of steps taken per day. Do these values differ from the estimates from the first part
### of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

### Calculation of steps taken in each 5-minute interval - except this time for the imputed data set
all_steps <- imputed_data %>%
               filter(!is.na(steps)) %>%
               group_by(date) %>%
               summarize(steps = sum(steps)) %>%
               print

### Histogram of the total number of steps taken each day after missing values are imputed
ggplot(all_steps, aes(x=steps)) +
          geom_histogram(fill="navyblue", binwidth=1000) +
          labs(title="Histogram of Steps/Day, Imputed Data Set", x="Steps/day", y="Frequency")

### Mean / Median values from imputed dataset
imean <- mean(all_steps$steps, na.rm=TRUE)
imed <- median(all_steps$steps, na.rm=TRUE)

imean
imed


# Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether 
### a given date is a weekday or weekend day.
### 2. Make a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

### Methods and Results

final <- mutate(all_steps, weektype= ifelse(weekdays(all_steps$date) == "Saturday" | weekdays(all_steps$date) == "Sunday", "weekend", "weekday"))
final$weektype <- as.factor(final$weektype)
head(final)




# All of the R code needed to reproduce the results (numbers, plots, etc) in the report.







Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r, echo=FALSE}




```