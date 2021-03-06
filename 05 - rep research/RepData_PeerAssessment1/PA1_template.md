# Reproducible Research: Peer Assessment 1

This document describes the steps and code blocks used to analyze the data and answer the questions for the Reproducible Research: Peer Assesment 1 assignment. 

## Loading and preprocessing the data

Assuming the zip archive is in the working directory, the following code was used to load and preprocess the data. 


```r
unzip("activity.zip")
StepsData <- read.csv("activity.csv")
StepsData$date <- as.Date(StepsData$date)
head(StepsData)
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

The only preprocessing step is converting the date string in a date variable, which will be useful later for the analysis.

The data is already clean and doesn't need any more processing. There are a number of NAs in the *steps* variable, but they will be dealt with later as per the assignment instructions. I'm keeping the *interval* variable as a number beacuse it's easily interpretable: for example, "35" means the interval betwenn 0:35 and 0:40 and "2020" means the interval between 20:20 and 20:25 in a 24h notation. 

## What is mean total number of steps taken per day?

The following code is used to generate a variable *TotalSteps* containing the total number of steps per day and its mean and median.

A histogram of that data is displayed.


```r
TotalSteps <- aggregate(steps~date,data=StepsData,na.action=na.pass,FUN=sum,na.rm=T)
mean(TotalSteps$steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(TotalSteps$steps, na.rm=TRUE)
```

```
## [1] 10395
```

```r
par(mfcol=c(1,1))
hist(TotalSteps$steps,xlab="Steps",main="Total Number of Steps")
abline(v=median(TotalSteps$steps, na.rm=TRUE),col="blue",lty=1,lwd=4)
abline(v=mean(TotalSteps$steps, na.rm=TRUE),col="red",lty=2,lwd=4)
legend("topright",c("median","mean"),col=c("blue","red"),lwd=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)


## What is the average daily activity pattern?

The variable *DailySteps* contains the avarage daily activity pattern across the 5 minute intervals.


```r
DailySteps <- aggregate(steps~interval,data=StepsData,na.action=na.pass,FUN=mean,na.rm=T)
par(mfcol=c(1,1))
with(DailySteps,plot(interval, steps, type="l",main="Average Daily Activity Pattern"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
DailySteps[which.max(DailySteps$steps),]$interval
```

```
## [1] 835
```

As calcualated, the 5 minute interval with the most avarage daily activity is the one between 8:35 to 8:40 in the morning.

## Imputing missing values

The total number of missing values in the dataset can be calculated as follows


```r
sum(is.na(StepsData$steps))
```

```
## [1] 2304
```

I will replace the missing values for each 5-minute interval with the mean of the same interval over the whole dataset, which has already been calculated in the variable *DailySteps*.


```r
FilledData <- StepsData
meansArray <- rep(DailySteps$steps,length(unique(StepsData$date)))
FilledData[is.na(FilledData$steps),]$steps <- meansArray[is.na(FilledData$steps)]
```

The new dataset *FilledData* is now identical to *StepsData*, exept for the filled in missing values.

I can now create the variable *TotalStepsFilled*, containing the total number of steps for each day, and calculate its mean and median.


```r
TotalStepsFilled <- aggregate(steps~date,data=FilledData,FUN=sum)
mean(TotalStepsFilled$steps)
```

```
## [1] 10766.19
```

```r
median(TotalStepsFilled$steps)
```

```
## [1] 10766.19
```

We can see that the mean and median values now coincide.

The following is a histogram of the total number of steps for the two datasets.


```r
par(mfcol=c(1,2))
hist(TotalSteps$steps,xlab="Steps",main="Original Data")
abline(v=median(TotalSteps$steps),col="blue",lty=1,lwd=4)
abline(v=mean(TotalSteps$steps),col="red",lty=2,lwd=4)
legend("topright",c("median","mean"),col=c("blue","red"),lwd=3,cex=0.7)
hist(TotalStepsFilled$steps,xlab="Steps",main="Filled-in Data")
abline(v=median(TotalStepsFilled$steps),col="blue",lty=1,lwd=4)
abline(v=mean(TotalStepsFilled$steps),col="red",lty=2,lwd=4)
legend("topright",c("median","mean"),col=c("blue","red"),lwd=3,cex=0.7)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)

All or most of the filled in data ends up in the third bucket and the two histograms look substantially different.

## Are there differences in activity patterns between weekdays and weekends?

The filled-in dataset is used for this answer.

First I create a new factor variable *daytype* in the dataset with the levels "weekend" and "weekday". This is done with the use of the lubridate package and further transformation.


```r
library(lubridate)
FilledData$daytype <- "weekday"
FilledData[wday(FilledData$date)=="1" | wday(FilledData$date)=="7",]$daytype <- "weekend"
FilledData$daytype <- as.factor(FilledData$daytype)
```

Then, I create a new aggregate dataset with the mean number of steps per interval and daytype.

The plot to compare the steps vs time interval for weekends and weekdays is done using the lattice package.


```r
DailyStepsFilled <- aggregate(steps~interval+daytype,data=FilledData,FUN=mean)
library(lattice)
xyplot(steps ~ interval | daytype, data=DailyStepsFilled, layout=c(1,2), 
       type="b",lty=1,cex=0, ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)

We can see that the average number of steps in weekend days is substantially higher than in weekdays, except for early morning hours and a lower peak around 19:00.
