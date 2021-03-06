---
title: "Reproducible Research-Peer Assessment 1"
author: "Jeff Roberts"
date: "Wednesday, November 12, 2014"
output: html_document
---
##Loading and preprocessing the data


```r
###read .csv file and format date field as default:  yyyy-mm-dd
pr1<-read.csv("activity.csv", stringsAsFactors=FALSE)
pr1$date <- as.Date(pr1$date)
###Omit rows that contain na
pr1.c <- na.omit(pr1)
```

##What is mean total number of steps taken per day?
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
######Calculate the Daily Means and plot a histogram
d.mean<-aggregate(pr1.c$steps, by=list(pr1.c $date), FUN=mean)
colnames(d.mean) <- c("date", "Average_Steps")
d.mean$date<-as.numeric(d.mean$date)
###plot histogram
hist(d.mean$Average_Steps, breaks = 12, ylim=c(0,14), xlim=c(0,80), xlab="Average Number of Steps per Day", main="Number of Steps Taken per Day: October-November 2012")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
###Calculate the daily mean and median
d.mean.me<-mean(d.mean$Average_Steps)
d.mean.md<-median(d.mean$Average_Steps)
a<-rbind(c(d.mean.me,d.mean.md))
colnames(a) <- c("Daily Mean", "Daily Median")
rownames(a)<-"Daily Steps taken: October-November 2012"
a
```

```
##                                          Daily Mean Daily Median
## Daily Steps taken: October-November 2012    37.3826     37.37847
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
############################################################What is the average daily activity pattern?
######Make a Time Series plot of 5-minute intervals
###average number of steps over 5-minute interval
Interval.mean<-aggregate(pr1.c$steps, by=list(pr1.c $interval), FUN=mean)
colnames(Interval.mean) <- c("Interval", "Interval_Mean")
count<-which.max(Interval.mean$Interval_Mean)
###plot and label maximum 5 minute interval
plot(Interval.mean, type="l", xlab="5 Minute Interval", ylab="Average Number of Steps Taken per Interval", main="Average Number of Steps Taken per 5 Minute Interval\nOctober-November 2012")
points(Interval.mean[count,], col=2, pch=19)
text(Interval.mean[count,], "Maximum Interval Value: Interval=835, 206.2 Steps", pos=4, offset = 0.5, cex=0.7)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

##Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
###total number of rows with NA's
(n.a<-length(pr1[is.na(pr1)]))
```

```
## [1] 2304
```

```r
####aggregate means by interval with NA's removed
######Calculate the Daily Means and plot a histogram from NA's removed
int.mean<-aggregate(pr1.c$steps, by=list(pr1.c$interval), FUN=mean)
colnames(int.mean) <- c("interval", "Average_Steps")
int.mean$Average_Steps<-as.character(int.mean$Average_Steps)
###replace NA's with interval means(modifyed code from: http://stackoverflow.com/questions/19593881/replace-na-values-in-dataframe-variable-with-values-from-other-dataframe-by-id)
pr1.i<-read.csv("activity.csv", stringsAsFactors=FALSE)
pr1.i$date <- as.Date(pr1.i$date)
pr1.i$steps<-as.character(pr1.i$steps)
pr1.i$steps <- ifelse(is.na(pr1.i$steps), int.mean$Average_Steps[match(pr1.i$interval, int.mean$interval)], pr1.i$steps)
pr1.i$steps<-as.numeric(pr1.i$steps)
######Calculate the Daily Imputed Means and plot a histogram vs. non-Imputed mean
int.mean$Average_Steps<-as.numeric(int.mean$Average_Steps)
int.mean.i<-aggregate(pr1.i$steps, by=list(pr1.i$interval), FUN=mean)
colnames(int.mean.i) <- c("interval", "Average_Steps")
int.mean.i$Average_Steps<-as.numeric(int.mean.i$Average_Steps)

par(mfrow=c(2,1))
hist(int.mean$Average_Steps, breaks = 50, ylim=c(0,100), xlim=c(0,250), xlab="Average Number of Steps per Interval", main="Average Number of Steps Per Interval: NA's not Replaced \nOctober-November 2012")
hist(int.mean.i$Average_Steps, breaks = 50, ylim=c(0,100), xlim=c(0,250), xlab="Average Number of Steps per Interval", main="Average Number of Steps Per Interval: NA's Replaced with 5 Minute Interval Means \nOctober-November 2012")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
par(mfrow=c(1,1))

###Imputed Mean and Median Steps taken per Day
d.mean.i<-aggregate(pr1.i$steps, by=list(pr1.i$date), FUN=mean)
colnames(d.mean.i) <- c("date", "Average_Steps")
i.mean<-mean(int.mean.i$Average_Steps)
i.median<-median(int.mean.i$Average_Steps)
c<-cbind(c(d.mean.me,i.mean, d.mean.md, i.median))
rownames(c)<-c("Daily Mean-NA's Removed", "Daily Mean-NA Imputed by Interval Mean", "Daily Median-NA's Removed", "Daily Median-NA Imputed by Interval Mean")
colnames(c)<-"Value"
c
```

```
##                                             Value
## Daily Mean-NA's Removed                  37.38260
## Daily Mean-NA Imputed by Interval Mean   37.38260
## Daily Median-NA's Removed                37.37847
## Daily Median-NA Imputed by Interval Mean 34.11321
```

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
############################################################Imputing missing values: Replacing na's with interval means
##############################################Are there differences between weekdays and weekends?
activity3.df<-read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
activity3.df$date<-weekdays(activity3.df$date)
activity3.df$date<-ifelse(activity3.df$date==c("Saturday", "Sunday"), "Weekend", "Weekday")
interval2.steps<-aggregate(steps~date+interval, data=activity3.df, sum, na.rm=TRUE)
Weekday<-subset(interval2.steps, date == "Weekday")
Weekend<-subset(interval2.steps, date == "Weekend")
par(mfrow = c(2, 1))
plot(Weekday[,2], Weekday[,3], type ="l", xlab="5 Minute Interval", ylab="Number of Steps Taken", main="5-Minute Interval Weekday Daily Step Count \nOctober 2012-Novemeber 2012", ylim=c(0,10000), xlim=c(0,2400), col="blue")
plot(Weekend[,2], Weekend[,3], type ="l", xlab="5 Minute Interval", ylab="Number of Steps Taken", main="5-Minute Interval Weekend Daily Step Count \nOctober 2012-Novemeber 2012", ylim=c(0,10000), xlim=c(0,2400), col="blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
par(mfrow = c(1, 1))
```





