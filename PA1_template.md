# Reproducible Research: Peer Assessment 1
Created by mbhani on May 7, 2017

This is Coursera "Reproducible Research" class , programming assignment#1

## Loading and preprocessing the data

* Download and store the activity.zip data file into your working directory
* Unzip the activity.zip file into your working directory 
* 1 Day = 288 rows or interval steps (number of intervals where data was collected every 5 minutes)


```r
unzip("activity.zip")
activitydata <- data.frame(read.csv("activity.csv"))
names(activitydata)
```

```
## [1] "steps"    "date"     "interval"
```

```r
dim(activitydata)
```

```
## [1] 17568     3
```

```r
head(activitydata)
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

```r
tail(activitydata)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

Create a new data frame that calculates the total number of step per day


```r
activity.dailydata <- aggregate(. ~ date, data=activitydata, FUN=sum)[, 1:2]
head(activity.dailydata)
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

```r
dim(activity.dailydata)
```

```
## [1] 53  2
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.  
Calculate the total number of steps taken per day  


```r
activity.dailydata <- aggregate(. ~ date, data=activitydata, FUN=sum)
names(activity.dailydata)[2] <- paste("totalsteps")
head(activity.dailydata)
```

```
##         date totalsteps interval
## 1 2012-10-02        126   339120
## 2 2012-10-03      11352   339120
## 3 2012-10-04      12116   339120
## 4 2012-10-05      13294   339120
## 5 2012-10-06      15420   339120
## 6 2012-10-07      11015   339120
```

```r
dim(activity.dailydata)
```

```
## [1] 53  3
```

This is a histogram of the total number of steps taken each day  


```r
hist(activity.dailydata$totalsteps , breaks = 30, col = "orange", 
     main="Histogram For Total Daily Steps", 
     xlab="Daily Steps")
abline(v = mean(activity.dailydata$totalsteps), col = "blue", lwd = 2)
```
<img src="https://github.com/mbhani/RepData_PeerAssessment1/blob/master/figure/histogram-1.png" />

This is a barplot of the total daily steps data distributed over two months:

```r
barplot(activity.dailydata$totalsteps, main="Barplot For Total Daily Steps Distribution",
        xlab="Date", names.arg = activity.dailydata$date, 
        ylab = "Total Daily Steps")
```
<img src="https://github.com/mbhani/RepData_PeerAssessment1/blob/master/figure/barplot-1.png" />

Calculate the mean of the total number of steps taken per day:  

```r
meantotalsteps <- mean(activity.dailydata$totalsteps)
meantotalsteps #mean value
```

```
## [1] 10766.19
```

Calculate the median of the total number of steps taken per day:


```r
mediantotalsteps <- median(activity.dailydata$totalsteps)
mediantotalsteps #50% or median value
```

```
## [1] 10765
```


## What is the average daily activity pattern?

First generate the time series data for the daily average number of steps:

```r
timeseriesdata1 <- aggregate(. ~ interval, data=activitydata, FUN=mean)[,1:2]
names(timeseriesdata1)[2] <- paste("average#steps")
head(timeseriesdata1)
```

```
##   interval average#steps
## 1        0     1.7169811
## 2        5     0.3396226
## 3       10     0.1320755
## 4       15     0.1509434
## 5       20     0.0754717
## 6       25     2.0943396
```

```r
dim(timeseriesdata1)
```

```
## [1] 288   2
```

The following code generates a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps 
taken, averaged across all days (y-axis):

```r
plot(timeseriesdata1, type = "l", col = "blue", lwd = 2, main = "Time Series Plot of the 5-Minute Interval"
     , xlab = "Time Intervals (5-Minutes)", ylab = "Average Number of Steps Across All Days")
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

Next we calculate Which 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps

```r
maxsteps <- which.max((timeseriesdata1$`average#steps`))
maxsteps
```

```
## [1] 104
```

```r
maxinterval <- timeseriesdata1[maxsteps, ]
maxinterval
```

```
##     interval average#steps
## 104      835      206.1698
```

## Imputing missing values

The following calculates the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activitydata$steps)) #number of rows with NA values
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.  
* The strategy we will use here is to substitute the NA missing values in any interval with the average dialy number of steps for that 
specific interval (i.e we will use the timeseries data extracted before to imputate the data and create a new data frame)

```r
n1 <- dim(activitydata)[1] #total number of rows in the original dataframe
activitydata2 <- activitydata
pointer = NULL
for(i in 1:n1) {
                if(is.na(activitydata2$steps[i])) {
                pointer <- which(timeseriesdata1$interval == activitydata2$interval[i])
                activitydata2$steps[i] = timeseriesdata1$`average#steps`[pointer]
                }
        }
summary(activitydata2)#imputated new data frame, it shows no NA values in the new dataframe
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```r
head(activitydata2)#imputated data
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
dim(activitydata2)
```

```
## [1] 17568     3
```

Calculate the total number of steps taken per day  (using new imputated data)


```r
activity.dailydata2 <- aggregate(. ~ date, data=activitydata2, FUN=sum)
names(activity.dailydata2)[2] <- paste("totalsteps")
head(activity.dailydata2)
```

```
##         date totalsteps interval
## 1 2012-10-01   10766.19   339120
## 2 2012-10-02     126.00   339120
## 3 2012-10-03   11352.00   339120
## 4 2012-10-04   12116.00   339120
## 5 2012-10-05   13294.00   339120
## 6 2012-10-06   15420.00   339120
```

```r
dim(activity.dailydata2)
```

```
## [1] 61  3
```

This is a histogram of the total number of steps taken each day  (using the new imputated data):


```r
hist(activity.dailydata2$totalsteps , breaks = 30, col = "orange", 
     main="Histogram For Total Daily Steps (Using Imputated Data)", 
     xlab="Daily Steps")
abline(v = mean(activity.dailydata2$totalsteps), col = "blue", lwd = 2)
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

This is a barplot of the total daily steps data distributed over two months (using the new imputated data):


```r
barplot(activity.dailydata2$totalsteps, main="Barplot For Total Daily Steps Distribution (Using Imputated Data)",
        xlab="Date", names.arg = activity.dailydata2$date, 
        ylab = "Total Daily Steps")
```

![](PA1_template_files/figure-html/barplot2-1.png)<!-- -->


Calculate the mean of the total number of steps taken per day (using imputated data):  


```r
meantotalsteps2 <- mean(activity.dailydata2$totalsteps)
meantotalsteps2 #mean value (using imputated data)
```

```
## [1] 10766.19
```

```r
meantotalsteps  #mean of older data
```

```
## [1] 10766.19
```

Calculate the median of the total number of steps taken per day (using imputated data):


```r
mediantotalsteps2 <- median(activity.dailydata2$totalsteps)
mediantotalsteps2 #50% or median value (using imputated data)
```

```
## [1] 10766.19
```

```r
mediantotalsteps  #median of older data
```

```
## [1] 10765
```

Comparison between the older mean and median with the new mean and median after imputating the data shows that the mean stays the same in value but the new median 
value using imputated data is slightly higher than the older median value, also after imputation, both mean and median are equal in value!

## Are there differences in activity patterns between weekdays and weekends?
For this part we used the weekdays() function and also used the dataset with the filled-in missing values. Also created a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a 
weekday or weekend day.

```r
activitydata2$date <- as.Date(activitydata2$date)#convert date from factor to Date
head(activitydata2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
class(activitydata2$date)
```

```
## [1] "Date"
```

```r
activitydata2$week <- ifelse(weekdays(activitydata2$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")#parse dates in new column
activitydata2$week <- factor(activitydata2$week) #create new factor variable with two levels - "Weekday" and Weekend"
class(activitydata2$week)
```

```
## [1] "factor"
```

```r
head(activitydata2)
```

```
##       steps       date interval    week
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

```r
dim(activitydata2)
```

```
## [1] 17568     4
```

Prepare and postrocess new factord data to generate new time series for the pannel plot:

```r
#split the data based of week factor (weekday or weekend)
weekday.activitydata <- subset(activitydata2, activitydata2$week == "weekday")
weekend.activitydata <- subset(activitydata2, activitydata2$week == "weekend")
#calculate timeseries data for weekday data  
timeseries.weekday <- aggregate(. ~ interval, data=weekday.activitydata, FUN=mean)[,c(1,2,4)] #[,1:4]
names(timeseries.weekday)[2] <- paste("average#steps")
head(timeseries.weekday)
```

```
##   interval average#steps week
## 1        0    2.25115304    1
## 2        5    0.44528302    1
## 3       10    0.17316562    1
## 4       15    0.19790356    1
## 5       20    0.09895178    1
## 6       25    1.59035639    1
```

```r
dim(timeseries.weekday)
```

```
## [1] 288   3
```

```r
#calculate timeseries data for weekend data 
timeseries.weekend <- aggregate(. ~ interval, data=weekend.activitydata, FUN=mean)[,c(1,2,4)] #[,1:4]
names(timeseries.weekend)[2] <- paste("average#steps")
head(timeseries.weekend)
```

```
##   interval average#steps week
## 1        0   0.214622642    2
## 2        5   0.042452830    2
## 3       10   0.016509434    2
## 4       15   0.018867925    2
## 5       20   0.009433962    2
## 6       25   3.511792453    2
```

```r
dim(timeseries.weekend)
```

```
## [1] 288   3
```

```r
newtimeseries <- rbind(timeseries.weekday, timeseries.weekend)
newtimeseries$week <- factor(newtimeseries$week)
newtimeseries$week <- ifelse(newtimeseries$week == 1 , "weekday", "weekend")
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  


```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.3.3
```

```r
xyplot(newtimeseries$`average#steps` ~ newtimeseries$interval | newtimeseries$week, 
       layout = c(1, 2), type = "l", main = "Time Series Plot of the 5-Minute Interval for Weekday and Weekend", 
       xlab = "Time Intervals (5-Minutes)", ylab = "Average Number of Steps Across All Days")
```

![](PA1_template_files/figure-html/panelplot-1.png)<!-- -->

There are differences in activity patterns between weekdays and weekends
