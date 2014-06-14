Assignment One
========================================================

## Loading and preprocessing the data:

we first setup system time zone to us (for display purpose), load the data from csv file and convert the "date" to class date 


```r
env<- Sys.setlocale("LC_TIME", "C")

Sys.setenv(LANG = "en_US.UTF-8")

data <- read.csv(file="activity.csv",sep=",")

data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

we first remove "NA"s from the data set. 

```r
data1 <- data[!is.na(data$steps),]
```

Then we use aggregate to get the sum of steps per day and plot the chart.


```r
data2 <- aggregate( data1$steps ~ data1$date, FUN=sum)

names(data2) <- c("date","steps")


plot(data2$date, data2$steps,type="h", col = "red", lwd = 10, xlab = "date", ylab = "steps")
```

![plot of chunk plot chart1](figure/plot chart1.png) 

compute the mean and median of  total number of steps taken per day


```r
mean(data2$steps)
```

```
## [1] 10766
```

```r
median(data2$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

first aggregate the steps by interval


```r
data4 <- aggregate(data1$steps ~ data1$interval, FUN=mean)

names(data4) <- c("interval","steps")
```

then we plot the chart


```r
plot(data4$interval,data4$steps,col="red",type="l", xlab = " 5 minute interval ", ylab = "steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?




```r
data4[data4$steps ==max(data4$steps),]
```

```
##     interval steps
## 104      835 206.2
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset: 


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset:

here we replace the na with the average steps for the interval across all days.The value data4 is got from previous step, here we just reuse it. Finally we 
get the data6.



```r
data6 <- merge(data,data4, by="interval", all.x=TRUE, all.y=FALSE)

names(data6) <- c("interval","steps","date","averagestepsbyinterval")


for (i in 1:(dim(data6)[1])) {
  
	if (is.na(data6[i,2])) {
		data6[i,2]<-data6[i,4] 
		 
	}
	
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
data7 <- aggregate( data6$steps ~ data6$date, FUN=sum)

names(data7) <- c("date","steps")
data7$date <- as.Date(data7$date)

plot(data7$date, data7$steps,type="h", col = "red", lwd = 10, xlab = "date", ylab="steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


mean and median


```r
mean(data7$steps)
```

```
## [1] 10766
```

```r
median(data7$steps)
```

```
## [1] 10766
```

The result plot is obviously different from the previous plot that exclude all the NAs. However, the mean and median are about the same as the previous one. This indicate the impact of the NA value is small.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

we use "weekdays" function to get the weekday from date. Then I use a function getweekdays to process the weekday and return weekend and weekdays. 


```r
data6 <- data

data6$date <- as.Date(data6$date)

data6$weekdaystemp <- weekdays(data6$date)

getweekdays <- function(date) {
  
	if((date == "Sunday") || (date == "Saturday")){
		return ("weekend")
		
	} else {
		return ("weekdays")
	}
	
} 

data6$weekdays <- sapply(as.vector(data6$weekdaystemp),getweekdays)
```

plot the charts to compare the average steps for each interval for weekdays and weekend




```r
weeklydata <- subset(data6, data6$weekdays=="weekdays")

weekenddata <- subset(data6, data6$weekdays=="weekend")

weeklydata <- aggregate(weeklydata$steps ~ weeklydata$interval, FUN=mean)

weekenddata <- aggregate(weekenddata$steps ~ weekenddata$interval, FUN=mean)

names(weeklydata) <- c("interval","steps")

names(weekenddata) <- c("interval","steps")


par(mfrow = c(2,1))

plot(weekenddata$interval,weekenddata$steps,col="blue",type="l", xlab = "5 minute interval", ylab="steps", main="weekend")

plot(weeklydata$interval,weeklydata$steps,col="blue",type="l", xlab="5 minute interval", ylab= "steps", main="weekly")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
