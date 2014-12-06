Reproducible Assignment 1
========================================================
## Zhizheng Wang

This is the R Markdown document for the Reproducible Research Assignment 1.
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### task 1
First we load the data into R. 

```r
opts_chunk$set(echo=TRUE, results = "asis")
```


```r
library(plyr)
activity <- read.csv("C://Users/User/Documents/R/activity.csv")
cdata <- ddply(activity, c("date"), summarise,
               N    = sum(!is.na(steps)),
               sum  = sum(steps, na.rm=TRUE),
               mean = mean(steps, na.rm=TRUE),
               sd   = sd(steps, na.rm=TRUE),
               se   = sd / sqrt(N),
               median = median(steps, na.rm=TRUE))
histinfo<-hist(cdata$sum, breaks = 20, xlab="total steps per day", main="Distribution of Total Steps", col="lightgreen")
```

![plot of chunk loadin](figure/loadin-1.png) 

```r
#histinfo$counts[1]
clean <- cdata[which(cdata$sum>1000),]
```

We can see there are 10 values that are lower than 1000, including the missing value. Therefore we discard them as outliers. 
Then we can plot the histgram to see the cleaned data and compute the mean and median. 


```r
hist(clean$sum, breaks = 10, xlab="total steps per day", main="Distribution of Total Steps", col="lightgreen")
```

![plot of chunk summary](figure/summary-1.png) 

```r
mean(clean$sum)
```

[1] 11185.12

```r
median(clean$sum)
```

[1] 11015

### task 2
We then make a time series plot of the 5-minute intervaland the average number of steps taken, averaged across all days. 


```r
tdata <- ddply(activity, c("interval"), summarise,
               N    = sum(!is.na(steps)),
               sum  = sum(steps, na.rm=TRUE),
               mean = mean(steps, na.rm=TRUE),
               sd   = sd(steps, na.rm=TRUE),
               se   = sd / sqrt(N),
               median = median(steps, na.rm=TRUE))
plot(tdata$interval, tdata$mean, type = "l", main =
             "Daily Activity Pattern in 5-minute", xlab="time in a day",
     ylab="average steps",cex = 0.6, lty="dashed")
```

![plot of chunk daily_pattern](figure/daily_pattern-1.png) 

We then need to find the 5-minute interval which contains the maximum number of steps. 

```r
library(xtable)
xt <- xtable(tdata[which.max(tdata$mean),])
print(xt, type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sat Dec 06 03:52:38 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> interval </TH> <TH> N </TH> <TH> sum </TH> <TH> mean </TH> <TH> sd </TH> <TH> se </TH> <TH> median </TH>  </TR>
  <TR> <TD align="right"> 104 </TD> <TD align="right"> 835 </TD> <TD align="right">  53 </TD> <TD align="right"> 10927 </TD> <TD align="right"> 206.17 </TD> <TD align="right"> 293.00 </TD> <TD align="right"> 40.25 </TD> <TD align="right">  19 </TD> </TR>
   </TABLE>

This shows the interval between 8:35 and 8:40 on average has the maximum walking steps, probably due to daily on the way to school or to work. 

### task 3
After clearly examining the dateset we can see only the steps column has NA. Therefore we count the NA on that column.

```r
sum(is.na(activity$steps))
```

[1] 2304

Since there are some days that has no data at all on that single data, we can imput missing values based on the daily bases. We choose to imput according to the mean for that 5-minute interval. We apply a rather dull method here by introducing a self-defined function to change the interval to row_number. Then we imput the mean value. To compare, we colon the dataset of activity to dataset activity_colon. 

```r
myf <-function(x) {
        #to change the interval to rownumber in tdata
        a <- x%%100
        b <- (x-a)/100
        return (12*b+a/5+1)
}
activity_colon <- activity
for (i in 1:17568){
        if (is.na(activity_colon$steps[i])){
                activity_colon$steps[i] <- tdata$mean[myf(activity_colon$interval[i])] 
        } 
}
sum(is.na(activity_colon$steps))
```

[1] 0

We can see that there is no missing value in this dataset activity_colon now. We plot the histgram and compare the mean and median with those in task 1. 

```r
cdata_colon <- ddply(activity_colon, c("date"), summarise,
               N    = sum(!is.na(steps)),
               sum  = sum(steps),
               mean = mean(steps),
               sd   = sd(steps),
               se   = sd / sqrt(N),
               median = median(steps))
hist(cdata_colon$sum, breaks = 10, xlab="total steps per day", main="Distribution of Total Steps", col="lightgreen")
```

![plot of chunk comparison](figure/comparison-1.png) 

```r
mean(cdata_colon$sum)
```

[1] 10766.19

```r
median(cdata_colon$sum)
```

[1] 10766.19

The mean and median both falls a bit because when I were in task 1, I discard both the missing values as well as those below 1000 as outliers. This time we adjust to the mean therefore we see more frequency in the middle spike. However, I still take account of those small values (below 1000), which lowers the mean as well as the median. 

### task 4
Finally, we create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
for (i in 1:17568){
        if (weekdays(as.POSIXct(activity_colon$date[i]))=="Saturday" || 
                    weekdays(as.POSIXct(activity_colon$date[i]))=="Sunday"){
                activity_colon$week[i] <- "weekend"
        }
        else {
                activity_colon$week[i] <- "weekday"
        }
}
```
We compare the weekday activity pattern and weekend activity pattern by plotting a panel plot.

```r
wdata <- ddply(activity_colon, c("interval", "week"), summarise,
               N    = sum(!is.na(steps)),
               sum  = sum(steps),
               mean = mean(steps),
               sd   = sd(steps, na.rm=TRUE),
               se   = sd / sqrt(N),
               median = median(steps, na.rm=TRUE))

par(mfrow=c(2,1))
plot(wdata$interval[wdata$week=="weekday"], wdata$mean[wdata$week=="weekday"],
     type = "l", main ="Daily Activity Pattern in 5-minute weekday", xlab="time in a day",
     ylab="average steps",cex = 0.6, lty="dashed")
plot(wdata$interval[wdata$week=="weekend"], wdata$mean[wdata$week=="weekend"], 
     type = "l", main ="Daily Activity Pattern in 5-minute weekend", xlab="time in a day",
     ylab="average steps",cex = 0.6, lty="dashed")
```

![plot of chunk week_compare](figure/week_compare-1.png) 

We can see from the plot that there is not big difference. Activity in the morning generally is less in the morning, while keeping on high and lows in the afternoon and evening. 
