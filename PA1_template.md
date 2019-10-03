---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## ***Loading and preprocessing the data***

This is an R Markdown document for the Coursera module Reproducible Research, self-graded assignment for week 2, which specifies the source data for analysis, repdata_data_activity.zip.  

Using the 'readr' package to read the source data into a large data frame of 17568 obs. of 3 variables.


```r
library(readr)
repdata_data_activity <-read_csv("repdata_data_activity.zip")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
head(repdata_data_activity,5)
```

```
## # A tibble: 5 x 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```
## ***What is mean total number of steps taken per day?***

### Total number of steps taken per day


```r
dailysums<-aggregate(x=repdata_data_activity$steps,by=list(Day=repdata_data_activity$date), simplify=TRUE, FUN=sum)
Steps <- dailysums$x
hist(Steps, breaks=15)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### Calculate and report mean and median number of total steps taken per day
1st value is mean, 2nd value is median.  Median is zero across due to the many intervals that have zero steps.


```r
mean_median<-tapply(repdata_data_activity$steps,repdata_data_activity$date,function(steps){c(mean(steps),median(steps))})
mean_median_df<-data.frame(matrix(unlist(mean_median),nrow=length(mean_median),byrow=T))
row.names(mean_median_df) <- names(mean_median)
colnames(mean_median_df) <-c("mean","median")
mean_median_df
```

```
##                  mean median
## 2012-10-01         NA     NA
## 2012-10-02  0.4375000      0
## 2012-10-03 39.4166667      0
## 2012-10-04 42.0694444      0
## 2012-10-05 46.1597222      0
## 2012-10-06 53.5416667      0
## 2012-10-07 38.2465278      0
## 2012-10-08         NA     NA
## 2012-10-09 44.4826389      0
## 2012-10-10 34.3750000      0
## 2012-10-11 35.7777778      0
## 2012-10-12 60.3541667      0
## 2012-10-13 43.1458333      0
## 2012-10-14 52.4236111      0
## 2012-10-15 35.2048611      0
## 2012-10-16 52.3750000      0
## 2012-10-17 46.7083333      0
## 2012-10-18 34.9166667      0
## 2012-10-19 41.0729167      0
## 2012-10-20 36.0937500      0
## 2012-10-21 30.6284722      0
## 2012-10-22 46.7361111      0
## 2012-10-23 30.9652778      0
## 2012-10-24 29.0104167      0
## 2012-10-25  8.6527778      0
## 2012-10-26 23.5347222      0
## 2012-10-27 35.1354167      0
## 2012-10-28 39.7847222      0
## 2012-10-29 17.4236111      0
## 2012-10-30 34.0937500      0
## 2012-10-31 53.5208333      0
## 2012-11-01         NA     NA
## 2012-11-02 36.8055556      0
## 2012-11-03 36.7048611      0
## 2012-11-04         NA     NA
## 2012-11-05 36.2465278      0
## 2012-11-06 28.9375000      0
## 2012-11-07 44.7326389      0
## 2012-11-08 11.1770833      0
## 2012-11-09         NA     NA
## 2012-11-10         NA     NA
## 2012-11-11 43.7777778      0
## 2012-11-12 37.3784722      0
## 2012-11-13 25.4722222      0
## 2012-11-14         NA     NA
## 2012-11-15  0.1423611      0
## 2012-11-16 18.8923611      0
## 2012-11-17 49.7881944      0
## 2012-11-18 52.4652778      0
## 2012-11-19 30.6979167      0
## 2012-11-20 15.5277778      0
## 2012-11-21 44.3993056      0
## 2012-11-22 70.9270833      0
## 2012-11-23 73.5902778      0
## 2012-11-24 50.2708333      0
## 2012-11-25 41.0902778      0
## 2012-11-26 38.7569444      0
## 2012-11-27 47.3819444      0
## 2012-11-28 35.3576389      0
## 2012-11-29 24.4687500      0
## 2012-11-30         NA     NA
```

## ***What is the average daily activity pattern?***

### Time series of daily average steps

```r
library(ggplot2)
mean_median_df_nona <- mean_median_df[!is.na(mean_median_df$mean),]
ggplot(data=mean_median_df_nona, aes(x=row.names(mean_median_df_nona),y=mean,group=1)) + geom_line(color="gray",size=1) + geom_point() + scale_x_discrete(breaks=c('2012-10-02','2012-10-29','2012-11-29'), labels=c('2012-10-02'='10/2','2012-10-29'='10/29','2012-11-29'='11/29')) + labs(title="Number of Steps, Daily Average October - November 2012", x="dates", y="average") + theme(plot.title = element_text(hjust=.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Time series of the interval average steps 

```r
mean_interval<-tapply(repdata_data_activity$steps,repdata_data_activity$interval,function(steps){mean(steps,na.rm=TRUE)})
mean_interval_df<-data.frame(matrix(unlist(mean_interval),nrow=length(mean_interval),byrow=T))
row.names(mean_interval_df) <- names(mean_interval)
colnames(mean_interval_df) <-"mean"
```
Here's the few top rows:

```r
head(mean_interval_df)
```

```
##         mean
## 0  1.7169811
## 5  0.3396226
## 10 0.1320755
## 15 0.1509434
## 20 0.0754717
## 25 2.0943396
```
Now the time series chart of the average number of steps in each interval across the time period Oct-Nov 2012

```r
ggplot(data=mean_interval_df,aes(x=c(1:288),y=mean,group=1))+ geom_line(color="gray",size=1) + geom_point() + scale_x_discrete(breaks=NULL)+labs(title="Number of Steps, Interval Average October - November 2012",x="interval", y="average") + theme(plot.title=element_text(hjust=.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### The 5-minute interval that contains the maximum number of steps, on average across all the days in the dataset is:

```r
interval_names<-row.names(mean_interval_df)
interval_names[which(mean_interval_df$mean==max(mean_interval_df$mean))]
```

```
## [1] "835"
```
## ***Imputing missing values***

The total number of missing values in the dataset with NA for steps are: 2304

```r
length(which(is.na(repdata_data_activity$steps)))
```

```
## [1] 2304
```
Fill in missing step values with the average step value for that interval over the Oct to Nov 2012 time period


```r
repdata_data_activity_filledNAs<-repdata_data_activity
naindexes<-which(is.na(repdata_data_activity$steps)) %% 288
naindexes[which(naindexes==0)]<- 288
repdata_data_activity_filledNAs[is.na(repdata_data_activity_filledNAs$steps),1] <- mean_interval_df[naindexes,1]
```

With the missing data filled in, here's the histogram again:

```r
dailysums<-aggregate(x=repdata_data_activity_filledNAs$steps,by=list(Day=repdata_data_activity_filledNAs$date), simplify=TRUE, FUN=sum)
Steps <- dailysums$x
hist(Steps, breaks=15, main="Histogram of Steps with Missing Data Filled")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The frequency distribution is more definitive at the center. Also, here is the new calculation of the mean and median of total number of steps taken per day.


```r
mean_median_filledNAs<-tapply(repdata_data_activity_filledNAs$steps,repdata_data_activity_filledNAs$date,function(steps){c(mean(steps),median(steps))})
mean_median_filledNAs_df<-data.frame(matrix(unlist(mean_median_filledNAs),nrow=length(mean_median_filledNAs),byrow=T))
row.names(mean_median_filledNAs_df) <- names(mean_median_filledNAs)
colnames(mean_median_filledNAs_df) <-c("mean","median")
mean_median_filledNAs_df
```

```
##                  mean   median
## 2012-10-01 37.3825996 34.11321
## 2012-10-02  0.4375000  0.00000
## 2012-10-03 39.4166667  0.00000
## 2012-10-04 42.0694444  0.00000
## 2012-10-05 46.1597222  0.00000
## 2012-10-06 53.5416667  0.00000
## 2012-10-07 38.2465278  0.00000
## 2012-10-08 37.3825996 34.11321
## 2012-10-09 44.4826389  0.00000
## 2012-10-10 34.3750000  0.00000
## 2012-10-11 35.7777778  0.00000
## 2012-10-12 60.3541667  0.00000
## 2012-10-13 43.1458333  0.00000
## 2012-10-14 52.4236111  0.00000
## 2012-10-15 35.2048611  0.00000
## 2012-10-16 52.3750000  0.00000
## 2012-10-17 46.7083333  0.00000
## 2012-10-18 34.9166667  0.00000
## 2012-10-19 41.0729167  0.00000
## 2012-10-20 36.0937500  0.00000
## 2012-10-21 30.6284722  0.00000
## 2012-10-22 46.7361111  0.00000
## 2012-10-23 30.9652778  0.00000
## 2012-10-24 29.0104167  0.00000
## 2012-10-25  8.6527778  0.00000
## 2012-10-26 23.5347222  0.00000
## 2012-10-27 35.1354167  0.00000
## 2012-10-28 39.7847222  0.00000
## 2012-10-29 17.4236111  0.00000
## 2012-10-30 34.0937500  0.00000
## 2012-10-31 53.5208333  0.00000
## 2012-11-01 37.3825996 34.11321
## 2012-11-02 36.8055556  0.00000
## 2012-11-03 36.7048611  0.00000
## 2012-11-04 37.3825996 34.11321
## 2012-11-05 36.2465278  0.00000
## 2012-11-06 28.9375000  0.00000
## 2012-11-07 44.7326389  0.00000
## 2012-11-08 11.1770833  0.00000
## 2012-11-09 37.3825996 34.11321
## 2012-11-10 37.3825996 34.11321
## 2012-11-11 43.7777778  0.00000
## 2012-11-12 37.3784722  0.00000
## 2012-11-13 25.4722222  0.00000
## 2012-11-14 37.3825996 34.11321
## 2012-11-15  0.1423611  0.00000
## 2012-11-16 18.8923611  0.00000
## 2012-11-17 49.7881944  0.00000
## 2012-11-18 52.4652778  0.00000
## 2012-11-19 30.6979167  0.00000
## 2012-11-20 15.5277778  0.00000
## 2012-11-21 44.3993056  0.00000
## 2012-11-22 70.9270833  0.00000
## 2012-11-23 73.5902778  0.00000
## 2012-11-24 50.2708333  0.00000
## 2012-11-25 41.0902778  0.00000
## 2012-11-26 38.7569444  0.00000
## 2012-11-27 47.3819444  0.00000
## 2012-11-28 35.3576389  0.00000
## 2012-11-29 24.4687500  0.00000
## 2012-11-30 37.3825996 34.11321
```
The mean and median differences are really just on the NA days. The impact to using the filled dataset is that we can reasonably expect a reduction of average error on estimation since we have the model more biased towards the center now than before. Here are the differences between the mean and median values between original and filled datasets:

```r
diffs<-mean_median_df - mean_median_filledNAs_df
diffs
```

```
##            mean median
## 2012-10-01   NA     NA
## 2012-10-02    0      0
## 2012-10-03    0      0
## 2012-10-04    0      0
## 2012-10-05    0      0
## 2012-10-06    0      0
## 2012-10-07    0      0
## 2012-10-08   NA     NA
## 2012-10-09    0      0
## 2012-10-10    0      0
## 2012-10-11    0      0
## 2012-10-12    0      0
## 2012-10-13    0      0
## 2012-10-14    0      0
## 2012-10-15    0      0
## 2012-10-16    0      0
## 2012-10-17    0      0
## 2012-10-18    0      0
## 2012-10-19    0      0
## 2012-10-20    0      0
## 2012-10-21    0      0
## 2012-10-22    0      0
## 2012-10-23    0      0
## 2012-10-24    0      0
## 2012-10-25    0      0
## 2012-10-26    0      0
## 2012-10-27    0      0
## 2012-10-28    0      0
## 2012-10-29    0      0
## 2012-10-30    0      0
## 2012-10-31    0      0
## 2012-11-01   NA     NA
## 2012-11-02    0      0
## 2012-11-03    0      0
## 2012-11-04   NA     NA
## 2012-11-05    0      0
## 2012-11-06    0      0
## 2012-11-07    0      0
## 2012-11-08    0      0
## 2012-11-09   NA     NA
## 2012-11-10   NA     NA
## 2012-11-11    0      0
## 2012-11-12    0      0
## 2012-11-13    0      0
## 2012-11-14   NA     NA
## 2012-11-15    0      0
## 2012-11-16    0      0
## 2012-11-17    0      0
## 2012-11-18    0      0
## 2012-11-19    0      0
## 2012-11-20    0      0
## 2012-11-21    0      0
## 2012-11-22    0      0
## 2012-11-23    0      0
## 2012-11-24    0      0
## 2012-11-25    0      0
## 2012-11-26    0      0
## 2012-11-27    0      0
## 2012-11-28    0      0
## 2012-11-29    0      0
## 2012-11-30   NA     NA
```
## ***Are there differences in activity patterns between weekdays and weekends?***

```r
repdata_data_activity$daytype <- 
  ifelse(as.numeric(strftime(as.Date(repdata_data_activity$date,"%Y-%m-%d"),"%u")) <=5,"Weekday", "Weekend")

repdata_data_activity_filledNAs$daytype <- ifelse(as.numeric(strftime(as.Date(repdata_data_activity$date,"%Y-%m-%d"),"%u")) <=5,"Weekday", "Weekend")

repdata_data_activity_filledNAs_wdays<-repdata_data_activity_filledNAs[repdata_data_activity_filledNAs$daytype=="Weekday",]

repdata_data_activity_filledNAs_wend<-repdata_data_activity_filledNAs[repdata_data_activity_filledNAs$daytype=="Weekend",]

mean_interval_wk<-tapply(repdata_data_activity_filledNAs_wdays$steps,repdata_data_activity_filledNAs_wdays$interval,function(steps){mean(steps,na.rm=TRUE)})
mean_interval_wk_df<-data.frame(matrix(unlist(mean_interval_wk),nrow=length(mean_interval_wk),byrow=T))
row.names(mean_interval_wk_df) <- names(mean_interval_wk)
colnames(mean_interval_wk_df) <-"mean"

mean_interval_wd<-tapply(repdata_data_activity_filledNAs_wend$steps,repdata_data_activity_filledNAs_wend$interval,function(steps){mean(steps,na.rm=TRUE)})
mean_interval_wd_df<-data.frame(matrix(unlist(mean_interval_wd),nrow=length(mean_interval_wd),byrow=T))
row.names(mean_interval_wd_df) <- names(mean_interval_wd)
colnames(mean_interval_wd_df) <-"mean"

p1<-ggplot(data=mean_interval_wk_df,aes(x=c(1:288),y=mean,group=1))+ geom_line(color="gray",size=1) + geom_point() + scale_x_discrete(breaks=NULL)+labs(title="Weekdays: Number of Steps, Interval Average October - November 2012",x="interval", y="average") + theme(plot.title=element_text(hjust=.5))

p2<-ggplot(data=mean_interval_wd_df,aes(x=c(1:288),y=mean,group=1))+ geom_line(color="gray",size=1) + geom_point() + scale_x_discrete(breaks=NULL)+labs(title="Weekend: Number of Steps, Interval Average October - November 2012",x="interval", y="average") + theme(plot.title=element_text(hjust=.5))

library(gridExtra)
grid.arrange(p1,p2,ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
