# Reproducible Research: Peer Assessment 1
Hankang  
December 11, 2014  

```r
# set global chunk options: 
library(knitr)
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
opts_chunk$set(fig.align='center', echo = TRUE)
```

## 1. Loading and preprocessing the data


```r
unzip("activity.zip")
df <-  read.csv("activity.csv", sep = ",")
```

## 2. What is mean total number of steps taken per day?

* 1 Make a histogram of the total number of steps taken each day

```r
df.steps <- ddply(df, .(date), summarize, total = sum(steps, na.rm = T))
```


```r
df.steps.hist <- as.POSIXlt(with(df.steps,rep(date,total)))
hist(df.steps.hist, "weeks", breaks = 61, freq = T, xlab = "Date",main = "Histogram of total steps in Oct and Nov", xaxt = "n")
axis.POSIXct(1, at=seq(as.POSIXlt("2012-10-01"), as.POSIXlt("2012-11-30"), by="1 weeks"), format="%Y-%m-%d",las=2, cex.axis = 0.5)
```

<img src="PA1_template_files/figure-html/histogram of the total number of steps taken each day-1.png" title="" alt="" style="display: block; margin: auto;" />

* 2 Calculate and report the mean and median total number of steps taken per day


```r
df.summary <- ddply(df, .(date), summarize, mean = mean(steps, na.rm = T), median = median(steps, na.rm = T))
print(df.summary)
```

```
##          date       mean median
## 1  2012-10-01        NaN     NA
## 2  2012-10-02  0.4375000      0
## 3  2012-10-03 39.4166667      0
## 4  2012-10-04 42.0694444      0
## 5  2012-10-05 46.1597222      0
## 6  2012-10-06 53.5416667      0
## 7  2012-10-07 38.2465278      0
## 8  2012-10-08        NaN     NA
## 9  2012-10-09 44.4826389      0
## 10 2012-10-10 34.3750000      0
## 11 2012-10-11 35.7777778      0
## 12 2012-10-12 60.3541667      0
## 13 2012-10-13 43.1458333      0
## 14 2012-10-14 52.4236111      0
## 15 2012-10-15 35.2048611      0
## 16 2012-10-16 52.3750000      0
## 17 2012-10-17 46.7083333      0
## 18 2012-10-18 34.9166667      0
## 19 2012-10-19 41.0729167      0
## 20 2012-10-20 36.0937500      0
## 21 2012-10-21 30.6284722      0
## 22 2012-10-22 46.7361111      0
## 23 2012-10-23 30.9652778      0
## 24 2012-10-24 29.0104167      0
## 25 2012-10-25  8.6527778      0
## 26 2012-10-26 23.5347222      0
## 27 2012-10-27 35.1354167      0
## 28 2012-10-28 39.7847222      0
## 29 2012-10-29 17.4236111      0
## 30 2012-10-30 34.0937500      0
## 31 2012-10-31 53.5208333      0
## 32 2012-11-01        NaN     NA
## 33 2012-11-02 36.8055556      0
## 34 2012-11-03 36.7048611      0
## 35 2012-11-04        NaN     NA
## 36 2012-11-05 36.2465278      0
## 37 2012-11-06 28.9375000      0
## 38 2012-11-07 44.7326389      0
## 39 2012-11-08 11.1770833      0
## 40 2012-11-09        NaN     NA
## 41 2012-11-10        NaN     NA
## 42 2012-11-11 43.7777778      0
## 43 2012-11-12 37.3784722      0
## 44 2012-11-13 25.4722222      0
## 45 2012-11-14        NaN     NA
## 46 2012-11-15  0.1423611      0
## 47 2012-11-16 18.8923611      0
## 48 2012-11-17 49.7881944      0
## 49 2012-11-18 52.4652778      0
## 50 2012-11-19 30.6979167      0
## 51 2012-11-20 15.5277778      0
## 52 2012-11-21 44.3993056      0
## 53 2012-11-22 70.9270833      0
## 54 2012-11-23 73.5902778      0
## 55 2012-11-24 50.2708333      0
## 56 2012-11-25 41.0902778      0
## 57 2012-11-26 38.7569444      0
## 58 2012-11-27 47.3819444      0
## 59 2012-11-28 35.3576389      0
## 60 2012-11-29 24.4687500      0
## 61 2012-11-30        NaN     NA
```
wired summary... possibly too manmy zeros in the data, try no include "0" or "NA" ?


```r
df.NA = df[df$steps != 0 & !is.na(df$steps),]

df.summary.NA <- ddply(df.NA, .(date), summarize, mean = mean(steps, na.rm = T), median = median(steps, na.rm = T))
print(df.summary.NA)
```

```
##          date      mean median
## 1  2012-10-02  63.00000   63.0
## 2  2012-10-03 140.14815   61.0
## 3  2012-10-04 121.16000   56.5
## 4  2012-10-05 154.58140   66.0
## 5  2012-10-06 145.47170   67.0
## 6  2012-10-07 101.99074   52.5
## 7  2012-10-09 134.85263   48.0
## 8  2012-10-10  95.19231   56.5
## 9  2012-10-11 137.38667   35.0
## 10 2012-10-12 156.59459   46.0
## 11 2012-10-13 119.48077   45.5
## 12 2012-10-14 160.61702   60.5
## 13 2012-10-15 131.67532   54.0
## 14 2012-10-16 157.12500   64.0
## 15 2012-10-17 152.86364   61.5
## 16 2012-10-18 152.36364   52.5
## 17 2012-10-19 127.19355   74.0
## 18 2012-10-20 125.24096   49.0
## 19 2012-10-21  96.93407   48.0
## 20 2012-10-22 154.71264   52.0
## 21 2012-10-23 101.34091   56.0
## 22 2012-10-24 104.43750   51.5
## 23 2012-10-25  56.63636   35.0
## 24 2012-10-26  77.02273   36.5
## 25 2012-10-27 134.92000   72.0
## 26 2012-10-28 110.17308   61.0
## 27 2012-10-29  80.93548   54.5
## 28 2012-10-30 110.32584   40.0
## 29 2012-10-31 179.23256   83.5
## 30 2012-11-02 143.24324   55.5
## 31 2012-11-03 117.45556   59.0
## 32 2012-11-05 141.06757   66.0
## 33 2012-11-06 100.40964   52.0
## 34 2012-11-07 135.61053   58.0
## 35 2012-11-08  61.90385   42.5
## 36 2012-11-11 132.71579   55.0
## 37 2012-11-12 156.01449   42.0
## 38 2012-11-13  90.56790   57.0
## 39 2012-11-15  20.50000   20.5
## 40 2012-11-16  89.19672   43.0
## 41 2012-11-17 183.83333   65.5
## 42 2012-11-18 162.47312   80.0
## 43 2012-11-19 117.88000   34.0
## 44 2012-11-20  95.14894   58.0
## 45 2012-11-21 188.04412   55.0
## 46 2012-11-22 177.62609   65.0
## 47 2012-11-23 252.30952  113.0
## 48 2012-11-24 176.56098   65.5
## 49 2012-11-25 140.88095   84.0
## 50 2012-11-26 128.29885   53.0
## 51 2012-11-27 158.67442   57.0
## 52 2012-11-28 212.14583   70.0
## 53 2012-11-29 110.10938   44.5
```

**NOTE: **_Much better, but we can see we only have 53 dates data!_

## What is the average daily activity pattern?

* 1 Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
df.average <- ddply(df, .(interval), summarize, mean = mean(steps, na.rm = T))

with(df.average, plot(interval, mean, type = "l",xlab = "Interval (mins)", ylab = "Averaged number of steps taken",main = "Averaged daily activity pattern"))
```

<img src="PA1_template_files/figure-html/time series plot-1.png" title="" alt="" style="display: block; margin: auto;" />

* 2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
with(df.average, interval[which(mean == max(mean))])
```

```
## [1] 835
```
## Imputing missing values

* 1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
NAtotal <- sum(is.na(df$steps))
print(NAtotal)
```

```
## [1] 2304
```
* 2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

    **Using mean for that 5-minute interval**

* 3 Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
temp <- merge(df, df.average, by = "interval")
temp$steps[is.na(temp$steps)] <- temp$mean[is.na(temp$steps)]

imputed <- temp %>%
    arrange(date, interval) %>%
    select(steps, date, interval)

imputed.steps.total <- ddply(imputed, .(date), summarize, 
                       total = sum(steps))
```

* 4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputed.steps.hist <- as.POSIXct(rep(imputed.steps.total$date,
                      imputed.steps.total$total))

hist(imputed.steps.hist, "weeks", breaks = 61, freq = T, xlab = "Date",
     main = "Histogram of total steps in Oct and Nov (imputed)", xaxt = "n")
axis.POSIXct(1, at=seq(as.POSIXlt("2012-10-01"), as.POSIXlt("2012-11-30"), 
                       by="1 weeks"), format="%Y-%m-%d",las=2, cex.axis = 0.5)
```

<img src="PA1_template_files/figure-html/histogram of imputed table-1.png" title="" alt="" style="display: block; margin: auto;" />

_Mean and media_


```r
## report mean and media
imputed.summary <- ddply(imputed , .(date), summarize, 
                 mean = mean(steps, na.rm = T), 
                 median = median(steps, na.rm = T))
print(imputed.summary)
```

```
##          date       mean   median
## 1  2012-10-01 37.3825996 34.11321
## 2  2012-10-02  0.4375000  0.00000
## 3  2012-10-03 39.4166667  0.00000
## 4  2012-10-04 42.0694444  0.00000
## 5  2012-10-05 46.1597222  0.00000
## 6  2012-10-06 53.5416667  0.00000
## 7  2012-10-07 38.2465278  0.00000
## 8  2012-10-08 37.3825996 34.11321
## 9  2012-10-09 44.4826389  0.00000
## 10 2012-10-10 34.3750000  0.00000
## 11 2012-10-11 35.7777778  0.00000
## 12 2012-10-12 60.3541667  0.00000
## 13 2012-10-13 43.1458333  0.00000
## 14 2012-10-14 52.4236111  0.00000
## 15 2012-10-15 35.2048611  0.00000
## 16 2012-10-16 52.3750000  0.00000
## 17 2012-10-17 46.7083333  0.00000
## 18 2012-10-18 34.9166667  0.00000
## 19 2012-10-19 41.0729167  0.00000
## 20 2012-10-20 36.0937500  0.00000
## 21 2012-10-21 30.6284722  0.00000
## 22 2012-10-22 46.7361111  0.00000
## 23 2012-10-23 30.9652778  0.00000
## 24 2012-10-24 29.0104167  0.00000
## 25 2012-10-25  8.6527778  0.00000
## 26 2012-10-26 23.5347222  0.00000
## 27 2012-10-27 35.1354167  0.00000
## 28 2012-10-28 39.7847222  0.00000
## 29 2012-10-29 17.4236111  0.00000
## 30 2012-10-30 34.0937500  0.00000
## 31 2012-10-31 53.5208333  0.00000
## 32 2012-11-01 37.3825996 34.11321
## 33 2012-11-02 36.8055556  0.00000
## 34 2012-11-03 36.7048611  0.00000
## 35 2012-11-04 37.3825996 34.11321
## 36 2012-11-05 36.2465278  0.00000
## 37 2012-11-06 28.9375000  0.00000
## 38 2012-11-07 44.7326389  0.00000
## 39 2012-11-08 11.1770833  0.00000
## 40 2012-11-09 37.3825996 34.11321
## 41 2012-11-10 37.3825996 34.11321
## 42 2012-11-11 43.7777778  0.00000
## 43 2012-11-12 37.3784722  0.00000
## 44 2012-11-13 25.4722222  0.00000
## 45 2012-11-14 37.3825996 34.11321
## 46 2012-11-15  0.1423611  0.00000
## 47 2012-11-16 18.8923611  0.00000
## 48 2012-11-17 49.7881944  0.00000
## 49 2012-11-18 52.4652778  0.00000
## 50 2012-11-19 30.6979167  0.00000
## 51 2012-11-20 15.5277778  0.00000
## 52 2012-11-21 44.3993056  0.00000
## 53 2012-11-22 70.9270833  0.00000
## 54 2012-11-23 73.5902778  0.00000
## 55 2012-11-24 50.2708333  0.00000
## 56 2012-11-25 41.0902778  0.00000
## 57 2012-11-26 38.7569444  0.00000
## 58 2012-11-27 47.3819444  0.00000
## 59 2012-11-28 35.3576389  0.00000
## 60 2012-11-29 24.4687500  0.00000
## 61 2012-11-30 37.3825996 34.11321
```

```r
# remove zero
imputed1 = imputed[imputed$steps != 0,]

imputed.summary1 <- ddply(imputed1, .(date), summarize, 
                  mean = mean(steps, na.rm = T), 
                  median = median(steps, na.rm = T))
print(imputed.summary1)
```

```
##          date      mean    median
## 1  2012-10-01  40.02301  37.45283
## 2  2012-10-02  63.00000  63.00000
## 3  2012-10-03 140.14815  61.00000
## 4  2012-10-04 121.16000  56.50000
## 5  2012-10-05 154.58140  66.00000
## 6  2012-10-06 145.47170  67.00000
## 7  2012-10-07 101.99074  52.50000
## 8  2012-10-08  40.02301  37.45283
## 9  2012-10-09 134.85263  48.00000
## 10 2012-10-10  95.19231  56.50000
## 11 2012-10-11 137.38667  35.00000
## 12 2012-10-12 156.59459  46.00000
## 13 2012-10-13 119.48077  45.50000
## 14 2012-10-14 160.61702  60.50000
## 15 2012-10-15 131.67532  54.00000
## 16 2012-10-16 157.12500  64.00000
## 17 2012-10-17 152.86364  61.50000
## 18 2012-10-18 152.36364  52.50000
## 19 2012-10-19 127.19355  74.00000
## 20 2012-10-20 125.24096  49.00000
## 21 2012-10-21  96.93407  48.00000
## 22 2012-10-22 154.71264  52.00000
## 23 2012-10-23 101.34091  56.00000
## 24 2012-10-24 104.43750  51.50000
## 25 2012-10-25  56.63636  35.00000
## 26 2012-10-26  77.02273  36.50000
## 27 2012-10-27 134.92000  72.00000
## 28 2012-10-28 110.17308  61.00000
## 29 2012-10-29  80.93548  54.50000
## 30 2012-10-30 110.32584  40.00000
## 31 2012-10-31 179.23256  83.50000
## 32 2012-11-01  40.02301  37.45283
## 33 2012-11-02 143.24324  55.50000
## 34 2012-11-03 117.45556  59.00000
## 35 2012-11-04  40.02301  37.45283
## 36 2012-11-05 141.06757  66.00000
## 37 2012-11-06 100.40964  52.00000
## 38 2012-11-07 135.61053  58.00000
## 39 2012-11-08  61.90385  42.50000
## 40 2012-11-09  40.02301  37.45283
## 41 2012-11-10  40.02301  37.45283
## 42 2012-11-11 132.71579  55.00000
## 43 2012-11-12 156.01449  42.00000
## 44 2012-11-13  90.56790  57.00000
## 45 2012-11-14  40.02301  37.45283
## 46 2012-11-15  20.50000  20.50000
## 47 2012-11-16  89.19672  43.00000
## 48 2012-11-17 183.83333  65.50000
## 49 2012-11-18 162.47312  80.00000
## 50 2012-11-19 117.88000  34.00000
## 51 2012-11-20  95.14894  58.00000
## 52 2012-11-21 188.04412  55.00000
## 53 2012-11-22 177.62609  65.00000
## 54 2012-11-23 252.30952 113.00000
## 55 2012-11-24 176.56098  65.50000
## 56 2012-11-25 140.88095  84.00000
## 57 2012-11-26 128.29885  53.00000
## 58 2012-11-27 158.67442  57.00000
## 59 2012-11-28 212.14583  70.00000
## 60 2012-11-29 110.10938  44.50000
## 61 2012-11-30  40.02301  37.45283
```

_visulize the difference_

```r
h1 <- hist(df.steps.hist, breaks = 61, freq = F, plot = F)
h2 <- hist(imputed.steps.hist, breaks = 61, freq = F,xaxt = "n",
           plot = F)
plot(h1, col=rgb(0,0,1,1/8), xaxt = "n",main="Comparison", xlab="Date", ylab="Frequency")
plot(h2, col=rgb(1,0,0,1/8), add=T)

axis.POSIXct(1, at=seq(as.POSIXlt("2012-10-01"), as.POSIXlt("2012-11-30"), 
                      by="1 weeks"), format="%Y-%m-%d",las=2, cex.axis = 0.5)
legend("topleft", c("original data","imputed", "overlap"), 
       fill = c(rgb(0,0,1,1/8), rgb(1,0,0,1/8),rgb(1,0,1,1/8)))
```

<img src="PA1_template_files/figure-html/visulize the difference-1.png" title="" alt="" style="display: block; margin: auto;" />
**Explanation:**
The `NA` was replaced by the mean of 5-minute interval which rised the interval value of those contained `NA` but remained those did not contain `NA` the same. That's why most of the histogram is overlaped.

## Are there differences in activity patterns between weekdays and weekends?

* 1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekdays <- as.POSIXlt(df$date)$wday
df.weekday <- cbind(df, weekdays)

df.weekday$weekdays[which(df.weekday$weekdays %in% c(1:5))] <- "weekday"
df.weekday$weekdays[which(df.weekday$weekdays %in% c(0,6))] <- "weekend"
df.weekday$weekdays <- as.factor(df.weekday$weekdays)
```

* 2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
weekday.summary <- ddply(df.weekday, .(interval,weekdays), summarize, 
                         mean = mean(steps, na.rm = T))
qplot(interval, mean, data = weekday.summary, facets = weekdays ~ ., geom = "path",
      ylab = "Averaged number of steps taken")
```

<img src="PA1_template_files/figure-html/panel plot-1.png" title="" alt="" style="display: block; margin: auto;" />
