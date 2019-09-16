---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Use read.csv() to load and read the data iinto a dataframe in R.

```r
setwd("C:\\Users\\nayak\\Desktop\\Johns_Hopkins_DS\\Reproducible Research\\Week2")
dSet = read.csv("activity.csv", header = TRUE)
```
# Question 1
## What is mean total number of steps taken per day?
What is mean total number of steps taken per day?  
You can ignore the missing values in dataset for this part.

```r
dSet$date = as.Date(dSet$date)
original = dSet              # Save a copy for later use
datewise_Steps = tapply(dSet$steps, dSet$date, sum)
hist(datewise_Steps, breaks = 50, xlab = "Steps", main = "Histogram of total steps per day (nbins = 50)")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meansteps = mean(datewise_Steps, na.rm = TRUE)
mediansteps = median(datewise_Steps, na.rm = TRUE)
```
**The mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 10765 respectively.**

# Question 2
## What is the average daily activity pattern?

```r
ts = tapply(dSet$steps, dSet$interval, mean, na.rm = TRUE)
plot(names(ts), ts, type = "l", xlab = "Time Interval", 
     ylab = "Average Steps", main = "Average steps vs Time of the day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
nstepmax = names(ts)[ts == max(ts)]
```
**We can see that the individual has a lot of locomotion in the early morning (5 am - 7/8 am). For the most part throughout the day after 10 am he walks substantially more compared to night time i.e. after 8 pm and before 5 am.**  
**The interval 835 has maximum number of steps on an average across all the days.**

# Question 3
## Impute Missing Values
Do the mean and median values of steps differ after imputation? What is the impact of imputing missing data on the estimates of the total daily number of steps?  
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

```r
totalNA = sum(is.na(dSet))
colSums(is.na(dSet))
```

```
##    steps     date interval 
##     2304        0        0
```
Looking at the data above, its evident that there are no Missing values in date and interval columns. The only column which has NA is steps. Let's impute the NAs in steps column with mean value of steps and check once again to confirm the same.  

```r
dSet$steps[is.na(dSet$steps)] = mean(dSet$steps, na.rm = TRUE)
colSums(is.na(dSet))
```

```
##    steps     date interval 
##        0        0        0
```
Now we can see that there's no NA value in our columns.  

```r
dSet$date = as.Date(dSet$date)
old_steps = tapply(original$steps, original$date, sum)
datewise_Steps = tapply(dSet$steps, dSet$date, sum)
par(mfrow = c(1,2), mar = c(4,4,1,2), cex = 0.9)
hist(old_steps, breaks = 50, xlab = "Steps (50 bins)", main = "Total steps per day pre imputing")
hist(datewise_Steps, breaks = 50, xlab = "Steps (50 bins)", main = "Total steps per day post imputing")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
newmeansteps = mean(datewise_Steps)
newmediansteps = median(datewise_Steps)
```
**Before imputing, mean = 1.0766189\times 10^{4}, median = 10765 and after imputing, mean = 1.0766189\times 10^{4} and median = 1.0766189\times 10^{4}. This means that after central imputation mean has remained unchanged (since we imputed with the mean) but new median has risen compared to the median before imputation.**

# Question 4
## Are there differences in activity patterns between weekdays and weekends?  
dSet has it's NAs in the steps feature imputed with the mean value of the same. We'll be using this dataset for answering this question.   

```r
# Add a category determining whether a day is weekend or a weekday
dSet$weekend = as.factor(as.numeric((weekdays(dSet$date) == "Saturday") | (weekdays(dSet$date) == "Sunday")))

# Subset weekend and weekday data separately
dSetwknd = dSet[dSet$weekend == "1",]
dSetwkday = dSet[dSet$weekend == "0",]

# Apply the aggregation of average based on time of the day
# On both the weekend and weekday dataframes
dSetwkndTS = tapply(dSetwknd$steps, dSetwknd$interval, mean)
dSetwkdayTS = tapply(dSetwkday$steps, dSetwkday$interval, mean)

# Plot the two time series separately in different panels
par(mfrow = c(2,1), mar = c(5,4,1,2), cex = 0.9)
plot(names(dSetwkndTS), dSetwkndTS, type = "l", xlab = "Time of the day", 
     ylab = "Average Steps", main = "Average steps vs Time of the day (Weekend)")
plot(names(dSetwkdayTS), dSetwkdayTS, type = "l", xlab = "Time of the day", 
     ylab = "Average Steps", main = "Average steps vs Time of the day (Weekday)")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

It can be seen that there are substantial differences between the two plots. It gives us two key insights which are

* During the **weekday**, the average steps are **high** in the morning approximately typically between **5:00 - 7:00 am**.  
* During the **weekend**, the average steps are **high** from a **couple of hours before the noon till the evening**.  
This means that our individual tends to walk/jog more in the early morning on weekdays and during the weekend, he walks/jogs more throughout the latter part of the day.  
This guy could be a working individual whose job involves little travel and is more of a sedentary style of work. That's why his step count during the day on a weekday is less. During the weekend, he might socialize, go out and travel owing to which his stepcount is high. All this is mere speculation based on the insights derived from the data.


