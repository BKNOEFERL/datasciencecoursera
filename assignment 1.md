

Loading and preprocessing the data:
--------------------------------------

```{r}
dat <- read.csv("C:\\Users\\Benny\\Desktop\\activity.csv", header=TRUE, sep=",")
head(dat)
```



What is mean total number of steps taken per day?
--------------------------------------------------

* Calculate the total number of steps taken per day

```{r}
steps_by_day <- aggregate(steps ~ date, dat, sum)
```



* Make a histogram of the total number of steps taken each day

```{r}
hist(steps_by_day$steps, main = paste("total steps each day"), col="green", xlab="# of Steps", breaks=10)
```



* calculate and report the mean and median of the total number of steps taken per day

```{r}
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```


mean of the total number of steps taken per day: `r rmean`

median of the total number of steps taken per day: `r rmedian` 



What is the average daily activity pattern?
---------------------------------------------

* time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```{r}
steps_by_interval <- aggregate(steps ~ interval, dat, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="# of steps",main="avg # of steps per day by interval", col="blue")
```



* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```

`r max_interval` is the interval we were looking for.



Imputing missing values
------------------------

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
incomplete <- sum(!complete.cases(dat))
```

`r incomplete` is the total number of missing values in the dataset



* Strategy for filling in all of the missing values in the dataset (simple one: average replacement)
* Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r}
imputed_data <- transform(dat, steps = ifelse(is.na(dat$steps), steps_by_interval$steps[match(dat$interval, steps_by_interval$interval)], dat$steps))
head(imputed_data)
```



* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```{r}
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("total steps each day"), col="green", xlab="# of steps", breaks=15)
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```

`rmean.i` is the mean regarding the imputed data
`rmedian.i`is the median regarding the imputed data


* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
```

Below you see the difference between imputed and non-imputed data:
difference of the means: `mean_diff`
difference of the medians: `med_diff` 


Looking at the total difference:

```{r}
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)
total_diff
```

The difference between total number of steps (imputed vs non-imputed data) is `r total_diff`, which means that we have `r total_diff` additional steps in the new (imputed) dataset.



Are there differences in activity patterns between weekdays and weekends?
----------------------------------------------------------------------------

* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
weekdays <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag") # language setting is German (sorry for possible confusions)

imputed_data$new_factor = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
table(imputed_data$dow)
imputed_data$new_factor = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + new_factor, imputed_data, mean)
```



* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$new_factor, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
















