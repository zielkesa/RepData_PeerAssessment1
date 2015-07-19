---
title: Peer Assesment 1
output: html_document
---

loading required packages: plyr, dplyr, stringr

```{r, echo=TRUE}
library(plyr)
library(dplyr)
library(stringr)
```

loading and proccessing data

```{r, echo=TRUE}
activity <- read.csv("activity.csv", header=TRUE, stringsAsFactors=FALSE)
activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")
```

calculating daily totals and making histogram

```{r, echo=TRUE}
total <- tapply(activity$steps, activity$date, FUN = sum, na.rm=TRUE)
hist(total, xlab="steps per day")
```

daily mean and median

```{r, echo=TRUE}
mean(total, na.rm=TRUE)
median(total, na.rm=TRUE)
```

plotting average number of steps per time interval

```{r, echo=TRUE}
intMean <- ddply(activity, "interval", function(x)mean(x$steps, na.rm=TRUE))
intMean <- mutate(intMean, intRaw = interval)
intMean$interval <- as.character(intMean$interval)
intMean$interval <- str_pad(intMean$interval, 4, side = "left", pad="0")
intMean$interval <- as.POSIXct(intMean$interval, format="%H%M")
plot(intMean$interval,intMean$V1, xlab="time interval", ylab="steps")
```

Interval with most steps

```{r, echo=TRUE}
x <- arrange(intMean, -V1)
x[1,]
```

number of missing data points

```{r, echo=TRUE}
length(which(is.na(activity$steps)))
```

Adding missing data. Used the interval average to add the missing data points

```{r, echo=TRUE}
activityfilled <- activity
for (i in 1:nrow(activity)){
  if (is.na(activityfilled[i,1])){
    x <- filter(intMean, intRaw == activityfilled[i,3])
    activityfilled[i,1] <- x[1,2]
  }
}
```

histogram of number of steps taken after NA values filled in

```{r, echo=TRUE}
totalfilled <- tapply(activityfilled$steps, activityfilled$date, FUN = sum, na.rm=TRUE)
hist(totalfilled, xlab="steps per day", main="Steps per day")
```

making plot comparing weekdays to weekends

```{r, echo=TRUE}
#Weekdays and Weekends
activityfilled <- mutate(activityfilled, weekday = weekdays(date, abbreviate=FALSE))
day <- ifelse(activityfilled$weekday %in% c("Saturday","Sunday"), "Weekend","Weekday")
activityfilled <- mutate(activityfilled, weekday = as.factor(day))

#making data to plot
weekend <- filter(activityfilled, weekday == "Weekend")
weekend <- ddply(activity, "interval", function(x)mean(x$steps, na.rm=TRUE))
weekend$interval <- as.character(weekend$interval)
weekend$interval <- str_pad(weekend$interval, 4, side = "left", pad="0")
weekend$interval <- as.POSIXct(weekend$interval, format="%H%M")

weekd <- filter(activityfilled, weekday == "Weekday")
weekd <- ddply(activity, "interval", function(x)mean(x$steps, na.rm=TRUE))
weekd$interval <- as.character(weekd$interval)
weekd$interval <- str_pad(weekd$interval, 4, side = "left", pad="0")
weekd$interval <- as.POSIXct(weekd$interval, format="%H%M")

#plotting
par(mfrow=c(2,1))
plot(weekend$interval,weekend$V1,type="l",ylab="steps",xlab="time interval",main="Weekend")
plot(weekd$interval,weekd$V1,type="l",ylab="steps",xlab="time interval",main="Weekday")
```
