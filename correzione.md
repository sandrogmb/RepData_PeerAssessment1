---
title: "asssignment1"
author: "ANIL_REDDY"
date: "8 March 2017"
output: html_document
---



##Question 1
###Code for reading in the dataset and/or processing the data

```{r}
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
DF <- read.csv('activity.csv')
str(DF)
```

##Question 2
Histogram of the total number of steps taken each day

```{r}
# Create the sums of steps per date
DFsteps <- tapply(DF$steps, DF$date, FUN=sum, na.rm=TRUE)

# Perform histogram of steps per day
library(ggplot2)
qplot(DFsteps, binwidth=1000, xlab="total number of steps taken each day")
```

##Question 3
###Mean and median number of steps taken each day

```{r}
# Create mean and median of steps per day
stepsMean <- mean(DFsteps, na.rm=TRUE)
stepsMedian <- median(DFsteps, na.rm=TRUE)
# Output mean and median
stepsMean
```


```{r}
stepsMedian
```

##Question 4
###Time series plot of the average number of steps taken


```{r}
library(ggplot2)

# Create the means by intervals
averages <- aggregate(x=list(steps=DF$steps), by=list(interval=DF$interval),FUN=mean, na.rm=TRUE)

ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  ggtitle("Time Series: average number of steps") +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

```

##Question 5
###The 5-minute interval that, on average, contains the maximum number of steps


```{r}
averages[which.max(averages$steps),]
```

##Question 6
###Code to describe and show a strategy for imputing missing data
###Idea: Replace the NA by the mean of the corresponding interval.


```{r}
# copy of data frame
DF2 <- DF

# add column for copleating index
DF2$CI <- "original"

# number of rows to check
l <- nrow(DF2)

# numbers of NAs
length(which(is.na(DF2$steps)))
```


```{r}
# replace NAs by corresponing mean of the same interval --> complete data frame DF2
for (i in 1:l) {
  if (is.na(DF2[i,1])) {
    DF2[i,1] <- averages[averages$interval == DF2[i,3],2]
    DF2[i,4] <- "completed"
  }
}

# numbers of NAs / completed (control)
length(which(is.na(DF2$steps)))
```


```{r}
length(which(DF2$CI=="completed"))
```


```{r}
# Recreate the sums of steps per date
DFsteps2 <- tapply(DF2$steps, DF2$date, FUN=sum, na.rm=TRUE )

# Recreate the mean and median of steps per date
stepsMean2 <- mean(DFsteps2)
stepsMedian2 <- median(DFsteps2)

c(stepsMean2, stepsMean)
```


```{r}
c(stepsMedian2, stepsMedian)
```

We see, that the completation of the data frame did strongly change mean and median of the steps per date. What did also is the distribution of the sum of steps per date, as we will see in the next section:

##Question 7
###Histogram of the total number of steps taken each day after missing values are imputed


```{r}
# Preparation environment
library(ggplot2)
library(gridExtra)
require(gridExtra)

# Perform histogram of steps per day
plot1 <- qplot(DFsteps, 
               binwidth=1000, 
               ylim=c(0,15),
               main="original", 
               xlab="total number of steps taken each day")

plot2 <- qplot(DFsteps2, 
               binwidth=1000, 
               ylim=c(0,15),
               main="completed", 
               xlab="total number of steps taken each day")

# Plotting 2 plot in grid
grid.arrange(plot1, plot2, ncol=2)
```

##Question 8
###Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```{r}
library(ggplot2)
library(gridExtra)

# Formatting and expanding DF2 by $WD (Weekday in German) an $WDG (WeekDayGroup)
DF2[,2] <- as.Date(DF2[,2])
DF2$WD <- weekdays(DF2[,2])
DF2$WDG <- "week"               # default = "week"

# Filling in the WeekDayGroup in German
for (i in 1:l) {
  if (DF2[i,5] == "Samstag" | DF2[i,5] == "Sonntag") {
    DF2[i,6] <- "weekend"
  }
}

DF2[,6] <- as.factor(DF2[,6])

DF2w <-subset(DF2,DF2[,6]=="week")
DF2we <-subset(DF2,DF2[,6]=="weekend")

# Recreate the means by intervals
averagesW <- aggregate(steps ~ interval, DF2w, FUN=mean)
#averagesWe <- aggregate(steps ~ interval, DF2we, FUN=mean)

# prepare the plots
plot1 <- ggplot(data=averagesW, aes(x=interval, y=steps)) +
         geom_line() +
         ylim(0, 250) +
          ggtitle("Weekdays") +
         xlab("5-minute interval") +
         ylab("average number of steps taken")

plot2 <- ggplot(data=averagesW, aes(x=interval, y=steps)) +
     geom_line() +
        ylim(0, 250) +
       ggtitle("Weekend Days") +
      xlab("5-minute interval") +
     ylab("average number of steps taken")

# use the library "gridExtra"
require(gridExtra)

# plot
grid.arrange(plot1, plot2, ncol=2)
```


##Question 9
###All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

###The underlying R Markdown document contains all of the R code needed to reproduce the report.

