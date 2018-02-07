---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This project uses the data collected by the personal activity monitoring device. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

**date**: The date on which the measurement was taken in YYYY-MM-DD format

**interval**: Identifier for the 5-minute interval in which measurement was taken

The report will explain the methods and outputs of the analysis of the personal activity data.

## Loading and preprocessing the data

The dataset for the personal movement analysis is loaded, and processed into data frame for future analysis. Additionally the new column is created that stores date and time information in date-time format.


```r
suppressWarnings(library(ggplot2))
suppressWarnings(library(knitr))
library(png)

#unzip file
destFile <- "activity.csv"
destArc <- "activity.zip"
destDir <- getwd()

if(!file.exists(destFile)){
    unzip(destArc,exdir = destDir)
}

figdir <- "./Figures/"
if(!dir.exists(figdir)){
    dir.create("./Figures/")
}

# read file
data <- read.csv(destFile)
#add datetime column
data$datetime <- strptime(paste(data$date,sprintf("%.4i",data$interval)),"%Y-%m-%d %H%M")
```

## What is mean total number of steps taken per day?

To calculate the mean total number of steps per day the new dataset is generated where the number of steps is summed for each day. The data is summarized on the histogram plot. There are many days where there total number of steps is zero. In fact, there are no measurements done during these days.


```r
#number of steps per day
byday <- tapply(data$steps,data$date,sum,na.rm=TRUE)
byday <- data.frame(date = names(byday),
                    steps = byday,row.names = NULL)
#histogram
fig1 <- "Figures/fig1.png"
png(file= fig1,width = 512, height = 288)
ggplot(byday,aes(steps))+
    stat_bin(breaks = seq(0,26000,1000))+
    scale_x_continuous(breaks=seq(0,26000,2000))
invisible(dev.off())
include_graphics(fig1)
```

<img src="Figures/fig1.png" width="512" />

_The mean_ number of steps per day is 9354.23 and _the median_ is 10395.00.

## What is the average daily activity pattern?

To calculate the average daily activity pattern the new data frame is created, where data is averaged for each 5-minute interval across all days. The data is summarized on the graph.


```r
# plot total number of steps for 5-minute intervals
byint <- tapply(data$steps,data$interval,mean,na.rm=TRUE)
byint <- data.frame(interval = names(byint),
                    steps = byint,
                    row.names = NULL)

byint$time <- format(strptime(sprintf("%04i",as.numeric(paste(byint$interval))),"%H%M"),"%H:%M")

fig2 <- "Figures/fig2.png"
png(file= fig2,width = 768, height = 288)
ggplot(byint,aes(x=time,y=steps,group=1))+
    geom_line()+
    scale_x_discrete(breaks=byint$time[seq(1,288,12)])
invisible(dev.off())
include_graphics(fig2)
```

<img src="Figures/fig2.png" width="768" />

The maximum number of steps is made during the time interval starting at 08:35.

## Imputing missing values

There are 2304 missing values in the daily activity data in the dataset. In order to avoid the bias introduced by them, these values have to be imputed. As a simple strategy we will use the average number of steps for each interval to substitute the missing values.

The results are summarized on the histogram. The amount of days with nearly zero number of steps is significanty reduced.


```r
# intervals with missing values
missing <- is.na(data$steps)

# create vector of mean values for every 5-min interval with missing values
datamean <- byint$steps[match(data$interval[missing],byint$interval)]
datafixed <- data
# substitute the missing values
datafixed$steps[missing] <- datamean

#number of steps per day
bydayfixed <- tapply(datafixed$steps,datafixed$date,sum,na.rm=TRUE)
bydayfixed <- data.frame(date = names(bydayfixed),
                         steps = bydayfixed,
                         row.names = NULL)
# newhistogram
fig3 <- "Figures/fig3.png"
png(file= fig3,width = 512, height = 288)
ggplot(bydayfixed,aes(steps))+
    stat_bin(breaks = seq(0,26000,1000))+
    scale_x_continuous(breaks=seq(0,26000,2000))
invisible(dev.off())
include_graphics(fig3)
```

<img src="Figures/fig3.png" width="512" />

_The mean_ number of steps taken each day is 10766.19 and _the median_ is 10766.19. These values are higher than in the initial analysis. After the missing values were filled-in in the original dataset, there are less days with zero steps.

## Are there differences in activity patterns between weekdays and weekends?

It is interesting to see the difference in pattern between _weekdays_ and _weekends_. The new factor column is added to the dataset that determines if the day is a _weekend_ or a _weekday_. The data is summarized for every 5-minute interval on a graph. There is a clear difference in the patterns between _weekdays_ and _weekends_. The main activity during _the weekend_ starts around 8:00, while during _the weekday_ the person is already active starting 6:00. The spike in the morning activity during _the weekday_ at 8:35 is more pronounced than during _the weekend_. Although during _the weekend_, there is more activity in the afternoon than in _the weekday_.


```r
weekends <- weekdays(datafixed$datetime)=="Saturday" | weekdays(datafixed$datetime)=="Sunday"
datafixed$weekdays[!weekends] <- "weekday"
datafixed$weekdays[weekends] <- "weekend"

byintw <- with(datafixed,aggregate(steps,by = list(interval = interval,weekday = weekdays),FUN = "mean"))
colnames(byintw)[names(byintw)=="x"] <- "steps"

byintw$time <- format(strptime(sprintf("%04i",as.numeric(paste(byintw$interval))),"%H%M"),"%H:%M")

fig4 <- "Figures/fig4.png"
png(file= fig4,width = 768, height = 432)
ggplot(byintw,aes(x=time,y=steps,group=1))+
    geom_line()+
    facet_grid(byintw$weekday~.)+
    scale_x_discrete(breaks=byintw$time[seq(1,288,12)])
invisible(dev.off())
include_graphics(fig4)
```

<img src="Figures/fig4.png" width="768" />

