# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Data set for analysis can be found at the following URL.  

[Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

Load libraries _data.table, dplyr, tidyr, lubridate_


```r
    library(data.table)
    library(plyr)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(lubridate)
    library("scales")
```

Download, unpack and read data set.


```r
file.name.zip <- "repdata-data-activity.zip"
file.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file.name <- "activity.csv"

if(!file.exists(file.name.zip)) {
    download.file(url = file.url, destfile = file.name.zip)
    unzip(file.name.zip)
}


colClasses <- c("steps" = "integer", "date" = "POSIXct",  "interval" = "integer")

activity <- as.data.table(read.csv(file.name, colClasses = colClasses, stringsAsFactors=FALSE))
```

## What is mean total number of steps taken per day?

### Calculate total steps per day ignoring NA values.  


```r
steps_per_day <- activity %>% 
    group_by(date) %>% 
    summarise(steps = sum(steps, na.rm = TRUE))

# break steps by quantiles for better plotting
steps_per_day$qSPD <- cut(steps_per_day$steps, 
                          breaks = quantile (steps_per_day$steps, probs = c(0, .25, .50, .75, 1)), 
                          include.lowest = TRUE,
                          dig.lab = 5)
```

### Plot of Histogram of the total steps per day. 


```r
 steps_hist <- ggplot(data = steps_per_day, aes(steps))
 steps_hist <- steps_hist + geom_histogram(col="red", 
                 fill="green", 
                 alpha = .2,
                 binwidth=100)
 steps_hist <- steps_hist + labs(title = "Histogram: Daily Step Totals by Quantile", x = "Number of Steps", y = "Number of Days")
 steps_hist <- steps_hist + facet_wrap(~qSPD, ncol=2, scales="free")
steps_hist
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

### Mean and median of the total number of steps taken per day


```r
mean <- round(mean(steps_per_day$steps))
median <- median(steps_per_day$steps)
```

**mean** = ~ 9354 (rounded to nearest step)  
**median** = 10395  



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
