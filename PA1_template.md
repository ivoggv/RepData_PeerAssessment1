---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: yes
---



## Loading and preprocessing the data

File must be in your working directory. For this example we haven't unzip it, but you may use the csv file directly. 


```r
library(readr)
data=read_csv(file = "activity.zip")
str(data)
```

```
## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```


## What is mean total number of steps taken per day?


```r
library(dplyr,quietly = TRUE)

total_steps=data %>% group_by(date) %>% summarise(steps_per_day=sum(steps,na.rm = T))

hist(total_steps$steps_per_day,col = "blue",xlab = "Steps per Day",main = "Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



```r
meansteps=format(mean(total_steps$steps_per_day,na.rm = TRUE),digits=2)
mediansteps=format(median(total_steps$steps_per_day,na.rm = TRUE),digits=2)
```

The **mean** total number of steps taken per day **9354**.  

The **median** total number of steps taken per day **10395**.  

## What is the average daily activity pattern?


```r
library(ggplot2)
data %>% 
  group_by(interval) %>% 
  summarise(steps=mean(steps,na.rm = TRUE)) %>%  ggplot(aes(x = interval,y=steps))+
    geom_line() + 
    geom_hline(aes(yintercept = mean(data$steps,na.rm=TRUE),linetype="Average"),color="blue",show.legend = TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
maximo=data %>% 
  group_by(interval) %>% 
  summarise(steps=mean(steps,na.rm = TRUE)) %>% filter(steps==max(steps))
```
The interval with the maximum number of steps on average is **835** with **206.1698113** steps.  

## Imputing missing values

```r
#Calculate the number of missing values
length(which(is.na(data$steps)))
```

```
## [1] 2304
```

```r
#Create a dataset
average=data %>% 
  group_by(interval) %>% 
  summarise(steps=mean(steps,na.rm = TRUE))

navalues=left_join(data %>% filter(is.na(steps)),average,by="interval") %>% 
      mutate(steps=steps.y) %>% select(date,interval,steps)

data$steps[which(is.na(data$steps))]=navalues$steps

newdf=data
newtotal=newdf %>% group_by(date) %>% summarise(steps_per_day=sum(steps,na.rm = T)) 

hist(newtotal$steps_per_day,col = "blue",xlab = "Steps per Day",main = "Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
newmeansteps=format(mean(newtotal$steps_per_day,na.rm = TRUE),digits=2)
newmediansteps=format(median(newtotal$steps_per_day,na.rm = TRUE),digits=2)
```

The **new mean** total number of steps taken per day **10766** in contrast with the previous amount of **9354** .  

The **new median** total number of steps taken per day **10766** in contrast with the previous amount of **10395** .  

The mean and median are not the same using my method, which was assigning the average value of the interval to the intervals in which we had NAs. 


## Are there differences in activity patterns between weekdays and weekends?


```r
newdf=newdf %>% mutate(day=weekdays(date)) 

newdf$week=c("Weekday")
newdf$week[grep(x=newdf$day,pattern = "^S")]="Weekend"

newdf %>% group_by(week,interval) %>% summarize(steps=mean(steps)) %>%  ggplot(aes(interval,steps)) + geom_line(aes(color=week),show.legend = FALSE) + facet_grid(.~week)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

