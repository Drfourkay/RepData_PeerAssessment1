---
title: "PA1_template.Rmd"
author: "Me"
date: "7/31/2020"
output:
  html_document: default
  pdf_document: default
---


## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
dataacti <- read.csv("activity.csv", header = TRUE)
head(dataacti)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
echo=TRUE
```
# Calculate the Number of Steps produced each day

```r
library(magrittr)
library(dplyr)
acti_date <- dataacti %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(acti_date$tsteps, xlab = "Total Day to Day Steps",main="Day to Day Histogram of Total Steps", breaks = 20)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
echo=TRUE
```


```r
#Computing the Mean
mean(acti_date$tsteps)
```

```
## [1] 10766.19
```

```r
echo=TRUE
```

```r
#Computing the Median
median(acti_date$tsteps)
```

```
## [1] 10765
```

```r
echo=TRUE
```
#Time Series Chart for Average Steps

```r
library(ggplot2)
data_time <- dataacti%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(data_time, aes(x=interval, y=tsteps))+ geom_line()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
echo=TRUE
```

```r
# Time Interval with Maximum Steps
data_time[which(data_time$tsteps== max(data_time$tsteps)),]
```

```
## # A tibble: 1 x 2
##   interval tsteps
##      <int>  <dbl>
## 1      835   206.
```

```r
echo=TRUE
```

```r
#To Impute Missing Values
misvals <- sum(is.na(data))
```

```
## Warning in is.na(data): is.na() applied to non-(list or vector) of
## type 'closure'
```

```r
misvals
```

```
## [1] 0
```

```r
echo=TRUE
```

```r
#Devise a strategy for filling in all of the missing values in the dataset. Iam going to apply the mean for that 5 -minute interval to replace all the missing values in the dataset. At the end, I will check if all the NAs have been replaced

repl_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mean_fill <- dataacti%>% group_by(interval) %>% mutate(steps= repl_mean(steps))
head(mean_fill)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <chr>         <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

```r
#Histogram Post Replacement with Means for Missing Data
data_impute <- aggregate(mean_fill$steps, by=list(mean_fill$date), sum)

names(data_impute)[1] ="date"
names(data_impute)[2] ="totalsteps"
head(data_impute,15)
```

```
##          date totalsteps
## 1  2012-10-01   10766.19
## 2  2012-10-02     126.00
## 3  2012-10-03   11352.00
## 4  2012-10-04   12116.00
## 5  2012-10-05   13294.00
## 6  2012-10-06   15420.00
## 7  2012-10-07   11015.00
## 8  2012-10-08   10766.19
## 9  2012-10-09   12811.00
## 10 2012-10-10    9900.00
## 11 2012-10-11   10304.00
## 12 2012-10-12   17382.00
## 13 2012-10-13   12426.00
## 14 2012-10-14   15098.00
## 15 2012-10-15   10139.00
```

```r
#Summary Statistic for New Data
summary(data_impute)
```

```
##      date             totalsteps   
##  Length:61          Min.   :   41  
##  Class :character   1st Qu.: 9819  
##  Mode  :character   Median :10766  
##                     Mean   :10766  
##                     3rd Qu.:12811  
##                     Max.   :21194
```

```r
#Histogram for the New Data

hist(data_impute$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


```r
#Comparing the Old and New Statistics
oldmean <- mean(acti_date$tsteps, na.rm = TRUE)
newmean <- mean(data_impute$totalsteps)
# Old mean and New mean
oldmean
```

```
## [1] 10766.19
```

```r
newmean
```

```
## [1] 10766.19
```

```r
omedian <- median(acti_date$tsteps, na.rm = TRUE)
nmedian <- median(data_impute$totalsteps)
# Old median and New median
omedian
```

```
## [1] 10765
```

```r
nmedian
```

```
## [1] 10766.19
```

```r
# Differences in Activitiy - Week Days vs. Week Ends
mean_fill$date <- as.Date(mean_fill$date)
mean_fill$weekday <- weekdays(mean_fill$date)
mean_fill$weekend <- ifelse(mean_fill$weekday=="Saturday" | mean_fill$weekday=="Sunday", "Weekend", "Weekday" )
library(ggplot2)
mean_wendwday <- aggregate(mean_fill$steps , by= list(mean_fill$weekend, mean_fill$interval), na.omit(mean))
names(mean_wendwday) <- c("weekend", "interval", "steps")

ggplot(mean_wendwday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)
