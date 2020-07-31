---
title: "PA1_template.Rmd"
author: "Me"
date: "7/31/2020"
output: html_document
---


## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r setup, include=FALSE}
dataacti <- read.csv("activity.csv", header = TRUE)
head(dataacti)

```
# Calculate the Number of Steps produced each day
```{r }
library(magrittr)
library(dplyr)
acti_date <- dataacti %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(acti_date$tsteps, xlab = "Total Day to Day Steps",main="Day to Day Histogram of Total Steps", breaks = 20)
```
```{r}
#Computing the Mean and Median
mean(acti_date$tsteps)
median(acti_date$tsteps)
```
#Time Series Chart for Average Steps
```{r}
library(ggplot2)
data_time <- dataacti%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(data_time, aes(x=interval, y=tsteps))+ geom_line()
```
```{r}
# Time Interval with Maximum Steps
data_time[which(data_time$tsteps== max(data_time$tsteps)),]
```
```{r}
#To Impute Missing Values
misvals <- sum(is.na(data))
misvals
```
```{r}
#Taking the mean as the Base for Filling Missing Values
repl_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mean_fill <- dataacti%>% group_by(interval) %>% mutate(steps= repl_mean(steps))
head(mean_fill)

#Histogram Post Replacement with Means for Missing Data
data_impute <- aggregate(mean_fill$steps, by=list(mean_fill$date), sum)

names(data_impute)[1] ="date"
names(data_impute)[2] ="totalsteps"
head(data_impute,15)


#Summary Statistic for New Data
summary(data_impute)

#Histogram for the New Data

hist(data_impute$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
```

```{r}
#Comparing the Old and New Statistics
oldmean <- mean(acti_date$tsteps, na.rm = TRUE)
newmean <- mean(data_impute$totalsteps)
# Old mean and New mean
oldmean
newmean
```
```{r}
omedian <- median(acti_date$tsteps, na.rm = TRUE)
nmedian <- median(data_impute$totalsteps)
# Old median and New median
omedian
```
```{r}
nmedian

```
```{r}
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
