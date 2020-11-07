#First SET WD
setwd("C:/Users/juanfidel18/Desktop/Coursera/ReproducibleResearch")
#load packages
library(ggplot2)
library(dplyr)

#CHECK SYSTEM DATE IN MEXICO_CITY

Sys.Date()
actividad <- read.csv("activity.csv")

actividad$date <- as.POSIXct(actividad$date, formato="%Y%m%d")

str(actividad)

#REVIEW DAIRY STEPS

pasosdiarios <- aggregate(actividad$steps, list(actividad$date), FUN=sum)

#CHANGE COLUMN NAMES IN ORDER TO GET THE CODE

colnames(pasosdiarios) <- c("Date", "Steps")

#FIRST GRAPHIC
histograma <- ggplot(pasosdiarios, aes(Steps))
histograma+geom_histogram(boundary=0, binwidth=2500, col="darkgreen", fill="lightgreen")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))

# Mean
mean(pasosdiarios$Steps, na.rm=TRUE)

#Mediana
median(pasosdiarios$Steps, na.rm=TRUE)

# create steps per time
pasostiempo <- aggregate(steps~interval,data=actividad,FUN=mean,na.action=na.omit)

# time variable
pasostiempo$time <- pasostiempo$interval/100

# Line plot 2ND GRAPHIC
histograma2<- ggplot(pasostiempo, aes(time, steps))
histograma2+geom_line(col="brown")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))

# table for dplyr
PT<- tbl_df(pasostiempo)
# find the column
PT %>% select(time, steps) %>% filter(steps==max(PT$steps))

# table for dplyr
TAC <- tbl_df(actividad)
# find the column
TAC %>% filter(is.na(steps)) %>% summarize(missing_values = n())

# values without NA are imputed in a new column
actividad$CompleteSteps <- ifelse(is.na(actividad$steps), round(pasostiempo$steps[match(actividad$interval, pasostiempo$interval)],0), actividad$steps)

# new dataset activityFull
actividadF <- data.frame(steps=actividad$CompleteSteps, interval=actividad$interval, date=actividad$date)
# see first 10 values of the new dataset
head(actividadF, n=10)

# prepare data
pasosdiafull <- aggregate(actividadF$steps, list(actividadF$date), FUN=sum)
#CHANGE NAMES OF FULLSTEPS
colnames(pasosdiafull) <- c("Date", "Steps")
# draw the histogram
histagrama3 <- ggplot(pasosdiafull, aes(Steps))
histagrama3+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))
# Mean
mean(pasosdiafull$Steps)
#Median
median(pasosdiafull$Steps)

# Create variable with date in correct format
actividadF$RealDate <- as.Date(actividadF$date, format = "%Y-%m-%d")

# create a variable with weekdays name
actividadF$weekday <- weekdays(actividadF$RealDate)
head(actividadF,n=9)
# create a new variable indicating weekday or weekend

actividadF$DayType <- ifelse(actividadF$weekday=="sábado","weekend",ifelse(actividadF$weekday=="domingo","weekend","weekday"))


# create table with steps per time across weekdaydays or weekend days
pasostiempoDT <- aggregate(steps~interval+DayType,data=actividadF,FUN=mean,na.action=na.omit)

# variable time (more comprensible for the graph axis)
pasostiempoDT$Time<- pasostiempo$interval/100

# draw the line plot
histograma4<- ggplot(pasostiempoDT, aes(x=interval, y=steps, color = DayType)) +
  geom_line()+
  facet_wrap(~DayType, ncol = 1, nrow=2)


