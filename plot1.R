library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
source("downloadData.R")
activities$date<-as.Date(activities$date)
#activities<-activities%>%mutate(timestamp=as.POSIXct(paste(as.character(date),sprintf("%00.4i",interval),sep=" "),format="%Y-%m-%d %HH%MM"))

activities$timestamp<-as.POSIXct(paste(as.character(activities$date),sprintf("%00.4i",activities$interval),sep=" "),
          format="%Y-%m-%d %H%M")

paste(as.character(activities$date),sprintf("%00.4i",activities$interval),sep=" ")

date.breaks<-seq(min(activities$date),max(activities$date),by="1 day")
activities.by.day<-activities%>%group_by(date)%>%summarize(steps.per.day=sum(steps))
mean.val<-mean(activities.by.day$steps.per.day[!is.na(activities.by.day$steps.per.day)])
ggplot(activities.by.day,aes(x=date,y=steps.per.day ))+
        geom_bar(stat="identity",color="black")+
        geom_smooth(aes(group=1))+
        labs(title="Steps by Day")+
        theme_bw()+
        theme(axis.text.x=element_text(angle=45,hjust=1,size=8 ))+
        scale_x_date (breaks=date.breaks)+
        annotate("text",x=as.Date("2012-10-29"),y=20000,label=sprintf("Mean %2.2f",mean.val))+
        guides(position="bottom")


activities.by.interval<-activities%>%filter(!is.na(steps))%>%group_by(interval)%>%summarize(avg.steps=mean(steps))
max.steps<-max(activities.by.interval$avg.steps)
max.interval<-(activities.by.interval%>%filter(avg.steps==max.steps))$interval

set.seed(1532)
rnorm(100, mean = mean.val, sd = 100)

activities.by.interval #Mean steps by interval - see above
for(record in activities) {
        if(is.na(record$steps)){
                int<-record$interval
                mean.val<-(activities.by.interval%>%filter(interval==int))$avg.steps
                record$steps<-mean.val
        }
        
}       

