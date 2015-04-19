for(i in 1:nrow(activities)){
        activity<-activities[i,]
        if(is.na(activity$steps)){
                int<-activity$interval
                mean.val<-(activities.by.interval%>%filter(interval==int))$avg.steps
        #        activity$steps<-mean.val
                activities[i,]$steps<--mean.val
        }
        
}