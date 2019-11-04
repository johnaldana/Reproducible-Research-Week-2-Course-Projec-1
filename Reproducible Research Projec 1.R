# 1 Loading and preprocessing the data
    
    activity<-read.csv("./activity.csv",header = TRUE)
    
    activity$date<-as.Date(activity$date)


# 2 What is mean total number of steps taken per day?
    
    # 2.1 Calculate the total number of steps taken per day
          
          stepsday<-with(activity,tapply(steps,date,sum,na.rm=TRUE))
    
    # 2.2 Make a histogram of the total number of steps taken each day
          
          hist(stepsday,col = "blue",xlab = "Total Steps",ylab = "Frequency",
               main = "Total Number of Steps per Day")
          
          dev.copy(png, file= "Total Number of Steps per Day.png", 
                   width=480, height=480)
          
          dev.off()
      
    # 2.3 Calculate and report the mean and median of the total number of steps
          #taken per day
          
          mean(stepsday)
          
          median(stepsday)
          
          # So the average total steps taken each day is 9354.23 and, 
          # the average median is is calculated to be 10395
          

# 3 What is the average daily activity pattern?
    
    # 3.1 What is the average daily activity pattern?
    
          averageStepsbyInterval<-aggregate(steps~interval, activity, mean)
          
          with(averageStepsbyInterval, plot(interval, steps, type = "l", 
          main = "Average Steps per Interval"))
          
          dev.copy(png, file= "Average Steps per Interval.png", 
                   width=480, height=480)
          
          dev.off()
          
          
    
    # 3.2 Which 5-minute interval, on average across all the days in the 
          #dataset, contains the maximum number of steps?
          
          averageStepsbyInterval[which.max(averageStepsbyInterval[,2]),1]
          
          # The 5-minute interval that contains the maximum number of steps is 
          # 835
          
# 4 Imputing missing values
          
    # 4.1 Calculate and report the total number of missing values in the dataset
          
          sum(is.na(activity$steps))
          
          # There are 2304 missing values in the dataset.
          
    # 4.2 Devise a strategy for filling in all of the missing values in the 
          #dataset.
          
          # I decided to fill in all of the missing values in the dataset by the
          # mean number of steps per interval.
          
          m<-mean(averageStepsbyInterval$steps)
          
          # m = 37.3825996
          
    # 4.3 Create a new dataset that is equal to the original dataset but with
          #the missing data filled in.
          
          activity1<-activity
          
          missingIndex<-is.na(activity[,1])
          
          activity1[missingIndex,1]<-m
    
    # 4.4 Make a histogram of the total number of steps taken each day and 
          #Calculate and report the mean and median total number of steps 
          #taken per day. 
          
          totalStepsByDay1<-aggregate(steps~date, activity1, sum)
          
          hist(totalStepsByDay1$steps, xlab="Class of Total Number of Steps 
          per day", ylab="Number of Days", main="Number of Steps taken each
          day after missing values are imputed", col="green")
          
          dev.copy(png, file= "Number of Steps taken each day.png", width=480,
                   height=480)
          
          dev.off()
          
          
          totalStepsByDay1<-aggregate(steps~date, activity1, sum)
          
          mean(totalStepsByDay1$steps)
          
          median(totalStepsByDay1$steps)
          
          # So the average total steps taken each day is 10766.19 and, 
          # the average median is is calculated to be 10766.19

          
# 5 Are there differences in activity patterns between weekdays and weekends?
          
          
    # 5.1 Create a new factor variable in the dataset with two levels - 
          #"weekday" and "weekend" indicating whether a given date is a weekday
          #or weekend day.  
          
          activity2<-activity
          
          activity2$date<-as.Date(activity2$date)
          
          activity2$date <- weekdays(activity2$date)
          
          daytype <- function (x) {
            
            if (x == "sábado" | x == "domingo"){
              x <- "Weekend"
              
            } else {
              x <- "Weekday"}
            
          }
          
          activity2$daytype <- sapply(activity2$date, daytype)
          
          activity_by_date <- aggregate(steps~interval + daytype, activity2,
                                        mean, na.rm = TRUE)
    
    # 5.2 Make a panel plot containing a time series plot (i.e. type="l") of 
          #the 5-minute interval (x-axis) and the average number of steps
          #taken, averaged across all weekday days or weekend days (y-axis)     
          
          library (ggplot2)
          
          plot<- ggplot(activity_by_date, aes(x = interval , y = steps, 
                 color = daytype)) + geom_line() + labs(title = 
                 "Average daily steps by type of date", x = "Interval", 
                 y = "Average number of steps") +facet_wrap(~daytype, 
                 ncol = 1, nrow=2)
          
          print(plot)
          
          dev.copy(png, file= "Average daily steps by type of date.png", width=480,
                   height=480)
          
          dev.off()
          