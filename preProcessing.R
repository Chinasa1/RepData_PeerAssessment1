library(dplyr)
library(lattice)

#1) The data is available in the folder, so we will unzip the file and load it

#2) Unzip
fileName <- "activity.zip"
if(!file.exists(fileName)){
  dataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(dataUrl, destfile = fileName, method = "curl")
}
#Unzip the file
unzip(fileName)

#read the data
fileName <- "activity.csv"
activities <- read.csv(fileName)
head(activities)
str(activities)
summary(activities)

#From the str, we found the data is taken as factor, we will change the date class from factor to date
#the date are in format of YYYY-MM-DD, conversion is simple 
activities$date <- as.Date(activities$date)
class(activities$date)


#=======================
#total step
allSteps <- tapply(activies$steps, activities$date, sum, na.rm = TRUE )

#Histogram
hist(allSteps)
#Means
#allMean <- tapply(activies$steps, activities$date, mean )
#the same as tapply activities %>% group_by(date) %>%summarise(mean = mean(steps))

#Mean
mean(allSteps)

#Median
median(allSteps)

#============================================
#daily activities
intervalMean <- tapply(activities$steps, activities$interval, mean, na.rm = TRUE)

#the plot
plot(intervalMean, names(intervalMean), type="l")

#835

#Missing values
#1
sum(is.na(activities$steps))
#create new data frame from the mean by day
newData <- data.frame(key= names(allMean), value= allMean)
#overall mean 
mean(newData$value, na.rm = TRUE)
#result = 37.3826
#replace na by overall mean
newData$value[which(is.na(newData$value))] = 37.3826

#Replace NA by mean
##temp %>% group_by(date) %>% mutate (steps = replace(steps, is.na(steps), mean(steps, na.rm = TRUE)))

#tempToto <- temp %>% mutate (steps = replace(steps, is.na(steps), mean(steps, na.rm = TRUE)))

###
overAllMean <- mean(activities$steps, na.rm = TRUE)
activities$steps[which(is.na(activities$steps))] <- overAllMean

##new histogram
allSteps1 <- tapply(activies$steps, activities$date, sum, na.rm = TRUE )
hist(allSteps)

#Mean
mean(allSteps)

#Median
median(allSteps)
#They do not differ

#No impact
#===========================================
#creating weekdays and weekend factors
activitiesFilled <- mutate(activities, days = ifelse(as.POSIXlt(date)$wday > 0 & as.POSIXlt(date)$wday < 6, "weekdays", "weekend"))
#the plot
#week end mean
weekDayData <- activitiesFilled %>% filter(days == "weekdays")
weekDayMean <- tapply(weekDayData$steps, weekDayData$interval, mean)

weekEndData <- activitiesFilled %>% filter(days == "weekend") 
weekEndMean <- tapply(weekEndData$steps, weekEndData$interval, mean)

finalData <- data.frame(names(weekDayMean), weekday = weekDayMean, weekend = weekEndMean)

par(mfrow=c(2,1))
plot(names(weekDayMean),weekDayMean,  type="l")
plot(names(weekEndMean),weekEndMean,  type="l")
