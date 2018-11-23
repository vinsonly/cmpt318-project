library("depmixS4")
library("ggplot2")
library("lubridate")
#import data
mydata <- read.table("C:/Users/akc24/Desktop/test1.txt",header=TRUE, sep= ",")
#check data
#head(mydata, n=2)

#isolate time frame values = wednesday nights 6-9pm
wednesdayEvenings <- mydata[(as.POSIXlt(mydata$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(mydata$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(mydata$Time, format="%H:%M:%S")) <= 21),]

#Average of every minute 
averageWedEvenings <- aggregate(Global_active_power~Time, wednesdayEvenings[,c(2,3)], mean)
head(averageWedEvenings, n=4)

# Converts dates to the season they are in
season <- function(dates) {
  winterEnd <- 79 # March 20
  springEnd <- 171 # June 21
  summerEnd <- 266 # September 23
  fallEnd <- 355 # December 21
  
  days <- yday(as.POSIXlt(dates, format="%d/%m/%Y"))
  
  ifelse(days < winterEnd, "Winter",
         ifelse(days < springEnd, "Spring",
                ifelse(days < summerEnd, "Summer",
                       ifelse(days < fallEnd, "Fall", "Winter"))))
}

#Average all weeks
meanWeekMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(week(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), mean)
names(meanWeekMornings) <- c("Week", "Global_active_power")
head(meanWeekMornings, n=4)

max(meanWeekMornings$Global_active_power)
min(meanWeekMornings$Global_active_power)

#Average all months
meanMonthMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(month(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), mean)
names(meanMonthMornings) <- c("Month", "Global_active_power")
head(meanMonthMornings, n=4)

max(meanMonthMornings$Global_active_power)
min(meanMonthMornings$Global_active_power)

#average all seasons
meanSeasonMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(season(wednesdayEvenings$Date)), mean)
names(meanSeasonMornings) <- c("Season", "Global_active_power")
meanSeasonMornings

max(meanSeasonMornings$Global_active_power)
min(meanSeasonMornings$Global_active_power)

#plot weeks
ggplot()+
  layer(data = averageWedEvenings, mapping = aes(x=Time, y=Global_active_power), geom = "point", stat="identity", position = position_identity()) +
  coord_cartesian()

#plots months
ggplot(meanMonthMornings, aes(x = Month, y = Global_active_power)) +
  geom_point() +
  ggtitle("Range Month Evenings") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1,52,1))


#plot seasons
ggplot(meanSeasonMornings, aes(x = Season, y = Global_active_power)) +
  geom_point() +
  ggtitle("Range Season Evenings")+
  theme(plot.title = element_text(hjust = 0.5))

#comparing standerd deviation

# Standard deviation of global active power for Sunday mornings (weekly, monthly, seasonly)
sdWeekMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(week(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), sd)
names(sdWeekMornings) <- c("Week", "Global_active_power")
head(sdWeekMornings, n=4)

mean(sdWeekMornings[["Global_active_power"]])

sdMonthMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(month(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), sd)
names(sdMonthMornings) <- c("Month", "Global_active_power")

mean(sdMonthMornings[["Global_active_power"]])

sdSeasonMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(season(wednesdayEvenings$Date)), sd)
names(sdSeasonMornings) <- c("Season", "Global_active_power")
#compare that with average values of test data

mean(sdSeasonMornings[["Global_active_power"]])
