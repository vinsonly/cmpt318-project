library("depmixS4")
library("ggplot2")
library("lubridate")
#import data
mydata <- read.table("C:/Users/akc24/Downloads/test1.txt",header=TRUE, sep= ",")
#check data
#head(mydata, n=2)

#isolate time frame values = wednesday nights 6-9pm
wednesdayEvenings <- mydata[(as.POSIXlt(mydata$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(mydata$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(mydata$Time, format="%H:%M:%S")) < 21),]

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
meanWeekEvening <- aggregate(wednesdayEvenings$Global_active_power, by=list(week(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), mean)
names(meanWeekEvening) <- c("Week", "Global_active_power")
head(meanWeekEvening, n=4)

max(meanWeekEvening$Global_active_power)
min(meanWeekEvening$Global_active_power)

#Average all months
meanMonthEvenings <- aggregate(wednesdayEvenings$Global_active_power, by=list(month(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), mean)
names(meanMonthEvenings) <- c("Month", "Global_active_power")
head(meanMonthEvenings, n=4)

max(meanMonthEvenings$Global_active_power)
min(meanMonthEvenings$Global_active_power)

#average all seasons
meanSeasonEvenings <- aggregate(wednesdayEvenings$Global_active_power, by=list(season(wednesdayEvenings$Date)), mean)
names(meanSeasonEvenings) <- c("Season", "Global_active_power")
meanSeasonEvenings

max(meanSeasonEvenings$Global_active_power)
min(meanSeasonEvenings$Global_active_power)

#plot weeks(figure 1.1 and 1.4)
ggplot()+ ggtitle("Average Weeks Evenings")+
  layer(data = averageWedEvenings, mapping = aes(x=Time, y=Global_active_power), geom = "point", stat="identity", position = position_identity()) +
  coord_cartesian()

#plots months (figures 1.2 and 1.5)
ggplot(meanMonthEvenings, aes(x = Month, y = Global_active_power)) +
  geom_point() +
  ggtitle("Average Month Evenings") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1,52,1))


#plot seasons (ficures 1.3 and 1.6)
ggplot(meanSeasonEvenings, aes(x = Season, y = Global_active_power)) +
  geom_point() +
  ggtitle("Average Season Evenings")+
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
