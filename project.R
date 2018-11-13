########## Setup ##########

library("depmixS4")
library("ggplot2")
library("lubridate")
set.seed(1)
df <- Train.Data

# correlation

A <- df$Global_active_power
B <- df$Global_reactive_power
C <- df$Voltage
D <- df$Global_intensity
E <- df$Sub_metering_1
F <- df$Sub_metering_2
G <- df$Sub_metering_3

# calculate correlation between all of the features
#   choose two features with high correlation with each other for multivariate HMM
corrAB <- cor(A, B, method="pearson", use="complete.obs")
corrAC <- cor(A, C, method="pearson", use="complete.obs")
corrAD <- cor(A, D, method="pearson", use="complete.obs")
corrAE <- cor(A, E, method="pearson", use="complete.obs")
corrAF <- cor(A, F, method="pearson", use="complete.obs")
corrAG <- cor(A, G, method="pearson", use="complete.obs")

corrBC <- cor(B, C, method="pearson", use="complete.obs")
corrBD <- cor(B, D, method="pearson", use="complete.obs")
corrBE <- cor(B, E, method="pearson", use="complete.obs")
corrBF <- cor(B, F, method="pearson", use="complete.obs")
corrBG <- cor(B, G, method="pearson", use="complete.obs")

corrCD <- cor(C, D, method="pearson", use="complete.obs")
corrCE <- cor(C, E, method="pearson", use="complete.obs")
corrCF <- cor(C, F, method="pearson", use="complete.obs")
corrCG <- cor(C, G, method="pearson", use="complete.obs")

corrDE <- cor(D, E, method="pearson", use="complete.obs")
corrDF <- cor(D, F, method="pearson", use="complete.obs")
corrDG <- cor(D, G, method="pearson", use="complete.obs")

corrEF <- cor(E, F, method="pearson", use="complete.obs")
corrEG <- cor(E, G, method="pearson", use="complete.obs")

corrFG <- cor(F, G, method="pearson", use="complete.obs")


wednesdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 21 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 24),]

averageWedEvenings <- aggregate(Global_active_power~Time, wednesdayEvenings[,c(2,3)], mean)

wednesdayAllDay <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 0 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 24),]

averageWedAllDay <- aggregate(Global_active_power~Time, wednesdayAllDay[,c(2,3)], mean)

averageWedAllDayVolt <- aggregate(Global_active_power~Voltage, wednesdayAllDay[,c(3,5)], mean)

# linear regression model, use time to predict global active power
ans = lm(formula = Global_active_power~Time, data = averageWedEvenings)
# predict
predictorTimes <- c("21:30:00", "22:00:00", "22:30:00", "23:00:00", "23:30:00", "23:59:00")

predictor <- data.frame(Time = predictorTimes)
predict(ans, predictor)

# seasonality
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

# convert date to integer
dateToInteger <- function(dframe) {
  days <- yday(as.POSIXlt(dframe$Date, format="%d/%m/%Y"))
  dframe$Date <- days
  dframe
}

newWedEvenings = dateToInteger(wednesdayEvenings)

# create the dataframes for the four seasons
emptydf <- data.frame(matrix(nrow=0,ncol = length(colnames(wednesdayEvenings))))
colnames(emptydf) <- colnames(wednesdayEvenings)
winter <- emptydf
spring <- emptydf
summer <- emptydf
fall <- emptydf

#set boundaries for seasons
winterEnd <- 79 # March 20
springEnd <- 171 # June 21
summerEnd <- 266 # September 23
fallEnd <- 355 # December 21

# loop through every row and do something
for(i in 1:nrow(newWedEvenings)) {
  thisDate <- newWedEvenings[i,"Date"]
  thisRow <- newWedEvenings[i,]
  # check which season row belongs to and add row to the corresponding dataframe
  if(thisDate < winterEnd) {
    winter <- rbind(winter, thisRow)
  } else if(thisDate < springEnd) {
    spring <- rbind(spring, thisRow)
  } else if(thisDate < summerEnd) {
    summer <- rbind(summer, thisRow)
  } else if(thisDate < fallEnd) {
    fall <- rbind(fall, thisRow)  
  } else {
    winter <- rbind(winter, thisRow)
  }
}

# create data frame for thisRow
newdf <- data.frame("Date" = thisRow$Date, "Time" = thisRow$Time, "Global_active_power"=thisRow$Global_active_power, "Global_reactive_power"="thisRow$Global_reative_power", "Voltage" = thisRow$Voltage, "Global_intensity"=thisRow$Global_intensity, "Sub_metering_1"=thisRow$Sub_metering_1, "Sub_metering_2"=thisRow$Sub_metering_2, "Sub_metering_3"=thisRow$Sub_metering_3)

# calculate averages for each season
averageWinter <- aggregate(Global_active_power~Time, winter[,c(2,3)], mean)
averageSpring <- aggregate(Global_active_power~Time, spring[,c(2,3)], mean)
averageSummer <- aggregate(Global_active_power~Time, summer[,c(2,3)], mean)
averageFall <- aggregate(Global_active_power~Time, fall[,c(2,3)], mean)


# average GAP for each season, can be done for max, min as well, and stdev
avgSeasonEvenings <- aggregate(wednesdayEvenings$Global_active_power, by=list(season(wednesdayEvenings$Date)), mean)
names(avgSeasonEvenings) <- c("Season", "Global_active_power")

# plotting 
ggplot()+
  layer(data = averageWedEvenings, mapping = aes(x=Time, y=Global_active_power), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_time() +
  scale_y_continuous() +
  ggtitle("Average Global Active Power on Wednesday Evenings")

