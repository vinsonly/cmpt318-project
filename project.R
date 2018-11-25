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

saturdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 6 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]

averageSatEvenings <- aggregate(Global_active_power~Time, saturdayEvenings[,c(2,3)], mean)

wednesdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]

averageWedEvenings <- aggregate(Global_active_power~Time, wednesdayEvenings[,c(2,3)], mean)

wednesdayAllDay <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 0 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 24),]

averageWedAllDay <- aggregate(Global_active_power~Time, wednesdayAllDay[,c(2,3)], mean)

averageWedAllDayVolt <- aggregate(Global_active_power~Voltage, wednesdayAllDay[,c(3,5)], mean)

# linear regression model, use time to predict global active power
linearModel = lm(formula = Global_active_power~Time, data = averageWedEvenings)
# predict
predictorTimes <- c("18:00:00", "18:30:00", "19:00:00", "19:30:00", "20:00:00", "20:59:00")

predictor <- data.frame(Time = predictorTimes)
predict(linearModel, predictor)


# create data frame for thisRow
# newdf <- data.frame("Date" = thisRow$Date, "Time" = thisRow$Time, "Global_active_power"=thisRow$Global_active_power, "Global_reactive_power"="thisRow$Global_reative_power", "Voltage" = thisRow$Voltage, "Global_intensity"=thisRow$Global_intensity, "Sub_metering_1"=thisRow$Sub_metering_1, "Sub_metering_2"=thisRow$Sub_metering_2, "Sub_metering_3"=thisRow$Sub_metering_3)

# calculate averages for each season
averageWinter <- aggregate(Global_active_power~Time, winter[,c(2,3)], mean)
averageSpring <- aggregate(Global_active_power~Time, spring[,c(2,3)], mean)
averageSummer <- aggregate(Global_active_power~Time, summer[,c(2,3)], mean)
averageFall <- aggregate(Global_active_power~Time, fall[,c(2,3)], mean)

# average GAP for each season, can be done for max, min as well, and stdev
avgSeasonEvenings <- aggregate(wednesdayEvenings$Global_active_power, by=list(season(wednesdayEvenings$Date)), mean)
names(avgSeasonEvenings) <- c("Season", "Global_active_power")


movingAvg <- wednesdayEvenings 
movingAvg$Global_active_power <- SMA(movingAvg$Global_active_power, n=10)

# plotting 
ggplot()+
  layer(data = averageWedEvenings, mapping = aes(x=Time, y=Global_active_power), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_time() +
  scale_y_continuous() +
  ggtitle("Average Global Active Power on Wednesday Evenings")

#Average all weeks
meanWeekMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(week(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), mean)
names(meanWeekMornings) <- c("Week", "Global_active_power")
head(meanWeekMornings, n=4)

#Average all months
meanMonthMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(month(as.Date(wednesdayEvenings$Date, format="%d/%m/%Y"))), mean)
names(meanMonthMornings) <- c("Month", "Global_active_power")
head(meanMonthMornings, n=4)

#average all seasons
meanSeasonMornings <- aggregate(wednesdayEvenings$Global_active_power, by=list(season(wednesdayEvenings$Date)), mean)
names(meanSeasonMornings) <- c("Season", "Global_active_power")
meanSeasonMornings

minute <- minute(as.POSIXlt(wednesdayEvenings$Time, format="%H:%M:%S"))
hour <- hour(as.POSIXlt(wednesdayEvenings$Time, format="%H:%M:%S"))

totalMinutes <- c()

# create list to find total minutes
for(i in 1:length(minute)) {
  totalMinutes[i] <- minute[i] + (hour[i] %% 18)*60
}

# bind this as a new column in wednesdayEvenings
newWedEvenings <- cbind(wednesdayEvenings, totalMinutes)
newAvgWedEvenings <- aggregate(Global_active_power~totalMinutes, newWedEvenings[,c(2,3)], mean)

coeff = coefficients(linearModel)
eq = paste0("y = ", round(coeff[2],5), "*x + ", round(coeff[1],5))

# plot graph with line and equation
plot <- ggplot() +
  layer(data = newAvgWedEvenings, mapping = aes(x=totalMinutes, y=Global_active_power, col="red"), geom = "point",stat="identity", position = position_identity()) +
  ggtitle("Average Global Active Power on Wednesday Evenings")

plot + geom_abline(intercept = coeff[1], slope = coeff[2]) +
  annotate(geom="text", x=30, y=1.8, label=eq,
           color="red")

# add value for linear regression in data frame

linearReg <- c()
m <- coeff[2]
b <- coeff[1]

totalCost = 0
# create list to find total minutes
for(i in 1:nrow(newAvgWedEvenings)) {
  x <- newAvgWedEvenings[i,"totalMinutes"]
  linearReg[i] <- m*x + b
  totalCost = totalCost + (newAvgWedEvenings[i,"Global_active_power"] - linearReg[i])^2  
}
totalCost = totalCost/nrow(newAvgWedEvenings)

newAvgWedEvenings <- cbind(newAvgWedEvenings, linearReg)