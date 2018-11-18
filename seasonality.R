# get convert the time column to integer in terms of minutes (eg. 1h 5 mins = 65)
# returns a data frame with the added field of the left called totalMinutes
timeToMinutes <- function(df) {
  minute <- minute(as.POSIXlt(df$Time, format="%H:%M:%S"))
  hour <- hour(as.POSIXlt(df$Time, format="%H:%M:%S"))
  
  totalMinutes <- c()
  # create list to find total minutes
  for(i in 1:length(minute)) {
    totalMinutes[i] <- minute[i] + (hour[i] %% 18)*60
  }
  df <- cbind(df, totalMinutes)
  df
}

averageWinter <- timeToMinutes(averageWinter)
averageSpring <- timeToMinutes(averageSpring)
averageSummer <- timeToMinutes(averageSummer)
averageFall <- timeToMinutes(averageFall)

lmWinter = lm(formula = Global_active_power~totalMinutes, data = averageWinter)
lmSpring = lm(formula = Global_active_power~totalMinutes, data = averageSpring)
lmSummer = lm(formula = Global_active_power~totalMinutes, data = averageSummer)
lmFall = lm(formula = Global_active_power~totalMinutes, data = averageFall)

coeffWinter = coefficients(lmWinter)
coeffSpring = coefficients(lmSpring)
coeffSummer = coefficients(lmSummer)
coeffFall = coefficients(lmFall)

eqWinter = paste0("y = ", round(coeffWinter[2],5), "*x + ", round(coeffWinter[1],5))
eqSpring = paste0("y = ", round(coeffSpring[2],5), "*x + ", round(coeffSpring[1],5))
eqSummer = paste0("y = ", round(coeffSummer[2],5), "*x + ", round(coeffSummer[1],5))
eqFall = paste0("y = ", round(coeffFall[2],5), "*x + ", round(coeffFall[1],5))

# calculate cost of a given data frame and linear reg
getCost <- function(df, m, b) {
  totalCost = 0
  for(i in 1:nrow(df)) {
    x <- df[i,"totalMinutes"]
    linearReg[i] <- m*x + b
    totalCost = totalCost + (df[i,"Global_active_power"] - linearReg[i])^2  
  }
  totalCost = totalCost / nrow(df)
  totalCost
}

costWinter <- getCost(averageWinter, coeffWinter[2], coeffWinter[1])
costSpringVsWinter <- getCost(averageSpring, coeffWinter[2], coeffWinter[1])
costSummerVsWinter <- getCost(averageSummer, coeffWinter[2], coeffWinter[1])
costFallVsWinter <- getCost(averageFall, coeffWinter[2], coeffWinter[1])

# plots 
# winter plot
winterPlot <- ggplot()+
  layer(data = averageWinter, mapping = aes(x=totalMinutes, y=Global_active_power, col="red"), geom = "point",stat="identity", position = position_identity()) +
  ggtitle("Average Global Active Power on Wednesday Evenings in Winter")

winterPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=30, y=2.5, label=eq,
           color="red")

# spring plot
springPlot <- ggplot()+
  layer(data = averageSpring, mapping = aes(x=totalMinutes, y=Global_active_power, col="red"), geom = "point",stat="identity", position = position_identity()) +
  ggtitle("Average Global Active Power on Wednesday Evenings in Spring against Winter Linear Regression")

springPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=30, y=1.8, label=eq,
           color="red")

# summer plot
summerPlot <- ggplot()+
  layer(data = averageSummer, mapping = aes(x=totalMinutes, y=Global_active_power, col="red"), geom = "point",stat="identity", position = position_identity()) +
  ggtitle("Average Global Active Power on Wednesday Evenings in Summer against Winter Linear Regression")

summerPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=30, y=1.8, label=eq,
           color="red")

# fall plot
fallPlot <- ggplot()+
  layer(data = averageFall, mapping = aes(x=totalMinutes, y=Global_active_power, col="red"), geom = "point",stat="identity", position = position_identity()) +
  ggtitle("Average Global Active Power on Wednesday Evenings in Summer against Winter Linear Regression")

fallPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=30, y=2.5, label=eq,
           color="red")

ggplot() +
  geom_point(data = averageWinter, aes(x=totalMinutes, y=Global_active_power, col="winter")) + 
  geom_point(data = averageSummer, aes(x=totalMinutes, y=Global_active_power, col="summer")) + 
  ggtitle("Average Global Active Power on Wednesday Evenings in Summer vs Winter")

# winter vs summer plot of all points
ggplot() +
  geom_point(data = winter, aes(x=totalMinutes, y=Global_active_power, col="winter")) + 
  geom_point(data = summer, aes(x=totalMinutes, y=Global_active_power, col="summer")) + 
  ggtitle("Global Active Power on Wednesday Evenings in Summer vs Winter")

# calculate average for Saturday 6- 9 and compare vs average Wednesday 6 - 9
saturdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 6 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]
saturdayEvenings <- timeToMinutes(saturdayEvenings)
averageSatEvenings <- aggregate(Global_active_power~Time, saturdayEvenings[,c(2,3)], mean)
averageSatEvenings <- timeToMinutes(averageSatEvenings)


wednesdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]
averageWedEvenings <- aggregate(Global_active_power~Time, wednesdayEvenings[,c(2,3)], mean)
averageWedEvenings <- timeToMinutes(averageWedEvenings)

ggplot() +
  geom_point(data = averageSatEvenings, aes(x=totalMinutes, y=Global_active_power, col="Saturday")) + 
  geom_point(data = averageWedEvenings, aes(x=totalMinutes, y=Global_active_power, col="Wednesday")) + 
  ggtitle("Average Global Active Power on Wednesday Evenings vs Saturday Evenings")


