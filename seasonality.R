# get convert the time column to integer in terms of minutes (eg. 1h 5 mins = 65)
# returns a data frame with the added field of the left called totalMinutes
library("ggplot2")
library("lubridate")

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


# separate data into seasons
# convert date to integer
dateToInteger <- function(dframe) {
  days <- yday(as.POSIXlt(dframe$Date, format="%d/%m/%Y"))
  dframe$Date <- days
  dframe
}

# calculate cost of a given data frame and linear reg
getCost <- function(df, m, b) {
  totalCost = 0
  linearReg <- c()
  for(i in 1:nrow(df)) {
    x <- df[i,"totalMinutes"]
    linearReg[i] <- m*x + b
    totalCost = totalCost + (df[i,"Global_active_power"] - linearReg[i])^2  
  }
  totalCost = totalCost / nrow(df)
  totalCost
}

wednesdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]

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

averageWinter <- aggregate(Global_active_power~Time, winter[,c(2,3)], mean)
averageSpring <- aggregate(Global_active_power~Time, spring[,c(2,3)], mean)
averageSummer <- aggregate(Global_active_power~Time, summer[,c(2,3)], mean)
averageFall <- aggregate(Global_active_power~Time, fall[,c(2,3)], mean)

averageWinter <- timeToMinutes(averageWinter)
averageSpring <- timeToMinutes(averageSpring)
averageSummer <- timeToMinutes(averageSummer)
averageFall <- timeToMinutes(averageFall)

winter <- timeToMinutes(winter)
spring <- timeToMinutes(spring)
summer <- timeToMinutes(summer)
fall <- timeToMinutes(fall)


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

costWinter <- getCost(averageWinter, coeffWinter[2], coeffWinter[1])
costSpringVsWinter <- getCost(averageSpring, coeffWinter[2], coeffWinter[1])
costSummerVsWinter <- getCost(averageSummer, coeffWinter[2], coeffWinter[1])
costFallVsWinter <- getCost(averageFall, coeffWinter[2], coeffWinter[1])

costSpring <- getCost(averageSpring, coeffSpring[2], coeffSpring[1])
costWinterVsSpring <- getCost(averageWinter, coeffSpring[2], coeffSpring[1])
costSummerVsSpring <- getCost(averageSummer, coeffSpring[2], coeffSpring[1])
costFallVsSpring <- getCost(averageFall, coeffSpring[2], coeffSpring[1])

costSummer <- getCost(averageSummer, coeffSummer[2], coeffSummer[1])
costWinterVsSummer <- getCost(averageWinter, coeffSummer[2], coeffSummer[1])
costSpringVsSummer <- getCost(averageSpring, coeffSummer[2], coeffSummer[1])
costFallVsSummer <- getCost(averageFall, coeffSummer[2], coeffSummer[1])

costFall <- getCost(averageFall, coeffFall[2], coeffFall[1])
costWinterVsFall <- getCost(averageWinter, coeffFall[2], coeffFall[1])
costSpringVsFall <- getCost(averageSpring, coeffFall[2], coeffFall[1])
costSummerVsFall <- getCost(averageSummer, coeffFall[2], coeffFall[1])

# plots 
# winter plot (Figure 1.8)
winterPlot <- ggplot()+
  layer(data = averageWinter, mapping = aes(x=totalMinutes, y=Global_active_power), geom = "point",stat="identity", position = position_identity()) + 
  ylab("Global Active Power") +
  xlab("Minute")


winterPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=100, y=2.0, label=eqWinter, size=8)

# spring plot
springPlot <- ggplot() +
  layer(data = averageSpring, mapping = aes(x=totalMinutes, y=Global_active_power, col="red"), geom = "point",stat="identity", position = position_identity()) +
  ggtitle("Average Global Active Power on Wednesday Evenings in Spring against Winter Linear Regression")

springPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=220, y=1.5, label=eqWinter,
           color="red")

# summer plot (Figure 1.9)
summerPlot <- ggplot()+
  layer(data = averageSummer, mapping = aes(x=totalMinutes, y=Global_active_power), geom = "point",stat="identity", position = position_identity()) +
  ylab("Global Active Power") +
  xlab("Minute")
  
summerPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=100, y=3, label=eqWinter, size=8)

# fall plot
fallPlot <- ggplot()+
  layer(data = averageFall, mapping = aes(x=totalMinutes, y=Global_active_power, col="red"), geom = "point",stat="identity", position = position_identity()) +
  ggtitle("Average Global Active Power on Wednesday Evenings in Summer against Winter Linear Regression")

fallPlot + geom_abline(intercept = coeffWinter[1], slope = coeffWinter[2]) +
  annotate(geom="text", x=200, y=3, label=eqWinter,
           color="red")

# Figure 1.11 - Average Global Active Power on Wednesday Evenings in Summer vs Winter
ggplot() + 
  geom_point(data = averageWinter, aes(x=totalMinutes, y=Global_active_power, col="winter")) + 
  geom_point(data = averageSummer, aes(x=totalMinutes, y=Global_active_power, col="summer")) + 
  ylab("Global Active Power") +
  xlab("Minute")

# Figure 1.10 - Global Active Power on Wednesday Evenings in Summer vs Winter (all points)
ggplot() +
  geom_point(data = winter, aes(x=totalMinutes, y=Global_active_power, col="winter")) + 
  geom_point(data = summer, aes(x=totalMinutes, y=Global_active_power, col="summer")) + 
  ylab("Global Active Power") +
  xlab("Minute")

#--- NOT USED IN REPORT --- 

# calculate average for Saturday 6- 9 and compare vs average Wednesday 6 - 9
saturdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 6 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]
saturdayEvenings <- timeToMinutes(saturdayEvenings)
averageSatEvenings <- aggregate(Global_active_power~Time, saturdayEvenings[,c(2,3)], mean)
averageSatEvenings <- timeToMinutes(averageSatEvenings)


wednesdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]
averageWedEvenings <- aggregate(Global_active_power~Time, wednesdayEvenings[,c(2,3)], mean)
averageWedEvenings <- timeToMinutes(averageWedEvenings)

satVsWedPlot = ggplot() +
  geom_point(data = averageSatEvenings, aes(x=totalMinutes, y=Global_active_power, col="Saturday")) + 
  geom_point(data = averageWedEvenings, aes(x=totalMinutes, y=Global_active_power, col="Wednesday")) + 
  ggtitle("Average Global Active Power on Wednesday Evenings vs Saturday Evenings")

# calculate linear regression and compare cost of Wednesday model to Saturday
lmWed = lm(formula = Global_active_power~totalMinutes, data = averageWedEvenings)
lmSat = lm(formula = Global_active_power~totalMinutes, data = averageSatEvenings)

coeffWed = coefficients(lmWed)
coeffSat = coefficients(lmSat)

eqWed = paste0("y = ", round(coeffWed[2],5), "*x + ", round(coeffWed[1],5))
eqSat = paste0("y = ", round(coeffSat[2],5), "*x + ", round(coeffSat[1],5))

costWed <- getCost(averageWedEvenings, coeffWed[2], coeffWed[1])
costSatvsWed <- getCost(averageSatEvenings, coeffWed[2], coeffWed[1])

satVsWedPlot + geom_abline(intercept = coeffWed[1], slope = coeffWed[2]) +
  annotate(geom="text", x=140, y=1.7, label=eq,
           color="black")

