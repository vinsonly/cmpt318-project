########## Setup ##########

library("depmixS4")
library("ggplot2")
library("lubridate")
set.seed(1)
df <- Train.Data

A <- df$Global_active_power
B <- df$Global_reactive_power
C <- df$Voltage
D <- df$Global_intensity

# Data frame from 6PM to 9PM (exclusive) for Wednesday Evenings
wednesdayEvenings <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]

# Wednesday evenings
Model1 <- depmix(response = Global_active_power ~ 1, data = wednesdayEvenings, family = gaussian(), nstates = 30, ntimes = rep(180, 154))
fitModel1 <- fit(Model1)
summary(fitModel1)
print(fitModel1)


# calculate correlation between all of the features
#   choose two features with high correlation with each other for multivariate HMM
corrAB <- cor(A, B, method="pearson", use="complete.obs")
corrAC <- cor(A, C, method="pearson", use="complete.obs")
corrAD <- cor(A, D, method="pearson", use="complete.obs")
corrBC <- cor(B, C, method="pearson", use="complete.obs")
corrBD <- cor(B, D, method="pearson", use="complete.obs")
corrCD <- cor(C, D, method="pearson", use="complete.obs")