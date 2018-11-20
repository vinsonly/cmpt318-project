########## Setup ##########

library("depmixS4")
library("ggplot2")
library("lubridate")
set.seed(1)
train_df <- Train.Data
test1_df <- test1
test2_df <- test2
test3_df <- test3
test4_df <- test4
test5_df <- test5

# Data frame from 6PM to 9PM (exclusive) for Wednesday Evenings
train_wednesdayEvenings <- train_df[(as.POSIXlt(train_df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(train_df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(train_df$Time, format="%H:%M:%S")) < 21),]
test1_wednesdayEvenings <- test1_df[(as.POSIXlt(test1_df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(test1_df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(test1_df$Time, format="%H:%M:%S")) < 21),]
test2_wednesdayEvenings <- test2_df[(as.POSIXlt(test2_df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(test2_df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(test2_df$Time, format="%H:%M:%S")) < 21),]
test3_wednesdayEvenings <- test3_df[(as.POSIXlt(test3_df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(test3_df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(test3_df$Time, format="%H:%M:%S")) < 21),]
test4_wednesdayEvenings <- test4_df[(as.POSIXlt(test4_df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(test4_df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(test4_df$Time, format="%H:%M:%S")) < 21),]
test5_wednesdayEvenings <- test5_df[(as.POSIXlt(test5_df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(test5_df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(test5_df$Time, format="%H:%M:%S")) < 21),]

# Wednesday evenings
Model <- depmix(response = Global_active_power ~ 1, data = train_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(180, 154))
fitModel_train <- fit(Model)

Model_1 <- depmix(response = Global_active_power ~ 1, data = test1_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(180, 52))
Model_1 <- setpars(Model_1, getpars(fitModel_train))
fb1 <- forwardbackward(Model_1)

Model_2 <- depmix(response = Global_active_power ~ 1, data = test2_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(180, 52))
Model_2 <- setpars(Model_2, getpars(fitModel_train))
fb2 <- forwardbackward(Model_2)

Model_3 <- depmix(response = Global_active_power ~ 1, data = test3_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(180, 52))
Model_3 <- setpars(Model_3, getpars(fitModel_train))
fb3 <- forwardbackward(Model_3)

Model_4 <- depmix(response = Global_active_power ~ 1, data = test4_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(180, 52))
Model_4 <- setpars(Model_4, getpars(fitModel_train))
fb4 <- forwardbackward(Model_4)

Model_5 <- depmix(response = Global_active_power ~ 1, data = test5_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(180, 52))
Model_5 <- setpars(Model_5, getpars(fitModel_train))
fb5 <- forwardbackward(Model_5)

print(fitModel_train)
print(fb1$logLike)
print(fb2$logLike)
print(fb3$logLike)
print(fb4$logLike)
print(fb5$logLike)
