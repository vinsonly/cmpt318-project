########## Setup ##########

library("lubridate")
set.seed(1)
df <- Train.Data

wednesdayEvenings   <- df[(as.POSIXlt(df$Date, format="%d/%m/%Y")$wday == 3 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) >= 18 & hour(as.POSIXlt(df$Time, format="%H:%M:%S")) < 21),]

wednesdayJanuary    <- wednesdayEvenings[yday(wednesdayEvenings$Date) <= 31,]
wednesdayFebruary   <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 31 & yday(wednesdayEvenings$Date) <= 59,]
wednesdayMarch      <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 59 & yday(wednesdayEvenings$Date) <= 90,]
wednesdayApril      <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 90 & yday(wednesdayEvenings$Date) <= 120,]
wednesdayMay        <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 120 & yday(wednesdayEvenings$Date) <= 151,]
wednesdayJune       <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 151 & yday(wednesdayEvenings$Date) <= 181,]
wednesdayJuly       <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 181 & yday(wednesdayEvenings$Date) <= 212,]
wednesdayAugust     <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 212 & yday(wednesdayEvenings$Date) <= 243,]
wednesdaySeptember  <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 243 & yday(wednesdayEvenings$Date) <= 273,]
wednesdayOctober    <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 273 & yday(wednesdayEvenings$Date) <= 304,]
wednesdayNovember   <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 304 & yday(wednesdayEvenings$Date) <= 334,]
wednesdayDecember   <- wednesdayEvenings[yday(wednesdayEvenings$Date) > 334,]

sdJanuary   <- sd(wednesdayJanuary$Global_active_power)
sdFebruary  <- sd(wednesdayFebruary$Global_active_power)
sdMarch     <- sd(wednesdayMarch$Global_active_power)
sdApril     <- sd(wednesdayApril$Global_active_power)
sdMay       <- sd(wednesdayMay$Global_active_power)
sdJune      <- sd(wednesdayJune$Global_active_power)
sdJuly      <- sd(wednesdayJuly$Global_active_power)
sdAugust    <- sd(wednesdayAugust$Global_active_power)
sdSeptember <- sd(wednesdaySeptember$Global_active_power)
sdOctober   <- sd(wednesdayOctober$Global_active_power)
sdNovember  <- sd(wednesdayNovember$Global_active_power)
sdDecember  <- sd(wednesdayDecember$Global_active_power)

