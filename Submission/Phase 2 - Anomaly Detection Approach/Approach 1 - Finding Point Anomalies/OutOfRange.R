library("lubridate")

# Get dataset and remove NA values
df <- Train.Data
df <- na.omit(df)

# Get test data to compare and remove NA values
test <- test4
test <- na.omit(test)

# Calculate min values of each feature for each month
monthlyMins <- aggregate(list(Global_active_power = df$Global_active_power, Voltage = df$Voltage, Global_intensity = df$Global_intensity), by=list(Month = month(as.Date(df$Date, format="%d/%m/%Y"))), FUN = min)

# Calculate max values of each feature for each month
monthlyMaxs <- aggregate(list(Global_active_power = df$Global_active_power, Global_reactive_power = df$Global_reactive_power, Voltage = df$Voltage, Global_intensity = df$Global_intensity, Sub_metering_1 = df$Sub_metering_1, Sub_metering_2 = df$Sub_metering_2, Sub_metering_3 = df$Sub_metering_3), by=list(Month = month(as.Date(df$Date, format="%d/%m/%Y"))), FUN = max)

# Find all global active power instances exceeding max/min values for their month
globalActivePowerBelowMin <- test[test$Global_active_power < monthlyMins$Global_active_power[month(test$Date)] & test$Global_active_power >= 0,]
globalActivePowerAboveMax <- test[test$Global_active_power > monthlyMaxs$Global_active_power[month(test$Date)],]

# Find all global reactive power instances exceeding max values for their month
globalReactivePowerAboveMax <- test[test$Global_reactive_power > monthlyMaxs$Global_reactive_power[month(test$Date)],]

# Find all voltage data instances exceeding max/min values for their month
voltageBelowMin <- test[test$Voltage < monthlyMins$Voltage[month(test$Date)] & test$Voltage >= 0,]
voltageAboveMax <- test[test$Voltage > monthlyMaxs$Voltage[month(test$Date)],]

# Find all global intensity data instances exceeding max/min values for their month
globalIntensityBelowMin <- test[test$Global_intensity < monthlyMins$Global_intensity[month(test$Date)] & test$Global_intensity >= 0,]
globalIntensityAboveMax <- test[test$Global_intensity > monthlyMaxs$Global_intensity[month(test$Date)],]

# Find all sub metering 1,2,3 data instances exceeding max values for their month
subMetering1AboveMax <- test[test$Sub_metering_1 > monthlyMaxs$Sub_metering_1[month(test$Date)],]
subMetering2AboveMax <- test[test$Sub_metering_2 > monthlyMaxs$Sub_metering_2[month(test$Date)],]
subMetering3AboveMax <- test[test$Sub_metering_3 > monthlyMaxs$Sub_metering_3[month(test$Date)],]
