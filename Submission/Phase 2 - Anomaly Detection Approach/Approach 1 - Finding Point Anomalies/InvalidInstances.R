#Get dataset

# Get dataset and remove NA values
df <- Train.Data
df <- na.omit(df)

# Get test data to compare and remove NA values
test <- test3
test <- na.omit(test)

# Determines if a number is perfectly divisible by some real number divisor
# True if x modulo divisor is 0
# Accounts for floating point rounding errors
divisible <- function(data, divisor) {
  data %% divisor == 0 | abs(data %% divisor - divisor) < sqrt(.Machine$double.eps) 
}

# Find all negative data instances for each individual feature
negativeGlobalActivePowerInstances <- test[test$Global_active_power < 0,]
negativeGlobalReactivePowerInstances <- test[test$Global_Reactive_power < 0,]
negativeVoltageInstances <- test[test$Voltage < 0,]
negativeGlobalIntensityInstances <- test[test$Global_intensity < 0,]
negativeSubMetering1Instances <- test[test$Sub_metering_1 < 0,]
negativeSubMetering2Instances <- test[test$Sub_metering_2 < 0,]
negativeSubMetering3Instances <- test[test$Sub_metering_3 < 0,]

# Finds all data frame rows where at least one feature has a negative value
negativeInstances <- test[test$Global_active_power < 0 | test$Global_reactive_power < 0 | test$Voltage < 0 | test$Global_intensity < 0 | test$Sub_metering_1 < 0 | test$Sub_metering_2 < 0 | test$Sub_metering_3 < 0,]


# Smallest measurable difference constants for each feature (sensor epsilons)
globalReactivePowerEpsilon <- 0.002
voltageEpsilon <- 0.01
globalIntensityEpsilon <- 0.2
subMeteringEpsilon <- 1

# Find all invalid data instances for each individual feature
invalidGlobalReactivePowerInstances <- test[!divisible(test$Global_reactive_power, globalReactivePowerEpsilon),]
invalidVoltageInstances <- test[!divisible(test$Voltage, voltageEpsilon),]
invalidGlobalIntensityInstances <- test[!divisible(test$Global_intensity, globalIntensityEpsilon),]
invalidSubMetering1Instances <- test[!divisible(test$Sub_metering_1, subMeteringEpsilon),]
invalidSubMetering2Instances <- test[!divisible(test$Sub_metering_2, subMeteringEpsilon),]
invalidSubMetering3Instances <- test[!divisible(test$Sub_metering_3, subMeteringEpsilon),]

# Find all data instances that are invalid for some feature
invalidInstances <- test[!divisible(test$Global_reactive_power, globalReactivePowerEpsilon) | !divisible(test$Voltage, voltageEpsilon) | !divisible(test$Global_intensity, globalIntensityEpsilon) | !divisible(test$Sub_metering_1, subMeteringEpsilon) | !divisible(test$Sub_metering_2, subMeteringEpsilon) | !divisible(test$Sub_metering_3, subMeteringEpsilon),]
