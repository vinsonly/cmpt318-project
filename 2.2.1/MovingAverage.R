library("TTR")
library("ggplot2")

# Get dataset and remove NA values
df <- Train.Data
df <- na.omit(df)

# Get test data to compare and remove NA values
test <- test1
test <- na.omit(test)

movingAverage <- test
movingSD <- test

# The z score threshold
# A data value is an anomaly if it is below mean - z*SD, or above mean + z*SD
# Can be changed to modify sensitivity to point anomalies
z <- 3

# Copy the raw data for each feature
movingSubMetering3 <- data.frame(Raw = test$Global_active_power)
movingGlobalReactivePower <- data.frame(Raw = test$Global_reactive_power)
movingVoltage <- data.frame(Raw = test$Voltage)
movingGlobalIntensity <- data.frame(Raw = test$Global_intensity)
movingSubMetering1 <- data.frame(Raw = test$Sub_metering_1)
movingSubMetering2 <- data.frame(Raw = test$Sub_metering_2)
movingSubMetering3 <- data.frame(Raw = test$Sub_metering_3)

# Calculate the moving average for each feature with an observation window of size 10
# When experimenting with moving median, the function was changed to runMedian
movingSubMetering3$Mean = runMean(movingSubMetering3$Raw, n=10)
movingGlobalReactivePower$Mean = runMean(movingGlobalReactivePower$Raw, n=10)
movingVoltage$Mean = runMean(movingVoltage$Raw, n=10)
movingGlobalIntensity$Mean = runMean(movingGlobalIntensity$Raw, n=10)
movingSubMetering1$Mean = runMean(movingSubMetering1$Raw, n=10)
movingSubMetering2$Mean = runMean(movingSubMetering2$Raw, n=10)
movingSubMetering3$Mean = runMean(movingSubMetering3$Raw, n=10)

# Calculate the moving standard deviation for each feature with an observation window of size 10
movingSubMetering3$SD = runSD(movingSubMetering3$Raw, n=10)
movingGlobalReactivePower$SD = runSD(movingGlobalReactivePower$Raw, n=10)
movingVoltage$SD = runSD(movingVoltage$Raw, n=10)
movingGlobalIntensity$SD = runSD(movingGlobalIntensity$Raw, n=10)
movingSubMetering1$SD = runSD(movingSubMetering1$Raw, n=10)
movingSubMetering2$SD = runSD(movingSubMetering2$Raw, n=10)
movingSubMetering3$SD = runSD(movingSubMetering3$Raw, n=10)

# Find all data instances where the difference between the mean and the original value is
# greater than z standard deviations in the moving window, for each individual feature
anomalySubMetering3 <- movingSubMetering3[((movingSubMetering3$Raw > movingSubMetering3$Mean + z * movingSubMetering3$SD) | (movingSubMetering3$Raw < movingSubMetering3$Mean - z * movingSubMetering3$SD)) & !is.na(movingSubMetering3$Mean),]

anomalyGlobalReactivePower <- movingGlobalReactivePower[((movingGlobalReactivePower$Raw > movingGlobalReactivePower$Mean + z * movingGlobalReactivePower$SD) | (movingGlobalReactivePower$Raw < movingGlobalReactivePower$Mean - z * movingGlobalReactivePower$SD)) & !is.na(movingGlobalReactivePower$Mean),]

anomalyVoltage <- movingVoltage[((movingVoltage$Raw > movingVoltage$Mean + z * movingVoltage$SD) | (movingVoltage$Raw < movingVoltage$Mean - z * movingVoltage$SD)) & !is.na(movingVoltage$Mean),]

anomalyGlobalIntensity <- movingGlobalIntensity[((movingGlobalIntensity$Raw > movingGlobalIntensity$Mean + z * movingGlobalIntensity$SD) | (movingGlobalIntensity$Raw < movingGlobalIntensity$Mean - z * movingGlobalIntensity$SD)) & !is.na(movingGlobalIntensity$Mean),]

anomalySubMetering1 <- movingSubMetering1[((movingSubMetering1$Raw > movingSubMetering1$Mean + z * movingSubMetering1$SD) | (movingSubMetering1$Raw < movingSubMetering1$Mean - z * movingSubMetering1$SD)) & !is.na(movingSubMetering1$Mean),]

anomalySubMetering2 <- movingSubMetering2[((movingSubMetering2$Raw > movingSubMetering2$Mean + z * movingSubMetering2$SD) | (movingSubMetering2$Raw < movingSubMetering2$Mean - z * movingSubMetering2$SD)) & !is.na(movingSubMetering2$Mean),]

anomalySubMetering3 <- movingSubMetering3[((movingSubMetering3$Raw > movingSubMetering3$Mean + z * movingSubMetering3$SD) | (movingSubMetering3$Raw < movingSubMetering3$Mean - z * movingSubMetering3$SD)) & !is.na(movingSubMetering3$Mean),]

