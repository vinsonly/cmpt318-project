# Get dataset and remove all NA values
df <- Train.Data
df <- na.omit(df)

# Aliases for each feature
A <- df$Global_active_power
B <- df$Global_reactive_power
C <- df$Voltage
D <- df$Global_intensity
E <- df$Sub_metering_1
F <- df$Sub_metering_2
G <- df$Sub_metering_3

# Calculate all possible correlation coefficients using Pearson method
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

# Print all correlation coefficients
print(corrAB)
print(corrAC)
print(corrAD)
print(corrAE)
print(corrAF)
print(corrAG)
print(corrBC)
print(corrBD)
print(corrBE)
print(corrBF)
print(corrBG)
print(corrCD)
print(corrCE)
print(corrCF)
print(corrCG)
print(corrDE)
print(corrDF)
print(corrDG)
print(corrEF)
print(corrEG)
print(corrFG)