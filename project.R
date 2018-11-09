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

# calculate correlation between all of the features
#   choose two features with high correlation with each other for multivariate HMM
corrAB <- cor(A, B, method="pearson", use="complete.obs")
corrAC <- cor(A, C, method="pearson", use="complete.obs")
corrAD <- cor(A, D, method="pearson", use="complete.obs")
corrBC <- cor(B, C, method="pearson", use="complete.obs")
corrBD <- cor(B, D, method="pearson", use="complete.obs")
corrCD <- cor(C, D, method="pearson", use="complete.obs")