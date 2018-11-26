########## Setup ##########

library("depmixS4")
library("ggplot2")
library("lubridate")
set.seed(1)

# Loading our training and test data
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

# Defining some magic numbers for rep
# calculating ratio of training data size vs test data
size_of_window <- 180
windows_t <- nrow(train_wednesdayEvenings)/size_of_window
windows_1 <- nrow(test1_wednesdayEvenings)/size_of_window
windows_2 <- nrow(test2_wednesdayEvenings)/size_of_window
windows_3 <- nrow(test3_wednesdayEvenings)/size_of_window
windows_4 <- nrow(test4_wednesdayEvenings)/size_of_window
windows_5 <- nrow(test5_wednesdayEvenings)/size_of_window

# Training the Model
Model <- depmix(response = Global_active_power ~ 1, data = train_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(size_of_window, windows_t))
fitModel_train <- fit(Model)

### Running test data against the HMM
# Using entire test data set - 1 year
Model_1 <- depmix(response = Global_active_power ~ 1, data = test1_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(size_of_window, windows_1))
Model_1 <- setpars(Model_1, getpars(fitModel_train))
fb1 <- forwardbackward(Model_1)

Model_2 <- depmix(response = Global_active_power ~ 1, data = test2_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(size_of_window, windows_2))
Model_2 <- setpars(Model_2, getpars(fitModel_train))
fb2 <- forwardbackward(Model_2)

Model_3 <- depmix(response = Global_active_power ~ 1, data = test3_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(size_of_window, windows_3))
Model_3 <- setpars(Model_3, getpars(fitModel_train))
fb3 <- forwardbackward(Model_3)

Model_4 <- depmix(response = Global_active_power ~ 1, data = test4_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(size_of_window, windows_4))
Model_4 <- setpars(Model_4, getpars(fitModel_train))
fb4 <- forwardbackward(Model_4)

Model_5 <- depmix(response = Global_active_power ~ 1, data = test5_wednesdayEvenings, family = gaussian(), nstates = 26, ntimes = rep(size_of_window, windows_5))
Model_5 <- setpars(Model_5, getpars(fitModel_train))
fb5 <- forwardbackward(Model_5)

# Normalizing the log-likelihoods to 1 Wednesday evening 6pm-9pm
nt <- logLik(fitModel_train)/windows_t
n1 <- fb1$logLike/windows_1
n2 <- fb2$logLike/windows_2
n3 <- fb3$logLike/windows_3
n4 <- fb4$logLike/windows_4
n5 <- fb5$logLike/windows_5

# Displaying our results
print(nt)
print(n1)
print(n2)
print(n3)
print(n4)
print(n5)


# separate data for comparison between two data frames


# calculate logliklihood value for each week
# loop through the dataset and calculate logliklihood for each week

hmmPlot <- function(testDf, trained_model) {
    # set initial values
    temp_df <- data.frame()
    ll_df <- data.frame()
    count <- 0
    
    # create empty list that stores data for each week
    logLikelihoods <- c()
    listCount <- 1
    
    # calculate logliklihood for each week
    for(i in 1:nrow(testDf)) {
      if(count < 180) {
        # add row to temp_df
        temp_df <- rbind(temp_df,testDf[i,])
        count <- count + 1
      } else {
        # perform 
        print(count)
        Model_1 <- depmix(response = Global_active_power ~ 1, data = temp_df, family = gaussian(), nstates = 26, ntimes = rep(count, 1))
        Model_1 <- setpars(Model_1, getpars(trained_model))
        fb1 <- forwardbackward(Model_1)
        
        # normalize data
        n1 <- fb1$logLike/windows_1
        print(n1)
        
        # save normalized value into list
        logLikelihoods[listCount] <- n1
        listCount <- listCount + 1
        
        # reset dataframes
        temp_df <- data.frame()
        ll_df <- data.frame()
        count <- 0
      }
    }
    logLikelihoods
}


getHmmPlot <- function(test_data) {
  # compares the loglikelihood of each week against the entire training model
  hmmRes <- hmmPlot(test_data, fitModel_train)
  length(hmmRes)
  
  size_of_window <- 180
  windows <- nrow(test_data)/size_of_window - 1
  weeks <- c(1:windows)
  print(windows)
  
  hmmDf <- data.frame("Week" = weeks, "Log-likelihood" = hmmRes)
  View(hmmDf)
  # plot loglikelihood over weeks
  logLikelihoodPlot <- ggplot() +
    layer(data = hmmDf, mapping = aes(x=Week, y=Log.likelihood, size=3), geom = "point",stat="identity", position = position_identity()) +
    # ggtitle("Normalized Log-likelihood Vs Week") +
    ylab("Normalized Log-likelihood") + 
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
  
  logLikelihoodPlot
}

plot1 <- getHmmPlot(test1_wednesdayEvenings)
plot4 <- getHmmPlot(test4_wednesdayEvenings)
plot5 <- getHmmPlot(test5_wednesdayEvenings)

# train model for only winter season
# filter only the winter season for the test datasets
getWinterData <- function(allData) {
  winterEnd <- 79 # March 20
  springEnd <- 171 # June 21
  summerEnd <- 266 # September 23
  fallEnd <- 355 # December 21
  
  # create the dataframes for the four seasons
  emptydf <- data.frame(matrix(nrow=0,ncol = length(colnames(allData))))
  colnames(emptydf) <- colnames(allData)
  winter <- emptydf
  spring <- emptydf
  summer <- emptydf
  fall <- emptydf
  
  dateToInteger <- function(dframe) {
    days <- yday(as.POSIXlt(dframe$Date, format="%d/%m/%Y"))
    dframe$Date <- days
    dframe
  }
  
  allData = dateToInteger(allData)
  
  for(i in 1:nrow(allData)) {
    thisDate <- allData[i,"Date"]
    thisRow <- allData[i,]
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
  winter
}

winterTest1 <- getWinterData(test1_wednesdayEvenings)
winterTest2 <- getWinterData(test2_wednesdayEvenings)
winterTest3 <- getWinterData(test3_wednesdayEvenings)
winterTest4 <- getWinterData(test4_wednesdayEvenings)
winterTest5 <- getWinterData(test5_wednesdayEvenings)

size_of_window <- 180
windows_t_w <- nrow(winter)/size_of_window
windows_1_w <- nrow(winterTest1)/size_of_window
windows_2_w <- nrow(winterTest2)/size_of_window
windows_3_w <- nrow(winterTest3)/size_of_window
windows_4_w <- nrow(winterTest4)/size_of_window
windows_5_w <- nrow(winterTest5)/size_of_window

# Training the Model
thisWinterModel <- depmix(response = Global_active_power ~ 1, data = winter, family = gaussian(), nstates=14, ntimes = rep(size_of_window, windows_t_w))
fitModel_train_w <- fit(thisWinterModel)

### Running test data against the HMM
# Using entire test data set - 1 year
Model_1_w <- depmix(response = Global_active_power ~ 1, data = winterTest1, family = gaussian(), nstates = 14, ntimes = rep(size_of_window, windows_1_w))
Model_1_w <- setpars(Model_1_w, getpars(fitModel_train_w))
fb1_w <- forwardbackward(Model_1_w)

Model_2_w <- depmix(response = Global_active_power ~ 1, data = winterTest2, family = gaussian(), nstates = 14, ntimes = rep(size_of_window, windows_2_w))
Model_2_w <- setpars(Model_2_w, getpars(fitModel_train_w))
fb2_w <- forwardbackward(Model_2_w)

Model_3_w <- depmix(response = Global_active_power ~ 1, data = winterTest3, family = gaussian(), nstates = 14, ntimes = rep(size_of_window, windows_3_w))
Model_3_w <- setpars(Model_3_w, getpars(fitModel_train_w))
fb3_w <- forwardbackward(Model_3_w)

Model_4_w <- depmix(response = Global_active_power ~ 1, data = winterTest4, family = gaussian(), nstates = 14, ntimes = rep(size_of_window, windows_4_w))
Model_4_w <- setpars(Model_4_w, getpars(fitModel_train_w))
fb4_w <- forwardbackward(Model_4_w)

Model_5_w <- depmix(response = Global_active_power ~ 1, data = winterTest5, family = gaussian(), nstates = 14, ntimes = rep(size_of_window, windows_5_w))
Model_5_w <- setpars(Model_5_w, getpars(fitModel_train_w))
fb5_w <- forwardbackward(Model_5_w)

# Normalizing the log-likelihoods to 1 Wednesday evening 6pm-9pm
nt_w <- logLik(fitModel_train_w)/windows_t_w
n1_w <- fb1_w$logLike/windows_1_w
n2_w <- fb2_w$logLike/windows_2_w
n3_w <- fb3_w$logLike/windows_3_w
n4_w <- fb4_w$logLike/windows_4_w
n5_w <- fb5_w$logLike/windows_5_w

print(logLik(fitModel_train_w))
print(fb1_w$logLike)
print(fb2_w$logLike)
print(fb3_w$logLike)
print(fb4_w$logLike)
print(fb5_w$logLike)

print(nt_w)
print(n1_w)
print(n2_w)
print(n3_w)
print(n4_w)
print(n5_w)

#figure 5.1
plot1_w <- getHmmPlot(winterTest1)
plot4_w <- getHmmPlot(winterTest4)
#figure 5.2
plot5_w <- getHmmPlot(winterTest5)

trainHmm <- function(modelToTrain, left, right, isMulti) {
  
  bic <- c()
  breakCount <- 0
  logLik <- c()
  sizeCounter <- 1
  thisModel
  
  sizeOfWindow <- 180
  windowsT <- nrow(modelToTrain)/size_of_window

  for(i in left:right) {
    if(isMulti) {
      thisModel <- depmix(response = list(Global_active_power ~ 1, Global_intensity ~ 1), data = modelToTrain, family = list(gaussian(), gaussian()), nstates =i, ntimes = rep(sizeOfWindow, windowsT))
    } else {
      thisModel <- depmix(response = Global_active_power ~ 1, data = modelToTrain, family = gaussian(), nstates =i, ntimes = rep(sizeOfWindow, windowsT))
    }
    
    fittedModel <- fit(thisModel) 
    bic[sizeCounter] <- BIC(fittedModel)
    logLik[sizeCounter] <- logLik(fittedModel)
    
    if(sizeCounter > 2 && bic[sizeCounter - 1] < bic[sizeCounter]) {
      breakCount <- breakCount + 1
      if(breakCount >= 3) {
        break
      }
    }
    
    sizeCounter <- sizeCounter + 1
    
  }
  
  lastState <-length(bic) + 1
  states <- c(left:lastState)
  res <- data.frame("nstates" = states, "BIC" = bic, "log-likelihood" = logLik)
  res
}

multiHmm <- depmix(response = list(Global_active_power ~ 1, Global_intensity ~ 1), data = train_wednesdayEvenings, family = list(gaussian(), gaussian()), nstates =26, ntimes = rep(size_of_window, windows_t))
fittedMultiModel <- fit(multiHmm) 
BIC(fittedMultiModel)

