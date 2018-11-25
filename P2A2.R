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

# Defining some magic numbers
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
  weeks <- c(1:51)
  
  hmmDf <- data.frame("Week" = weeks, "Log-likelihood" = hmmRes)
  
  # plot loglikelihood over weeks
  logLikelihoodPlot <- ggplot() +
    layer(data = hmmDf, mapping = aes(x=Week, y=Log.likelihood), geom = "point",stat="identity", position = position_identity()) +
    ggtitle("Normalized Log-likelihood Vs Week") +
    ylab("Normalized Log-likelihood")
  
  logLikelihoodPlot
}

plot1 <- getHmmPlot(test1_wednesdayEvenings)
plot4 <- getHmmPlot(test4_wednesdayEvenings)
plot5 <- getHmmPlot(test5_wednesdayEvenings)
