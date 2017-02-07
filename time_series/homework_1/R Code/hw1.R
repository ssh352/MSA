

#Modern Retail Incorporated (hereafter the “Store”), acting by and through its department of 
# Marketing and Sales Analysis is seeking proposals for retail analytics services. The scope of 
# services includes the following:


library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(dplyr)

train <- read.csv('AUGUST_TRAIN.csv', na.strings = c("", " ", "NA"), stringsAsFactors = FALSE)

# #One observation where drybulbfarenheit is not numeric
# train[is.na(as.numeric(train$DryBulbFarenheit)),]

# Notice Time is not in the form of minutes or hours.. but actual time
# of the day
summary(train$Time)

#convert into a time that makes sense (minutes of the day)
train$Time <- ifelse(nchar(train$Time) == 4, 
                     as.numeric(substr(train$Time,1,2)) * 60 +
                       as.numeric(substr(train$Time,3,4)), 
                     ifelse(nchar(train$Time) == 3,
                            as.numeric(substr(train$Time,1,1)) * 60 +
                              as.numeric(substr(train$Time,2,3)), train$Time)) 


#group by date and look at the number of stamps in each day
train %>% 
  group_by(Date) %>% 
  summarise(n = length(Time)) -> n_obs
data.frame(n_obs)

# Notice the timestamps are not uniform for each day
plot(train$Time)

#' This function returns 1 every time the sum of a sequence of numbers sum to >= 60
#' return 0 if the sum of numbers are < 60
cumsum_time <- function(times){
  tot <- 0
  output <- NULL
  for(time in times){
    tot <- tot + time
    if(tot >= 60){
      output <- c(output,1)
      tot <- 0
    }else{
      output <- c(output,0)
    }
  }
  return(output)
}

train %>% 
  mutate(DryBulbFarenheit = as.numeric(DryBulbFarenheit)) %>% 
  group_by(Date) %>%    
  mutate(lag_time = lag(Time)) %>% 
  mutate(time_diff = ifelse(is.na(Time - lag_time),
                            60,Time - lag_time)) %>% 
  mutate(spaced = cumsum_time(time_diff)) %>% 
  filter(spaced == 1) -> clean_train

clean_train %>% 
  group_by(Date) %>% 
  summarise(n_obs = n()) -> n_obs
print(data.frame(n_obs))

par(mfrow  = c(1,2))
plot(train$Time, ylab = "Time of the day")
plot(clean_train$Time, ylab = 'Time of the day')


#still have one NA temperature
#linear interpolation to estimate missing temp
clean_train$DryBulbFarenheit <- na.approx(clean_train$DryBulbFarenheit)



#clean data



#remove each row that has a outlier lag







#   
#  Creation of an hourly forecast for temperature in Phoenix, AZ for September 1, 2016 
#  from an Exponential Smoothing Model (ESM), to be used for evaluating consumer behavior; 
#  The Store’s analysts believe that extreme outdoor temperatures may affect the sales of 
#  the main retail location in Phoenix, AZ; They want a forecast of these temperatures to help 
#  them further evaluate this claim.
#  
#  Creation of easy to read and interpret visualizations of the following:

# o For the trend/cycle and seasonal breakdown, the current team uses classical decomposition; 
#     The Store is open to other techniques as long as the reasons are clearly stated and supported.
# o Forecasted temperature values overlaid with the actual temperature values on September 1, 2016 
#   (validation set).
#  The Store’s analysts are open to either additive or multiplicative ESM’s; However,
#     the reasons for choosing either must be clearly stated and supported.
#  The Store uses Mean Absolute Percentage Error (MAPE) in calculating the accuracy of its forecasts;
#     Report this measure for the 24 hourly forecasted temperatures on September 1, 2016; The Store is 
#     open to other measurements in addition to the MAPE as long as they are clearly stated and 
#     supported.
#  The Store’s analysts recommend testing the residuals from the final ESM to check i
#     f they are white noise; The p-value and test statistic should be listed as well 
#     as results interpreted.



par(mfrow = c(1,1))
# Creation of Time Series Data Object #
temp <- ts(clean_train$DryBulbFarenheit, frequency = 24)

# Time Series Decomposition #
model <- stl(temp, s.window = 7)
trend <- model$time.series[,2]
seasonal <- model$time.series[,1]
error <- model$time.series[,3]

# temp_error <- ts(error, frequency = 24)
# model_error <- stl(error, s.window = 7)
# trend_error <- model_error$time.series[,2]
# seasonal_error <- model_error$time.series[,1]
# error_error <- model_error$time.series[,3]

plot(model)
plot(error)
# plot(error)
# plot(error_error)

# o Actual temperatures overlaid with the exponentially smoothed trend/cycle for the training set.
plot(temp, col = "grey", main = "Pheonix, AZ: September Temperature (F)", xlab = "", ylab = "Degrees (F)", lwd = 2)
lines(trend , col = "red", lwd = 2)

# o Actual temperatures overlaid with the seasonally adjusted temperatures for the training set.
plot(temp, col = "grey", main = " - Seasonally Adjusted", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)
lines(temp-seasonal, col = "red", lwd = 2)

monthplot(seasonal, main = "Arizona Temperatures - Daily Effects", ylab = "Seasonal Sub Series", xlab = "Seasons (Days)", lwd = 2)



# 
# # o Actual temperatures overlaid with the exponentially smoothed trend/cycle for the training set.
# plot(temp_error, col = "grey", main = "Pheonix, AZ: September Temperature (F)", xlab = "", ylab = "Degrees (F)", lwd = 2)
# lines(trend_error, col = "red", lwd = 2)
# 
# # o Actual temperatures overlaid with the seasonally adjusted temperatures for the training set.
# plot(temp_error, col = "grey", main = " - Seasonally Adjusted", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)
# lines(temp-seasonal, col = "red", lwd = 2)
# 
# monthplot(model$time.series[,"seasonal"], main = "Arizona Temperatures - Daily Effects", ylab = "Seasonal Sub Series", xlab = "Seasons (Days)", lwd = 2)
# 
# #pick 
# 
# 


# Building a Single Exponential Smoothing Model - Steel Data #
ses_temp <- ses(temp, h = 24)
summary(ses_temp)

plot(temp, main = "Temperature simple ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
lines(ses_temp$fitted, col = 'blue')

plot(temp - ses_temp$fitted, main = 'SES residuals')


# Ljung-Box Test for temp SES Model #
White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(ses_temp$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")





# Building a Trend Exponential Smoothing Model - Steel Data #
holt_temp <- holt(temp, h = 5)
summary(holt_temp)

plot(holt_temp, main = "US Steel Shipments with Linear ESM Forecast", 
     xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")

hist(temp - holt_temp$fitted)
plot(temp - holt_temp$fitted, main = 'trend SES residuals')

holt_temp <- holt(temp, initial = "optimal", h = 24, damped = TRUE)


# Ljung-Box Test for temp SES + trend Model
White.LB <- rep(NA, 50)
for(i in 1:length(White.LB)){
  White.LB[i] <- Box.test(holt_temp$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


# Building a trend + seasonal Exponential Smoothing Model - Pheonix, AZ temperatures
hw_temp <- hw(temp, h = 24)
summary(hw_temp)

plot(temp, main = "US Airline Passengers with Linear ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
lines(hw_temp$fitted, col = 'red')
plot(temp - hw_temp$fitted)


# Ljung-Box Test for SES + trend + seasonal Model
White.LB <- rep(NA, 50)
for(i in 1:length(White.LB)){
  White.LB[i] <- Box.test(hw_temp$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")




hw_damp_temp <- hw(temp, h = 24, damped = TRUE)
plot(temp, main = "US Airline Passengers with Linear ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
plot(hw_damp_temp, col = 'red')
plot(temp - hw_damp_temp$fitted)

# Ljung-Box Test for  trend + seasonal + damped exponential smoothing model
White.LB <- rep(NA, 50)
for(i in 1:length(White.LB)){
  White.LB[i] <- Box.test(hw_damp_temp$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


hw_forecast <- summary(hw_temp)
hw_forecast <- hw_forecast$`Point Forecast`
hw_damp_forecast <- summary(hw_damp_temp)
hw_damp_forecast <- hw_damp_forecast$`Point Forecast`

forecast <- (hw_forecast + hw_damp_forecast)/2
test <- read.csv('SEPTEMBER_VALID.csv')
test <- test[grep('51$', test$Time),]

plot(test$DryBulbFarenheit, ylim = c(85,105))
points(hw_forecast, pch = 6)
points(hw_damp_forecast, pch = 10)

points(forecast, pch = 10)


mape <- function(forecast, actual){
  n <- length(forecast)
  return(sum(abs(actual - forecast)/actual)/n)
}
  
mape(hw_damp_forecast, test$DryBulbFarenheit)
mape(hw_forecast, test$DryBulbFarenheit)
mape(forecast, test$DryBulbFarenheit)

#residuals
plot(test$DryBulbFarenheit - hw_forecast, ylim = c(-6,6))
plot(test$DryBulbFarenheit - forecast, ylim = c(-6,6))
plot(test$DryBulbFarenheit - hw_damp_forecast, ylim = c(-6,6))
abline(h = 0)
#choose the hw_forecast

