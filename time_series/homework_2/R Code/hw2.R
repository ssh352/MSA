#

#time Series 2

library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(dplyr)

# Creation of an weekly forecast for sales in both Phoenix, AZ and Tuscan, AZ 
# from an Exponential Smoothing Model (ESM); The Store’s analysts know that the
# data is not seasonal and would like ESM’s for each store built separately 
# (no aggregation of sale across stores).
# 

df <- read.csv('../../data/AZ_SALES.csv')
df <- df[complete.cases(df),]
df$Date <- as.Date(df$Date, format = '%m/%d/%y')

validation <- df[(nrow(df)-15):nrow(df),]
df <- df[1:(nrow(df)-16),]
valid_tu <- validation[,c(1,3)]
valid_ph <- validation[,c(1,2)]

#Date checks out
df$Date - lag(df$Date)

summary(df)

hist(df$SALES_PH)
hist(df$SALES_TU)
###############################

#create time series objects
sales_ph <- ts(df$SALES_PH)
sales_tu <- ts(df$SALES_TU)

#Create simple exponential smoothing models for sales in pheonix
ses_ph <- ses(sales_ph, initial = "optimal", h = 16)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(ses_ph$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

#Create simple exponential smoothing models for sales in tuscan
ses_tu <- ses(sales_tu, initial = "optimal", h = 16)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(ses_tu$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


#Create holt-exponential smoothing models for sales in pheonix
holt_ph <- holt(sales_ph, initial = "optimal", h = 16)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(holt_ph$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


#Create holt-exponential smoothing models for sales in tuscan
holt_tu <- holt(sales_tu, initial = "optimal", h = 16)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(holt_tu$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


#plot models
summary(holt_tu)
summary(ses_tu)
plot(sales_tu, col = 'grey', lwd = 2)
lines(holt_tu$fitted, col = 'blue', lwd = 2)
lines(ses_tu$fitted, col = 'red', lwd = 2)


summary(holt_ph)
summary(ses_ph)
plot(sales_ph, col = 'grey', lwd = 2)
lines(holt_ph$fitted, col = 'blue', lwd = 2)
lines(ses_ph$fitted, col = 'red', lwd = 2)


#compare models to the validation data
mape <- function(y,y_hat){
  n <- length(y)
  mape <- sum(abs(y-y_hat)/y)/n
  return(mape)
}
plot(holt_tu)
plot(ses_tu)
holt_tu <- data.frame(holt_tu)
ses_tu <- data.frame(ses_tu)
mape(valid_tu$SALES_TU, holt_tu$Point.Forecast)
mape(valid_tu$SALES_TU, ses_tu$Point.Forecast)


plot(holt_ph)
plot(ses_ph)
holt_ph <- data.frame(holt_ph)
ses_ph <- data.frame(ses_ph)
mape(valid_ph$SALES_PH, holt_ph$Point.Forecast)
mape(valid_ph$SALES_PH, ses_ph$Point.Forecast)


#Select the holt models for pheonix and tuscan as they both have lower mapes
# than simple exponential smoothing.

#Perform cross validation to determine which models will likely perform better
######


#   Evaluation of the data from each store to handle further time series modeling:
#   o Check the stationarity of the sales from each store; the analysts recommend using 
#     the Augmented Dickey-Fuller tests up to lag 2 for the results, however, you are welcome to 
#     suggest other techniques as long as the reasons are clearly stated and supported.
#   o What strategies (if any) should the marketing analysis team take to make the 
#   data stationary?
# 

#ADF for pheonix
ADF_Pvalues_ph <- rep(NA, 3)
for(i in 0:2){
  ADF_Pvalues_ph[i+1] <- adf.test(sales_ph, alternative = "stationary", k = i)$p.value
}
ADF_Pvalues_ph  
#The augmented dickey-fuller fails to reject the nulls on lags 2&3 showing non-stationarity


#Take a difference to achieve stationarity
stationary_ph <- diff(sales_ph)
ADF_Pvalues_ph_diff <- rep(NA, 3)
for(i in 0:2){
  ADF_Pvalues_ph_diff[i+1] <- adf.test(stationary_ph, alternative = "stationary", k = i)$p.value
}
ADF_Pvalues_ph_diff 


#sales_tu is stationary around the trend line
ADF_Pvalues_tu <- rep(NA, 3)
for(i in 0:2){
  ADF_Pvalues_tu[i+1] <- adf.test(sales_tu, alternative = "stationary", k = i)$p.value
}
ADF_Pvalues_tu

#remove linear trend
time <- 1:length(as.numeric(sales_tu))
stationary_tu <- summary(lm(as.numeric(sales_tu) ~ time))$residuals

ADF_Pvalues_tu <- rep(NA, 3)
for(i in 0:2){
  ADF_Pvalues_tu[i+1] <- adf.test(stationary_tu, alternative = "stationary", k = i)$p.value
}
ADF_Pvalues_tu


#display stationary residuals
plot(stationary_tu, type = 'l', main = 'Stationary Tuscan Sales',
     ylab = 'Sales Minus Trend', xlab = 'Time')
plot(stationary_ph, type = 'l', main = 'Stationary Pheonix Sales', 
     ylab = 'Difference between sales', xlab = 'Time')


# write.csv(stationary_tu, 'stationary_tu.csv')
# write.csv(stationary_ph, 'stationary_ph.csv')
# 
#################################################################
####    End of homework 1



Acf(AR.Model$residuals, main = "")$acf
abline(h = .05)
abline(h = -.05)

Pacf(AR.Model$residuals, main = "")
abline(h = .05)
abline(h = -.05)



par(mfrow = c(1,2))
Acf(sales_ph, lag = 10)$acf
Pacf(sales_ph, lag = 10)$acf

AR.Model <- Arima((sales_tu - mean(sales_tu)), order = c(2, 0, 0),
                  xreg = 1:length(sales_ph))

#The augmented dickey-fuller test on pheonix sales states stationarity







#   The Store uses Mean Absolute Percentage Error (MAPE) in calculating the accuracy 
#   of its forecasts; Report this measure for the validation data set; The Store is 
#   open to other measurements in addition to the MAPE as long as they are clearly
#   stated and supported.
#   The Store’s analysts recommend testing the residuals from the final ESM’s to check if they are white noise; The p-value(s) should be listed as well as results
#   interpreted.







#Random Walk correlation dissapates over time
e <- rnorm(10000,0,1)
y1 <- 0 
y <- cumsum(e) 
plot(y, type = 'l')
plot(diff(y), type = 'l')
plot(e, type = 'l')

lag_y <- c(y[100001:length(y)],rep(NA, 100000))
df <- data.frame(y,lag_y)
df <- df[complete.cases(df),]

plot(df[,1], df[,2])
cor(df[,1], df[,2])




#A random walk will not revert to its mean
e <- rnorm(100000,0,1)
y1 <- 0 
y <- cumsum(e) 
plot(y, type = 'l')

mean(y)
-6.377   #The value it should revert to is zero








