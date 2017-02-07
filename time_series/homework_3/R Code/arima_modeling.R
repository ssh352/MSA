
library(forecast)



#TimeSeries Homework 3

stationary_tu <- read.csv("../data/stationary_tu.csv")
stationary_ph <- read.csv("../data/stationary_ph.csv")
  

stationary_tu <- ts(stationary_tu)
stationary_ph <- ts(stationary_ph)
plot(stationary_tu, type = 'l')
plot(stationary_ph, type = 'l')


lag <- function(x,k){
  x <- c(rep(NA,k),x[1:(length(x)-k)])
  return(x)
}

#Model Tuscan data

#look at ACF and PACF plots to determine how to model the data
par(mfrow = c(1,2))
Acf(stationary_tu)
Pacf(stationary_tu)

#There is one spike in the ACF and an exponentially decreasing PACF telling me 
# to use an moving average ARIMA model
arima_tu <- Arima(stationary_tu, order = c(0,0,1))
residuals_tu <- arima_tu$residuals

par(mfrow = c(1,3))
Acf(residuals_tu, ylim = c(-1,1), main = 'ACF Tuscan Sales Residuals')$acf
Pacf(residuals_tu, ylim = c(-1,1), main = 'PACF Tuscan Sales Residuals')

White.LB <- rep(NA, 20)
for(i in 1:20){
  White.LB[i] <- Box.test(residuals_tu, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

##tuscan model = arima MA-1 model

#model phoenix data
#Look at ACF and PACF plots to determine best model
Acf(stationary_ph)
Pacf(stationary_ph)

#The PACF spikes at the first lag telling me to use a AR-1 model
arima_mod_ph <- lm(stationary_ph ~ lag(stationary_ph,1))
arima_mod_ph <- Arima(stationary_ph,  c(1,0,0))
par(mfrow = c(1,3))
Acf(arima_mod_ph$residuals, ylim = c(-1,1), main = 'ACF Tuscan Sales Residuals')
Pacf(arima_mod$residuals, ylim = c(-1,1), main = 'PACF Tuscan Sales Residuals')


White.LB <- rep(NA, 20)
for(i in 1:20){
  White.LB[i] <- Box.test(arima_mod_ph$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

#phoenix model = arima AR-1 model




#try moving average model



