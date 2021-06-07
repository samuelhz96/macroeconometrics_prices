# clear workspace 
rm(list=ls())

# load needed libraries
library(readr)
library(vars)

# set working directory
setwd("/Users/samue/Downloads/Studium/Economics (Master - Vienna)/2. Semester/Macroeconometrics/Project/macroeconometrics_prices")

# import search trends
data <- read.csv("btc-vs-gold-2004.csv")
# import prices data:
gold_pr <- read.csv("gold-2004.csv")

# plot gold price
plot(gold_pr$GOLDPMGBD228NLBM,type = 'l', lwd = 2, col = 'red',
     ylim = c(0,2000), main = 'Gold Price and Search Interest',
     xlab = 'Time', ylab = 'Search Interest (scaled )and Price (unscaled)')
# add gold search interest scaled up 
lines(25*data$GOLD, lwd = 2, col = 'blue')
legend('topleft', legend = c('search','price'),
       col = c('blue','red'), bty = "n", pch = c(19,19))


# create first differenced prices and search interest
t <- length(gold_pr$DATE)
gold_price_FD <- rep(0,t-1)
for(i in 2:209){gold_price_FD[i-1] <- gold_pr$GOLDPMGBD228NLBM[i]-gold_pr$GOLDPMGBD228NLBM[i-1]}
gold_search_FD <- rep(0,t-1)
for(i in 2:209){gold_search_FD[i-1] <- data$GOLD[i]-data$GOLD[i-1]}

# plot first differenced variables
plot(gold_price_FD, type = 'l', lwd = 1, col = 'red',
     xlab = 'Time', ylab = '1-Period Differences',
     main = 'First Differences: Gold Price and Search Interest')
lines(gold_search_FD*8, lwd = 1, col = 'blue')
legend('topleft', legend = c('search','price'),
       col = c('blue','red'), bty = "n", pch = c(19,19))

# plot ACF for unmodified variables:
par(mfrow=c(2,2))     # changes the plot layout to more easily compare them
acf(gold_pr$GOLDPMGBD228NLBM, main = 'ACF Gold Price')
acf(data$GOLD, main = 'ACF Gold Search Interest')

# plot ACF for differenced variables
acf(gold_price_FD,main = 'ACF Gold Price FD')
acf(gold_search_FD, main = 'ACF Gold Search Interest FD')
par(mfrow = c(1,1))   # revert layout changes


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
plot(range01(gold_pr$GOLDPMGBD228NLBM), lwd = 2, type = 'l',
     ylab = 'Scaled Price and Search Interest',
     xlab = 'Time', col = 'red')
lines(range01(data$GOLD), lwd = 2, col = 'blue')
legend('topleft', legend = c('search','price'),
       col = c('blue','red'), bty = "n", pch = c(19,19))

# save scaled variables
gold_price_scaled <- range01(gold_pr$GOLDPMGBD228NLBM)
gold_search_scaled <- range01(data$GOLD)

# create first difference on scaled variables:
gold_search_scaled_FD <- rep(0,t-1)
gold_price_scaled_FD <- rep(0,t-1)

for(i in 2:t-1){
  gold_price_scaled_FD[i-1] <- gold_price_scaled[i]-gold_price_scaled[i-1]
}
for(i in 2:t-1){
  gold_search_scaled_FD[i-1] <- gold_search_scaled[i]-gold_search_scaled[i-1]
}

# plot first differenced:
plot(gold_price_scaled_FD, lwd = 1, type = 'l',
     ylab = 'FD Scaled Price and Search Interest',
     xlab = 'Time', col = 'red')
lines(gold_search_scaled_FD, lwd = 1, col = 'blue')
legend('topleft', legend = c('search','price'),
       col = c('blue','red'), bty = "n", pch = c(19,19))

# plot ACFs
par(mfrow=c(2,2))     # changes the plot layout to more easily compare them
acf(gold_price_scaled, main = 'ACF Scaled Gold Price')
acf(gold_search_scaled, main = 'ACF Scaled Gold Search Interest')
acf(gold_price_scaled_FD,main = 'ACF Scaled Gold Price FD')
acf(gold_search_scaled_FD, main = 'ACF Scaled Gold Search Interest FD')
par(mfrow = c(1,1))   # revert layout changes

#####################################################
####### From here on: data saved as time series #####
#####################################################

# save variable vectors as time series format:
gold_price_scaled <- ts(gold_price_scaled, frequency = 12,
                        start = c(2004, 1), end = c(2021, 5))
gold_search_scaled <- ts(gold_search_scaled, frequency = 12,
                         start = c(2004,1), end = c(2021,5))

# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(gold_price_scaled, gold_search_scaled),
                   start = c(2004, 1), end = c(2021, 5))

# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 2)
summary(VAR_est)

# augmented df test on only the gold price
df_test_gold <- urca::ur.df(gold_price_scaled, type = c('drift'),
                            selectlags = 'BIC')
summary(df_test_gold)

# augmented df test on only the differenced gold price
df_test_gold_FD <- urca::ur.df(gold_price_scaled_FD, type = 'none',
                               selectlags = 'BIC')
summary(df_test_gold_FD)

# VAR model with unscaled prices
# save variable vectors as time series format:
gold_price <- ts(gold_pr$GOLDPMGBD228NLBM, frequency = 12,
                 start = c(2004, 1), end = c(2021, 5))
gold_search <- ts(data$GOLD, frequency = 12,
                  start = c(2004,1), end = c(2021,5))

# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(gold_price, gold_search),
                   start = c(2004, 1), end = c(2021, 5))

# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 1, type = 'both')
summary(VAR_est)

# compare the VAR to the AR(1) model for the prices
T <-length(gold_price)
gold_price_2 <- as.numeric(gold_price[-1])
gold_price_lagged <- as.numeric(gold_price[-T])

plot(gold_price_2, type = 'l', lwd = 1, col = 'red',
     main = 'Gold Price and Lagged Gold Price',
     ylab = 'Gold Price', xlab = 'Months from 01.2004')
lines(gold_price_lagged, lwd = 1, col = 'blue')
legend('topleft', legend = c('Lagged Price','Price'),
       col = c('blue','red'), bty = "n", pch = c(19,19))

# estimate model
gold_price_AR1 <- lm(gold_price_2 ~ gold_price_lagged)
# estimate robust standard errors
coeftest(gold_price_AR1, vcov. = vcovHC, type = "HC1")

# verify the 'by-hand' results with built-in function
ar.ols(gold_price, order.max = 1, intercept = T)
forecast::auto.arima(gold_price, ic = 'aic')


# Differenced AR(1) and ARCH Model for Gold Prices 
gold_price_FD <- ts(gold_price_FD, frequency = 12,
                    start = c(2004, 2), end = c(2021, 5))
plot(gold_price_FD, ylab = 'FD Gold Prices', col = 'red', lwd = 1.5)
ar1mod_FD <- arima(gold_price_FD, order = c(1,0,0))
ar1mod_FD
plot(forecast::arima.errors(ar1mod_FD),type = 'l', lwd = 1.5, col = 'blue',
     ylab = 'ARIMA residuals')
mean(forecast::arima.errors(ar1mod_FD))
