# clear workspace 
rm(list=ls())

# load needed libraries
library(readr)
library(vars)
library(zoo)
library(tseries)
library(rugarch)

# set working directory
setwd("/Users/samue/Downloads/Studium/Economics (Master - Vienna)/2. Semester/Macroeconometrics/Project/macroeconometrics_prices")

# import search trends
data <-  read_csv("btc-vs-gold-2004.csv", col_types = cols(Month = col_date(format = "%Y-%m")))
# import prices data:
gold_pr <- read_csv("gold-2004.csv", col_types = cols(DATE = col_date(format = "%Y-%m-%d")))
# import high-frequency prices for gold:
gold_HF <- read_csv('gold-2001-HF.csv', col_types = cols(DATE = col_date(format = '%Y-%m-%d'),GOLDPMGBD228NLBM = col_double()))

#renaming variables
gold_price <- gold_pr$GOLDPMGBD228NLBM
gold_price_HF <- gold_HF$GOLDPMGBD228NLBM
which(is.na(gold_price_HF))
gold_price_HF <- na.locf(gold_price_HF)
gold_date <- gold_pr$DATE
gold_search <- data$GOLD

# plot HF gold (normal and FDs)

plot(y = gold_price_HF, x = gold_HF$DATE, col = 'red', lwd = 0.01, type = 'l',
     xlab = 'Time', ylab = 'Daily Gold Price')
plot(y = gold_price_HF[2:5333]-gold_price_HF[1:5332], x = gold_HF$DATE[2:5333], 
     col = 'red', lwd = 0.01, type = 'l', xlab = 'Time', ylab = 'Daily Gold Price FDs',
     ylim = c(-150,150))
abline(h = c(-15,0,15), col = c('grey','green','grey'))

# save as time series:
gold_price <- ts(gold_price,frequency = 12,start = c(2004, 1), end = c(2021, 5))
gold_price_HF <- ts(gold_price_HF, frequency = 365, start = c(2001,1,2), end = c(2021,6,22))
# some issue with the ts() function resolving:
gold_price_HF <- gold_price_HF[1:5342]

# differenced gold prices:
gold_price_HF_FD <- gold_price_HF[2:5342]-gold_price_HF[1:5341]


df_test_gold_price_HF <- urca::ur.df(gold_price_HF, type = c('trend'),
                                  selectlags = 'AIC')
summary(df_test_gold_price_HF)

# augmented DF test with a trend on gold price
df_test_gold_price <- urca::ur.df(gold_price, type = c('trend'),
                                  selectlags = 'AIC')
summary(df_test_gold_price)

# augmented DF test with a trend on gold search index
df_test_gold_search <- urca::ur.df(gold_search, type = c('trend'),
                                   selectlags = 'AIC')
summary(df_test_gold_search)

# plot scaled variables (for the unit root test)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
plot(y=range01(gold_price),x=gold_pr$DATE, lwd = 2, type = 'l',
     ylab = 'Scaled Price and Search Interest',
     xlab = 'Time', col = 'red')
lines(y=range01(gold_search),x=gold_pr$DATE, lwd = 2, col = 'blue')
legend('topleft', legend = c('search','price'),
       col = c('blue','red'), bty = "n", pch = c(19,19))


# Auto arima daily:
forecast::auto.arima(gold_price_HF, ic = 'aic')
# Auto arima monthly:
forecast::auto.arima(gold_price, ic = 'aic')

# Comparison models:
arima(gold_price_HF, order = c(0,1,2))

#################
# ARCH Modeling #
#################

#fit the rugarch sGarch model 
spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution="norm")
test_garch_gold_price_FD <- ugarchfit(spec=spec, data=gold_price_HF_FD)
test_garch_gold_price_FD.
coef(test_garch_gold_price_FD)

# ARCH
gold_price_FD_arch1 <- garch(gold_price_HF_FD,c(0,1))     #ARCH   
gold_price_FD_arch1
AIC_arch_1<-AIC(gold_price_FD_arch1)
AIC_arch_1

# uARCH
spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution="norm")
egarch_gold_price_FD<- ugarchfit(spec=spec, data=gold_price_HF_FD, solver = 'hybrid')
egarch_gold_price_FD

#calculating AIC:
AIC_GARCH_2 <- 6.8428*length(gold_price_FD_clean)
AIC_GARCH_2


#Plot of squared residuals and est. cond. variance
gold_price_FD_res<-test_garch_gold_price_FD@fit$residuals
gold_price_FD_var<-test_garch_gold_price_FD@fit$var
plot((gold_price_FD_res)^2, type = "l", col="blue",ylab = 'residuals^2 / variance', main="GARCH(1,1)")
lines(gold_price_FD_var, col="green")
legend('topleft', legend = c('FD Gold Price reiduals^2','FD Gold Price vairance'),
       col = c('blue','green'), bty = "n", pch = c(19,19))




# plot PACF and ACF
par(mfrow=c(2,2))
pacf(gold_price_HF_FD, lwd = 5, col = 'red')
acf(gold_price_HF_FD, lwd = 5, col = 'red')
par(mfrow=c(1,1))

# plot gold price
plot(gold_pr$GOLDPMGBD228NLBM,type = 'l', lwd = 2, col = 'red',
     ylim = c(0,2000), main = 'Gold Price and Search Interest',
     xlab = 'Time', ylab = 'Search Interest (scaled )and Price (unscaled)')
# add gold search interest scaled up 
lines(25*data$GOLD, lwd = 2, col = 'blue')
legend('topleft', legend = c('search','price'),
       col = c('blue','red'), bty = "n", pch = c(19,19))

# plot gold search interest
gold_xxx <- ts(data$GOLD,frequency = 12,start = c(2004, 1), end = c(2021, 5))
plot(gold_xxx, type = 'l', lwd = 2, col = 'blue',
     ylim = c(0,90), ylab = 'Search Interest Index')


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
