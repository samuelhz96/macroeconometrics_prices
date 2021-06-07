# clear workspace 
rm(list=ls())

# load needed libraries
library(readr)

# set working directory
setwd("/Users/samue/Downloads/Studium/Economics (Master - Vienna)/2. Semester/Macroeconometrics/Project")

# import search trends
data <- read.csv("btc-vs-gold-2004.csv")
# import prices data:
gold_pr <- read.csv("gold-2004.csv")




# plot gold price
plot(gold_pr$GOLDPMGBD228NLBM,type = 'l', lwd = 2, col = 'red',
     ylim = c(0,2000))
# add gold search interest scaled up 
lines(25*data$GOLD, lwd = 2, col = 'blue')


# create first differenced prices and search interest
t <- length(gold_pr$DATE)
gold_price_FD <- rep(0,t-1)
for(i in 2:209){gold_price_FD[i-1] <- gold_pr$GOLDPMGBD228NLBM[i]-gold_pr$GOLDPMGBD228NLBM[i-1]}
gold_search_FD <- rep(0,t-1)
for(i in 2:209){gold_search_FD[i-1] <- data$GOLD[i]-data$GOLD[i-1]}

# plot first differenced variables
plot(gold_price_FD, type = 'l', lwd = 1, col = 'red')
lines(gold_search_FD*8, lwd = 1, col = 'blue')


# plot ACF for differenced variables
par(mfrow=c(1,2))     # changes the plot layout to more easily compare them
acf(gold_price_FD,main = 'Autocorrelation Gold Price FD')
acf(gold_search_FD, main = 'Autocorrelation Gold Search Interest FD')
par(mfrwo = c(1,1))   # revert layout changes

