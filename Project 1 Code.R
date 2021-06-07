# clear entire workspace
rm(list = ls())

# load libraries needed
library(readr)

# load datasets for CPI and gold prices
CPI <- read_csv("C:/Users/samue/Downloads/Studium/Economics (Master - Vienna)/2. Semester/Macroeconometrics/Project/CPI - 1982.csv")
GOLD <- read_csv("C:/Users/samue/Downloads/Studium/Economics (Master - Vienna)/2. Semester/Macroeconometrics/Project/GOLD.csv")
METAL <- read_csv("C:/Users/samue/Downloads/Studium/Economics (Master - Vienna)/2. Semester/Macroeconometrics/Project/Metal-prod Prices - 1982.csv")
CONF <- read_csv("C:/Users/samue/Downloads/Studium/Economics (Master - Vienna)/2. Semester/Macroeconometrics/Project/Consumer Confidence.csv")
CONF <- CONF[-c(1:27),]

View(CPI);View(GOLD);View(METAL);View(CONF)

# match time horizons:
# CPI <- CPI[-(1:255),]
# View(CPI)

# properly name time series:
gold <- GOLD$GOLDAMGBD228NLBM;
goldTS <- ts(gold, start = c(1968,4), frequency = 12)
cpi <- CPI$CPIAUCSL
cpiTS <- ts(cpi, start = c(1968,4), frequency = 12)
metal <- METAL$WPU10
metalTS <- ts(metal, start = c(1968,4), frequency = 12)
confidence <- CONF$CSCICP03USM665S
confidenceTS <- ts(confidence, start = c(1968,4), frequency = 12)


# index gold prices to prices in 1982

# find mean price in 1982:
indexed <- mean(gold[166:177])
gold <- (gold/indexed)*100


plot(GOLD$DATE, gold, type = 'l', lwd = 2)
lines(CPI$DATE, cpi, type = 'l', lwd = 2, col = 'red')
lines(METAL$DATE, metal, type = 'l', lwd = 2, col = 'blue')


# look at gold being first-differenced:
t <- length(gold)
goldFD <- rep(0,t-1)
for(i in 2:t) goldFD[i-1] <- gold[i]-gold[i-1]
plot(GOLD$DATE[-1],goldFD, type = 'l', lwd = 1, col = 'red')


# look at CPI being first-differenced
t3 <- length(cpi)
cpiFD <- rep(0,t3-1)
for(i in 2:t3) cpiFD[i-1] <- cpi[i]-cpi[i-1]
plot(CPI$DATE[-i],cpiFD, type = 'l', lwd = 1, col = 'blue')


# look at metal prices first-differenced
t2 <- length(metal)
metalFD <- rep(0,t2-1)
for(i in 2:t2) metalFD[i-1] <- metal[i]-metal[i-1]
plot(METAL$DATE[-1],metalFD, type = 'l', lwd = 1, col = 'green')


# plot the consumer confidence and gold
plot(GOLD$DATE[-1],goldFD, type = 'l', lwd = 1, col = 'green')
lines(CONF$DATE,confidence-100, lwd = 1, col = 'red')


# autocorrelation functions:
par(mfrow=c(2,2))
acf(confidence)
acf(gold)
acf(metal)
acf(cpi)


# now with FD'd variables:
acf(confidence)
acf(goldFD)
acf(metalFD)
acf(cpiFD)

# reset plot parameters
par(mfrow=c(1,1))


# combine all plots:
plot(GOLD$DATE[-1],goldFD, type = 'l', lwd = 1, col = 'red')
lines(METAL$DATE[-1],metalFD, type = 'l', lwd = 1, col = 'green')
lines(CPI$DATE[-i],cpiFD, type = 'l', lwd = 1, col = 'blue')
lines(CONF$DATE,confidence, type = 'l', lwd = 1, col = 'violet')

# look at metal prices more detailed:
plot(METAL$DATE[-1],metalFD, type = 'l', lwd = 1, col = 'red')
plot(METAL$DATE,metal, type = 'l', lwd = 1, col = 'blue')



# AR model on the gold prices
goldAR1 <- arima(goldTS , order = c(1,0,0))
goldAR1



