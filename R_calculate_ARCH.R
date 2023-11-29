library(tseries)
library(fGarch)
library(rugarch)
library(gogarch)
library(PerformanceAnalytics)
library(zoo)
library(quantmod)
library(xts)

# Retrieve MSFT stock prices from Yahoo Finance
MSFT <-getSymbols.yahoo("MSFT", from="2017-12-23", 
                             to ="2022-12-23", periodicity = "daily",
                             auto.assign=FALSE)[,4]
# Remove any missing values
na.omit(MSFT)

# Plot IBM stock prices
plot(MSFT)

# Create a zoo object for plotting
library(zoo)
MSFT<- zoo(MSFT)

# Create a new time series object for the first difference of the natural log of the closing prices
RMSFT<-ts(diff(log(MSFT$MSFT.Close)))

# Check stationary of data
adf.test(RMSFT)

# Plot the new time series
plot(RMSFT, col='BLUE')

# Fit several GARCH models to the time series
g10<-garch(RMSFT, order=c(1,0), trace=FALSE)
g11<-garch(RMSFT, order=c(1,1), trace=FALSE)
g20<-garch(RMSFT, order=c(2,0), trace=FALSE)
g21<-garch(RMSFT, order=c(2,1), trace=FALSE)
g22<-garch(RMSFT, order=c(2,2), trace=FALSE)
g02<-garch(RMSFT, order=c(0,2), trace=FALSE)
g12<-garch(RMSFT, order=c(1,2), trace=FALSE)

# Create a list of the GARCH models
models <- list(g10, g11, g20, g21, g22, g02, g12)

# Extract the AIC values from the models
AIC_values <- sapply(models, AIC)

# Find the index of the model with the lowest AIC value
best_model_index <- which.min(AIC_values)

# Select the best model
best_model <- models[[best_model_index]]

# Print the best model's AIC value
print((best_model))

plot(g11)

#build model garch 1-1 with garchFit
m2 <- garchFit(~garch(1,1), data = RMSFT, trace = F)
summary(m2)
plot(m2)
plot(m2, which="all")
predict(m2, 5)
#build model with urgrachfit

specmodel = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = 'iGARCH', garchOrder = c(1,1)))
          
m3  <- ugarchfit(data = RMSFT, spec = specmodel)
summary(m3)
plot(m3, which="all")
#forecast
ugarchforecast(m3, n.ahead = 5)
