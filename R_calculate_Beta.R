# Importing libraries
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)
# Define the tickers and weights of the portfolio
tickers <- c("IBM", "DELL", "GOOG", "MSFT", "ORCL")
weights <- c(.2, .2, .2, .2, .2)
# Retrieve the historical prices for the tickers, the portfolioPrices object will be a matrix where the rows are 
# the dates and the columns are the prices for each ticker
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2017-12-23", to= "2022-12-23", periodicity = "daily",
                                            auto.assign=FALSE)[,4])
# Checking the first and last rows of the portfolioPrices matrix
head(portfolioPrices)
length(portfolioPrices)
tail(portfolioPrices)

# Retrieve the historical prices for the benchmark
benchmarkPrices <- getSymbols.yahoo("SPY", 
                                    from="2017-12-23", to = "2022-12-23", 
                                    periodicity = "daily", 
                                    auto.assign=FALSE)[,4]

# Count the number of missing values in the benchmark prices
colSums(is.na(benchmarkPrices))

# Calculate the returns for the benchmark
# ROC function is used to calculate returns, type is set to "discrete"
ROC_benchmarkPrices <- ROC(benchmarkPrices, type = "discrete")

# Omit any rows with NA values
benchmarkReturns <- na.omit(ROC_benchmarkPrices)

#Rename Columns
colnames(portfolioPrices) <- tickers

#Get sum of NA per column
colSums(is.na(portfolioPrices))

#Plot
plot(portfolioPrices, legend = tickers)

#Calculate Returns For DF
# ROC function is used to calculate returns, type is set to "discrete"
ROC_portfolioPrices <- ROC(portfolioPrices, type="discrete")

# Omit any rows with NA values
dailyReturns <- na.omit(ROC_portfolioPrices)

#Calculate Portfolio Returns
# Return.portfolio function is used to calculate returns, with the weights specified
portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)

#Plot Performance
# chart.CumReturns function is used to plot cumulative returns
chart.CumReturns(portfolioReturn)

# charts.PerformanceSummary function is used to plot performance summary
charts.PerformanceSummary(portfolioReturn)

#Calculate Metrics with the risk-free rate =1.5% per year
# CAPM.beta function is used to calculate beta
CAPM.beta(portfolioReturn, benchmarkReturns, .015/252)

# CAPM.beta.bull function is used to calculate beta during bullish market
CAPM.beta.bull(portfolioReturn, benchmarkReturns, .015/252)

# CAPM.beta.bear function is used to calculate beta during bearish market
CAPM.beta.bear(portfolioReturn, benchmarkReturns, .015/252)

#CAPM.alpha(portfolioReturn, benchmarkReturns, .015/252)
CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .015/252)

SharpeRatio(portfolioReturn, Rf = .015/252, p = 0.95, FUN = "StdDev",
            weights = NULL, annualize = FALSE)

table.AnnualizedReturns(portfolioReturn, Rf=.015/252, geometric=TRUE)

