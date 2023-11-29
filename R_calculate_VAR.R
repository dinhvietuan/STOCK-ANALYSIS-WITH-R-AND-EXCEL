# Load the quantmod, PerformanceAnalytics and PortfolioAnalytics libraries
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

# Define the tickers and weights for the portfolio
tickers <- c("IBM", "DELL", "GOOG", "MSFT", "ORCL")
weights <- c(.20, 0.20,.20,0.20,0.20)

# Create an empty matrix for portfolio prices
portfolioPrices <- NULL

# Loop through the tickers and get the closing prices for each
for (Ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2017-12-23", to= "2022-12-23", 
                                            periodicity = "daily", auto.assign=FALSE)[,4])
}

# Calculate the returns for the portfolio
portfolioReturns <- na.omit(ROC(portfolioPrices))

# Calculate the Value at Risk (VaR) for the portfolio using the Gaussian method, with a confidence level of 95%
PorVaR.Gaus <- VaR(portfolioReturns, p=0.05, weights=weights
                   ,portfolio_method = "component",method="gaussian")

# Print the VaR results
print(PorVaR.Gaus)

# Get the contributions to VaR for each stock
contribution <- PorVaR.Gaus$contribution

# Print the contributions
print(contribution)

# Calculate the percentage contributions to VaR for each stock
pct_contrib_VaR <- PorVaR.Gaus$pct_contrib_VaR*100

# Print the percentage contributions
print(pct_contrib_VaR)

#plot

library(ggplot2)
contribution_df <- data.frame(tickers, contribution)
colnames(contribution_df) <- c("Stock", "Contribution")
ggplot(contribution_df, aes(x = Stock, y = Contribution)) + 
  geom_bar(stat = "identity", fill = "skyblue") + 
  ggtitle("VaR Contribution") + xlab("Stocks") + ylab("Contribution")

#plot percentage
pct_contrib_VaR_df = data.frame(tickers, pct_contrib_VaR)
print(pct_contrib_VaR_df)
ggplot(pct_contrib_VaR_df, aes(x = tickers, y = pct_contrib_VaR)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(Percentage_Contribution,2),"%"), y = Percentage_Contribution, x = Stock),
            position = position_stack(vjust = 0.5)) +
  ggtitle("VaR Percentage Contribution") + xlab("Stocks") + ylab("Percentage Contribution")




#This code creates a portfolio of stocks using the R programming language and the quantmod, PerformanceAnalytics, and PortfolioAnalytics libraries.

#The code first loads the necessary libraries: quantmod, PerformanceAnalytics, and PortfolioAnalytics
#Then it defines a variable "tickers" that contains a list of the ticker symbols for IBM, DELL, GOOG, MSFT, and ORCL.
#The variable "weights" is defined as a list of equal weights for each ticker (.20 for each) in the portfolio
#Then it creates a for loop to iterate through the list of tickers, and for each ticker, it retrieves historical stock data using the function getSymbols.yahoo() and stores it in the variable "portfolioPrices". The function gets the closing price of the stock from 2017-12-23 to 2022-12-23 with daily periodicity.
#The next step is to use the ROC() function to calculate the rate of return for each stock in the portfolio and store it in the variable "portfolioReturns"
#The final step is to calculate the Value at Risk (VaR) using the VaR() function, with a probability level of 0.05, using the weights specified earlier in the "weights" variable and the portfolio_method = "component" and method = "gaussian" which means it use the gaussian method to calculate VaR.
#The calculated VaR is then printed.
#Overall, this code creates a portfolio of stocks, calculates their historical returns, and then calculates the Value at Risk (VaR) of the portfolio using the Gaussian method. The VaR is a measure of the potential loss of the portfolio over a given time period (in this case, with a 5% probability).



