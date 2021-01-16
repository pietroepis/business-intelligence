# Pietro Epis - 845045

library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(tseries)
library(shiny)
library(lubridate)
library(forecast)

rm(list = ls())

# Colors associated to stocks along the whole application
stocks_colors = c("#cc3300", "#2eb82e", "#0052cc", "#ff9900", "#b3b300", "#cc0099")

#### DATA SUMMARY ####

date_from <- as.Date("2018-10-01")
date_to <- as.Date("2020-10-31")

# Sector: Consumer Discretionary
# SBUX    Starbucks Corp.
# MCD     McDonald's Corp.
SBUX <- getSymbols("SBUX", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
MCD <- getSymbols("MCD", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)

# Sector: Financials
# AXP     American Express Co
# AON     Aon plc
AXP <- getSymbols("AXP", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
AON <- getSymbols("AON", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)

# Sector: Consumer Staples
# KO      Coca Cola
# PEP   PepsiCo, Inc.
KO <- getSymbols("KO", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
PEP <- getSymbols("PEP", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)

stocks <- merge(SBUX$SBUX.Adjusted, MCD$MCD.Adjusted, AXP$AXP.Adjusted, AON$AON.Adjusted, 
                KO$KO.Adjusted, PEP$PEP.Adjusted)
stocks <- na.omit(stocks) # remove possible NA values
colnames(stocks) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")

# Adjusted Close Prices Plot
dygraph(stocks, xlab = "Date", ylab = "Adjusted Close", main = "Adjusted Close Price") %>%
  dyOptions(colors = stocks_colors) %>%
  dyOptions(strokeWidth = 2.0)

#### DESCRIPTIVE ANALYTICS 1/3 ####

# Set monthly granularity
stocks = to.monthly(stocks, OHLC = F)

# Calculate Simple and Continuously Compounded Returns
# Discard first row, it's NA
stocks_simple_returns = CalculateReturns(stocks, method = "simple")[-1,]
stocks_cc_returns = CalculateReturns(stocks, method = "compound")[-1,]

# Simple Returns Plot
dygraph(stocks_simple_returns, xlab = "Date", ylab = "Return", main = "Simple Returns") %>%
  dyOptions(colors = stocks_colors) %>%
  dyOptions(strokeWidth = 2.0)
# Continuously Compounded Returns Plot
dygraph(stocks_cc_returns, xlab = "Date", ylab = "Return", main = "Continuously Compounded Returns") %>%
  dyOptions(colors = stocks_colors) %>%
  dyOptions(strokeWidth = 2.0)

# For each stock, show histogram and quantiles lines
for (i in 1:dim(stocks_cc_returns)[2]){
  hist(stocks_cc_returns[,i], main = colnames(stocks_cc_returns)[i], xlab = "Returns")
  q = quantile(stocks_cc_returns[,i]);
  abline(v=q[1], col="red", lwd=2) 
  abline(v=q[2], col="blue", lwd=2) 
  abline(v=q[3], col="green", lwd=2.5) 
  abline(v=q[4], col="blue", lwd=2) 
  abline(v=q[5], col="red", lwd=2)
}
  
#### DESCRIPTIVE ANALYTICS 2/3 ####

# For every stock, show histogram, PDF function, Boxplot and Q-Q Plot of CC Returns

# SBUX
par(mfrow = c(2, 2)) # Split window in four sections
hist(stocks_cc_returns$SBUX, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - SBUX")
points(density(stocks_cc_returns$SBUX), type="l", lwd = 2, col = stocks_colors[1]) # Add PDF line
plot(density(stocks_cc_returns$SBUX), type="l", lwd = 2, col = stocks_colors[1], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - SBUX")
boxplot(as.numeric(stocks_cc_returns$SBUX), horizontal = T, col = stocks_colors[1], main = "Boxplot - SBUX")
qqnorm(stocks_cc_returns$SBUX, main = "Q-Q Plot - SBUX")
qqline(stocks_cc_returns$SBUX, lwd = 2, col = stocks_colors[1])

# Display statistical indices in Console
mean(stocks_cc_returns$SBUX)
var(stocks_cc_returns$SBUX)[1]
sd(stocks_cc_returns$SBUX)
skewness(stocks_cc_returns$SBUX)
kurtosis(stocks_cc_returns$SBUX)
quantile(stocks_cc_returns$SBUX)

# MCD
par(mfrow = c(2, 2))
hist(stocks_cc_returns$MCD, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - MCD")
points(density(stocks_cc_returns$MCD), type="l", lwd = 2, col = stocks_colors[2])
plot(density(stocks_cc_returns$MCD), type="l", lwd = 2, col = stocks_colors[2], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - MCD")
boxplot(as.numeric(stocks_cc_returns$MCD), horizontal = T, col = stocks_colors[2], main = "Boxplot - MCD")
qqnorm(stocks_cc_returns$MCD, main = "Q-Q Plot - MCD")
qqline(stocks_cc_returns$MCD, lwd = 2, col = stocks_colors[2])

mean(stocks_cc_returns$MCD)
var(stocks_cc_returns$MCD)[1]
sd(stocks_cc_returns$MCD)
skewness(stocks_cc_returns$MCD)
kurtosis(stocks_cc_returns$MCD)
quantile(stocks_cc_returns$MCD)

# AXP
par(mfrow = c(2, 2))
hist(stocks_cc_returns$AXP, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - AXP")
points(density(stocks_cc_returns$AXP), type="l", lwd = 2, col = stocks_colors[3])
plot(density(stocks_cc_returns$AXP), type="l", lwd = 2, col = stocks_colors[3], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - AXP")
boxplot(as.numeric(stocks_cc_returns$AXP), horizontal = T, col = stocks_colors[3], main = "Boxplot - AXP")
qqnorm(stocks_cc_returns$AXP, main = "Q-Q Plot - AXP")
qqline(stocks_cc_returns$AXP, lwd = 2, col = stocks_colors[3])

mean(stocks_cc_returns$AXP)
var(stocks_cc_returns$AXP)[1]
sd(stocks_cc_returns$AXP)
skewness(stocks_cc_returns$AXP)
kurtosis(stocks_cc_returns$AXP)
quantile(stocks_cc_returns$AXP)

# AON
par(mfrow = c(2, 2))
hist(stocks_cc_returns$AON, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - AON")
points(density(stocks_cc_returns$AON), type="l", lwd = 2, col = stocks_colors[4])
plot(density(stocks_cc_returns$AON), type="l", lwd = 2, col = stocks_colors[4], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - AON")
boxplot(as.numeric(stocks_cc_returns$AON), horizontal = T, col = stocks_colors[4], main = "Boxplot - AON")
qqnorm(stocks_cc_returns$AON, main = "Q-Q Plot - AON")
qqline(stocks_cc_returns$AON, lwd = 2, col = stocks_colors[4])

mean(stocks_cc_returns$AON)
var(stocks_cc_returns$AON)[1]
sd(stocks_cc_returns$AON)
skewness(stocks_cc_returns$AON)
kurtosis(stocks_cc_returns$AON)
quantile(stocks_cc_returns$AON)

# KO
par(mfrow = c(2, 2))
hist(stocks_cc_returns$KO, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - KO")
points(density(stocks_cc_returns$KO), type="l", lwd = 2, col = stocks_colors[5])
plot(density(stocks_cc_returns$KO), type="l", lwd = 2, col = stocks_colors[5], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - KO")
boxplot(as.numeric(stocks_cc_returns$KO), horizontal = T, col = stocks_colors[5], main = "Boxplot - KO")
qqnorm(stocks_cc_returns$KO, main = "Q-Q Plot - KO")
qqline(stocks_cc_returns$KO, lwd = 2, col = stocks_colors[5])

mean(stocks_cc_returns$KO)
var(stocks_cc_returns$KO)[1]
sd(stocks_cc_returns$KO)
skewness(stocks_cc_returns$KO)
kurtosis(stocks_cc_returns$KO)
quantile(stocks_cc_returns$KO)

# PEP
par(mfrow = c(2, 2))
hist(stocks_cc_returns$PEP, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - PEP")
points(density(stocks_cc_returns$PEP), type="l", lwd = 2, col = stocks_colors[6])
plot(density(stocks_cc_returns$PEP), type="l", lwd = 2, col = stocks_colors[6], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - PEP")
boxplot(as.numeric(stocks_cc_returns$PEP), horizontal = T, col = stocks_colors[6], main = "Boxplot - PEP")
qqnorm(stocks_cc_returns$PEP, main = "Q-Q Plot - PEP")
qqline(stocks_cc_returns$PEP, lwd = 2, col = stocks_colors[6])

mean(stocks_cc_returns$PEP)
var(stocks_cc_returns$PEP)[1]
sd(stocks_cc_returns$PEP)
skewness(stocks_cc_returns$PEP)
kurtosis(stocks_cc_returns$PEP)
quantile(stocks_cc_returns$PEP)

# Merged Q-Q Plot
# Show all Q-Q Plots in a single window
par(mfrow=c(3,2))
qqnorm(stocks_cc_returns$SBUX, main = "QQ-PLOT SBUX", col = stocks_colors[1])
qqline(stocks_cc_returns$SBUX)

qqnorm(stocks_cc_returns$MCD, main = "QQ-PLOT MCD", col = stocks_colors[2])
qqline(stocks_cc_returns$MCD)

qqnorm(stocks_cc_returns$AXP, main = "QQ-PLOT AXP", col = stocks_colors[3])
qqline(stocks_cc_returns$AXP)

qqnorm(stocks_cc_returns$AON, main = "QQ-PLOT AON", col = stocks_colors[4])
qqline(stocks_cc_returns$AON)

qqnorm(stocks_cc_returns$KO, main = "QQ-PLOT KO", col = stocks_colors[5])
qqline(stocks_cc_returns$KO)

qqnorm(stocks_cc_returns$PEP, main = "QQ-PLOT PEP", col = stocks_colors[6])
qqline(stocks_cc_returns$PEP)

# Merged Boxplot
par(mfrow = c(1, 1))
boxplot(as.numeric(stocks_cc_returns$SBUX), as.numeric(stocks_cc_returns$MCD), as.numeric(stocks_cc_returns$AXP),
        as.numeric(stocks_cc_returns$AON), as.numeric(stocks_cc_returns$KO), as.numeric(stocks_cc_returns$PEP),
        names = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP"), main = "CC Returns Boxplot")
abline(h = 0, lwd = 2, lty = 2) # Draw dashed line on zero 

#### DESCRIPTIVE ANALYTICS 3/3 ####

# Calculate covariance of stocks CC Returns and print it in Console
covariance = cov(stocks_cc_returns)
print(covariance)

# Calculate correlation coefficient and represent it through heatmap
correlation = round(cor(stocks_cc_returns), 2)
heatmap(correlation, main = "Correlation", scale = "none", Colv = NA, Rowv = NA)

SBUX_num <- as.numeric(stocks_cc_returns$SBUX)
MCD_num <- as.numeric(stocks_cc_returns$MCD)
AXP_num <- as.numeric(stocks_cc_returns$AXP)
AON_num <- as.numeric(stocks_cc_returns$AON)
KO_num <- as.numeric(stocks_cc_returns$KO)
PEP_num <- as.numeric(stocks_cc_returns$PEP)
# Show pairwise scatter plots
pairs(cbind(SBUX_num, MCD_num, AXP_num, AON_num, KO_num, PEP_num), pch=18, 
      col="blue", main="Correlation")

#### PREDICTIVE ANALYTICS ####

n = 80 # Training Set number of months
m = 30 # Test Set number of months
l = 10 # Final Set number of months

# Reload data considering a wider period of time, to allow forecasting on n + m + l months
dataset <- getSymbols("SBUX", to = date_to, src = "yahoo", auto.assign = FALSE,)$SBUX.Adjusted
dataset <- cbind(dataset, getSymbols("MCD", to = date_to, src = "yahoo", auto.assign = FALSE)$MCD.Adjusted)
dataset <- cbind(dataset, getSymbols("AXP", to = date_to, src = "yahoo", auto.assign = FALSE)$AXP.Adjusted)
dataset <- cbind(dataset, getSymbols("AON", to = date_to, src = "yahoo", auto.assign = FALSE)$AON.Adjusted)
dataset <- cbind(dataset, getSymbols("KO", to = date_to, src = "yahoo", auto.assign = FALSE)$KO.Adjusted)
dataset <- cbind(dataset, getSymbols("PEP", to = date_to, src = "yahoo", auto.assign = FALSE)$PEP.Adjusted)
colnames(dataset) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")

dataset_returns <- diff(log(dataset)) # Calculate CC returns in a different way than before
dataset_returns <- na.omit(dataset_returns)
colnames(dataset_returns) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
dataset_returns <- aggregate(dataset_returns, index(dataset_returns), tail, 1) # Restore data in a convenient format

dataset_monthly <- to.monthly(dataset_returns, OHLC = F)

# Show Seasonal Decomposition plot for every stock
plot(stl(dataset_monthly[,1], s.window="period" ), main=paste("SBUX - Seasonal Decomposition"))
plot(stl(dataset_monthly[,2], s.window="period" ), main=paste("MCD - Seasonal Decomposition"))
plot(stl(dataset_monthly[,3], s.window="period" ), main=paste("AXP - Seasonal Decomposition"))
plot(stl(dataset_monthly[,4], s.window="period" ), main=paste("AON - Seasonal Decomposition"))
plot(stl(dataset_monthly[,5], s.window="period" ), main=paste("KO - Seasonal Decomposition"))
plot(stl(dataset_monthly[,6], s.window="period" ), main=paste("PEP - Seasonal Decomposition"))

# Extract subset from dataset in relation to previously listed number of months for each step of forecasting
trainSet <- dataset_monthly[(dim(dataset_monthly)[1] - (n + m + l) + 1) : (dim(dataset_monthly)[1] - (m + l))]
testSet <- dataset_monthly[(dim(dataset_monthly)[1] - (m + l) + 1) : (dim(dataset_monthly)[1] - l)]
finalSet <- dataset_monthly[(dim(dataset_monthly)[1] - l + 1) : (dim(dataset_monthly)[1])]

# Detect the combination of AR [1, 10] and MA [1, 10] that produces the lower RMSE error
# and return it as best model for forecasting
getBestModel <- function(train, test, stockName){
  minErrorModel <- NULL
  
  for(ar in 1:10){
    for(ma in 1:10){
      fit <- arima(train, order = c(ar, 0, ma), method="ML")
      # Consider interval of confidence of 80 and 95 %
      arma.forecast <- forecast(fit, h = length(test), level = c(95,80))
      plot(arma.forecast, main = paste0("Forecasts for ARIMA(", ar, ", 0, ", ma, ") - ", stockName))
      lines(test)
      
      rmse <- accuracy(arma.forecast, test)[2] 
      
      if (is.null(minErrorModel) || rmse < minErrorModel[3])
        minErrorModel <- c(ar, ma, rmse, arma.forecast)
    }
  }
  
  return(minErrorModel)
}

# Get AR and MA parameters for every stock
SBUX_model <- getBestModel(trainSet$SBUX, testSet$SBUX, "SBUX")
MCD_model <- getBestModel(trainSet$MCD, testSet$MCD, "MCD")
AXP_model <- getBestModel(trainSet$AXP, testSet$AXP, "AXP")
AON_model <- getBestModel(trainSet$AON, testSet$AON, "AON")
KO_model <- getBestModel(trainSet$SBUX, testSet$KO, "KO")
PEP_model <- getBestModel(trainSet$SBUX, testSet$PEP, "PEP")

# Return predictions on l months by using ar and ma as parameters for forecast
predictReturns <- function(train, ar, ma){
  fit <- arima(train, order = c(ar, 0, ma))
  arma.forecast <- forecast(fit, h = l)
  
  return(arma.forecast)
}

# Do prediction on l months by using n + m months as training set
# Afterwards print RMSE of predicted values
print("ACCURACY")
SBUX_pred_returns <- predictReturns(rbind(trainSet$SBUX, testSet$SBUX), SBUX_model[[1]], SBUX_model[[2]])
cat("SBUX: ", accuracy(SBUX_pred_returns, finalSet$SBUX)[2])
MCD_pred_returns <- predictReturns(rbind(trainSet$MCD, testSet$MCD), MCD_model[[1]], MCD_model[[2]])
cat("MCD: ", accuracy(MCD_pred_returns, finalSet$MCD)[2])
AXP_pred_returns <- predictReturns(rbind(trainSet$AXP, testSet$AXP), AXP_model[[1]], AXP_model[[2]])
cat("AXP: ", accuracy(AXP_pred_returns, finalSet$AXP)[2])
AON_pred_returns <- predictReturns(rbind(trainSet$AON, testSet$AON), AON_model[[1]], AON_model[[2]])
cat("AON: ", accuracy(AON_pred_returns, finalSet$AON)[2])
KO_pred_returns <- predictReturns(rbind(trainSet$KO, testSet$KO), KO_model[[1]], KO_model[[2]])
cat("KO: ", accuracy(KO_pred_returns, finalSet$KO)[2])
PEP_pred_returns <- predictReturns(rbind(trainSet$PEP, testSet$PEP), PEP_model[[1]], PEP_model[[2]])
cat("PEP: ", accuracy(PEP_pred_returns, finalSet$PEP)[2])

# Calculate Markowitz Optimal Portfolio basing on l-months predicted values
pred_returns = cbind(SBUX_pred_returns$mean, MCD_pred_returns$mean, AXP_pred_returns$mean,
                     AON_pred_returns$mean, KO_pred_returns$mean, PEP_pred_returns$mean)
MOP_pred <- portfolio.optim(x = pred_returns)

cat("SBUX Weight: ", MOP_pred$pw[1], "\n")
cat("MCD Weight: ", MOP_pred$pw[2], "\n")
cat("AXP Weight: ", MOP_pred$pw[3], "\n")
cat("AON Weight: ", MOP_pred$pw[4], "\n")
cat("KO Weight: ", MOP_pred$pw[5], "\n")
cat("PEP Weight: ", MOP_pred$pw[6], "\n")

# Show optimal weights with a barplot
barplot(MOP_pred$pw, col = stocks_colors, names = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP"))

#### BETA COMPUTATION ####

# Fetch data of S&P500 market index (to which all considered stocks belong)
SP500 <- getSymbols("^GSPC", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
SP500 <- to.monthly(SP500)
SP500_return <- na.omit(CalculateReturns(SP500$SP500.Adjusted, method = "compound"))
colnames(SP500_return) <- c("SP500")

n_periods <- dim(SP500_return)[1]
delta <- 4

SBUX_betas <- NULL
MCD_betas <- NULL
AXP_betas <- NULL
AON_betas <- NULL
KO_betas <- NULL
PEP_betas <- NULL

# For every month in the two years period, calculate beta coefficient in relation to
# the previous delta months
for (index in (delta + 1):n_periods)
{
  from <- index - delta
  to <- index - 1
  
  SBUX_beta <- as.xts(cov(stocks_cc_returns$SBUX[from:to], SP500_return[from:to]) / var(SP500_return[from:to]),
                      order.by = index(stocks_cc_returns$SBUX[to]))
  MCD_beta <- as.xts(cov(stocks_cc_returns$MCD[from:to], SP500_return[from:to]) / var(SP500_return[from:to]),
                      order.by = index(stocks_cc_returns$MCD[to]))
  AXP_beta <- as.xts(cov(stocks_cc_returns$AXP[from:to], SP500_return[from:to]) / var(SP500_return[from:to]),
                     order.by = index(stocks_cc_returns$AXP[to]))
  AON_beta <- as.xts(cov(stocks_cc_returns$AON[from:to], SP500_return[from:to]) / var(SP500_return[from:to]),
                     order.by = index(stocks_cc_returns$AON[to]))
  KO_beta <- as.xts(cov(stocks_cc_returns$KO[from:to], SP500_return[from:to]) / var(SP500_return[from:to]),
                     order.by = index(stocks_cc_returns$KO[to]))
  PEP_beta <- as.xts(cov(stocks_cc_returns$PEP[from:to], SP500_return[from:to]) / var(SP500_return[from:to]),
                     order.by = index(stocks_cc_returns$PEP[to]))
  
  if (is.null(SBUX_betas))
  {
    SBUX_betas <- SBUX_beta
    MCD_betas <- MCD_beta
    AXP_betas <- AXP_beta
    AON_betas <- AON_beta
    KO_betas <- KO_beta
    PEP_betas <- PEP_beta
  }
  else
  {
    SBUX_betas <- rbind(SBUX_betas, SBUX_beta)
    MCD_betas <- rbind(MCD_betas, MCD_beta)
    AXP_betas <- rbind(AXP_betas, AXP_beta)
    AON_betas <- rbind(AON_betas, AON_beta)
    KO_betas <- rbind(KO_betas, KO_beta)
    PEP_betas <- rbind(PEP_betas, PEP_beta)
  }
}

# Show the trend of beta over time
plot(SBUX_betas, main = "SBUX Beta")
plot(MCD_betas, main = "MCD Beta")
plot(AXP_betas, main = "AXP Beta")
plot(AON_betas, main = "AON Beta")
plot(KO_betas, main = "KO Beta")
plot(PEP_betas, main = "PEP Beta")

# Calculate beta on the whole two years period
SBUX_beta_whole <- cov(stocks_cc_returns$SBUX, SP500_return) / var(SP500_return)
MCD_beta_whole <- cov(stocks_cc_returns$MCD, SP500_return) / var(SP500_return)
AXP_beta_whole <- cov(stocks_cc_returns$AXP, SP500_return) / var(SP500_return)
AON_beta_whole <- cov(stocks_cc_returns$AON, SP500_return) / var(SP500_return)
KO_beta_whole <- cov(stocks_cc_returns$KO, SP500_return) / var(SP500_return)
PEP_beta_whole <- cov(stocks_cc_returns$PEP, SP500_return) / var(SP500_return)

cat("Beta nell'intero periodo\n")
cat("SBUX: ", SBUX_beta_whole)
cat("MCD: ", MCD_beta_whole)
cat("AXP: ", AXP_beta_whole)
cat("AON: ", AON_beta_whole)
cat("KO: ", KO_beta_whole)
cat("PEP: ", PEP_beta_whole)

# Fictitious Risk Free asset return rate, to simulate expected return calculation
risk_free_rate <- 0.02
# Geometric Average (to get return on 10 months)
rf_return <- (1 + risk_free_rate)^(10/12)
# Market return (r(t) / r(t-1))
market_return <- as.numeric(SP500$SP500.Adjusted[as.yearmon("ott 2020")]) / 
                as.numeric(SP500$SP500.Adjusted[as.yearmon("gen 2020")])
            
# Expected Returns
er_SBUX <- rf_return + as.numeric(SBUX_betas[as.yearmon("gen 2020")]) * (market_return - rf_return)
er_MCD <- rf_return + as.numeric(MCD_betas[as.yearmon("gen 2020")]) * (market_return - rf_return)
er_AXP <- rf_return + as.numeric(AXP_betas[as.yearmon("gen 2020")]) * (market_return - rf_return)
er_AON <- rf_return + as.numeric(AON_betas[as.yearmon("gen 2020")]) * (market_return - rf_return)
er_KO <- rf_return + as.numeric(KO_betas[as.yearmon("gen 2020")]) * (market_return - rf_return)
er_PEP <- rf_return + as.numeric(PEP_betas[as.yearmon("gen 2020")]) * (market_return - rf_return)

print("RITORNI ATTESI")
cat("SBUX: ", er_SBUX, "\n")
cat("MCD: ", er_MCD, "\n")
cat("AXP: ", er_AXP, "\n")
cat("AON: ", er_AON, "\n")
cat("KO: ", er_KO, "\n")
cat("PEP: ", er_PEP, "\n")

#### PORTFOLIO MANAGEMENT ####

# Convert returns to yearly granularity
SBUX_yearly <- periodReturn(x = dataset$SBUX, period = "yearly")
MCD_yearly <- periodReturn(x = dataset$MCD, period = "yearly")
AXP_yearly <- periodReturn(x = dataset$AXP, period = "yearly")
AON_yearly <- periodReturn(x = dataset$AON, period = "yearly")
KO_yearly <- periodReturn(x = dataset$KO, period = "yearly")
PEP_yearly <- periodReturn(x = dataset$PEP, period = "yearly")

yearly_returns = cbind(SBUX_yearly, MCD_yearly, AXP_yearly, AON_yearly, KO_yearly, PEP_yearly)
yearly_returns = yearly_returns[-dim(yearly_returns)[1]]
colnames(yearly_returns) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
index(yearly_returns) <- as.yearmon(index(yearly_returns))

# Calculate Markowitz Optimal Portfolio in relation basing on yearly returns
MOP <- portfolio.optim(x = yearly_returns)

cat("SBUX Weight: ", MOP$pw[1], "\n")
cat("MCD Weight: ", MOP$pw[2], "\n")
cat("AXP Weight: ", MOP$pw[3], "\n")
cat("AON Weight: ", MOP$pw[4], "\n")
cat("KO Weight: ", MOP$pw[5], "\n")
cat("PEP Weight: ", MOP$pw[6], "\n")

# Show optimal weights with a barplot
par(mfrow = c(1, 1))
barplot(MOP$pw, col = stocks_colors, names = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP"))

# Calculate 150 points in the mu-sigma (mean-variance) plane
rs <- seq(0.0,1.0,length.out=150)
risk <- numeric(length(rs))+NA
for( i in 1:length(rs) ) {
  p <- NULL
  try( p <- portfolio.optim(x = yearly_returns, pm = rs[i]) )
  if( is.null(p) ) {
    risk[i] <- NA
  } else {
    risk[i] <- p$ps
  }
}

# Show Efficient Frontier
plot(risk, rs, pch=20, col="blue", xlab="risk (sigma)", ylab="return (mean)")
# Add Market Portfolio point on efficient frontier
points(MOP$ps, MOP$pm, pch=17, col="red")

# Imaginary budget to simulate investment in relation to optimal portfolio weights
budget <- 10000
# Get stocks prices at the moment of portfolio creation
stocks_initial_prices <- window(dataset, start = index(dataset[dim(dataset)[1]]) %m+% months(-l))[1]
# Get stocks prices at the end of the investing period
stocks_final_prices <- dataset[dim(dataset)[1]]

# Likely transaction cost
transaction_cost = 0.02

# Define the number of shares of stock i that can be bought by considering total budget,
# i-th optimal weight, and price of i stock at the moment of portfolio composition (including transation costs) 
SBUX_nshares <- floor((budget * MOP$pw[1]) / (as.numeric(stocks_initial_prices$SBUX) + as.numeric(stocks_initial_prices$SBUX) * transaction_cost))
MCD_nshares <- floor((budget * MOP$pw[2]) / (as.numeric(stocks_initial_prices$MCD) + as.numeric(stocks_initial_prices$MCD) * transaction_cost))
AXP_nshares <- floor((budget * MOP$pw[3]) / (as.numeric(stocks_initial_prices$AXP) + as.numeric(stocks_initial_prices$AXP) * transaction_cost))
AON_nshares <- floor((budget * MOP$pw[4]) / (as.numeric(stocks_initial_prices$AON) + as.numeric(stocks_initial_prices$AON) * transaction_cost))
KO_nshares <- floor((budget * MOP$pw[5]) / (as.numeric(stocks_initial_prices$KO) + as.numeric(stocks_initial_prices$KO) * transaction_cost))
PEP_nshares <- floor((budget * MOP$pw[6]) / (as.numeric(stocks_initial_prices$PEP) + as.numeric(stocks_initial_prices$PEP) * transaction_cost))  

# Calculate total amount of wealth invested in every stock
SBUX_cost <- round(SBUX_nshares * as.numeric(stocks_initial_prices$SBUX), 2)
MCD_cost <- round(MCD_nshares * as.numeric(stocks_initial_prices$MCD), 2)
AXP_cost <- round(AXP_nshares * as.numeric(stocks_initial_prices$AXP), 2)
AON_cost <- round(AON_nshares * as.numeric(stocks_initial_prices$AON), 2)
KO_cost <- round(KO_nshares * as.numeric(stocks_initial_prices$KO), 2)
PEP_cost <- round(PEP_nshares * as.numeric(stocks_initial_prices$PEP), 2)

# Calculate actual amount of money (will be less than or equal to budget)
investment <- SBUX_cost + MCD_cost + AXP_cost + AON_cost + KO_cost + PEP_cost

# Display Portfolio Summary
cat("Markowitz Optimal Portfolio composition:\n")
cat("SBUX -> ", SBUX_nshares, "quote a", stocks_initial_prices$SBUX, "EUR, totale ", SBUX_cost, "EUR")
cat("MCD -> ", MCD_nshares, "quote a", stocks_initial_prices$MCD, "EUR, totale ", MCD_cost, "EUR")
cat("AXP -> ", AXP_nshares, "quote a", stocks_initial_prices$AXP, "EUR, totale ", AXP_cost, "EUR")
cat("AON -> ", AON_nshares, "quote a", stocks_initial_prices$AON, "EUR, totale ", AON_cost, "EUR")
cat("KO -> ", SBUX_nshares, "quote a", stocks_initial_prices$KO, "EUR, totale ", KO_cost, "EUR")
cat("PEP -> ", SBUX_nshares, "quote a", stocks_initial_prices$PEP, "EUR, totale ", PEP_cost, "EUR")
cat("Totale Investito:", investment)
cat("Residuo rispetto al budget: ", budget - investment)

# Value of portfolio at the end of investing period
portfolio_value <- as.numeric(SBUX_nshares * stocks_final_prices$SBUX) +
                   as.numeric(MCD_nshares * stocks_final_prices$MCD) +
                   as.numeric(AXP_nshares * stocks_final_prices$AXP) + 
                   as.numeric(AON_nshares * stocks_final_prices$AON) +
                   as.numeric(KO_nshares * stocks_final_prices$KO) + 
                   as.numeric(PEP_nshares * stocks_final_prices$PEP)
cat("Valore finale del Portfolio =", portfolio_value, "EUR")

# Calculate return basing on real data
actual_return <- MOP$pw[1] * (as.numeric(stocks_final_prices$SBUX) / as.numeric(stocks_initial_prices$SBUX) - 1) +
                    MOP$pw[2] * (as.numeric(stocks_final_prices$MCD) / as.numeric(stocks_initial_prices$MCD) - 1) +
                    MOP$pw[3] * (as.numeric(stocks_final_prices$AXP) / as.numeric(stocks_initial_prices$AXP) - 1) +
                    MOP$pw[4] * (as.numeric(stocks_final_prices$AON) / as.numeric(stocks_initial_prices$AON) - 1) +
                    MOP$pw[5] * (as.numeric(stocks_final_prices$KO) / as.numeric(stocks_initial_prices$KO) - 1) +
                    MOP$pw[6] * (as.numeric(stocks_final_prices$PEP) / as.numeric(stocks_initial_prices$PEP) - 1)

# Calculate total return basing on expected returns previously estimated with beta coefficient
beta_return <- as.numeric(MOP$pw[1] * er_SBUX +
                  MOP$pw[2] * er_MCD +
                  MOP$pw[3] * er_AXP +
                  MOP$pw[4] * er_AON +
                  MOP$pw[5] * er_KO +
                  MOP$pw[6] * er_PEP) - 1

# Calculate total return basing on every stock predicted returns (can be summed since they're log returns)              
predicted_return <- as.numeric(MOP$pw[1] * sum(SBUX_pred_returns$mean)) + 
                    as.numeric(MOP$pw[2] * sum(MCD_pred_returns$mean)) + 
                    as.numeric(MOP$pw[3] * sum(AXP_pred_returns$mean)) + 
                    as.numeric(MOP$pw[4] * sum(AON_pred_returns$mean)) + 
                    as.numeric(MOP$pw[5] * sum(KO_pred_returns$mean)) + 
                    as.numeric(MOP$pw[6] * sum(PEP_pred_returns$mean))

cat("Ritorno atteso (beta):", beta_return)
cat("Ritorno effettivo:", actual_return)
cat("Ritorno predetto:", predicted_return)

setwd("./webapp")
source("server.r")
source("ui.r")

shinyApp(ui, server)