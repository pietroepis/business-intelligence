library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)

stocks_colors = c("#cc3300", "#2eb82e", "#0052cc", "#ff9900", "#b3b300", "#cc0099")

#### DATA SUMMARY ####

date_from <- as.Date("2018-10-01")
date_to <- as.Date("2020-10-31")

# Sector: Information Technology
# CSCO  Cisco Systems
# INTC  Intel Corporation
CSCO <- getSymbols("CSCO", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
INTC <- getSymbols("INTC", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)

# Sector: Utilities
# EXC   Exelon
# AEP   American Electric Power
EXC <- getSymbols("EXC", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
AEP <- getSymbols("AEP", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)

# Sector: Consumer Staples
# KHC   Kraft Heinz Co
# PEP   PepsiCo, Inc.
KHC <- getSymbols("KHC", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
PEP <- getSymbols("PEP", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)

stocks <- merge(CSCO$CSCO.Adjusted, INTC$INTC.Adjusted, EXC$EXC.Adjusted, AEP$AEP.Adjusted, 
                KHC$KHC.Adjusted, PEP$PEP.Adjusted)
colnames(stocks) <- c("CSCO", "INTC", "EXC", "AEP", "KHC", "PEP")
complete.cases(stocks)

dygraph(stocks, xlab = "Date", ylab = "Adjusted Close", main = "Adjusted Close Price") %>%
  dyOptions(colors = stocks_colors) %>%
  dyOptions(strokeWidth = 2.0)

#### DESCRIPTIVE ANALYTICS 1/3 ####

stocks = to.monthly(stocks, OHLC = F)
stocks_simple_returns = CalculateReturns(stocks, method = "simple")[-1,]
stocks_cc_returns = CalculateReturns(stocks, method = "compound")[-1,]

dygraph(stocks_simple_returns, xlab = "Date", ylab = "Return", main = "Simple Returns") %>%
  dyOptions(colors = stocks_colors) %>%
  dyOptions(strokeWidth = 2.0)
dygraph(stocks_cc_returns, xlab = "Date", ylab = "Return", main = "Continuously Compounded Returns") %>%
  dyOptions(colors = stocks_colors) %>%
  dyOptions(strokeWidth = 2.0)

#### DESCRIPTIVE ANALYTICS 2/3 ####

# CSCO
par(mfrow = c(2, 2))
hist(stocks_cc_returns$CSCO, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - CSCO")
points(density(stocks_cc_returns$CSCO), type="l", lwd = 2, col = stocks_colors[1])
plot(density(stocks_cc_returns$CSCO), type="l", lwd = 2, col = stocks_colors[1], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - CSCO")
boxplot(as.numeric(stocks_cc_returns$CSCO), horizontal = T, col = stocks_colors[1], main = "Boxplot - CSCO")
qqnorm(stocks_cc_returns$CSCO, main = "Q-Q Plot - CSCO")
qqline(stocks_cc_returns$CSCO, lwd = 2, col = stocks_colors[1])

mean(stocks_cc_returns$CSCO)
var(stocks_cc_returns$CSCO)[1]
sd(stocks_cc_returns$CSCO)
skewness(stocks_cc_returns$CSCO)
kurtosis(stocks_cc_returns$CSCO)
quantile(stocks_cc_returns$CSCO)

# INTC
par(mfrow = c(2, 2))
hist(stocks_cc_returns$INTC, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - INTC")
points(density(stocks_cc_returns$INTC), type="l", lwd = 2, col = stocks_colors[2])
plot(density(stocks_cc_returns$INTC), type="l", lwd = 2, col = stocks_colors[2], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - INTC")
boxplot(as.numeric(stocks_cc_returns$INTC), horizontal = T, col = stocks_colors[2], main = "Boxplot - INTC")
qqnorm(stocks_cc_returns$INTC, main = "Q-Q Plot - INTC")
qqline(stocks_cc_returns$INTC, lwd = 2, col = stocks_colors[2])

mean(stocks_cc_returns$INTC)
var(stocks_cc_returns$INTC)[1]
sd(stocks_cc_returns$INTC)
skewness(stocks_cc_returns$INTC)
kurtosis(stocks_cc_returns$INTC)
quantile(stocks_cc_returns$INTC)

# EXC
par(mfrow = c(2, 2))
hist(stocks_cc_returns$EXC, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - EXC")
points(density(stocks_cc_returns$EXC), type="l", lwd = 2, col = stocks_colors[3])
plot(density(stocks_cc_returns$EXC), type="l", lwd = 2, col = stocks_colors[3], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - EXC")
boxplot(as.numeric(stocks_cc_returns$EXC), horizontal = T, col = stocks_colors[3], main = "Boxplot - EXC")
qqnorm(stocks_cc_returns$EXC, main = "Q-Q Plot - EXC")
qqline(stocks_cc_returns$EXC, lwd = 2, col = stocks_colors[3])

mean(stocks_cc_returns$EXC)
var(stocks_cc_returns$EXC)[1]
sd(stocks_cc_returns$EXC)
skewness(stocks_cc_returns$EXC)
kurtosis(stocks_cc_returns$EXC)
quantile(stocks_cc_returns$EXC)

# AEP
par(mfrow = c(2, 2))
hist(stocks_cc_returns$AEP, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - AEP")
points(density(stocks_cc_returns$AEP), type="l", lwd = 2, col = stocks_colors[4])
plot(density(stocks_cc_returns$AEP), type="l", lwd = 2, col = stocks_colors[4], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - AEP")
boxplot(as.numeric(stocks_cc_returns$AEP), horizontal = T, col = stocks_colors[4], main = "Boxplot - AEP")
qqnorm(stocks_cc_returns$AEP, main = "Q-Q Plot - AEP")
qqline(stocks_cc_returns$AEP, lwd = 2, col = stocks_colors[4])

mean(stocks_cc_returns$AEP)
var(stocks_cc_returns$AEP)[1]
sd(stocks_cc_returns$AEP)
skewness(stocks_cc_returns$AEP)
kurtosis(stocks_cc_returns$AEP)
quantile(stocks_cc_returns$AEP)

# KHC
par(mfrow = c(2, 2))
hist(stocks_cc_returns$KHC, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - AEP")
points(density(stocks_cc_returns$KHC), type="l", lwd = 2, col = stocks_colors[5])
plot(density(stocks_cc_returns$KHC), type="l", lwd = 2, col = stocks_colors[5], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - KHC")
boxplot(as.numeric(stocks_cc_returns$KHC), horizontal = T, col = stocks_colors[5], main = "Boxplot - AEP")
qqnorm(stocks_cc_returns$KHC, main = "Q-Q Plot - KHC")
qqline(stocks_cc_returns$KHC, lwd = 2, col = stocks_colors[5])

mean(stocks_cc_returns$KHC)
var(stocks_cc_returns$KHC)[1]
sd(stocks_cc_returns$KHC)
skewness(stocks_cc_returns$KHC)
kurtosis(stocks_cc_returns$KHC)
quantile(stocks_cc_returns$KHC)

# PEP
par(mfrow = c(2, 2))
hist(stocks_cc_returns$PEP, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - AEP")
points(density(stocks_cc_returns$PEP), type="l", lwd = 2, col = stocks_colors[6])
plot(density(stocks_cc_returns$PEP), type="l", lwd = 2, col = stocks_colors[6], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - PEP")
boxplot(as.numeric(stocks_cc_returns$PEP), horizontal = T, col = stocks_colors[6], main = "Boxplot - AEP")
qqnorm(stocks_cc_returns$PEP, main = "Q-Q Plot - PEP")
qqline(stocks_cc_returns$PEP, lwd = 2, col = stocks_colors[6])

mean(stocks_cc_returns$PEP)
var(stocks_cc_returns$PEP)[1]
sd(stocks_cc_returns$PEP)
skewness(stocks_cc_returns$PEP)
kurtosis(stocks_cc_returns$PEP)
quantile(stocks_cc_returns$PEP)

# Merged Boxplot
par(mfrow = c(1, 1))
boxplot(as.numeric(stocks_cc_returns$CSCO), as.numeric(stocks_cc_returns$INTC), as.numeric(stocks_cc_returns$EXC),
        as.numeric(stocks_cc_returns$AEP), as.numeric(stocks_cc_returns$KHC), as.numeric(stocks_cc_returns$PEP),
        names = c("CSCO", "INTC", "EXC", "AEP", "KHC", "PEP"), main = "CC Returns Boxplot")
abline(h = 0, lwd = 2)

#### DESCRIPTIVE ANALYTICS 3/3 ####

cov(stocks_cc_returns)
cor(stocks_cc_returns)
