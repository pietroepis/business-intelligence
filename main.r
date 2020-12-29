library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(tseries)
library(shiny)

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
colnames(stocks) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
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

# SBUX
par(mfrow = c(2, 2))
hist(stocks_cc_returns$SBUX, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - SBUX")
points(density(stocks_cc_returns$SBUX), type="l", lwd = 2, col = stocks_colors[1])
plot(density(stocks_cc_returns$SBUX), type="l", lwd = 2, col = stocks_colors[1], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - SBUX")
boxplot(as.numeric(stocks_cc_returns$SBUX), horizontal = T, col = stocks_colors[1], main = "Boxplot - SBUX")
qqnorm(stocks_cc_returns$SBUX, main = "Q-Q Plot - SBUX")
qqline(stocks_cc_returns$SBUX, lwd = 2, col = stocks_colors[1])

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
hist(stocks_cc_returns$KO, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - AON")
points(density(stocks_cc_returns$KO), type="l", lwd = 2, col = stocks_colors[5])
plot(density(stocks_cc_returns$KO), type="l", lwd = 2, col = stocks_colors[5], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - KO")
boxplot(as.numeric(stocks_cc_returns$KO), horizontal = T, col = stocks_colors[5], main = "Boxplot - AON")
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
hist(stocks_cc_returns$PEP, xlab = "Return", ylab = "Frequency", main = "CC Returns Histogram - AON")
points(density(stocks_cc_returns$PEP), type="l", lwd = 2, col = stocks_colors[6])
plot(density(stocks_cc_returns$PEP), type="l", lwd = 2, col = stocks_colors[6], xlab = "Return", ylab = "Probability", 
     main = "Probability Density - PEP")
boxplot(as.numeric(stocks_cc_returns$PEP), horizontal = T, col = stocks_colors[6], main = "Boxplot - AON")
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
boxplot(as.numeric(stocks_cc_returns$SBUX), as.numeric(stocks_cc_returns$MCD), as.numeric(stocks_cc_returns$AXP),
        as.numeric(stocks_cc_returns$AON), as.numeric(stocks_cc_returns$KO), as.numeric(stocks_cc_returns$PEP),
        names = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP"), main = "CC Returns Boxplot")
abline(h = 0, lwd = 2, lty = 2)

#### DESCRIPTIVE ANALYTICS 3/3 ####

covariance = cov(stocks_cc_returns)

correlation = round(cor(stocks_cc_returns), 2)
heatmap(correlation, main = "Correlation", scale = "none", Colv = NA, Rowv = NA,)

SBUX_num <- as.numeric(stocks$SBUX)
MCD_num <- as.numeric(stocks$MCD)
AXP_num <- as.numeric(stocks$AXP)
AON_num <- as.numeric(stocks$AON)
KO_num <- as.numeric(stocks$KO)
PEP_num <- as.numeric(stocks$PEP)
pairs(cbind(SBUX_num, MCD_num, AXP_num, AON_num, KO_num, PEP_num), pch=18, 
      col="blue", main="Correlation")

#### PREDICTIVE ANALYTICS ####

n = 80
m = 30
l = 10

#### BETA COMPUTATION ####

SP500 <- getSymbols("^GSPC", from = date_from, to = date_to, src = "yahoo", auto.assign = FALSE)
SP500 <- to.monthly(SP500)
SP500 <- na.omit(CalculateReturns(SP500$SP500.Adjusted, method = "compound"))
colnames(SP500) <- c("SP500")

n_periods <- dim(SP500)[1]
delta <- 5

SBUX_betas <- NULL
MCD_betas <- NULL
AXP_betas <- NULL
AON_betas <- NULL
KO_betas <- NULL
PEP_betas <- NULL

for (index in (delta + 1):n_periods)
{
  from <- index - delta
  to <- index - 1
  
  SBUX_beta <- as.xts(cov(stocks_cc_returns$SBUX[from:to], SP500[from:to]) / var(SP500[from:to]),
                      order.by = index(stocks_cc_returns$SBUX[to]))
  MCD_beta <- as.xts(cov(stocks_cc_returns$MCD[from:to], SP500[from:to]) / var(SP500[from:to]),
                      order.by = index(stocks_cc_returns$MCD[to]))
  AXP_beta <- as.xts(cov(stocks_cc_returns$AXP[from:to], SP500[from:to]) / var(SP500[from:to]),
                     order.by = index(stocks_cc_returns$AXP[to]))
  AON_beta <- as.xts(cov(stocks_cc_returns$AON[from:to], SP500[from:to]) / var(SP500[from:to]),
                     order.by = index(stocks_cc_returns$AON[to]))
  KO_beta <- as.xts(cov(stocks_cc_returns$KO[from:to], SP500[from:to]) / var(SP500[from:to]),
                     order.by = index(stocks_cc_returns$KO[to]))
  PEP_beta <- as.xts(cov(stocks_cc_returns$PEP[from:to], SP500[from:to]) / var(SP500[from:to]),
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

plot(SBUX_betas, main = "SBUX Beta")
plot(MCD_betas, main = "MCD Beta")
plot(AXP_betas, main = "AXP Beta")
plot(AON_betas, main = "AON Beta")
plot(KO_betas, main = "KO Beta")
plot(PEP_betas, main = "PEP Beta")

#### PORTFOLIO MANAGEMENT ####

date_to <- as.Date("2018-10-31")
SBUX <- getSymbols("SBUX", to = date_to, src = "yahoo", auto.assign = FALSE)
MCD <- getSymbols("MCD", to = date_to, src = "yahoo", auto.assign = FALSE)
AXP <- getSymbols("AXP", to = date_to, src = "yahoo", auto.assign = FALSE)
AON <- getSymbols("AON", to = date_to, src = "yahoo", auto.assign = FALSE)
KO <- getSymbols("KO", to = date_to, src = "yahoo", auto.assign = FALSE)
PEP <- getSymbols("PEP", to = date_to, src = "yahoo", auto.assign = FALSE)

stocks <- merge(SBUX$SBUX.Adjusted, MCD$MCD.Adjusted, AXP$AXP.Adjusted, AON$AON.Adjusted, 
                KO$KO.Adjusted, PEP$PEP.Adjusted)
colnames(stocks) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")

SBUX_yearly <- periodReturn(x = stocks$SBUX, period = "yearly")
MCD_yearly <- periodReturn(x = stocks$MCD, period = "yearly")
AXP_yearly <- periodReturn(x = stocks$AXP, period = "yearly")
AON_yearly <- periodReturn(x = stocks$AON, period = "yearly")
KO_yearly <- periodReturn(x = stocks$KO, period = "yearly")
PEP_yearly <- periodReturn(x = stocks$PEP, period = "yearly")

yearly_returns = cbind(SBUX_yearly, MCD_yearly, AXP_yearly, AON_yearly, KO_yearly, PEP_yearly)
yearly_returns = yearly_returns[-length(yearly_returns)]
colnames(yearly_returns) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
index(yearly_returns) <- as.yearmon(index(yearly_returns))

MOP <- portfolio.optim(x = yearly_returns)

cat("SBUX Weight: ", MOP$pw[1], "\n")
cat("MCD Weight: ", MOP$pw[2], "\n")
cat("AXP Weight: ", MOP$pw[3], "\n")
cat("AON Weight: ", MOP$pw[4], "\n")
cat("KO Weight: ", MOP$pw[5], "\n")
cat("PEP Weight: ", MOP$pw[6], "\n")

rs <- seq(0.0,1.0,length.out=150)
risk <- numeric(length(rs))+NA
for( i in 1:length(rs) ) {
  p <- NULL
  try( p <- portfolio.optim( x=yearly_returns, pm=rs[i] ) )
  if( is.null(p) ) {
    risk[i] <- NA
  } else {
    risk[i] <- p$ps
  }
}

plot(risk, rs, pch=20, col="blue", xlab="risk (sigma)", ylab="return (mean)")
points( MOP$ps, MOP$pm, pch=17, col="red" )