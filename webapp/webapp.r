library(quantmod)
library(dygraphs)

SBUX <- getSymbols("SBUX", src = "yahoo", auto.assign = FALSE)
MCD <- getSymbols("MCD", src = "yahoo", auto.assign = FALSE)
AXP <- getSymbols("AXP", src = "yahoo", auto.assign = FALSE)
AON <- getSymbols("AON", src = "yahoo", auto.assign = FALSE)
KO <- getSymbols("KO", src = "yahoo", auto.assign = FALSE)
PEP <- getSymbols("PEP", src = "yahoo", auto.assign = FALSE)

stocks <- merge(SBUX$SBUX.Adjusted, MCD$MCD.Adjusted, AXP$AXP.Adjusted, AON$AON.Adjusted, 
                KO$KO.Adjusted, PEP$PEP.Adjusted)
na.omit(stocks)
colnames(stocks) <- c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
stocks = to.monthly(stocks, OHLC = F)
stocks_cc_returns = CalculateReturns(stocks, method = "compound")[-1,]

#### WEB APPLICATION ####

setwd("./webapp")
source("server.r")
source("ui.r")

shinyApp(ui, server)
