library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options("getSymbols.warning4.0"=FALSE) # disables warning message from showing 
options(scipen=999)
cat("\014")

library(IBrokers)
tws = twsConnect(port=7497 )  # 4002 for IB Gateway and 7496 for tws
reqCurrentTime(tws)
IBrokersRef()

acc<-reqAccountUpdates(tws,acctCode="")
acc[[1]]$AvailableFunds[1]
portfolio<-twsPortfolioValue(acc)

OrdId<-reqIds(tws)
equity<-twsEquity(symbol='AAPL',exch='SMART',primary="ISLAND")
order<-twsOrder(OrdId,action="BUY",totalQuantity = 100,orderType="LMT",tif="GTC",lmtPrice="125.00")
placeOrder(tws,equity,order)

cancelOrder(tws,OrdId)

twsDisconnect(tws)
