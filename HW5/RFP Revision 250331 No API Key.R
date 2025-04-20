# **************************************************************************
# Production platform for the Random Forest daily trading strategy.  
# Enter on open of next day and exit on close of next day. Requires 
# separate sell orders to be entered from either within trader workstation
# or through another script (to be developed).  This script is not 
# sophisticated enough to track open/executed orders to place stop outs or 
# other exits.  Generates trades based on data up until the last complete 
# end-of-day trading data.  Allows user to review/modify orders and then 
# either quit without trading or execute them through the API.  For the 
# API to work, the IB Gateway or Trader Workstation must be logged into and 
# active prior to running this script.

# *********SET WORKING DIRECTORY AND CLEAR ENVIRONMENT *********************
rm(list=ls())
options("getSymbols.warning4.0"=FALSE) # disables warning message from showing 
options(scipen=999)
cat("\014")

# ************ SET DATE RANGES AND SYSTEM PARAMETERS ***************************
library(Quandl)
library(IBrokers)
library(quantmod)
library(dplyr)
library(tidyquant)
windowsize<-15
longestindicator<-100
currentdate<-Sys.Date()
maxtradepct<-0.05                    # maximum value of any single trade
maxdaytrades<-8                     # maximum trades in one day
longthreshold<-1.01
shortthreshold<-0.99
defaultscalinglength<-10000
currentSP500<-tq_index("SP500")[,c(1,6)]
IBport=7497 #7497 is the port for tws and 4002 is for the gateway
# ************************* GET DATA FROM FROM STORED UNIVERSE AND IMPORT NEW FROM QUANDL *****************
getData=function(){
  stock<-NULL
  fromdate<-currentdate-2*(longestindicator+windowsize) 
  Quandl.api_key("4REPYvYN6PkUD57Cgy31")
  temp<-NULL
  temp<-tryCatch({
    temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker="AAPL")   # Use tryCatch to handle the error  
  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
  if(!is.null(temp)){
    symbols<-currentSP500$symbol
    for (i in 1:5) {
      print(c(i,symbols[i]))
      temp<-NULL
      temp<-tryCatch({
        temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker=symbols[i])   # Use tryCatch to handle the error
      }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
        print(temp)
        stock<-rbind(stock,temp)
    }
    stock<-na.omit(stock)
    names(stock)[1]<-"symbol"
    stock<-stock[,c(1:7)]
    rownames(stock)<-seq(1,nrow(stock),1)
    days<-unique(stock$date)
    days<-days[order(days)]
    lastdate<-max(stock$date)
    datastart<-which(days==lastdate)-windowsize-longestindicator+1
    stock<-subset(stock,stock$date>=days[datastart])
  }
return(stock)  
}

# ************************* GENERATE INDICATORS *******************************
genIndicators=function(sym){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-subset(universe,universe$symbol==sym) # work with one symbol at a time
  lastclose<-stock$close  #
  leadopen<-lead(as.vector(stock$open),1)
  leadclose<-lead(as.vector(stock$close),1)        
  savel<-data.frame(date=stock$date,nextopen=leadopen,nextclose=leadclose,lastclose=lastclose)
  stock.xts<-xts(stock[,c(3:7)],stock$date)             
  stock.xts$momentum1<-diff(stock.xts$close)  
  stock.xts$accel<-diff(stock.xts$momentum1)
  stock.xts$momentum10<-tryCatch({
    stock.xts$momentum10<-momentum(stock.xts$close,n=10)    
  }, warning=function(w) {stock.xts$momentum10<-NA }, error=function(e) {stock.xts$momentum10<-NA})
  stock.xts$momentum20<-tryCatch({
    stock.xts$momentum20<-momentum(stock.xts$close,n=20)    
  }, warning=function(w) {stock.xts$momentum20<-NA }, error=function(e) {stock.xts$momentum20<-NA})
  macd<-tryCatch({
    macd<-MACD(stock.xts$close,maType="EMA")              
  }, warning=function(w) {macd<-NULL }, error=function(e) {macd<-NULL})
  if (is.null(macd)) {
    stock.xts$macdDiff<-NA
  } else stock.xts$macdDiff<-macd[,1]-macd[,2]                    
  stock.xts$sma5<-tryCatch({
    stock.xts$sma5<-SMA(stock.xts$close,n=5)        
  }, warning=function(w) {stock.xts$sma5<-NA }, error=function(e) {stock.xts$sma5<-NA})
  stock.xts$sma10<-tryCatch({
    stock.xts$sma10<-SMA(stock.xts$close,n=10)        
  }, warning=function(w) {stock.xts$sma10<-NA }, error=function(e) {stock.xts$sma10<-NA})
  stock.xts$sma20<-tryCatch({
    stock.xts$sma20<-SMA(stock.xts$close,n=20)        
  }, warning=function(w) {stock.xts$sma20<-NA }, error=function(e) {stock.xts$sma20<-NA})
  stock.xts$sma40<-tryCatch({
    stock.xts$sma40<-SMA(stock.xts$close,n=40)        
  }, warning=function(w) {stock.xts$sma40<-NA }, error=function(e) {stock.xts$sma40<-NA})
  stock.xts$sma60<-tryCatch({
    stock.xts$sma60<-SMA(stock.xts$close,n=60)        
  }, warning=function(w) {stock.xts$sma60<-NA }, error=function(e) {stock.xts$sma60<-NA})
  stock.xts$sma100<-tryCatch({
    stock.xts$sma100<-SMA(stock.xts$close,n=100)        
  }, warning=function(w) {stock.xts$sma100<-NA }, error=function(e) {stock.xts$sma100<-NA})
  stock.xts$sma5.lag1<-stats::lag(stock.xts$sma5,k=1)
  stock.xts$sma10.lag1<-stats::lag(stock.xts$sma10,k=1)
  stock.xts$sma20.lag1<-stats::lag(stock.xts$sma20,k=1)
  stock.xts$sma40.lag1<-stats::lag(stock.xts$sma40,k=1)
  stock.xts$sma60.lag1<-stats::lag(stock.xts$sma60,k=1)
  stock.xts$cross510<-stock.xts$sma5/stock.xts$sma10
  stock.xts$cross520<-stock.xts$sma5/stock.xts$sma20
  stock.xts$cross540<-stock.xts$sma5/stock.xts$sma40
  stock.xts$cross1020<-stock.xts$sma10/stock.xts$sma20
  stock.xts$cross1040<-stock.xts$sma10/stock.xts$sma40
  stock.xts$cross1060<-stock.xts$sma10/stock.xts$sma60  
  stock.xts$cross10100<-stock.xts$sma10/stock.xts$sma100  
  stock.xts$cross2040<-stock.xts$sma20/stock.xts$sma40
  stock.xts$cross2060<-stock.xts$sma20/stock.xts$sma60
  stock.xts$cross20100<-stock.xts$sma20/stock.xts$sma100      
  bb<-tryCatch({
    bb<-BBands(HLC(stock.xts), n = 20, maType="SMA", sd = 2)   # sometimes calls to BBands will crash the system   
  }, warning=function(w) {bb<-NULL }, error=function(e) {bb<-NULL})
  if (is.null(bb)) {                                          # we won't trade if there are no BBands 
    stock.xts$dn<-NA
    stock.xts$mavg<-NA
    stock.xts$up<-NA
    stock.xts$range<-NA
  } else {                                                    # otherwise create the indicators from the bb object
    stock.xts$dn<-bb$dn                                       # note we consider trading in period t based on the 
    stock.xts$mavg<-bb$mavg                                   # the band calculations in period t-1 (hence the lag)
    stock.xts$up<-bb$up
    stock.xts$range<-bb$up-bb$dn                              # keep track of band range/width as a measure of risk
  }
  stock.xts$rsi5<-tryCatch({
    stock.xts$rsi5<-RSI(stock.xts$close,n=5)        
  }, warning=function(w) {stock.xts$rsi5<-NA }, error=function(e) {stock.xts$rsi5<-NA})
  stock.xts$rsi10<-tryCatch({
    stock.xts$rsi10<-RSI(stock.xts$close,n=10)      
  }, warning=function(w) {stock.xts$rsi10<-NA }, error=function(e) {stock.xts$rsi10<-NA})
  stock.xts$rsi20<-tryCatch({
    stock.xts$rsi20<-RSI(stock.xts$close,n=20)        
  }, warning=function(w) {stock.xts$rsi20<-NA }, error=function(e) {stock.xts$rsi20<-NA})
  stock.xts<-na.omit(stock.xts)
  scaled.xts<-NULL
  lengthforscaling<-min(defaultscalinglength,nrow(stock.xts))
  if (lengthforscaling>0) {
    maxs <- apply(stock.xts[c(1:lengthforscaling),], 2, max,na.rm=TRUE)
    mins <- apply(stock.xts[c(1:lengthforscaling),], 2, min,na.rm=TRUE)
    scaled.xts<-tryCatch({
      scaled.xts<-scale(stock.xts[c(1:lengthforscaling),],center = mins, scale = maxs - mins)        # sometimes calls to TTR functions will crash the system   
    }, warning=function(w) {scaled.xts<-NULL }, error=function(e) {scaled.xts<-NULL})
  }
  if (!is.null(scaled.xts)){ 
#    scaled.xts<-na.omit(scaled.xts)
    scaled.xts$cross510up<-ifelse(stock.xts$sma5.lag1<stock.xts$sma10.lag1&
                                    stock.xts$sma5>stock.xts$sma10,1,0)
    scaled.xts$cross510dn<-ifelse(stock.xts$sma5.lag1>stock.xts$sma10.lag1&
                                    stock.xts$sma5<stock.xts$sma10,1,0)
    scaled.xts$cross520up<-ifelse(stock.xts$sma5.lag1<stock.xts$sma20.lag1&
                                    stock.xts$sma5>stock.xts$sma20,1,0)
    scaled.xts$cross520dn<-ifelse(stock.xts$sma5.lag1>stock.xts$sma20.lag1&
                                    stock.xts$sma5<stock.xts$sma20,1,0)
    scaled.xts$cross540up<-ifelse(stock.xts$sma5.lag1<stock.xts$sma40.lag1&
                                    stock.xts$sma5>stock.xts$sma40,1,0)
    scaled.xts$cross540dn<-ifelse(stock.xts$sma5.lag1>stock.xts$sma40.lag1&
                                    stock.xts$sma5<stock.xts$sma40,1,0)
    scaled.xts$cross1020up<-ifelse(stock.xts$sma10.lag1<stock.xts$sma20.lag1&
                                     stock.xts$sma10>stock.xts$sma20,1,0)
    scaled.xts$cross1020dn<-ifelse(stock.xts$sma10.lag1>stock.xts$sma20.lag1&
                                     stock.xts$sma10<stock.xts$sma20,1,0)
    scaled.xts$cross1040up<-ifelse(stock.xts$sma10.lag1<stock.xts$sma40.lag1&
                                     stock.xts$sma10>stock.xts$sma40,1,0)
    scaled.xts$cross1040dn<-ifelse(stock.xts$sma10.lag1>stock.xts$sma40.lag1&
                                     stock.xts$sma10<stock.xts$sma40,1,0)
    scaled.xts$cross510<-stock.xts$cross510
    scaled.xts$cross520<-stock.xts$cross520
    scaled.xts$cross540<-stock.xts$cross540
    scaled.xts$cross1020<-stock.xts$cross1020
    scaled.xts$cross1040<-stock.xts$cross1040
    scaled.xts$cross1060<-stock.xts$cross1060
    scaled.xts$cross10100<-stock.xts$cross10100
    scaled.xts$cross2040<-stock.xts$cross2040
    scaled.xts$cross2060<-stock.xts$cross2060
    scaled.xts$cross20100<-stock.xts$cross20100
    scaled.xts$rsi5<-stock.xts$rsi5
    scaled.xts$rsi10<-stock.xts$rsi10
    scaled.xts$rsi20<-stock.xts$rsi20
#   scaled.xts$nextreturn<-(lead(as.vector(stock.xts$close),1)-lead(as.vector(stock.xts$open),1))/lead(as.vector(stock.xts$open),1)+1
#   scaled.xts$nextopen<-lead(as.vector(stock.xts$open),1)
#   scaled.xts$nextclose<-lead(as.vector(stock.xts$close),1)
    scaled.xts<-na.omit(scaled.xts)
    stock<-data.frame(scaled.xts)                                # convert back to dataframe object
    date<-as.Date(rownames(stock))                              
    stock<-cbind(sym,date,stock)
    names(stock)[1]<-"symbol"
    stock<-merge(stock,currentSP500)
    stock$sector<-as.factor(stock$sector)
    stock$dow<-as.factor(weekdays(stock$date,abbreviate=TRUE))
    stock<-merge(stock,savel)
    stock<-stock[,c(2,1,3:ncol(stock))]
    stock$nextreturn<-(stock$nextclose-stock$nextopen)/stock$nextopen+1
    rownames(stock)<-seq(1,nrow(stock),1)
    } else stock<-NULL
  return(stock)
}

# ************************ GENERATE PREDICTIONS *******************************
genPredictions=function(stock,tradedate){
  library(ranger)
  results<-NULL
  print("training model...")
  train<-subset(stock,stock$date<as.Date(tradedate))
  train<-na.omit(train)
  rf.model=ranger(nextreturn~.-nextopen -nextclose -symbol -date ,data=train, 
                  mtry=10,num.trees=500)
  rsq<-round(mean(rf.model$r.squared),3)
  print(paste("RSQ:",rsq))
  preds<-subset(stock,stock$date==as.Date(tradedate))    
  preds$prediction<-predict(rf.model,preds)$predictions
  results<-preds
  colremove<-which(names(results)=="nextreturn")
  results<-results[,-colremove]
  colremove<-which(names(results)=="nextopen")
  results<-results[,-colremove]
  colremove<-which(names(results)=="nextclose")
  results<-results[,-colremove]
  return(results)
}

# **************************** GENERATE SIGNALS *******************************

genSignals=function(stock){
  stock$short<-ifelse(stock$prediction<shortthreshold,1,0)
  stock$long<-ifelse(stock$prediction>longthreshold,1,0)
  stock$price<-stock$lastclose
  stock<-subset(stock,stock$short==1|stock$long==1)
  return(list(stock))
}

# *************************** GENERATE TRADES *********************************

genTrades=function(candidates,equity){
  cashin<-0
  cashout<-0
  transcost<-0
  numtrades<-0
  maxtrade<-maxtradepct*equity
  if (nrow(candidates)>0) {  # there are potential candidates 1 or more
    candidates<-candidates[order(-candidates$prediction),]  # sort them by decreasing predicted returns
    numtrades<-nrow(candidates)  
    if (numtrades>maxdaytrades) { # make sure we don't exceed the # of trades allowed
      candidates<-candidates[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
    }
    tradeamount<-max(min(maxtrade,equity/numtrades),0) # invest equally
    cashout<-0
    # now figure out how much cash we have used to invest in our new positions
    if (numtrades>0) {
      candidates$position<-NA
      for (i in c(1:numtrades)){
        candidates$position[i]<-trunc(tradeamount/candidates$price[i])
        if (candidates$long[i]==1) {
            cashout<-cashout+candidates$position[i]*candidates$price[i]
        } else {
            cashin<-cashin+candidates$position[i]*candidates$price[i]
        }
      }
      candidates<-subset(candidates,candidates$position>0)
    }
  } else candidates<-NULL
  transcost<-1*numtrades+0.01*sum(candidates$position)
  return(list(trades=candidates,cashin=cashin,cashout=cashout,transcost=transcost))  
}

# ***************************** APPLY RULES ************************************
applyRules=function(day,equity){
  cashin<-0
  cashout<-0
  transcost<-0
  candidates<-subset(signals,signals$date==day&signals$long==1)
  longs<-genTrades(candidates,equity)
  equity<-equity+longs$cashin-longs$cashout-longs$transcost
  candidates<-subset(signals,signals$date==day&signals$short==1)
  candidates$prediction<-2-candidates$prediction
  shorts<-genTrades(candidates,equity)
  cashin=longs$cashin+shorts$cashin
  cashout=longs$cashout+shorts$cashout+longs$transcost+shorts$transcost
  transcost=longs$transcost+shorts$transcost
  return(list(long=longs$trades,short=shorts$trades,cashin=cashin,
              cashout=cashout,transcost=transcost))
}

# *************************** REVIEW TRADES ************************************
reviewTrades=function(trades,tradetype) {
  done<-FALSE
  if (nrow(trades)>0){
    trades<-trades[,c("symbol","sector","prediction","price","position")]
    trades$prediction<-round(trades$prediction,digits=4)
    rownames(trades)<-seq(1,nrow(trades),1)
  } else {
    done<-TRUE
    choice<-"Q"
  }
  while (!done) {
    View(trades)
    cat("\014")
    print(paste("REVIEWING CANDIDATE",toupper(tradetype),
                " AND CHOOSING TO EXECUTE WILL SEND TRADES IMMEDIATELY TO IB."))
    choice<-readline(prompt="Choose D)elete trade, M)odify Price, C)hange position, E)xecute, Q)uit without trading:  ")
    choice<-toupper(choice)
    done<-ifelse(choice=="E"|choice=="Q",TRUE,FALSE)
    if (choice=="M"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to modify: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newprice<-as.numeric(readline(prompt=paste("Enter the new limit price for",
                                                   trades$symbol[rownum],": ")))
        valid<-is.numeric(newprice)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$price[rownum]<-newprice
        } else { 
            print("invalid price")  
            Sys.sleep(2)
        }
      } else {
          print("invalid row number") 
          Sys.sleep(2)
      }
    }
    if (choice=="C"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to change: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newposition<-as.numeric(readline(prompt=paste("Enter the new position size for",trades$symbol[rownum],": ")))
        valid<-is.numeric(newposition)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$position[rownum]<-newposition
        } else { 
          print("invalid position")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
        }
    }
    if (choice=="D"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to delete: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        confirm<-as.character(readline(prompt=paste("Enter Y)es to confirm, N)o to abort removing the trade for ",trades$symbol[rownum],": ")))
        valid<-is.character(confirm)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          if (toupper(confirm)=="Y") {
            trades<-trades[-rownum,]
            if(nrow(trades)==0){
              print("Last candidate trade deleted")
              Sys.sleep(2)
            }
            }
        } else { 
          print("invalid entry")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
    }
  if (choice!="E"){
    trades<-trades[-c(1:nrow(trades)),]
  }
  return(trades)
}

# ************************************** EXECUTE TRADES **********************************************************

executeTrades=function(trades,tradetype,tws){
if (isConnected(twsconn=tws)){
  print("Transmitting Orders...")
  openAction<-ifelse(toupper(tradetype)=="LONG","BUY","SELL")
  closeAction<-ifelse(toupper(tradetype)=="LONG","SELL","BUY")
  for(i in c(1:nrow(trades))){ # Enter orders to open positions
    equity<-twsEquity(as.character(trades$symbol[i]),'SMART',primary="ISLAND")
    OrdId<-reqIds(tws)
    order<-twsOrder(OrdId,action=openAction,totalQuantity = trades$position[i],
                    orderType="MKT",tif="OPG") 
    placeOrder(tws,equity,order)
    }
  for(i in c(1:nrow(trades))){ # Enter orders to close positions
    equity<-twsEquity(as.character(trades$symbol[i]),'SMART',primary="ISLAND")
    OrdId<-reqIds(tws)
    order<-twsOrder(OrdId,action=closeAction,totalQuantity = trades$position[i],
                    orderType="MOC") 
    placeOrder(tws,equity,order)
    }
  dummy<-readline(prompt="Orders submitted, press <Enter> to continue:")
  } else {
    print("Connection with Interactive Brokers lost.  Trades not submitted!")
    dummy<-readline(prompt="Press <Enter> to continue:")
    }
}

OkToProceed=function(message){
  cat("\014")
  proceed<-readline(prompt=message)
  proceed<-toupper(proceed)
  
}

# ********************************  RUN STRATEGY **********************************
cat("\014")
universe<-getData()
lasttradedate<-max(as.Date(universe$date))
done<-FALSE
proceed<-""
message<-paste("Make sure that the IB Gateway is open.  Last data date is",
               as.character(lasttradedate),"proceed (Y/N)?")
while (!done) {
  cat("\014")
  proceed<-OkToProceed(message)
  done<-ifelse(proceed=="Y"|proceed=="N",TRUE,FALSE)  # Commented out |proceed=="Y"
}  

if (proceed=="Y"){
  symbols<-unique(universe$symbol)
  indicators<-NULL            # we will put all OHLC data and our generated
  for (sym in symbols) {      # indicators into a dataframe named "indicators"
    temp<-genIndicators(sym)  # by looping through all the symbols 
    indicators<-rbind(indicators,temp)
    }
  predictions<-data.frame(genPredictions(indicators,lasttradedate))  
  signals<-data.frame(genSignals(predictions))
  signals<-na.omit(signals)

# ******************* OPEN CONNECTION & PROCESS TRADES  ************************

  tws <-tryCatch({                  # IBport is a global parameter   
  tws = twsConnect(port=IBport)     # connection can fail with an error  
  }, warning=function(w) {tws<-NULL }, error=function(e) {tws<-NULL})
  if(!is.null(tws)) {
    acc<-reqAccountUpdates(tws)  
    available<-as.numeric(acc[[1]]$AvailableFunds[1])/2
    trades<-applyRules(lasttradedate,available)
    if (!is.null(trades$long)) {
      longs<-reviewTrades(trades[[1]],"Long Trades")
      if(nrow(longs)>0){
        executeTrades(longs,"Long",tws)
      } else {print("No long trades executed")}
    } else {print("No long trades to execute.")}
    Sys.sleep(5)
    if (!is.null(trades$short)) {
      shorts<-reviewTrades(trades[[2]],"Short Trades")
      if (nrow(shorts)>0) {
        executeTrades(shorts,"Short",tws)
      } else {print("No short trades executed")}
    } else {print("No short trades to execute.")}
  twsDisconnect(tws)
  } else {print("Aborting: Cannot connect to Interactive Brokers")}
} else {print("script cancelled by user")}

