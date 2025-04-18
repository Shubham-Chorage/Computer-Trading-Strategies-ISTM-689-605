rm(list=ls())
library(rstudioapi)
load("OHLC.rdata")
stockdata <- read.csv("sectors.csv")
print(ls())
apple_data <-subset(stock, symbol=="AAPL")
apple_data <-apple_data[order(apple_data$date),]
attach(apple_data)
daily_return<-close/open
cumulative_return<-cumprod(daily_return)
max_cumulative_return<-cummax(cumulative_return)
apple_data<-data.frame(apple_data,daily_return,cumulative_return,max_cumulative_return)
plot(apple_data$date, apple_data$daily_return, 
     type = "l",                     # Line plot
     col = "steelblue",              # Line color
     lwd = 2,                        # Line width
     xlab = "Date", 
     ylab = "Daily Return",
     main = "Apple Daily Return Over Time")
grid()
plot(apple_data$date, apple_data$cumulative_return, 
     type = "l",                     # Line plot
     col = "darkgreen",              # Line color (choose any color you like)
     lwd = 2,                        # Line width
     xlab = "Date", 
     ylab = "Cumulative Return",
     main = "Apple Cumulative Return Over Time")
grid()
plot(apple_data$date, apple_data$max_cumulative_return, 
     type = "l",                     # Line plot
     col = "purple",                 # Line color (you can choose any color)
     lwd = 2,                        # Line width
     xlab = "Date", 
     ylab = "Max Cumulative Return",
     main = "Apple Max Cumulative Return Over Time")
grid()