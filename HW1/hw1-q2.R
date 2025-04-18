load("OHLC.rdata")

first_day <- aggregate(date ~ symbol, data = stock, min)
first_day <- subset(first_day, date == min(stock$date))
last_day <- aggregate(date ~ symbol, data = stock, max)
last_day <- subset(last_day, date == max(stock$date))

open_prices <- merge(stock[, c("symbol", "date", "open")], first_day)
close_prices <- merge(stock[, c("symbol", "date", "close")], last_day)
filtered <- merge(open_prices, close_prices, by = "symbol")
filtered$return <- round(
  100 * (filtered$close - filtered$open) / filtered$open, digits = 1
)

sectors <- read.csv("sectors.csv")
filtered <- merge(filtered, sectors)
returns_by_sector <- aggregate(return ~ sector, data = filtered, mean)
returns_by_sector$return <- round(returns_by_sector$return, digits = 1)
returns_by_sector <- returns_by_sector[
  order(returns_by_sector$return, decreasing = TRUE),
]
