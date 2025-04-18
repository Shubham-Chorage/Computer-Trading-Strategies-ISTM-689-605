load("OHLC.rdata")

TRADING_DAYS <- 250

stock$date <- as.Date(stock$date)
symbols <- levels(stock$symbol)

returns <- data.frame(
  symbol = character(),
  return = double()
)
for (sym in symbols) {
  data <- subset(stock, stock$symbol == sym)

  # Filter out stocks that don't have enough data points
  if (nrow(data) < TRADING_DAYS) next

  first_day <- head(data, n = 1)
  last_day <- tail(data, n = 1)

  # Calculate each stock's annual return
  annual_return <- (last_day$close - first_day$open) / first_day$open * 100
  returns <- rbind(returns, data.frame(symbol = sym, return = annual_return))
}

# Choose lowest 10 and highest 10 into separate dataframes
highest_returns <- head(returns, n = 10)
lowest_returns <- tail(returns, n = 10)
lowest_returns <- lowest_returns[order(lowest_returns$return), ]

# Round returns to 1 decimal
highest_returns$return <- round(highest_returns$return, digits = 1)
lowest_returns$return <- round(lowest_returns$return, digits = 1)

# Clear unneeded variables
vars <- ls()
rm(list = vars[vars != "highest_returns" & vars != "lowest_returns"])
rm(vars)