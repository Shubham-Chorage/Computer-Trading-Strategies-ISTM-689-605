library(tidyverse)
library(forecast)
library(rstudioapi)
library(furrr)

plan(multisession, workers = 12)

# Set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list = ls())
options(scipen = 999)

train_start <- as_date("2021-01-01")
valid_start <- as_date("2023-01-01")
valid_end <- as_date("2024-12-31")

# Include data from 20 days before the start period
stocks <- read_csv("stocks.csv") |>
  filter(sector == "Industrials" & date >= train_start - 40 & date <= valid_end) |>
  arrange(ticker, date) |>
  select(ticker:close)

# Filter out some tickers
# 1. For some reason ticker "IR" has duplicate dates
# 2. ticker "AXON", "BLDR" has data from 2023-05-04
excluded_tickers <- c("IR", "AXON", "BLDR")
stocks <- stocks |> filter(!ticker %in% excluded_tickers)

# Requirements
# 1. Make predictions using a time-series forecasting method
# 2. Backtest from 1/1/23 to 12/31/24 (use data prior to 1/1/23 to accommodate windows)
# 3. Include both long and short positions, trading on a daily basis
# 4. Filter predicions in terms of a "threshold return level" and filter out low-confidence predictions
# 5. Start with $100,000 and add investment contraints:
#   - Max # short/long positions
#   - Max amount per position
#   - Max amount invested
# 6. Include performance measures:
#   - Long trades: # trades, % winning trades, average return
#   - Short trades: # trades, % winning trades, average return
#   - Sharpe ratio
#   - Cumulative portfolio return
#   - Maximum drawdown %
#   - Maximum drawdown period
#   - Overall % winning trades

# Filters
# Mean prediction error (on training set)

# Indicators
# - Preivious day close price
# - Predicted close price
# - Predicted close price confidence interval

# Keep the top k stocks that our model performs best on
filter_stocks <- function(stocks, top_k) {
  tickers <- stocks$ticker |> unique()
  errors <- tickers |> future_map_dbl(function(t) {
    stock <- stocks |> filter(ticker == t & date < valid_start)
    if (nrow(stock) < 20) return(Inf)

    stock$pred <- NA
    # Filter out bad models
    window <- 20
    for (i in 20:nrow(stock)) {
      s <- i - window + 1
      roll_ts <- stock[s:i, ] |>
        select(close) |>
        ts(start = c(1, 1), end = c(1, 20), frequency = 20)
      model <- ets(roll_ts, model = "AAN")
      stock$pred[i] <- as.double(forecast(model, h = 1)$mean)
    }

    print(t)

    mse <- sum((stock$pred - stock$close)^2, na.rm = TRUE) |> mean()
    return(mse)
  })

  sorted_by_err <- as_tibble(cbind(tickers, errors)) |> arrange(errors)
  best_stocks <- sorted_by_err[1:top_k, 1]
  filtered_stocks <- stocks |>
    filter(ticker %in% best_stocks$tickers) |>
    drop_na()
  return(filtered_stocks)
}

gen_indicators <- function(stocks, window) {
  tickers <- stocks$ticker |> unique()
  tickers |> map_dfr(function(t) {
    stock <- stocks |> filter(ticker == t)
    trade_days <- stock |> filter(date >= valid_start) |> nrow()

    # Need 20 trading days of data from before 1/1/23
    start <- stock |> nrow() - trade_days - window
    end <- stock |> nrow()
    stock <- stock[start:end, ]
    stock$slope <- 0

    cat(paste("Generating Indicators for:", t, "\n"))

    for (i in window:nrow(stock)) {
      s <- i - window + 1
      roll_ts <- stock[s:i, ] |>
        select(close) |>
        ts(start = c(1, 1), end = c(1, window), frequency = window)
      if (length(roll_ts) < window) next
      model <- ets(roll_ts, model = "AAN")
      stock$slope[i] <- model$par["b"]
    }

    return(stock)
  })
}

set_rules <- function(stocks) {
  tickers <- stocks$ticker |> unique()
  tickers |> map_dfr(function(t) {
    stock <- stocks |> filter(ticker == t)
    stock$long <- stock$slope > 0
    stock$short <- stock$slope < 0
    stock$trade <- stock$long | stock$short
    stock$buy <- ifelse(stock$long, stock$open, ifelse(stock$short, stock$close, 0))
    stock$sell <- ifelse(stock$long, stock$close, ifelse(stock$short, stock$open, 0))
    stock$return <- ifelse(stock$trade, 1 + (stock$sell - stock$buy) / stock$buy, 1)
    return(stock)
  })
}

apply_trades <- function(stocks, days, initial_equity, max_trade) {
  tickers <- stocks$ticker |> unique()
  num_tickers <- length(tickers)

  slopes <- matrix(data = 0, nrow = days, ncol = num_tickers)
  for (t in 1:num_tickers) {
    stock <- stocks |> filter(ticker == tickers[t])
    for (i in 20:days) {
      slopes[i, t] <- abs(stock$slope[i])
    }
  }

  slopes[is.na(slopes)] <- 0
  totals <- rowSums(slopes)

  # Use abs value of slopes to apply weights
  weights <- slopes / totals
  weights[is.nan(weights)] <- 0
  weights[is.na(weights)] <- 0

  invested <- matrix(data = 0, nrow = days, ncol = num_tickers)
  revenue <- matrix(data = 0, nrow = days, ncol = num_tickers)
  start_equity <- 0
  end_equity <- initial_equity
  return <- 0

  for (period in 20:days) {
    start_equity <- end_equity
    for (t in 1:num_tickers) {
      stock <- stocks |> filter(ticker == tickers[t])
      if (is.na(stock$return[period])) next
      trade_amount <- min(weights[period, t] * start_equity, max_trade)
      invested[period, t] <- trade_amount
      revenue[period, t] <- trade_amount * stock$return[period]
      end_equity <- end_equity - trade_amount + revenue[period, t]
    }
  }

  return <- end_equity - initial_equity
  ret_pct <- return / initial_equity * 100
  return(ret_pct)
}

# Evaluate strategy
eval_strategy <- function(portfolio) {
  portfolio$cumreturn <- cumprod(portfolio$return)
  portfolio$maxreturn <- cummax(portfolio$cumreturn)

  # e.g. Treasury Bills
  risk_free_return <- 1 + 0.04 / 365
  daily_sharpe <- mean(portfolio$return - risk_free_return, na.rm = TRUE) /
    sd(portfolio$return, na.rm = TRUE)

  performance_summary <- tibble(
    "Long Trades" = sum(portfolio$long),
    "% Winning Long Trades" = mean(portfolio$return[portfolio$long] > 1, na.rm = TRUE) * 100,
    "Avg Long Trade Return" = mean(portfolio$return[portfolio$long], na.rm = TRUE),
    "Short Trades" = sum(portfolio$short),
    "% Winning Short Trades" = mean(portfolio$return[portfolio$short] > 1, na.rm = TRUE) * 100,
    "Avg Short Trade Return" = mean(portfolio$return[portfolio$short], na.rm = TRUE),
    "Daily Sharpe Ratio" = daily_sharpe,
    "Cumulative Portfolio Return" = tail(portfolio$cumreturn, 1),
    "Max Drawdown %" = min(portfolio$cumreturn / portfolio$maxreturn - 1, na.rm = TRUE) * 100,
    "Overall % Winning Trades" = mean(portfolio$return > 1, na.rm = TRUE) * 100
  )
  glimpse(performance_summary)
}

# Run the backtest
portfolio <- stocks |>
  filter_stocks(10) |>
  gen_indicators(20) |>
  set_rules()

max_trade <- 10000
initial_equity <- 100000
trade_days <- 250
ret_pct <- apply_trades(portfolio, trade_days, initial_equity, max_trade)
ret_pct

eval_strategy(portfolio)
