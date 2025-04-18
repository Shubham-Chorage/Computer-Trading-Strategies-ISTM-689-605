library(tidyverse)
library(forecast)
library(rstudioapi)
library(TTR)
library(furrr)
library(quantmod)

plan(multisession, workers = 12)

# Set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list = ls())

stocks_Ind <- read_csv("stocks.csv") |>
  filter(sector == "Industrials")
         
write.csv(stocks_Ind, "stocks_Ind.csv", row.names = FALSE)

stocks <- read_csv("stocks_Energy.csv") |> 
  mutate(date = as.Date(date)) |>
  filter(date >= as.Date("2023-01-01") & date <= as.Date("2024-12-31"))

tickers <- unique(stocks$ticker)
initial_equity <- 100000
max_trade <- 5000  #maximum value invested money per trade--maintain diversification across positions
max_day_trades <- 8 #Max number of new trades per day--prevent giving all money in 1 day

# Use two trend system
# Function to generate indicators (Two MAs)
gen_indicators <- function(sym, fast = 20, slow = 63) {
  stock <- stocks |> filter(ticker == sym) |> arrange(date)
  
  stock <- stock |>
    mutate(
      ma_fast = zoo::rollmean(close, k = fast, fill = NA, align = "right"),
      ma_slow = zoo::rollmean(close, k = slow, fill = NA, align = "right")
    )
  
  return(stock)
}

# Method 1: Signals based on MA crossover
gen_signals_method1 <- function(stock) {
  stock |> mutate(
    long_signal = if_else(lag(ma_fast) < lag(ma_slow) & ma_fast > ma_slow, 1, 0),
    short_signal = if_else(lag(ma_fast) > lag(ma_slow) & ma_fast < ma_slow, 1, 0),
    close_long = if_else(ma_fast < ma_slow, 1, 0),
    close_short = if_else(ma_fast > ma_slow, 1, 0)
  )
}


# Method 3: Signals based on trend direction (MA turning up/down)
gen_signals_method3 <- function(stock) {
  stock |> mutate(
    fast_slope = ma_fast - lag(ma_fast),
    slow_slope = ma_slow - lag(ma_slow),
    
    long_signal = if_else(fast_slope > 0 & lag(fast_slope) <= 0 & slow_slope > 0, 1, 0),
    short_signal = if_else(fast_slope < 0 & lag(fast_slope) >= 0 & slow_slope < 0, 1, 0),
    
    close_long = if_else(sign(fast_slope) != sign(slow_slope) & lag(long_signal) == 1, 1, 0),
    close_short = if_else(sign(fast_slope) != sign(slow_slope) & lag(short_signal) == 1, 1, 0)
  )
}

# Backtest skeleton for any method
run_backtest <- function(signals_data) {
  tdays <- sort(unique(signals_data$date))
  position <- tibble()
  closed <- tibble()
  pvalue <- numeric(length(tdays))
  current_cash <- initial_equity
  
  for (i in seq_along(tdays)) {
    curr_date <- tdays[i]
    today_signals <- signals_data |> filter(date == curr_date)
    
    # Close positions
    if (nrow(position) > 0) {
      closes <- position |>
        left_join(today_signals |> select(ticker, open, close_long, close_short), by = "ticker") |>
        filter((type == "Long" & close_long == 1) | (type == "Short" & close_short == 1)) |>
        mutate(
          exit_price = open,
          profit = case_when(
            type == "Long" ~ (exit_price - entry_price) * shares,
            type == "Short" ~ (entry_price - exit_price) * shares
          ),
          close_date = curr_date
        )
      
      if (nrow(closes) > 0) {
        current_cash <- current_cash + sum(closes$exit_price * closes$shares) - nrow(closes) * (1 + 0.005 * closes$shares)
        closed <- bind_rows(closed, closes)
        position <- anti_join(position, closes, by = "ticker")
      }
    }
    
    # Open new positions
    available_cash <- current_cash
    trades <- tibble()
    longs <- today_signals |> filter(long_signal == 1 & !(ticker %in% position$ticker))
    shorts <- today_signals |> filter(short_signal == 1 & !(ticker %in% position$ticker))
    opens <- bind_rows(
      longs |> mutate(type = "Long"),
      shorts |> mutate(type = "Short")
    ) |> arrange(date)
    
    if (nrow(opens) > 0) {
      trade_count <- min(nrow(opens), max_day_trades)
      opens <- opens |> slice_head(n = trade_count)
      trade_amt <- min(max_trade, available_cash / trade_count)
      opens <- opens |>
        mutate(shares = floor(trade_amt / open),
               entry_price = open,
               entry_date = curr_date) |>
        filter(shares > 0) |>
        mutate(trade_cost = 1 + 0.005 * shares,
               cash_used = shares * open + trade_cost)
      current_cash <- current_cash - sum(opens$cash_used)
      trades <- opens
    }
    
    if (nrow(trades) > 0) {
      position <- bind_rows(position, trades |> select(ticker, entry_date, entry_price, shares, type))
    }
    
    # Calculate portfolio value
    prices_today <- signals_data |> filter(date == curr_date) |> select(ticker, close)
    if (nrow(position) > 0) {
      values <- position |> left_join(prices_today, by = "ticker") |>
        mutate(val = shares * close)
      
      val_sum <- if (!is.null(values$val) && length(values$val) > 0 && all(!is.na(values$val))) {
        sum(values$val, na.rm = TRUE)
      } else {
        0
      }
      
      pvalue[i] <- val_sum + current_cash
    } else {
      pvalue[i] <- current_cash
    }
  }
  
  # ✅ Return a result
  return(tibble(date = tdays, portfolio_value = pvalue))
}


# Run backtests
results_method1 <- map_df(tickers, ~gen_indicators(.x)) |> group_by(ticker) |> group_modify(~gen_signals_method1(.x))
results_method3 <- map_df(tickers, ~gen_indicators(.x)) |> group_by(ticker) |> group_modify(~gen_signals_method3(.x))

portfolio_method1 <- run_backtest(results_method1)
portfolio_method3 <- run_backtest(results_method3)

#Plot results (optional)
library(ggplot2)
ggplot() +
geom_line(data = portfolio_method1, aes(x = date, y = portfolio_value), color = "blue") +
geom_line(data = portfolio_method3, aes(x = date, y = portfolio_value), color = "green") +
labs(title = "Backtest Results", y = "Portfolio Value", x = "Date")

# Results ready for evaluation
