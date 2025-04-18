# -----------------------------
# 1. Pre-Filtering & Setup
# -----------------------------
library(tidyverse)
library(lubridate)
library(furrr)
library(scales)     
library(ggplot2)
library(rstudioapi)

# Use 8 workers for multiprocessing
plan(multisession, workers = 8)

# Set working directory based on the current file path
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list = ls())
options(scipen = 999)

# Set backtest period
backtest_start <- as_date("2023-01-01")
backtest_end   <- as_date("2024-12-31")

# -----------------------------
# 2. Load Data and Pre-filter
# -----------------------------
# Load data and filter for S&P 500 Energy sector stocks.
# We include 40 days before the start date to cover the 20-day rolling window.
energy_stocks <- read_csv("stocks.csv") %>%
  filter(sector == "Energy" & date >= backtest_start - 40 & date <= backtest_end) %>%
  arrange(ticker, date) %>%
  select(ticker, date, open, close, sector)

# Exclude problematic tickers.
excluded_tickers <- c("IR", "AXON", "BLDR")
energy_stocks <- energy_stocks %>% filter(!ticker %in% excluded_tickers)

# Industrials sector
industrials_stocks <- read_csv("stocks.csv") %>%
  filter(sector == "Industrials" & date >= backtest_start - 40 & date <= backtest_end) %>%
  arrange(ticker, date) %>%
  select(ticker, date, open, close, sector) %>%
  filter(!ticker %in% excluded_tickers)

# Information Technology sector
tech_stocks <- read_csv("stocks.csv") %>%
  filter(sector == "Information Technology" & date >= backtest_start - 40 & date <= backtest_end) %>%
  arrange(ticker, date) %>%
  select(ticker, date, open, close, sector) %>%
  filter(!ticker %in% excluded_tickers)

# (A) Generate Momentum Signals
gen_momentum_signals <- function(stocks, window = 20, threshold = 0.15) {
  stocks <- stocks %>% 
    group_by(ticker) %>% 
    arrange(date) %>%
    mutate(momentum = (close / lag(close, window)) - 1,
           long = if_else(momentum > threshold, TRUE, FALSE),
           short = if_else(momentum < -threshold, TRUE, FALSE),
           trade = long | short,
           # For long trades: buy at open, sell at close;
           # for short trades: sell at open, cover at close.
           buy_price = if_else(long, open, if_else(short, close, NA_real_)),
           sell_price = if_else(long, close, if_else(short, open, NA_real_))) %>% 
    ungroup()
  return(stocks)
}

# (B) Simulate Trades & Record Trade Log and Daily Returns
simulate_trades <- function(stocks, backtest_start, backtest_end,
                            initial_equity = 100000, max_trade = 5000) {
  # Select only dates within the backtest period.
  trade_dates <- sort(unique(stocks$date[stocks$date >= backtest_start & stocks$date <= backtest_end]))
  equity <- initial_equity
  daily_returns <- tibble(date = trade_dates, equity = NA_real_, daily_return = NA_real_)
  trade_log <- tibble(date = as.Date(character()), ticker = character(),
                      trade_type = character(), trade_amount = double(),
                      buy_price = double(), sell_price = double(),
                      shares = double(), tx_cost = double(),
                      gross_revenue = double(), net_revenue = double(),
                      trade_return = double())
  
  for(i in seq_along(trade_dates)) {
    current_date <- trade_dates[i]
    # Filter stocks with a trade signal for the current day.
    day_data <- stocks %>% filter(date == current_date, trade == TRUE)
    
    # Instead of using absolute momentum directly, weight = (|momentum|^2) to emphasize stronger signals.
    day_data <- day_data %>% mutate(signal_strength = if_else(trade, abs(momentum)^2, 0))
    total_signal <- sum(day_data$signal_strength, na.rm = TRUE)
    
    if(total_signal == 0) {
      daily_returns$equity[i] <- equity
      daily_returns$daily_return[i] <- 0
      next
    }
    
    day_data <- day_data %>% mutate(weight = signal_strength / total_signal)
    
    # Simulate trades for each stock with a signal.
    for(j in 1:nrow(day_data)) {
      stock_row <- day_data[j, ]
      trade_amount <- min(stock_row$weight * equity, max_trade)
      if(trade_amount <= 0) next
      
      if(stock_row$long) {
        trade_type <- "long"
        buy_price <- stock_row$open
        sell_price <- stock_row$close
        trade_return <- (sell_price - buy_price) / buy_price
      } else if(stock_row$short) {
        trade_type <- "short"
        # For short: sell at open and cover at close.
        buy_price <- stock_row$close   # cover price
        sell_price <- stock_row$open     # sale price
        trade_return <- (buy_price - sell_price) / buy_price
      } else {
        next
      }
      
      shares <- trade_amount / buy_price
      # Transaction cost: fixed $1 per leg plus $0.005 per share each time.
      tx_cost <- 2 * (1 + (shares * 0.005))
      gross_revenue <- trade_amount * (1 + trade_return)
      net_revenue <- gross_revenue - tx_cost
      
      equity <- equity - trade_amount + net_revenue
      
      trade_log <- trade_log %>% add_row(date = current_date,
                                         ticker = stock_row$ticker,
                                         trade_type = trade_type,
                                         trade_amount = trade_amount,
                                         buy_price = buy_price,
                                         sell_price = sell_price,
                                         shares = shares,
                                         tx_cost = tx_cost,
                                         gross_revenue = gross_revenue,
                                         net_revenue = net_revenue,
                                         trade_return = trade_return)
    }
    
    daily_returns$equity[i] <- equity
    if(i == 1) {
      daily_returns$daily_return[i] <- 0
    } else {
      daily_returns$daily_return[i] <- (equity - daily_returns$equity[i-1]) / daily_returns$equity[i-1]
    }
  }
  
  list(final_equity = equity, 
       ret_pct = (equity - initial_equity) / initial_equity * 100,
       daily_returns = daily_returns,
       trade_log = trade_log)
}

# (C) Evaluate Performance Metrics & Plot Graphs
eval_performance <- function(trade_log, daily_returns, title_suffix = "") {
  # Ensure there are no missing values in daily_returns
  daily_returns <- daily_returns %>% mutate(daily_return = replace_na(daily_return, 0))
  
  # Compute cumulative return
  cum_return <- prod(1 + daily_returns$daily_return, na.rm = TRUE) - 1
  
  # Compute cumulative equity and drawdowns
  cum_eq <- cumprod(1 + daily_returns$daily_return)
  max_cum_eq <- cummax(cum_eq)
  drawdowns <- cum_eq / max_cum_eq - 1
  
  # Handle cases where drawdowns are NA
  if (all(is.na(drawdowns))) {
    max_drawdown <- NA
  } else {
    max_drawdown <- min(drawdowns, na.rm = TRUE) * 100
  }
  
  # Handle cases where standard deviation is NA or zero
  daily_return_sd <- sd(daily_returns$daily_return, na.rm = TRUE)
  if (is.na(daily_return_sd) || daily_return_sd == 0) {
    sharpe_ratio <- NA
  } else {
    sharpe_ratio <- round((mean(daily_returns$daily_return, na.rm = TRUE) - (0.04/365)) / daily_return_sd, 4)
  }
  
  # Compute trade performance summary
  long_trades <- trade_log %>% filter(trade_type == "long")
  short_trades <- trade_log %>% filter(trade_type == "short")
  
  performance_summary <- tibble(
    "Long Trades" = nrow(long_trades),
    "% Winning Long Trades" = if(nrow(long_trades) > 0) round(mean(long_trades$trade_return > 0, na.rm = TRUE) * 100, 2) else NA,
    "Avg Long Trade Return" = if(nrow(long_trades) > 0) round(mean(long_trades$trade_return, na.rm = TRUE), 4) else NA,
    "Short Trades" = nrow(short_trades),
    "% Winning Short Trades" = if(nrow(short_trades) > 0) round(mean(short_trades$trade_return > 0, na.rm = TRUE) * 100, 2) else NA,
    "Avg Short Trade Return" = if(nrow(short_trades) > 0) round(mean(short_trades$trade_return, na.rm = TRUE), 4) else NA,
    "Daily Sharpe Ratio" = sharpe_ratio,
    "Cumulative Portfolio Return" = round(cum_return, 4),
    "Max Drawdown (%)" = round(max_drawdown, 2),
    "Overall % Winning Trades" = if(nrow(trade_log) > 0) round(mean(trade_log$trade_return > 0, na.rm = TRUE) * 100, 2) else NA
  )
  
  cat("Performance Summary", title_suffix, ":\n")
  glimpse(performance_summary)
  cat("\nFinal Equity:", daily_returns$equity[nrow(daily_returns)], "\n")
  
  # Equity Curve Plot
  p1 <- ggplot(daily_returns, aes(x = date, y = equity)) +
    geom_line(color = "blue") +
    labs(title = paste("Equity Curve", title_suffix),
         x = "Date", y = "Equity ($)") +
    theme_minimal()
  
  # Drawdown Plot
  dd_df <- tibble(date = daily_returns$date, drawdown = drawdowns)
  p2 <- ggplot(dd_df, aes(x = date, y = drawdown)) +
    geom_line(color = "red") +
    scale_y_continuous(labels = percent) +
    labs(title = paste("Drawdowns", title_suffix),
         x = "Date", y = "Drawdown (%)") +
    theme_minimal()
  
  print(p1)
  print(p2)
  
  return(performance_summary)
}

#  Running Backtests & Generating Plots

# Energy sector
energy_signals <- gen_momentum_signals(energy_stocks, window = 20, threshold = 0.15)
energy_result <- simulate_trades(energy_signals, backtest_start, backtest_end,
                                 initial_equity = 100000, max_trade = 5000)
energy_performance <- eval_performance(energy_result$trade_log, energy_result$daily_returns,
                                       title_suffix = "(Energy Sector)")
cat("\nEnergy Final Equity:", energy_result$final_equity, "\n")
cat("Energy Return (%):", round(energy_result$ret_pct, 2), "%\n")

# Information Technology sector
tech_signals <- gen_momentum_signals(tech_stocks, window = 20, threshold = 0.15)
tech_result <- simulate_trades(tech_signals, backtest_start, backtest_end,
                               initial_equity = 100000, max_trade = 5000)
tech_performance <- eval_performance(tech_result$trade_log, tech_result$daily_returns,
                                     title_suffix = "(Information Technology Sector)")
cat("\nInformation Technology Final Equity:", tech_result$final_equity, "\n")
cat("Information Technology Return (%):", round(tech_result$ret_pct, 2), "%\n")

# Industrials sector
industrials_signals <- gen_momentum_signals(industrials_stocks, window = 20, threshold = 0.15)
industrials_result <- simulate_trades(industrials_signals, backtest_start, backtest_end,
                                      initial_equity = 100000, max_trade = 5000)
industrials_performance <- eval_performance(industrials_result$trade_log, industrials_result$daily_returns,
                                            title_suffix = "(Industrials Sector)")

cat("\nIndustrials Final Equity:", industrials_result$final_equity, "\n")
cat("Industrials Return (%):", round(industrials_result$ret_pct, 2), "%\n")

# Last part requires to compare performance with Energy from year 20-21
# Set new backtest period for Energy sector (2020-2021)

backtest_start_energy <- as_date("2020-01-01")
backtest_end_energy   <- as_date("2021-12-31")

energy_stocks_2020_2021 <- read_csv("stocks.csv") %>%
  filter(sector == "Energy" & date >= backtest_start_energy - 40 & date <= backtest_end_energy) %>%
  arrange(ticker, date) %>%
  select(ticker, date, open, close, sector) %>%
  filter(!ticker %in% excluded_tickers)

# Generate momentum signals for Energy sector (2020-2021) and simulate trades
energy_signals_2020_2021 <- gen_momentum_signals(energy_stocks_2020_2021, window = 20, threshold = 0.15)

energy_result_2020_2021 <- simulate_trades(energy_signals_2020_2021, backtest_start_energy, backtest_end_energy,
                                           initial_equity = 100000, max_trade = 5000)

# Evaluate performance and generate plots for Energy sector (2020-2021)
energy_performance_2020_2021 <- eval_performance(energy_result_2020_2021$trade_log, energy_result_2020_2021$daily_returns,
                                                 title_suffix = "(Energy Sector 2020-2021)")

# Print final equity and returns for Energy sector (2020-2021)
cat("\nEnergy (2020-2021) Final Equity:", energy_result_2020_2021$final_equity, "\n")
cat("Energy (2020-2021) Return (%):", round(energy_result_2020_2021$ret_pct, 2), "%\n")



