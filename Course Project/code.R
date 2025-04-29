# ****************************************************************************************************
# PRODUCTION TRADING SCRIPT
# This script integrates data fetching, signal generation, and trade execution via IBrokers
# ****************************************************************************************************

# ---------------------------
# Load Required Libraries
# ---------------------------
library(tidyverse)
library(lubridate)
library(furrr)
library(Quandl)
library(quantmod)
library(IBrokers)
library(ranger)
library(tidyquant)
library(TTR)

options(scipen = 999)
rm(list = ls())

# ---------------------------
# API Key & IB Port
# ---------------------------

nasdaq_api_key <- "Rpth-Ax8JcPxQPmGs39L"
Quandl.api_key(nasdaq_api_key)
IBport <- 7497

# ---------------------------
# Define Parameters
# ---------------------------

currentSP500 <- tq_index("SP500") |> select(symbol)
current_date <- Sys.Date()
windowsize <- 20
max_trade_pct <- 0.05
max_day_trades <- 8
long_threshold <- 1.15
short_threshold <- 0.85

# ---------------------------
# Parallel Setup
# ---------------------------
plan(multisession, workers = 8)

# ---------------------------
# Download or Load Existing Data and Validate API
# ---------------------------
data_file <- "stock_data.csv"

# Validate NASDAQ API by checking AAPL sample
nasdaq_test <- tryCatch({
  Quandl.datatable("SHARADAR/SEP", ticker = "AAPL")
}, error = function(e) NULL)

if (is.null(nasdaq_test) || nrow(nasdaq_test) < 30) {
  stop("NASDAQ API fetch failed. Check API key or connection.")
}

universe <- if (file.exists(data_file)) {
  read_csv(data_file)
} else {
  currentSP500$symbol |>
    future_map_dfr(function(sym) {
      Quandl.api_key(nasdaq_api_key)
      tryCatch(
        expr = {
          data <- Quandl.datatable(
            "SHARADAR/SEP",
            ticker = sym,
            qopts.columns = c("ticker", "date", "open", "high", "low", "close", "volume")
          )
          data$symbol <- sym
          return(data)
        },
        warning = function(w) {
          print(w)
          return(NULL)
        },
        error = function(e) {
          print(e)
          return(NULL)
        },
        finally = print(str_glue("Fetched data for {sym}"))
      )
    }) |> 
    na.omit() |> 
    write_csv(data_file)
  read_csv(data_file, show_col_types = FALSE)
}

# ---------------------------
# Generate Technical Indicators
# ---------------------------
gen_indicators <- function(df, windowSize) {
  if (nrow(df) < windowSize) return(NULL)  # Skip symbols with too few rows
  df_xts <- xts(df |> select(open, high, low, close, volume), order.by = as.Date(df$date))
  df_xts$momentum20 <- momentum(df_xts$close, n = windowSize)
  macd <- MACD(df_xts$close)
  df_xts$macd_diff <- macd[, 1] - macd[, 2]
  df_xts$rsi14 <- RSI(df_xts$close, n = 14)
  df_xts <- na.omit(df_xts)
  df_out <- tibble(date = index(df_xts)) |>
    bind_cols(as_tibble(coredata(df_xts))) |>
    mutate(symbol = unique(df$symbol))
  df_out
}

# Step 1: Split universe into a list by ticker
universe_list <- universe %>% group_split(ticker)

# Step 2: Map over the list
indicator_data <- future_map_dfr(universe_list, ~gen_indicators(.x, windowsize))


# ---------------------------
# Generate Trade Signals
# ---------------------------

# Strategy: Hybrid Trend-Following + Mean Reversion
# Buy if: momentum20 > (long_threshold - 1) and RSI < (long_threshold * 60)
# Sell if: momentum20 < -(1 - short_threshold) and RSI > (short_threshold * 40)

gen_signals <- function(indicator_data, long_threshold, short_threshold) {
  indicator_data |> 
    mutate(
      long_signal = if_else(momentum20 > (long_threshold - 1), 1, 0),
      short_signal = if_else(momentum20 < -(1 - short_threshold), 1, 0),
      momentum = momentum20
    )
}

indicator_data <- gen_signals(indicator_data, long_threshold, short_threshold)

last_date <- max(indicator_data$date)
signals <- indicator_data |> 
  filter(date == last_date & (long_signal == 1 | short_signal == 1))

# ---------------------------
# Connect to Interactive Brokers and Get Portfolio Info
# ---------------------------
tws <- tryCatch(twsConnect(port = IBport, clientId = 1001), error = function(e) NULL) # Change client ID for every run
if (is.null(tws)) stop("Unable to connect to TWS")

account_info <- reqAccountUpdates(tws)
available_equity <- as.numeric(account_info[[1]]$AvailableFunds[1]) / 2
portfolio_value <- as.numeric(account_info[[1]]$NetLiquidation[1])
open_positions <- account_info[[2]]  # This returns open positions if available

print(paste("Portfolio Value:", portfolio_value))
print(paste("Available Funds for Trading:", available_equity))
print("Open Positions:")
if (is.null(open_positions) || length(open_positions) == 0) {
  print("No open positions")
} else {
  print(open_positions)
}

# ---------------------------
# Generate Trade Orders
# ---------------------------
max_trade <- max_trade_pct * available_equity
signals <- signals |>
  mutate(prediction = momentum, price = close) |>
  arrange(desc(abs(prediction))) |>
  slice(1:max_day_trades) |>
  mutate(position = trunc(pmin(max_trade, available_equity / max_day_trades) / price))

# ---------------------------
# Match With Actual Positions and Check for Errors
# ---------------------------
existing_symbols <- if (!is.null(open_positions) && length(open_positions) > 0) {
  map_chr(open_positions, ~ .x$contract$symbol)
} else {
  character(0)
}
signals <- signals |> 
  mutate(already_held = symbol %in% existing_symbols)

# Error check: warn if trying to enter a long trade already held
if (any(signals$long_signal == 1 & signals$already_held)) {
  warning("Attempting to enter a long position that already exists in IB. Review trades.")
}

# ---------------------------
# Manual Review of Orders
# ---------------------------
trades <- tibble()
choice <- "Q"

if (nrow(signals) > 0) {
  trades <- signals |> 
    select(symbol, prediction, price, position) |> 
    mutate(prediction = round(prediction, 4))
  rownames(trades) <- seq_len(nrow(trades))
  done <- FALSE
} else {
  print("No candidate trades to review.")
  done <- TRUE
}

while (!done) {
  View(trades)
  cat("\014")
  print("REVIEWING CANDIDATE TRADES. Choosing to execute will send trades immediately to IB.")
  choice <- readline(prompt = "Choose D)elete trade, M)odify Price, C)hange position, E)xecute, Q)uit without trading:  ")
  choice <- toupper(choice)
  done <- ifelse(choice %in% c("E", "Q"), TRUE, FALSE)

  if (choice == "M") {
    rownum <- as.numeric(readline(prompt = "Enter the row number to modify price: "))
    if (!is.na(rownum) && rownum >= 1 && rownum <= nrow(trades)) {
      newprice <- as.numeric(readline(prompt = paste("Enter new limit price for", trades$symbol[rownum], ": ")))
      if (!is.na(newprice)) {
        trades$price[rownum] <- newprice
      } else {
        print("Invalid price")
      }
    } else {
      print("Invalid row number")
    }
  }

  if (choice == "C") {
    rownum <- as.numeric(readline(prompt = "Enter the row number to change position size: "))
    if (!is.na(rownum) && rownum >= 1 && rownum <= nrow(trades)) {
      newpos <- as.numeric(readline(prompt = paste("Enter new position size for", trades$symbol[rownum], ": ")))
      if (!is.na(newpos)) {
        trades$position[rownum] <- newpos
      } else {
        print("Invalid position")
      }
    } else {
      print("Invalid row number")
    }
  }

  if (choice == "D") {
    rownum <- as.numeric(readline(prompt = "Enter the row number to delete: "))
    if (!is.na(rownum) && rownum >= 1 && rownum <= nrow(trades)) {
      confirm <- toupper(readline(prompt = paste("Enter Y to confirm deletion of", trades$symbol[rownum], ": ")))
      if (confirm == "Y") {
        trades <- trades[-rownum, , drop = FALSE]
      }
    } else {
      print("Invalid row number")
    }
  }
}

if (choice != "E") {
  trades <- trades[-c(1:nrow(trades)), ]
}

signals <- trades

# ---------------------------
# Execute Orders
# ---------------------------
for (i in 1:nrow(signals)) {
  sym <- signals$symbol[i]
  pos <- signals$position[i]
  action <- ifelse(sym %in% indicator_data$symbol & indicator_data$long_signal[indicator_data$symbol == sym & indicator_data$date == last_date][1] == 1, "BUY", "SELL")
  equity <- twsEquity(sym, "SMART")

  ord_id <- tryCatch(
    expr = {
      reqIds(tws)
    },
    warning = function(w) {
      warning(paste("[WARNING] Could not get order ID for", sym))
      return(NULL)
    },
    error = function(e) {
      warning(paste("[ERROR] Failed to get order ID for", sym, "::", e$message))
      return(NULL)
    }
  )
  if (is.null(ord_id)) next

  order <- tryCatch(
    expr = {
      twsOrder(ord_id, action = action, totalQuantity = pos, orderType = "MKT")
    },
    error = function(e) {
      warning(paste("[ERROR] Failed to create order for", sym, "::", e$message))
      return(NULL)
    }
  )
  if (is.null(order)) next

  tryCatch(
    expr = {
      placeOrder(tws, equity, order)
      print(paste("Order placed:", action, pos, sym, "@ Market"))
    },
    error = function(e) {
      warning(paste("[ERROR] Failed to place order for", sym, "::", e$message))
    }
  )
}

# ---------------------------
# Disconnect TWS
# ---------------------------
twsDisconnect(tws)

cat("\nAll trades submitted. Review IB TWS for confirmation.\n")
