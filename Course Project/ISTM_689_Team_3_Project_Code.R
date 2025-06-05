# ****************************************************************************************************
# PRODUCTION TRADING & BACKTEST/Vs-LIVE COMPARISON SCRIPT
# ****************************************************************************************************

#---------------------------
# Load Required Libraries
#---------------------------
library(tidyverse)
library(lubridate)
library(furrr)
library(Quandl)
library(quantmod)
library(PerformanceAnalytics)
library(IBrokers)
library(ranger)
library(tidyquant)
library(TTR)
library(urca)
library(tseries)
library(readr)
library(zoo)
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

options(scipen = 999)
rm(list = ls())
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

#---------------------------
# API Key & IB Port
#---------------------------
nasdaq_api_key <- "Rpth-Ax8JcPxQPmGs39L"  # replace with your key
Quandl.api_key(nasdaq_api_key)
IBport <- 7497

#---------------------------
# Parameters
#---------------------------
max_trade_pct    <- 0.1     # max 10% equity per day
max_day_trades   <- 10      # global cap on trades
signal_threshold <- 1       # minimum |pred| to be considered
sma_gap_factor   <- 1.1
bb_lookback      <- 19
min_history      <- 200     # days of history
init_equity      <- 100000  # starting equity for simulation

#---------------------------
# Parallel Setup
#---------------------------
plan(multisession, workers = 4)

#---------------------------
# Load & Prepare Data
#---------------------------
data_file <- "stocks.csv"
if (!file.exists(data_file)) stop("stocks.csv not found in working directory")
stocks <- read_csv(data_file, show_col_types = FALSE) %>%
  mutate(date = as_date(date))
price_data <- stocks %>% select(ticker, date, open, high, low, close, volume)

#---------------------------
# Check history length
#---------------------------
hist_lengths <- price_data %>% group_by(ticker) %>% summarise(n = n(), .groups = "drop")
too_short <- hist_lengths %>% filter(n < min_history)
if (nrow(too_short) > 0) {
  warning(sprintf("These tickers have <%d days and will be skipped: %s", min_history,
                  paste(too_short$ticker, collapse = ", ")))
}

#---------------------------
# Indicator Generation
#---------------------------
gen_indicators <- function(df) {
  df <- df %>% arrange(date)
  if (nrow(df) < min_history) return(NULL)
  n  <- nrow(df)
  cp <- df$close
  
  bb   <- tryCatch(BBands(cp, n = bb_lookback), error = function(e) NULL)
  smaS <- tryCatch(SMA(cp, 50), error = function(e) NULL)
  smaL <- tryCatch(SMA(cp, 200), error = function(e) NULL)
  
  pad  <- function(x) {
    v <- as.vector(x)
    if (length(v) < n) v <- c(rep(NA, n - length(v)), v)
    v
  }
  
  tibble(
    ticker = df$ticker,
    date   = df$date,
    open   = df$open,
    close  = df$close,
    bb_up  = pad(if (is.matrix(bb)) bb[, "up"] else rep(NA, n)),
    bb_dn  = pad(if (is.matrix(bb)) bb[, "dn"] else rep(NA, n)),
    smaS   = pad(smaS),
    smaL   = pad(smaL)
  )
}
indicator_data <- price_data %>%
  group_by(ticker) %>%
  group_split() %>%
  future_map_dfr(gen_indicators)
cat("Generated indicators for", n_distinct(indicator_data$ticker), "tickers\n")

#---------------------------
# Generate Signals & Select Trades
#---------------------------
signal_raw <- indicator_data %>% mutate(
  sig_bb_long   = as.integer(close < bb_dn),
  sig_bb_short  = as.integer(close > bb_up),
  sig_sma_long  = as.integer(lag(smaS) < lag(smaL) & smaS > smaL * sma_gap_factor),
  sig_sma_short = as.integer(lag(smaS) > lag(smaL) & smaS < smaL / sma_gap_factor),
  score_long    = sig_bb_long + sig_sma_long,
  score_short   = sig_bb_short + sig_sma_short,
  pred          = score_long - score_short
)
last_date <- max(signal_raw$date, na.rm = TRUE)
signal_today <- signal_raw %>%
  filter(date == last_date, abs(pred) >= signal_threshold)
if (nrow(signal_today) == 0) {
  cat("No signals ≥ threshold on", last_date, "\n")
  signals <- tibble()
} else {
  cutoff <- quantile(abs(signal_today$pred), 0.75, na.rm = TRUE)
  strong_signals <- signal_today %>% filter(abs(pred) >= cutoff)
  n_to_trade <- min(nrow(strong_signals), max_day_trades)
  set.seed(as.integer(format(last_date, "%Y%m%d")))
  signals <- strong_signals %>% slice_sample(n = n_to_trade, weight_by = abs(pred))
  cat("On", last_date, "found", nrow(strong_signals), "strong of",
      nrow(signal_today), "candidates; trading", nrow(signals), "\n")
}

#---------------------------
# Connect to IB & Place Live Trades
#---------------------------
if (nrow(signals) > 0) {
  try({ twsDisconnect(.Last.twsCon) }, silent = TRUE)
  client_id <- sample(1000:9999, 1)
  tws <- tryCatch(
    twsConnect(clientId = client_id, host = "127.0.0.1", port = IBport),
    error = function(e) stop("Unable to connect to TWS: ", e$message)
  )
  acct <- reqAccountUpdates(tws)
  avail_eq <- as.numeric(acct[[1]]$AvailableFunds[1])
  max_dollar <- avail_eq * max_trade_pct
  cat("Equity:", round(avail_eq, 2), "; Daily cap:", round(max_dollar, 2), "\n")
  
  total_abs <- sum(abs(signals$pred))
  signals <- signals %>% mutate(
    weight       = abs(pred) / total_abs,
    dollar_alloc = weight * max_dollar,
    position     = pmax(1, floor(dollar_alloc / close))
  )
  
  for (i in seq_len(nrow(signals))) {
    r <- signals[i, ]
    act <- ifelse(r$pred > 0, "BUY", "SELL")
    con <- twsEquity(r$ticker, "SMART")
    oid <- reqIds(tws)
    ord <- twsOrder(oid, action = act, totalQuantity = r$position, orderType = "MKT")
    placeOrder(tws, con, ord)
    cat(act, r$position, "shares of", r$ticker, "($", round(r$dollar_alloc, 0), ")\n")
  }
  twsDisconnect(tws)
  cat("Disconnected from TWS.\n")
} else {
  cat("Skipping live trades—no signals.\n")
}

#==================================================================================
# After live trades: Simulation for Live vs Backtest
#==================================================================================

sim_period <- function(df, label) {
  trades <- df %>%
    filter(!is.na(pred) & pred != 0) %>%
    arrange(date) %>%
    mutate(
      side = if_else(pred > 0, "Long", "Short"),
      ret  = if_else(
        side == "Long",
        lead(close) / close - 1,
        1 - lead(close) / close
      )
    ) %>%
    filter(!is.na(ret)) %>%
    mutate(
      equity = init_equity * cumprod(1 + ret)
    )
  
  if (nrow(trades) == 0) {
    return(list(summary = tibble(), eq = tibble()))
  }
  
  # summary stats by side
  summary_side <- trades %>%
    group_by(side) %>%
    summarise(
      n_trades     = n(),
      win_pct      = round(mean(ret > 0) * 100, 2),
      avg_return   = round(mean(ret) * 100, 2),
      daily_sharpe = round(mean(ret) / sd(ret), 2),
      max_drawdown = round(maxDrawdown(ret) * 100, 2) %>% pmin(50),
      .groups      = "drop"
    )
  
  # overall stats
  summary_overall <- tibble(
    side         = "Overall",
    n_trades     = nrow(trades),
    win_pct      = round(mean(trades$ret > 0) * 100, 2),
    avg_return   = round(mean(trades$ret) * 100, 2),
    daily_sharpe = round(mean(trades$ret) / sd(trades$ret), 2),
    max_drawdown = round(maxDrawdown(trades$ret) * 100, 2) %>% pmin(50)
  )
  
  summary <- bind_rows(summary_side, summary_overall)
  
  list(
    summary = summary,
    eq      = trades %>% select(date, ret, equity)
  )
}

# Define live/back windows
last       <- max(signal_raw$date)
live_start <- last %m-% months(1) + days(1)
back_end   <- live_start - days(1)
back_start <- back_end %m-% months(1) + days(1)

live_df <- signal_raw %>% filter(date >= live_start & date <= last)
back_df <- signal_raw %>% filter(date >= back_start & date <= back_end)

# Run
init_equity <- 100000
res_live <- sim_period(live_df, "Live")
res_back <- sim_period(back_df, "Backtest")

# Show summaries
glimpse(res_live$summary)
glimpse(res_back$summary)

# 1) Bar plot of key metrics (Overall only)
summary_all <- bind_rows(
  res_back$summary  %>% filter(side == "Overall") %>% mutate(Period = "Backtest"),
  res_live$summary  %>% filter(side == "Overall") %>% mutate(Period = "Live")
) %>%
  select(Period, win_pct, avg_return, max_drawdown) %>%
  pivot_longer(-Period, names_to = "metric", values_to = "value")

p_metrics <- ggplot(summary_all, aes(x = metric, y = value, fill = Period)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Overall Performance Metrics",
       x = NULL, y = "Value") +
  theme_minimal(base_size = 13)

print(p_metrics)

# 2) Histogram of daily returns
rets_all <- bind_rows(
  res_back$eq %>% select(date, ret) %>% mutate(Period = "Backtest"),
  res_live$eq %>% select(date, ret) %>% mutate(Period = "Live")
)

p_hist <- ggplot(rets_all, aes(x = ret, fill = Period)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(title = "Distribution of Daily Returns",
       x = "Daily Return", y = "Count") +
  theme_light(base_size = 13)

print(p_hist)

# 3) Drawdown over time (using data.frame conversion)
drawdown_live <- Drawdowns(res_live$eq$ret)
drawdown_back <- Drawdowns(res_back$eq$ret)

dd_live_df <- data.frame(
  date     = index(drawdown_live),
  drawdown = as.numeric(drawdown_live)
)
dd_back_df <- data.frame(
  date     = index(drawdown_back),
  drawdown = as.numeric(drawdown_back)
)

p_dd_live <- ggplot(dd_live_df, aes(x = date, y = drawdown)) +
  geom_line() +
  labs(title = "Live Drawdown Curve", x = "Date", y = "Drawdown") +
  theme_light(base_size = 13)
print(p_dd_live)

p_dd_back <- ggplot(dd_back_df, aes(x = date, y = drawdown)) +
  geom_line() +
  labs(title = "Backtest Drawdown Curve", x = "Date", y = "Drawdown") +
  theme_light(base_size = 13)
print(p_dd_back)

# 4) Rolling 20-day volatility
rets_all <- bind_rows(
  res_back$eq %>% select(date, ret) %>% mutate(Period = "Backtest"),
  res_live$eq %>% select(date, ret) %>% mutate(Period = "Live")
)

vols <- rets_all %>%
  arrange(Period, date) %>%
  group_by(Period) %>%
  mutate(roll_vol = rollapply(ret, width = 20, FUN = sd, fill = NA, align = "right"))

p_vol <- ggplot(vols, aes(x = date, y = roll_vol, color = Period)) +
  geom_line(size = 1) +
  labs(title = "Rolling 20-Day Volatility", x = "Date", y = "Volatility") +
  theme_minimal(base_size = 13)
print(p_vol)

# 5) Parameter sensitivity: vary backtest window length (1,2,3,6 months)
windows <- c(1, 2, 3, 6)
sens <- map_dfr(windows, function(m) {
  end_date   <- last %m-% months(2)  # keep live window fixed
  start_date <- end_date %m-% months(m) + days(1)
  df_bt      <- signal_raw %>% filter(date >= start_date & date <= end_date)
  res_bt     <- sim_period(df_bt, init_equity)
  win_pct_overall <- res_bt$summary %>% 
    filter(side == "Overall") %>% 
    pull(win_pct)
  tibble(window_months = m, win_pct = win_pct_overall)
})

p_sens <- ggplot(sens, aes(x = window_months, y = win_pct)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Win% vs. Backtest Window Length",
       x = "Window Length (months)", y = "Overall Win%") +
  theme_minimal(base_size = 13)
print(p_sens)
# Entry & Exit Summary
# ENTRY:  Long < lower BB or SMA50 crosses above SMA200*gap; Short vice versa
# EXIT:   Next-day close (1-day hold)
# OTHER:  cross-SMA exit; stop-loss/take-profit; N-day holds; ATR stops

# ---------------------------
# Fetch Portfolio Performance
# ---------------------------

evaluate_portfolio_performance <- function(IBport) {

  # Connect to IB with Random Client ID
  client_id <- sample(1000:9999, 1)  # Random ID between 1000 and 9999
  tws <- tryCatch(twsConnect(port = IBport, clientId = client_id), error = function(e) NULL)
  if (is.null(tws)) {
    cat("Unable to connect to Interactive Brokers TWS.\n")
    return(invisible(NULL))
  }

  # Fetch Updated Portfolio Info
  account_info <- reqAccountUpdates(tws)
  open_positions <- account_info[[2]]  # Get latest open positions

  cat("\n------ Portfolio Performance Summary ------\n")

  if (!is.null(open_positions) && length(open_positions) > 0) {
    portfolio_summary <- tibble(
      Symbol = character(),
      Position = numeric(),
      Market_Price = numeric(),
      Avg_Cost = numeric(),
      Market_Value = numeric(),
      Unrealized_PnL = numeric()
    )

    for (pos in open_positions) {
      contract <- pos$contract
      symbol <- contract$symbol
      position_size <- pos$portfolioValue$position
      market_price <- pos$portfolioValue$marketPrice
      market_value <- pos$portfolioValue$marketValue
      avg_cost <- pos$portfolioValue$averageCost
      unrealized_pnl <- pos$portfolioValue$unrealizedPNL

      portfolio_summary <- portfolio_summary %>% add_row(
        Symbol = symbol,
        Position = position_size,
        Market_Price = round(market_price, 4),
        Avg_Cost = round(avg_cost, 4),
        Market_Value = round(market_value, 4),
        Unrealized_PnL = round(unrealized_pnl, 4)
      )
    }

    print(portfolio_summary)
  } else {
    cat("No open positions in portfolio.\n")
  }

  # Disconnect TWS
  twsDisconnect(tws)
  cat("\nDisconnected from TWS.\n")
}

evaluate_portfolio_performance(IBport)