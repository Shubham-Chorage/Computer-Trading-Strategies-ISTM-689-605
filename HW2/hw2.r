# Stock symbols provided in "SP Tickers.csv"
# Task: create database, by day for each stock, that contains the adjusted:
# open, high, low, close, and volume
# Date range should cover last 10 years available in Sharadar Equities Bundles
library(tidyverse)
library(furrr)
library(NasdaqDataLink)
NasdaqDataLink.api_key("4REPYvYN6PkUD57Cgy31")
plan(multisession, workers = 12)

symbols <- read_csv("SP Tickers.csv", col_names = c("symbol")) |> unique()

stocks <- symbols$symbol |>
  future_map_dfr(function(symbol) {
    NasdaqDataLink.api_key("4REPYvYN6PkUD57Cgy31")
    tryCatch(
      NasdaqDataLink.datatable(
        "SHARADAR/SEP",
        ticker = symbol,
        qopts.columns = c("ticker", "date", "open", "high", "low", "close", "volume")
      ),
      warning = function(w) {
        print(w)
        return(NULL)
      },
      error = function(e) {
        print(e)
        return(NULL)
      },
      finally = print(str_glue("Fetched data for {symbol}"))
    )
  }) |>
  as_tibble()

# "SP Additions.csv", "SP Removals.csv" contain dates
additions <- read_csv(
  "SP Additions.csv",
  col_types = cols(
    symbol = col_character(),
    `date added` = col_date("%m/%d/%Y")
  )
) # Contains some missing values

# There are no missing values here
removals <- read_csv(
  "SP Removals.csv",
  col_types = cols(
    symbol = col_character(),
    `date removed` = col_date("%m/%d/%Y")
  )
)

# Only include observations from stocks in the time period they are/were in the S&P 500
stocks <- stocks |>
  left_join(additions, by = join_by(ticker == symbol), relationship = "many-to-many") |>
  left_join(removals, by = join_by(ticker == symbol), relationship = "many-to-many")
stocks <- stocks |>
  filter(
    date >= coalesce(`date added`, as_date("2000-12-31")) &
      date < coalesce(`date removed`, as_date("2100-12-31"))
  ) |>
  select(ticker:volume) |>
  arrange(ticker, desc(date))

# Homework questions:
# 1. Add column to stock df that specifies financial sector (healthcare, energy, etc.) contained in "sectors.csv"
sectors <- read_csv("sectors.csv")
stocks <- stocks |> left_join(sectors, by = join_by(ticker == symbol))

# 2. For each stock, add insider trading data from Sharadar Equities Bundle.
# Summarize the data in such a way that there is only one observation per stock per day
# e.g. "Number of insiders buying," "number of insider shares sold", etc.
start_date <- "2015-01-01"
insider_data <- symbols$symbol |>
  future_map_dfr(function(symbol) {
    NasdaqDataLink.api_key("4REPYvYN6PkUD57Cgy31")
    tryCatch(
      NasdaqDataLink.datatable("SHARADAR/SF2", ticker = symbol, filingdate.gte = start_date),
      warning = function(w) {
        print(w)
        return(NULL)
      },
      error = function(e) {
        print(e)
        return(NULL)
      },
      finally = print(str_glue("Fetched insider data for {symbol}"))
    )
  }) |>
  as_tibble()

grouped_data <- insider_data |> group_by(ticker, filingdate)

insider_shares_sold <- grouped_data |>
  filter(transactionshares < 0) |>
  summarize(insider_shares_sold = sum(abs(transactionshares)))

insider_shares_bought <- grouped_data |>
  filter(transactionshares >= 0) |>
  summarize(insider_shares_bought = sum(transactionshares))

insiders_trading <- grouped_data |>
  distinct(ownername) |>
  summarize(insiders_trading = n())

insider_summary <- insider_shares_sold |>
  full_join(insider_shares_bought, by = join_by(ticker, filingdate)) |>
  full_join(insiders_trading, by = join_by(ticker, filingdate))

stocks <- stocks |>
  left_join(insider_summary, by = join_by(ticker, date == filingdate)) |>
  mutate(across(where(is.numeric), ~coalesce(.x, 0)))

# 3. For each stock & day, integrate outside data from another data set in the Sharadar bundle
# e.g. calculate market cap of stock for each day
daily_data <- symbols$symbol |>
  future_map_dfr(function(symbol) {
    NasdaqDataLink.api_key("4REPYvYN6PkUD57Cgy31")
    tryCatch(
      NasdaqDataLink.datatable(
        "SHARADAR/DAILY",
        ticker = symbol,
        qopts.columns = c("ticker", "date", "marketcap", "pe", "ps")
      ),
      warning = function(w) {
        print(w)
        return(NULL)
      },
      error = function(e) {
        print(e)
        return(NULL)
      },
      finally = print(str_glue("Fetched daily data for {symbol}"))
    )
  }) |>
  as_tibble()

stocks <- stocks |>
  left_join(daily_data, by = join_by(ticker, date))
