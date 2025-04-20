# Import the  libraries required
library(Quandl)
library(IBrokers)
library(quantmod)
library(dplyr)
library(tidyquant)
library(NasdaqDataLink)
library(furrr)

# Set API Key and IB localhost port
nasdaq_api_key <- "4REPYvYN6PkUD57Cgy31"
NasdaqDataLink.api_key(nasdaq_api_key)
ib_port <- 4002

# Set workers based on your CPU configuration (# of cores)
plan(multisession, workers = 8)

# Set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list=ls())
options(scipen=999)
cat("\014")

# Set the start and end dates


# Set variables for strategy
windowsize<-15
longestindicator<-100
currentdate<-Sys.Date()
maxtradepct<-0.05        # maximum value of any single trade
maxdaytrades<-8          # maximum trades in one day
longthreshold<-1.01
shortthreshold<-0.99
defaultscalinglength<-10000

# Retrieve the current S&P500 stock list
symbols <- tq_index("SP500")[, c(1)] |> unique()

# Function to get data from NASDAQ API and store it
get_data <- function(symbols) {

  # Check whether the NASDAQ API is working
  NasdaqDataLink.api_key(nasdaq_api_key)
  check <- tryCatch(
    check <- NasdaqDataLink.datatable(
      "SHARADAR/SF2",
      ticker = 'AAPL',
      qopts.columns = c("ticker", "date", "open", "high", "low", "close", "volume")
    ),
    warning = function(w) {
      print(w)
      return(NULL)
    },
    error = function(e) {
      print(e)
      return(NULL)
    }
  )

  if (is.null(check)) {
    print("NASDAQ API is not working\n")
    return
  } else {
    print("NASDAQ API is working\n")
  }

  stocks <- NULL
  stocks <- symbols$symbol |>
    future_map_dfr(function(symbol) {
      NasdaqDataLink.api_key(nasdaq_api_key)
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

  return(stocks)
}

# Function to generate indicators
