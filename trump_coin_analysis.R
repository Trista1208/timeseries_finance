# Trump Coin Analysis Script
# This script analyzes Trump coin data using simulated data if API access fails

# Install and load required packages
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("forecast")) install.packages("forecast")
if (!require("tseries")) install.packages("tseries")

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

# Function to generate simulated Trump coin data
generate_simulated_data <- function() {
  cat("Generating simulated Trump coin data for analysis...\n")
  
  # Set seed for reproducibility
  set.seed(2024)
  
  # Generate dates for the past year (365 days)
  end_date <- Sys.Date()
  start_date <- end_date - 365
  dates <- seq(from = start_date, to = end_date, by = "day")
  
  # Generate initial price (around $0.05)
  initial_price <- 0.05
  
  # Create trending component with some volatility
  trend <- cumsum(rnorm(length(dates), mean = 0.0002, sd = 0.005))
  
  # Create seasonal component (weekly pattern)
  seasonal <- 0.002 * sin(2 * pi * seq_along(dates) / 7)
  
  # Create news impact events (sudden jumps in price)
  # Simulate Trump campaign announcements, debates, elections, etc.
  news_impact <- rep(0, length(dates))
  
  # Simulate major news events (big jumps)
  major_events <- sample(30:330, 5)  # 5 major events throughout the year
  for (day in major_events) {
    impact <- runif(1, 0.01, 0.03) * sample(c(-1, 1), 1, prob = c(0.4, 0.6))  # Bias toward positive news
    news_impact[day:(day + sample(3:7, 1))] <- impact  # Impact lasts for 3-7 days
  }
  
  # Simulate minor news events (small jumps)
  minor_events <- sample(setdiff(1:365, major_events), 15)  # 15 minor events
  for (day in minor_events) {
    impact <- runif(1, 0.002, 0.01) * sample(c(-1, 1), 1, prob = c(0.5, 0.5))
    news_impact[day:(day + sample(1:3, 1))] <- impact  # Impact lasts for 1-3 days
  }
  
  # Combine components to generate price
  price <- initial_price + trend + seasonal + news_impact
  price <- pmax(price, 0.001)  # Ensure price never goes below 0.001
  
  # Generate volume data (correlated with absolute price changes)
  price_changes <- c(0, diff(price))
  base_volume <- 10000  # Base daily volume
  volume <- base_volume + abs(price_changes) * 1000000 * runif(length(dates), 0.5, 1.5)
  
  # Generate market cap data
  market_cap <- price * volume * 0.1
  
  # Create simulated data frame
  simulated_data <- data.frame(
    timestamp = dates,
    price = price,
    volume = volume,
    market_cap = market_cap
  )
  
  return(simulated_data)
}

# Function to fetch Trump coin data from CryptoCompare
fetch_trump_coin_data <- function(symbol = "TRUMP", limit = 2000) {
  cat("Fetching data for symbol:", symbol, "\n")
  
  # CryptoCompare API endpoint for historical daily data
  url <- "https://min-api.cryptocompare.com/data/v2/histoday"
  
  # Parameters for the API request
  params <- list(
    fsym = symbol,      # From Symbol
    tsym = "USD",       # To Symbol (USD)
    limit = limit,      # Number of data points
    api_key = ""        # No API key required for basic usage
  )
  
  tryCatch({
    # Make API request
    response <- GET(url, query = params)
    
    # Check if request was successful
    if (status_code(response) == 200) {
      # Parse JSON response
      data <- fromJSON(rawToChar(response$content))
      
      if (data$Response == "Success") {
        cat("Successfully fetched data for", symbol, "\n")
        
        # Extract the data
        raw_data <- data$Data$Data
        
        # Convert to data frame
        prices_df <- data.frame(
          timestamp = as.POSIXct(raw_data$time, origin = "1970-01-01"),
          price = raw_data$close,
          volume = raw_data$volumefrom,
          market_cap = raw_data$volumeto
        )
        
        # Sort by timestamp (oldest to newest)
        prices_df <- prices_df[order(prices_df$timestamp), ]
        
        return(prices_df)
      } else {
        cat("API returned error:", data$Message, "\n")
        return(NULL)
      }
    } else if (status_code(response) == 429) {
      cat("Rate limit exceeded. Waiting 60 seconds before retrying...\n")
      Sys.sleep(60)
      return(NULL)
    } else {
      cat("Failed to fetch data with status code:", status_code(response), "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error fetching data:", e$message, "\n")
    return(NULL)
  })
}

# Function to search for Trump coin symbol
search_trump_coin <- function() {
  cat("Searching for Trump coin symbol...\n")
  
  # CryptoCompare API endpoint for coin list
  url <- "https://min-api.cryptocompare.com/data/all/coinlist"
  
  tryCatch({
    # Make API request
    response <- GET(url)
    
    # Check if request was successful
    if (status_code(response) == 200) {
      # Parse JSON response
      data <- fromJSON(rawToChar(response$content))
      
      if (data$Response == "Success") {
        # Extract the data
        coins <- data$Data
        
        # Convert to data frame
        coins_df <- data.frame(
          symbol = names(coins),
          name = sapply(coins, function(x) x$FullName),
          stringsAsFactors = FALSE
        )
        
        # Filter for Trump-related coins
        trump_coins <- coins_df[grep("trump", tolower(coins_df$name), fixed = TRUE), ]
        
        if (nrow(trump_coins) > 0) {
          cat("\nFound Trump-related coins:\n")
          for (i in 1:nrow(trump_coins)) {
            cat(sprintf("%d. %s (%s)\n", 
                       i, 
                       trump_coins$name[i], 
                       trump_coins$symbol[i]))
          }
          
          return(trump_coins$symbol[1])  # Return the first match
        } else {
          cat("No Trump-related coins found in search results.\n")
          return(NULL)
        }
      } else {
        cat("API returned error:", data$Message, "\n")
        return(NULL)
      }
    } else {
      cat("Search API request failed with status code:", status_code(response), "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error searching for Trump coin:", e$message, "\n")
    return(NULL)
  })
}

# Function to perform time series analysis
analyze_time_series <- function(data) {
  # Create time series object
  ts_data <- ts(data$price, frequency = 365, start = c(year(min(data$timestamp)), yday(min(data$timestamp))))
  
  # Decompose time series
  decomp <- decompose(ts_data)
  
  # Perform Augmented Dickey-Fuller test for stationarity
  adf_test <- adf.test(ts_data, alternative = "stationary")
  
  # Fit ARIMA model
  arima_model <- auto.arima(ts_data, seasonal = TRUE)
  
  # Generate forecasts
  forecast_periods <- 30  # Forecast next 30 days
  forecast_result <- forecast(arima_model, h = forecast_periods)
  
  return(list(
    time_series = ts_data,
    decomposition = decomp,
    adf_test = adf_test,
    arima_model = arima_model,
    forecast = forecast_result
  ))
}

# Function to create time series visualization plots
create_time_series_plots <- function(data, analysis_results) {
  # Extract components
  ts_data <- analysis_results$time_series
  decomp <- analysis_results$decomposition
  forecast_result <- analysis_results$forecast
  
  # 1. Original Time Series with Trend
  p1 <- autoplot(ts_data) +
    labs(title = "Trump Coin Price Time Series",
         x = "Date",
         y = "Price (USD)") +
    theme_minimal()
  
  # 2. Decomposition Plot
  p2 <- autoplot(decomp) +
    labs(title = "Time Series Decomposition") +
    theme_minimal()
  
  # 3. Forecast Plot
  p3 <- autoplot(forecast_result) +
    labs(title = "30-Day Price Forecast",
         x = "Date",
         y = "Price (USD)") +
    theme_minimal()
  
  # 4. ACF and PACF Plots
  p4 <- ggAcf(ts_data) +
    labs(title = "Autocorrelation Function") +
    theme_minimal()
  
  p5 <- ggPacf(ts_data) +
    labs(title = "Partial Autocorrelation Function") +
    theme_minimal()
  
  return(list(p1, p2, p3, p4, p5))
}

# Main analysis function
main <- function() {
  # List of known Trump coin symbols to try
  trump_symbols <- c(
    "TRUMP",       # Trump
    "TRUMP2024",   # Trump 2024
    "TRUMPCOIN",   # TrumpCoin
    "TRUMPDAO",    # TrumpDAO
    "TRUMPT",      # Trump Token
    "TRUMPD",      # Trump Digital
    "TRUMPX"       # Trump X
  )
  
  # Try each symbol until we find one that works
  trump_data <- NULL
  successful_symbol <- NULL
  
  for (symbol in trump_symbols) {
    cat("Trying symbol:", symbol, "\n")
    data <- fetch_trump_coin_data(symbol)
    
    if (!is.null(data)) {
      trump_data <- data
      successful_symbol <- symbol
      break
    }
    
    # Wait a bit before trying the next symbol to avoid rate limiting
    Sys.sleep(2)
  }
  
  # If API access fails, use simulated data
  if (is.null(trump_data)) {
    cat("\nCould not fetch actual Trump coin data from APIs.\n")
    cat("Using simulated data for demonstration purposes...\n")
    
    trump_data <- generate_simulated_data()
    successful_symbol <- "TRUMP (Simulated)"
  }
  
  if (!is.null(trump_data)) {
    cat("\nAnalyzing Trump coin with symbol:", successful_symbol, "\n")
    
    # Perform time series analysis
    cat("Performing time series analysis...\n")
    analysis_results <- analyze_time_series(trump_data)
    
    # Create plots
    cat("Creating visualizations...\n")
    plots <- create_time_series_plots(trump_data, analysis_results)
    
    # Display plots
    print(plots[[1]])  # Original Time Series
    print(plots[[2]])  # Decomposition
    print(plots[[3]])  # Forecast
    print(plots[[4]])  # ACF
    print(plots[[5]])  # PACF
    
    # Display analysis results
    cat("\nTime Series Analysis Results:\n")
    
    # Stationarity test
    cat("\nAugmented Dickey-Fuller Test:\n")
    print(analysis_results$adf_test)
    
    # ARIMA model summary
    cat("\nARIMA Model Summary:\n")
    print(summary(analysis_results$arima_model))
    
    # Forecast summary
    cat("\n30-Day Forecast Summary:\n")
    print(summary(analysis_results$forecast))
    
    # Calculate basic statistics
    cat("\nBasic Statistics:\n")
    print(summary(trump_data$price))
    
    # Calculate returns and volatility
    returns <- diff(log(trump_data$price))
    volatility <- sd(returns) * sqrt(365) * 100
    
    cat("\nReturn Statistics:\n")
    print(summary(returns))
    cat("\nAnnualized Volatility: ", round(volatility, 2), "%\n")
    
    # Calculate price change percentages
    current_price <- tail(trump_data$price, 1)
    last_7_days <- tail(trump_data$price, 8)[1]
    last_30_days <- tail(trump_data$price, 31)[1]
    
    change_7d <- ((current_price - last_7_days) / last_7_days) * 100
    change_30d <- ((current_price - last_30_days) / last_30_days) * 100
    
    cat("\nPrice Changes:\n")
    cat("7-day change: ", round(change_7d, 2), "%\n")
    cat("30-day change: ", round(change_30d, 2), "%\n")
    
    # Add note if using simulated data
    if (successful_symbol == "TRUMP (Simulated)") {
      cat("\nNOTE: This analysis uses simulated data for demonstration purposes.\n")
      cat("Real Trump coin data could not be accessed through available APIs.\n")
      cat("The simulated data aims to mimic realistic cryptocurrency behavior but does not represent actual market performance.\n")
    }
  }
}

# Run the analysis
main() 