# Richemont Stock Time Series Analysis (Simplified Version)
# This script performs basic time series analysis on Richemont stock data

# Check for required packages - using minimal dependencies
required_packages <- c("ggplot2", "stats")
for(pkg in required_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load minimal required libraries
library(ggplot2)  # For plotting

# Function to fetch the latest Richemont historical data
get_latest_data <- function() {
  # Source the stock scraper to ensure we have the latest data
  source("richemont_stock_scraper_final.R")
  
  # Check if the file was created by the scraper
  if(file.exists("richemont_historical_data.csv")) {
    return(read.csv("richemont_historical_data.csv", stringsAsFactors = FALSE))
  } else {
    cat("Failed to fetch latest historical data\n")
    return(NULL)
  }
}

# Function to load historical data
load_data <- function(file_path = "richemont_historical_data.csv", refresh = FALSE) {
  # Check if we should refresh the data
  if(refresh) {
    cat("Fetching latest Richemont historical data...\n")
    data <- get_latest_data()
    if(is.null(data)) {
      cat("Failed to refresh data. Trying to load existing file.\n")
    } else {
      cat("Successfully fetched latest data.\n")
    }
  }
  
  # If we don't have data yet, try to load from file
  if(!exists("data") || is.null(data)) {
    # Check if file exists
    if(!file.exists(file_path)) {
      cat("Historical data file not found. Please run the scraper first.\n")
      return(NULL)
    }
    
    # Read the CSV
    data <- read.csv(file_path, stringsAsFactors = FALSE)
  }
  
  # Convert Date to Date type
  data$Date <- as.Date(data$Date)
  
  # Order by date (ascending)
  data <- data[order(data$Date), ]
  
  # Return the data
  cat("Successfully loaded data with", nrow(data), "records\n")
  return(data)
}

# Function to plot stock price history
plot_price_history <- function(data) {
  # Basic line plot of closing prices
  plot(data$Date, data$Close, type = "l", col = "blue",
       main = "Richemont Stock Price History",
       xlab = "Date", ylab = "Closing Price (CHF)")
  
  # Add a grid for better readability
  grid()
}

# Function to create a basic candlestick-like chart
plot_candlestick_simple <- function(data, days = 60) {
  # Use only the most recent days
  if(nrow(data) > days) {
    recent_data <- tail(data, days)
  } else {
    recent_data <- data
  }
  
  # Create a line plot for closing prices
  plot(recent_data$Date, recent_data$Close, type = "l", col = "blue",
       main = paste("Richemont Stock - Last", days, "Trading Days"),
       xlab = "Date", ylab = "Price (CHF)")
  
  # Add high-low ranges as vertical lines
  for(i in 1:nrow(recent_data)) {
    lines(x = c(recent_data$Date[i], recent_data$Date[i]),
          y = c(recent_data$Low[i], recent_data$High[i]),
          col = "darkgray")
  }
  
  # Add grid
  grid()
}

# Function to calculate and plot simple moving averages
plot_moving_averages_simple <- function(data, short_ma = 50, long_ma = 200) {
  # Calculate moving averages
  data$ShortMA <- calculate_ma(data$Close, n = short_ma)
  data$LongMA <- calculate_ma(data$Close, n = long_ma)
  
  # Plot closing prices
  plot(data$Date, data$Close, type = "l", col = "black",
       main = "Richemont Stock Price with Moving Averages",
       xlab = "Date", ylab = "Price (CHF)")
  
  # Add moving averages
  lines(data$Date, data$ShortMA, col = "red", lwd = 2)
  lines(data$Date, data$LongMA, col = "blue", lwd = 2)
  
  # Add legend
  legend("topleft", 
         legend = c("Price", paste(short_ma, "Day MA"), paste(long_ma, "Day MA")),
         col = c("black", "red", "blue"),
         lty = 1, lwd = c(1, 2, 2))
  
  # Add grid
  grid()
}

# Helper function to calculate moving average
calculate_ma <- function(x, n = 10) {
  # Simple moving average calculation
  stats::filter(x, rep(1/n, n), sides = 1)
}

# Function to calculate and plot returns
plot_returns_simple <- function(data) {
  # Calculate daily returns
  returns <- c(NA, diff(log(data$Close)) * 100)
  
  # Plot returns
  plot(data$Date, returns, type = "l", col = "blue",
       main = "Richemont Daily Returns",
       xlab = "Date", ylab = "Daily Return (%)")
  
  # Add a horizontal line at y=0
  abline(h = 0, col = "darkgray", lty = 2)
  
  # Add grid
  grid()
  
  # Create a histogram of returns
  hist(returns, breaks = 50, col = "lightblue", border = "white",
       main = "Distribution of Daily Returns",
       xlab = "Daily Return (%)")
  
  # Return the returns vector
  return(returns)
}

# Function to calculate simple momentum indicator
plot_momentum <- function(data, period = 14) {
  # Calculate momentum (difference between current price and price 'period' days ago)
  momentum <- c(rep(NA, period), diff(data$Close, lag = period))
  
  # Plot momentum
  plot(data$Date, momentum, type = "l", col = "purple",
       main = paste("Richemont", period, "Day Momentum"),
       xlab = "Date", ylab = "Momentum")
  
  # Add a horizontal line at y=0
  abline(h = 0, col = "darkgray", lty = 2)
  
  # Add grid
  grid()
}

# Function to save a price chart to file
save_price_chart <- function(data, filename = "richemont_price_chart.png") {
  # Create PNG file
  png(filename, width = 1000, height = 600)
  
  # Plot price history
  plot_price_history(data)
  
  # Close the device
  dev.off()
  
  cat("Price chart saved to", filename, "\n")
}

# Main analysis function
run_analysis <- function(refresh_data = TRUE) {
  # Load the data with optional refresh
  cat("Loading Richemont stock data...\n")
  stock_data <- load_data(refresh = refresh_data)
  
  if(is.null(stock_data)) {
    return(NULL)
  }
  
  # Basic summary
  cat("\nBasic summary of closing prices:\n")
  print(summary(stock_data$Close))
  
  # 1. Create visualization of price history
  cat("\nCreating price history visualization...\n")
  plot_price_history(stock_data)
  
  # 2. Create candlestick-like chart
  cat("\nPress Enter to continue to candlestick chart...")
  invisible(readline())
  plot_candlestick_simple(stock_data, days = 60)
  
  # 3. Plot moving averages
  cat("\nPress Enter to continue to moving averages...")
  invisible(readline())
  plot_moving_averages_simple(stock_data)
  
  # 4. Plot returns
  cat("\nPress Enter to continue to returns analysis...")
  invisible(readline())
  returns <- plot_returns_simple(stock_data)
  
  # 5. Plot momentum
  cat("\nPress Enter to continue to momentum indicator...")
  invisible(readline())
  plot_momentum(stock_data)
  
  # 6. Save a chart to file
  cat("\nSaving price chart to file...\n")
  save_price_chart(stock_data)
  
  # Calculate some basic statistics
  cat("\nBasic statistics for Richemont stock:\n")
  cat("Average closing price:", mean(stock_data$Close, na.rm = TRUE), "CHF\n")
  cat("Volatility (standard deviation of returns):", 
      sd(returns, na.rm = TRUE), "%\n")
  
  avg_volume <- mean(stock_data$Volume, na.rm = TRUE)
  cat("Average daily trading volume:", format(avg_volume, big.mark = ","), "shares\n")
  
  # Find the highest and lowest prices
  max_price <- max(stock_data$High, na.rm = TRUE)
  max_date <- stock_data$Date[which.max(stock_data$High)]
  min_price <- min(stock_data$Low, na.rm = TRUE)
  min_date <- stock_data$Date[which.min(stock_data$Low)]
  
  cat("Highest price:", max_price, "CHF on", format(max_date, "%Y-%m-%d"), "\n")
  cat("Lowest price:", min_price, "CHF on", format(min_date, "%Y-%m-%d"), "\n")
  
  # Calculate year-to-date performance
  current_year <- format(Sys.Date(), "%Y")
  ytd_data <- stock_data[format(stock_data$Date, "%Y") == current_year, ]
  
  if(nrow(ytd_data) > 0) {
    start_price <- ytd_data$Close[1]
    current_price <- tail(ytd_data$Close, 1)
    ytd_return <- ((current_price / start_price) - 1) * 100
    
    cat("\nYear-to-date performance:", round(ytd_return, 2), "%\n")
  }
  
  cat("\nTime series analysis complete!\n")
}

# Run the analysis with refreshed data
cat("Starting Richemont stock time series analysis...\n")
cat("(Basic version with minimal dependencies)\n\n")
run_analysis(refresh_data = TRUE) 