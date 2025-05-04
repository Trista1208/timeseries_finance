# LVMH Time Series Analysis
# Script to analyze LVMH (MC.PA) stock data using time series methods

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load required packages
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}
library(stats)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
library(zoo)

# Function to load and prepare the data
prepare_data <- function(filename = "lvmh_historical_data.csv") {
  # Read the CSV file
  data <- read.csv(filename, stringsAsFactors = FALSE)
  
  # Convert Date column to Date type
  data$Date <- as.Date(data$Date)
  
  # Sort by date
  data <- data[order(data$Date), ]
  
  # Create time series object
  ts_data <- zoo(data$Close, order.by = data$Date)
  
  return(list(raw_data = data, ts_data = ts_data))
}

# Function to perform basic time series analysis
analyze_ts <- function(ts_data) {
  # Save historical price plot
  png("lvmh_historical_prices.png", width = 1600, height = 900, res = 150)
  
  # Set margins and plot style
  par(mar = c(5, 5, 4, 4))  # Increase margins for better label visibility
  
  # Price plot with enhanced styling
  plot(ts_data, 
       main = "LVMH Historical Stock Prices (2015-2025)",
       ylab = "Price (EUR)",
       xlab = "Date",
       type = "l",
       lwd = 2,
       col = "black",
       cex.main = 1.2,
       cex.lab = 1.1,
       cex.axis = 1)
  
  # Add grid
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
  
  dev.off()
  
  # Calculate performance metrics
  first_price <- as.numeric(head(ts_data, 1))
  last_price <- as.numeric(tail(ts_data, 1))
  total_return <- (last_price / first_price - 1) * 100
  
  cat("\nPerformance Metrics:\n")
  cat("Start Date:", format(head(index(ts_data), 1)), "\n")
  cat("End Date:", format(tail(index(ts_data), 1)), "\n")
  cat("Starting Price:", round(first_price, 2), "EUR\n")
  cat("Ending Price:", round(last_price, 2), "EUR\n")
  cat("Total Return:", round(total_return, 2), "%\n")
  
  cat("\nPlot has been saved to:\n")
  cat("lvmh_historical_prices.png (historical prices)\n")
  
  return(list(
    ts_data = ts_data
  ))
}

# Main function
main <- function() {
  cat("Starting LVMH Time Series Analysis...\n")
  
  # Load and prepare data
  cat("\n1. Loading and preparing data...\n")
  data_list <- prepare_data()
  
  # Perform analysis
  cat("\n2. Generating historical price plot...\n")
  analysis <- analyze_ts(data_list$ts_data)
  
  cat("\nAnalysis complete!\n")
  return(analysis)
}

# Run the analysis
results <- main() 