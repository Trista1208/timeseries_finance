# Richemont Model Validation

This folder contains a script for validating the Richemont stock price data used in the currency analysis.

## Contents

- `simple_validation.R`: A script that performs basic validation of the Richemont stock data.

## What the Validation Script Does

The simple validation script performs the following tasks:

1. **Data Loading and Inspection**: Loads the Richemont stock data and displays basic information about it.

2. **Basic Statistical Analysis**:
   - Computes summary statistics for stock prices
   - Fits a simple linear trend model to the stock prices
   - Performs residual analysis on the model

3. **Returns Analysis**:
   - Calculates daily returns from price data
   - Examines correlations between current and lagged returns
   - Tests for autocorrelation in the return series

4. **Visualization**:
   - Creates plots of price trends over time
   - Visualizes returns volatility
   - Plots model residuals to check for patterns

## How to Run

1. Make sure you have R installed with the necessary base packages.

2. Run the validation script:
   ```R
   source("validation/simple_validation.R")
   ```

3. Review the results in the `output/validation` directory, which will contain:
   - Text summary of the data and model (`basic_summary.txt`)
   - Correlation matrix of returns (`autocorrelations.csv`)
   - Visualizations of price, returns, and residuals (PNG files)

## Interpreting Results

- The summary statistics show the range and distribution of Richemont stock prices.
- The linear model reveals if there's a significant time trend in the price data.
- The autocorrelation analysis shows if past returns predict future returns.
- The visualizations help identify patterns, outliers, or anomalies in the data.

This validation helps ensure the quality and characteristics of the data are understood before performing more complex analyses with currency exchange rates. 