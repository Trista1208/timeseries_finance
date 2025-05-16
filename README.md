# Luxury Brand Time Series Analysis

This repository contains code and analysis for examining the relationship between currency exchange rates and luxury brand stock prices, with a particular focus on Richemont (CFR.SW).

## Project Overview

This project analyzes how currency fluctuations affect the stock prices of luxury brands, particularly Richemont. Luxury brands, with their global presence and exposure to international markets, are often sensitive to currency exchange rate movements. This analysis aims to quantify these relationships and provide insights into how currency movements correlate with stock performance.

## Repository Structure

```
timeseries_finance/
├── analysis/          # Primary analysis scripts
├── charts/            # Generated visualizations
├── data/              # Historical stock and forex data
├── output/            # Analysis outputs and results
│   └── validation/    # Model validation outputs
├── scrapers/          # Data scraping and collection scripts
└── validation/        # Scripts for data and model validation
```

## Key Features

### Data Collection
- Historical stock price data for Richemont and other luxury brands
- Currency exchange rate data for CHF, EUR, USD, and CNY
- Automated data collection scripts in R

### Analysis Components
- Time series preprocessing including log returns calculation
- Currency correlation analysis with Richemont stock prices
- Yearly correlation breakdowns
- Linear modeling of stock prices against currency movements
- Residual analysis

### Validation
- Stock price trend validation
- Returns distribution analysis
- Autocorrelation testing
- Model residual diagnostics

## Key Findings

- Richemont stock prices show varying correlation patterns with different currencies
- Yearly correlation analysis reveals changing relationships over time
- The strongest correlations are observed with the EUR/CHF exchange rate
- China's currency (CNY) shows significant influence during certain time periods

## Getting Started

### Prerequisites
- R (version 4.0 or above)
- Basic R packages (no special dependencies required for validation)

### Running the Analysis
1. Clone this repository
2. Run the scripts in the `analysis` directory to reproduce the results
3. For validation, run:
   ```R
   source("validation/simple_validation.R")
   ```

## Outputs

The analysis generates several outputs:
- Correlation matrices in CSV format
- Time series plots in PNG format
- Statistical summaries and model diagnostics

## Future Work

- Incorporate additional macroeconomic factors
- Implement more sophisticated time series models (ARIMA, GARCH)
- Expand analysis to more luxury brands for comparison
- Develop predictive models based on currency movements