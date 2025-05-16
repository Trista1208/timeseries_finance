# Richemont Model Validation

This folder contains scripts for validating the Richemont stock price data and models used in the currency analysis.

## Contents

- `unified_validation.R`: A comprehensive script that performs both basic validation of the Richemont stock data and advanced nonlinear model testing.

## Validation Process

The unified validation script performs the following tasks in a single integrated workflow:

### 1. Data Loading and Inspection
- Loads the Richemont stock data
- Displays basic information and dimensions
- Shows summary statistics and data structure

### 2. Basic Stock Data Validation
- **Summary Statistics**: Computes basic statistics for stock prices
- **Linear Trend Analysis**: Fits a simple linear trend model to the stock prices
- **Residual Analysis**: Examines model residuals for patterns
- **Returns Analysis**: Calculates and analyzes daily returns
- **Autocorrelation Testing**: Tests for correlations between current and lagged returns
- **Basic Visualizations**: Creates plots of price trends, returns volatility, and model residuals

### 3. Advanced Nonlinear Modeling
- **Currency Data Generation**: For demonstration purposes, creates synthetic currency data
- **Nonlinear Transformations**:
  - Logarithmic transformations
  - Squared and cubic terms
  - Interaction terms between currencies
  - Other nonlinear relationships
- **Model Comparison**: Tests multiple model specifications:
  - Basic linear models
  - Log-transformed models
  - Quadratic models
  - Models with interaction terms
  - Polynomial models
  - Complex models with multiple transformations
- **Best Model Analysis**:
  - Identifies the best performing model based on adjusted R-squared
  - Provides detailed coefficient analysis
  - Creates visualizations of actual vs. fitted values
  - Analyzes residuals for patterns or heteroscedasticity

## How to Run

Simply run the unified validation script:
```R
source("validation/unified_validation.R")
```

This will execute the entire validation process and save all results to the appropriate output directories.

## Output Structure

The unified validation creates output in two directories:

### 1. `output/validation/`
Contains basic validation results:
- `basic_summary.txt`: Summary statistics and linear model details
- `autocorrelations.csv`: Correlation matrix of returns and lagged returns
- `price_plot.png`: Time series plot of Richemont stock prices
- `returns_plot.png`: Time series plot of daily returns
- `residuals_plot.png`: Plot of linear model residuals over time

### 2. `output/nonlinear/`
Contains advanced nonlinear model validation results:
- `model_comparison.csv`: Comparison metrics for all tested models
- `best_model_formula.txt`: Formula and coefficients of the best model
- `best_model_coefficients.csv`: Detailed coefficient table
- `best_model_residuals.png`: Diagnostic plots for the best model
- `actual_vs_fitted.png`: Comparison of actual prices vs. model predictions
- `residuals_time.png`: Residuals of the best model over time

## Interpreting Results

### Basic Validation
- The summary statistics show the range and distribution of Richemont stock prices.
- The linear time trend model (Price ~ Time) reveals if there's a significant trend in the price data.
- The R-squared value of this model indicates how much of the price variation is explained by the time trend.
- The autocorrelation analysis shows if past returns can predict future returns.
- The visualizations help identify patterns, outliers, or anomalies in the data.

### Nonlinear Model Validation
- The model comparison shows which form of relationship best captures currency effects on stock prices.
- The R-squared and adjusted R-squared values indicate how much of the price variation is explained by the currency variables.
- The AIC values help compare model quality (lower is better).
- The RMSE (Root Mean Square Error) measures prediction accuracy.
- The best model analysis shows which currency variables and transformations are most significant.

## Note on Currency Data

The current implementation uses synthetic currency data for demonstration purposes. In a real analysis, you should:

1. Replace the synthetic data generation with actual currency exchange rate data
2. Adapt the model specifications as needed based on your hypotheses
3. Interpret the results in the context of real-world economic factors 