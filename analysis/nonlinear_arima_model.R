# =============================================
# Nonlinear Currency Relationships and ARIMA Modeling
# =============================================

# Load packages with proper conflict handling
# Load forecast and time series packages first
if (!require(forecast, quietly = TRUE)) {
  install.packages("forecast", dependencies = TRUE)
  library(forecast)
}

if (!require(tseries, quietly = TRUE)) {
  install.packages("tseries")
  library(tseries)
}

if (!require(zoo, quietly = TRUE)) {
  install.packages("zoo")
  library(zoo)
}

if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Load tidyverse last to handle conflicts
if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}

# Explicitly resolve function conflicts by storing references
stats_filter <- stats::filter
stats_lag <- stats::lag

cat("All packages loaded successfully. Starting analysis...\n")

# Create output directory
dir.create("output/nonlinear_arima", recursive = TRUE, showWarnings = FALSE)

# =============================================
# Load Data
# =============================================

# Load Richemont stock data
richemont_data <- read.csv("data/richemont_historical_data.csv")
richemont_data$Date <- as.Date(richemont_data$Date)

# Try to load forex data from csv files if they exist
try({
  forex_eur <- read.csv("data/forex_eur_chf.csv")
  forex_usd <- read.csv("data/forex_usd_chf.csv")
  forex_cny <- read.csv("data/forex_cny_chf.csv")
  
  forex_eur$Date <- as.Date(forex_eur$Date)
  forex_usd$Date <- as.Date(forex_usd$Date)
  forex_cny$Date <- as.Date(forex_cny$Date)
}, silent = TRUE)

# Check if forex data exists, if not create synthetic data for demonstration
if (!exists("forex_eur")) {
  cat("Creating synthetic forex data based on Richemont dates...\n")
  
  # Extract unique dates from Richemont data
  dates <- richemont_data$Date
  n_dates <- length(dates)
  
  # Create synthetic data with some correlation to Richemont
  set.seed(123)
  
  # EUR/CHF with moderate correlation
  forex_eur <- data.frame(
    Date = dates,
    Close = 1.1 + 0.05 * sin(seq(0, 4*pi, length.out = n_dates)) + rnorm(n_dates, 0, 0.01)
  )
  
  # USD/CHF with different pattern
  forex_usd <- data.frame(
    Date = dates,
    Close = 0.95 + 0.08 * cos(seq(0, 3*pi, length.out = n_dates)) + rnorm(n_dates, 0, 0.01)
  )
  
  # CNY/CHF with trend
  forex_cny <- data.frame(
    Date = dates,
    Close = seq(6.5, 7.0, length.out = n_dates) + 0.1 * sin(seq(0, 5*pi, length.out = n_dates)) + rnorm(n_dates, 0, 0.02)
  )
}

# =============================================
# Data Preparation
# =============================================

# Merge data
merged_data <- richemont_data %>%
  select(Date, Close) %>%
  rename(Richemont = Close) %>%
  left_join(forex_eur %>% select(Date, Close) %>% rename(EUR = Close), by = "Date") %>%
  left_join(forex_usd %>% select(Date, Close) %>% rename(USD = Close), by = "Date") %>%
  left_join(forex_cny %>% select(Date, Close) %>% rename(CNY = Close), by = "Date")

# Handle missing values using forward fill
merged_data <- merged_data %>%
  arrange(Date) %>%
  mutate(
    EUR = zoo::na.locf(EUR, na.rm = FALSE),
    USD = zoo::na.locf(USD, na.rm = FALSE),
    CNY = zoo::na.locf(CNY, na.rm = FALSE)
  )

# Remove any remaining rows with NA
merged_data <- na.omit(merged_data)

# =============================================
# Nonlinear Transformations
# =============================================

# Create various nonlinear transformations to capture different relationships
merged_data <- merged_data %>%
  mutate(
    # Logarithmic transformations
    log_Richemont = log(Richemont),
    log_EUR = log(EUR),
    log_USD = log(USD),
    log_CNY = log(CNY),
    
    # Squared terms (for quadratic relationships)
    EUR_squared = EUR^2,
    USD_squared = USD^2,
    CNY_squared = CNY^2,
    
    # Interaction terms
    EUR_USD = EUR * USD,
    EUR_CNY = EUR * CNY,
    USD_CNY = USD * CNY,
    
    # Reciprocals (for inverse relationships)
    inv_EUR = 1/EUR,
    inv_USD = 1/USD,
    inv_CNY = 1/CNY,
    
    # Cube root (for moderating extreme values)
    cbrt_EUR = sign(EUR) * abs(EUR)^(1/3),
    cbrt_USD = sign(USD) * abs(USD)^(1/3),
    cbrt_CNY = sign(CNY) * abs(CNY)^(1/3)
  )

# =============================================
# Exploratory Visualizations of Transformations
# =============================================

# Create scatterplots to visualize relationships
plot_relationships <- function(data, x_var, y_var = "Richemont", title_suffix = "") {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "loess", color = "red") +
    theme_minimal() +
    labs(title = paste("Relationship:", y_var, "vs", x_var, title_suffix),
         x = x_var, y = y_var)
}

# Plot original relationships
p1 <- plot_relationships(merged_data, "EUR")
p2 <- plot_relationships(merged_data, "USD")
p3 <- plot_relationships(merged_data, "CNY")

# Plot log-transformed relationships
p4 <- plot_relationships(merged_data, "log_EUR", "log_Richemont", "(Log-Transformed)")
p5 <- plot_relationships(merged_data, "log_USD", "log_Richemont", "(Log-Transformed)")
p6 <- plot_relationships(merged_data, "log_CNY", "log_Richemont", "(Log-Transformed)")

# Save plots
ggsave("output/nonlinear_arima/original_relationships.png", gridExtra::arrangeGrob(p1, p2, p3, ncol=3), width = 15, height = 5)
ggsave("output/nonlinear_arima/log_relationships.png", gridExtra::arrangeGrob(p4, p5, p6, ncol=3), width = 15, height = 5)

# =============================================
# Nonlinear Model Fitting
# =============================================

# Fit a linear model with original variables (baseline)
lm_base <- lm(Richemont ~ EUR + USD + CNY, data = merged_data)

# Fit a model with log transformations
lm_log <- lm(log_Richemont ~ log_EUR + log_USD + log_CNY, data = merged_data)

# Fit a model with quadratic terms
lm_quadratic <- lm(Richemont ~ EUR + USD + CNY + 
                    EUR_squared + USD_squared + CNY_squared, 
                  data = merged_data)

# Fit a model with interaction terms
lm_interaction <- lm(Richemont ~ EUR + USD + CNY + 
                       EUR_USD + EUR_CNY + USD_CNY, 
                     data = merged_data)

# Fit a complex model with multiple transformations
lm_complex <- lm(Richemont ~ EUR + USD + CNY + 
                   EUR_squared + USD_squared + 
                   log_EUR + log_USD + log_CNY +
                   EUR_USD + EUR_CNY,
                 data = merged_data)

# Compare models
model_comparison <- data.frame(
  Model = c("Base Linear", "Log Transformed", "Quadratic", "Interaction", "Complex"),
  R_Squared = c(summary(lm_base)$r.squared,
                summary(lm_log)$r.squared,
                summary(lm_quadratic)$r.squared,
                summary(lm_interaction)$r.squared,
                summary(lm_complex)$r.squared),
  Adj_R_Squared = c(summary(lm_base)$adj.r.squared,
                    summary(lm_log)$adj.r.squared,
                    summary(lm_quadratic)$adj.r.squared,
                    summary(lm_interaction)$adj.r.squared,
                    summary(lm_complex)$adj.r.squared),
  AIC = c(AIC(lm_base),
          AIC(lm_log),
          AIC(lm_quadratic),
          AIC(lm_interaction),
          AIC(lm_complex))
)

# Save comparison to CSV
write.csv(model_comparison, "output/nonlinear_arima/nonlinear_model_comparison.csv", row.names = FALSE)

# Print model comparison
cat("\nNONLINEAR MODEL COMPARISON:\n")
print(model_comparison)

# Identify best performing model based on adjusted R-squared
best_model_idx <- which.max(model_comparison$Adj_R_Squared)
cat("\nBest performing nonlinear model:", model_comparison$Model[best_model_idx], "\n")

# Get the best model
best_model <- switch(best_model_idx,
                     lm_base,
                     lm_log,
                     lm_quadratic,
                     lm_interaction,
                     lm_complex)

# =============================================
# ARIMA Modeling
# =============================================

# Convert to time series
ts_richemont <- ts(merged_data$Richemont, frequency = 252)  # Approx trading days in a year

# Plot the time series
png("output/nonlinear_arima/richemont_timeseries.png", width = 1000, height = 600)
plot(ts_richemont, main = "Richemont Stock Price Time Series", 
     xlab = "Time", ylab = "Price (CHF)")
dev.off()

# Check stationarity
adf_test <- tseries::adf.test(ts_richemont)
cat("\nADF Test for Stationarity:\n")
print(adf_test)

# If non-stationary, difference the series
if (adf_test$p.value > 0.05) {
  cat("Series is non-stationary. Differencing...\n")
  ts_richemont_diff <- diff(ts_richemont)
  
  # Check stationarity of differenced series
  adf_test_diff <- tseries::adf.test(ts_richemont_diff)
  cat("ADF Test for Stationarity (differenced series):\n")
  print(adf_test_diff)
  
  # Plot differenced series
  png("output/nonlinear_arima/richemont_differenced.png", width = 1000, height = 600)
  plot(ts_richemont_diff, main = "Differenced Richemont Stock Price", 
       xlab = "Time", ylab = "Price Change (CHF)")
  dev.off()
} else {
  cat("Series is stationary. No differencing needed.\n")
  ts_richemont_diff <- ts_richemont
}

# Examine ACF and PACF
png("output/nonlinear_arima/acf_pacf.png", width = 1000, height = 800)
par(mfrow = c(2, 1))
acf(ts_richemont_diff, main = "ACF of Richemont Stock Prices")
pacf(ts_richemont_diff, main = "PACF of Richemont Stock Prices")
dev.off()

# Fit ARIMA model automatically
cat("\nFitting ARIMA model...\n")
arima_model <- forecast::auto.arima(ts_richemont, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)

# Print model summary
cat("\nARIMA MODEL SUMMARY:\n")
print(summary(arima_model))

# Save model order
arima_order <- paste0("ARIMA(", arima_model$arima$order[1], ",", 
                     arima_model$arima$order[2], ",", 
                     arima_model$arima$order[3], ")")
cat("\nBest ARIMA model:", arima_order, "\n")

# Generate forecasts
forecast_horizon <- 30  # 30 days ahead
arima_forecast <- forecast::forecast(arima_model, h = forecast_horizon)

# Plot forecasts
png("output/nonlinear_arima/arima_forecast.png", width = 1000, height = 600)
plot(arima_forecast, main = paste("ARIMA Forecast -", arima_order), 
     xlab = "Time", ylab = "Price (CHF)")
dev.off()

# Check residuals
png("output/nonlinear_arima/arima_residuals.png", width = 1000, height = 800)
forecast::checkresiduals(arima_model)
dev.off()

# =============================================
# ARIMAX - ARIMA with External Regressors
# =============================================

# Prepare external regressors (from the best nonlinear model)
# This combines ARIMA time-series modeling with our nonlinear currency relationships

# Get model formula terms
formula_terms <- attr(terms(best_model), "term.labels")
cat("\nUsing external regressors from best nonlinear model:", paste(formula_terms, collapse = ", "), "\n")

# Create a matrix of external regressors
xreg_matrix <- as.matrix(merged_data[, formula_terms, drop = FALSE])

# Fit ARIMAX model
cat("\nFitting ARIMAX model with external regressors...\n")
arimax_model <- forecast::auto.arima(ts_richemont, xreg = xreg_matrix, seasonal = FALSE)

# Print model summary
cat("\nARIMAX MODEL SUMMARY:\n")
print(summary(arimax_model))

# Save model order
arimax_order <- paste0("ARIMAX(", arimax_model$arima$order[1], ",", 
                      arimax_model$arima$order[2], ",", 
                      arimax_model$arima$order[3], ") with ", 
                      ncol(xreg_matrix), " regressors")
cat("\nBest ARIMAX model:", arimax_order, "\n")

# Generate forecasts (assuming constant regressors for simplicity)
future_xreg <- matrix(colMeans(xreg_matrix), nrow = forecast_horizon, ncol = ncol(xreg_matrix), byrow = TRUE)
colnames(future_xreg) <- colnames(xreg_matrix)
arimax_forecast <- forecast::forecast(arimax_model, xreg = future_xreg, h = forecast_horizon)

# Plot forecasts
png("output/nonlinear_arima/arimax_forecast.png", width = 1000, height = 600)
plot(arimax_forecast, main = paste("ARIMAX Forecast -", arimax_order), 
     xlab = "Time", ylab = "Price (CHF)")
dev.off()

# Check residuals
png("output/nonlinear_arima/arimax_residuals.png", width = 1000, height = 800)
forecast::checkresiduals(arimax_model)
dev.off()

# =============================================
# Model Comparison - ARIMA vs ARIMAX vs Linear
# =============================================

# Create a function to calculate accuracy metrics
calculate_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  n <- length(actual)
  
  mae <- mean(abs(residuals))
  rmse <- sqrt(mean(residuals^2))
  mape <- mean(abs(residuals/actual)) * 100
  
  return(c(MAE = mae, RMSE = rmse, MAPE = mape))
}

# Extract fitted values from each model
fitted_linear <- fitted(best_model)
fitted_arima <- ts_richemont - residuals(arima_model)
fitted_arimax <- ts_richemont - residuals(arimax_model)

# Calculate in-sample accuracy metrics
linear_metrics <- calculate_metrics(merged_data$Richemont, fitted_linear)
arima_metrics <- calculate_metrics(as.numeric(ts_richemont), as.numeric(fitted_arima))
arimax_metrics <- calculate_metrics(as.numeric(ts_richemont), as.numeric(fitted_arimax))

# Combine metrics
final_comparison <- data.frame(
  Model = c("Best Nonlinear", "ARIMA", "ARIMAX"),
  MAE = c(linear_metrics["MAE"], arima_metrics["MAE"], arimax_metrics["MAE"]),
  RMSE = c(linear_metrics["RMSE"], arima_metrics["RMSE"], arimax_metrics["RMSE"]),
  MAPE = c(linear_metrics["MAPE"], arima_metrics["MAPE"], arimax_metrics["MAPE"]),
  AIC = c(AIC(best_model), AIC(arima_model), AIC(arimax_model))
)

# Save final comparison
write.csv(final_comparison, "output/nonlinear_arima/final_model_comparison.csv", row.names = FALSE)

# Print final comparison
cat("\nFINAL MODEL COMPARISON:\n")
print(final_comparison)

# Identify best overall model
best_overall_idx <- which.min(final_comparison$RMSE)
cat("\nBest overall model based on RMSE:", final_comparison$Model[best_overall_idx], "\n")

# =============================================
# Visualize Best Model Fit and Residuals
# =============================================

# Create dataset for plotting
best_fitted <- switch(best_overall_idx,
                     fitted_linear,
                     fitted_arima,
                     fitted_arimax)

plot_data <- data.frame(
  Date = merged_data$Date,
  Actual = merged_data$Richemont,
  Fitted = best_fitted,
  Residuals = merged_data$Richemont - best_fitted
)

# Plot actual vs fitted
p_fit <- ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Fitted, color = "Fitted")) +
  theme_minimal() +
  labs(title = paste("Actual vs Fitted Values -", final_comparison$Model[best_overall_idx]),
       x = "Date", y = "Price (CHF)") +
  scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +
  theme(legend.title = element_blank())

# Plot residuals
p_resid <- ggplot(plot_data, aes(x = Date, y = Residuals)) +
  geom_line(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = paste("Residuals -", final_comparison$Model[best_overall_idx]),
       x = "Date", y = "Residuals")

# Save plots
ggsave("output/nonlinear_arima/best_model_fit.png", p_fit, width = 10, height = 6)
ggsave("output/nonlinear_arima/best_model_residuals.png", p_resid, width = 10, height = 6)

# =============================================
# Conclusion
# =============================================

cat("\nANALYSIS COMPLETE\n")
cat("Models and visualizations saved to output/nonlinear_arima/\n")
cat("Check the final_model_comparison.csv for performance metrics of all models.\n") 