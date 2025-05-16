# =============================================
# Unified Validation Script for Richemont Analysis
# =============================================
# This script performs both basic validation and advanced nonlinear model validation

# Load necessary packages
library(stats)
library(graphics)

# Create output directories
dir.create("output/validation", recursive = TRUE, showWarnings = FALSE)
dir.create("output/nonlinear", recursive = TRUE, showWarnings = FALSE)

cat("Starting unified validation process...\n")

# =============================================
# PART 1: Load Data
# =============================================

# Load Richemont stock data
cat("\n==== LOADING DATA ====\n")
cat("Loading Richemont data...\n")
richemont_data <- read.csv("data/richemont_historical_data.csv")

# Check if the data was loaded correctly
cat("Data dimensions:", dim(richemont_data), "\n")
cat("First few rows of Richemont data:\n")
print(head(richemont_data))

# Convert Date to proper format
richemont_data$Date <- as.Date(richemont_data$Date)

# =============================================
# PART 2: Basic Data Validation
# =============================================

cat("\n==== BASIC DATA VALIDATION ====\n")

# Summary statistics
cat("\nSummary of Richemont stock prices:\n")
print(summary(richemont_data$Close))

# Create a simple linear model based on time
richemont_data$TimeIndex <- 1:nrow(richemont_data)
lm_model <- lm(Close ~ TimeIndex, data = richemont_data)

cat("\nLinear model summary (Price ~ Time):\n")
print(summary(lm_model))

# Calculate residuals
richemont_data$Residuals <- residuals(lm_model)

# Basic residual analysis
cat("\nResidual summary:\n")
print(summary(richemont_data$Residuals))

# NEW: Test for normality of residuals
cat("\nShapiro-Wilk test for normality of residuals:\n")
shapiro_test <- shapiro.test(richemont_data$Residuals)
print(shapiro_test)
cat("p-value < 0.05 indicates residuals are not normally distributed\n")

# Calculate returns (percentage change)
richemont_data$Returns <- c(NA, diff(richemont_data$Close) / richemont_data$Close[-nrow(richemont_data)])

# Calculate lagged returns
richemont_data$Lag1 <- c(NA, richemont_data$Returns[-nrow(richemont_data)])
richemont_data$Lag2 <- c(NA, NA, richemont_data$Returns[-c(nrow(richemont_data), nrow(richemont_data)-1)])

# Remove NA values for correlation analysis
richemont_clean <- na.omit(richemont_data[, c("Returns", "Lag1", "Lag2")])

# Calculate correlations
correlations <- cor(richemont_clean)

cat("\nCorrelation between returns and lagged returns:\n")
print(correlations)

# Save correlation matrix to CSV
write.csv(correlations, "output/validation/autocorrelations.csv")

# Save basic summary to text file
sink("output/validation/basic_summary.txt")
cat("RICHEMONT STOCK DATA VALIDATION\n")
cat("===============================\n\n")
cat("Data Range:", min(richemont_data$Date), "to", max(richemont_data$Date), "\n")
cat("Number of Observations:", nrow(richemont_data), "\n\n")
cat("Price Summary:\n")
print(summary(richemont_data$Close))
cat("\nReturns Summary:\n")
print(summary(richemont_data$Returns, na.rm = TRUE))
cat("\nLinear Model Summary:\n")
print(summary(lm_model))
cat("\nAutocorrelation Analysis:\n")
print(correlations)
cat("\nShapiro-Wilk Normality Test for Linear Model Residuals:\n")
print(shapiro_test)
sink()

# Create basic plots
# Price plot
png("output/validation/price_plot.png", width = 800, height = 600)
plot(richemont_data$Date, richemont_data$Close, 
     type = "l", col = "blue", 
     main = "Richemont Stock Price Over Time",
     xlab = "Date", ylab = "Price")
dev.off()

# Returns plot
png("output/validation/returns_plot.png", width = 800, height = 600)
plot(richemont_data$Date[-1], richemont_data$Returns[-1], 
     type = "l", col = "red", 
     main = "Richemont Stock Returns Over Time",
     xlab = "Date", ylab = "Returns")
dev.off()

# Residual plot
png("output/validation/residuals_plot.png", width = 800, height = 600)
plot(richemont_data$Date, richemont_data$Residuals,
     type = "l", col = "green",
     main = "Model Residuals Over Time",
     xlab = "Date", ylab = "Residuals")
abline(h = 0, lty = 2)
dev.off()

# NEW: Create QQ plot and histogram for residuals
png("output/validation/residuals_qq_plot.png", width = 800, height = 600)
qqnorm(richemont_data$Residuals, main = "Q-Q Plot of Linear Model Residuals")
qqline(richemont_data$Residuals, col = "red", lwd = 2)
dev.off()

png("output/validation/residuals_histogram.png", width = 800, height = 600)
hist(richemont_data$Residuals, breaks = 30, 
     main = "Histogram of Linear Model Residuals", 
     xlab = "Residuals", 
     prob = TRUE,
     col = "lightblue")
# Add normal density curve
curve(dnorm(x, mean = mean(richemont_data$Residuals), sd = sd(richemont_data$Residuals)), 
      add = TRUE, col = "red", lwd = 2)
dev.off()

cat("\nBasic data validation complete! Results saved to output/validation/\n")

# =============================================
# PART 3: Generate Synthetic Currency Data
# =============================================

cat("\n==== GENERATING CURRENCY DATA ====\n")
cat("Creating synthetic currency data for demonstration...\n")

set.seed(123)  # For reproducibility
n <- nrow(richemont_data)

# Create synthetic EUR/CHF data with some correlation to stock price
eur_chf <- 1.05 + 0.0002 * richemont_data$Close + rnorm(n, 0, 0.02)

# Create synthetic USD/CHF data with different pattern
usd_chf <- 0.90 + 0.0001 * richemont_data$Close + rnorm(n, 0, 0.03)

# Create synthetic CNY/CHF data with trend
cny_chf <- 6.5 + 0.0003 * richemont_data$Close + rnorm(n, 0, 0.05)

# Add currency data to the main dataframe
richemont_data$EUR_CHF <- eur_chf
richemont_data$USD_CHF <- usd_chf
richemont_data$CNY_CHF <- cny_chf

# =============================================
# PART 4: Create Nonlinear Transformations
# =============================================

cat("\n==== CREATING NONLINEAR TRANSFORMATIONS ====\n")
cat("Creating nonlinear transformations of currency variables...\n")

# Log transformations (for percentage changes)
richemont_data$log_Close <- log(richemont_data$Close)
richemont_data$log_EUR <- log(richemont_data$EUR_CHF)
richemont_data$log_USD <- log(richemont_data$USD_CHF)
richemont_data$log_CNY <- log(richemont_data$CNY_CHF)

# Squared terms (for quadratic relationships)
richemont_data$EUR_squared <- richemont_data$EUR_CHF^2
richemont_data$USD_squared <- richemont_data$USD_CHF^2
richemont_data$CNY_squared <- richemont_data$CNY_CHF^2

# Interaction terms (relationships between currencies)
richemont_data$EUR_USD <- richemont_data$EUR_CHF * richemont_data$USD_CHF
richemont_data$EUR_CNY <- richemont_data$EUR_CHF * richemont_data$CNY_CHF
richemont_data$USD_CNY <- richemont_data$USD_CHF * richemont_data$CNY_CHF

# Polynomial terms (third-degree)
richemont_data$EUR_cubed <- richemont_data$EUR_CHF^3
richemont_data$USD_cubed <- richemont_data$USD_CHF^3
richemont_data$CNY_cubed <- richemont_data$CNY_CHF^3

# =============================================
# PART 5: Fit Multiple Models with Different Transformations
# =============================================

cat("\n==== FITTING NONLINEAR MODELS ====\n")
cat("Fitting various models with different transformations...\n")

# Model 1: Basic linear model (baseline)
model1 <- lm(Close ~ EUR_CHF + USD_CHF + CNY_CHF, data = richemont_data)

# Model 2: Log-transformed model
model2 <- lm(log_Close ~ log_EUR + log_USD + log_CNY, data = richemont_data)

# Model 3: Quadratic model
model3 <- lm(Close ~ EUR_CHF + USD_CHF + CNY_CHF + EUR_squared + USD_squared + CNY_squared, data = richemont_data)

# Model 4: Model with interaction terms
model4 <- lm(Close ~ EUR_CHF + USD_CHF + CNY_CHF + EUR_USD + EUR_CNY + USD_CNY, data = richemont_data)

# Model 5: Polynomial model
model5 <- lm(Close ~ EUR_CHF + EUR_squared + EUR_cubed + USD_CHF + USD_squared + USD_cubed + CNY_CHF + CNY_squared + CNY_cubed, data = richemont_data)

# Model 6: Complex model with multiple transformations
model6 <- lm(Close ~ EUR_CHF + USD_CHF + CNY_CHF + EUR_squared + USD_squared + EUR_USD + EUR_CNY + log_EUR + log_USD + log_CNY, data = richemont_data)

# =============================================
# PART 6: Compare Models
# =============================================

cat("\n==== COMPARING MODELS ====\n")

# Create a function to summarize model results
summarize_model <- function(model, name) {
  r_squared <- summary(model)$r.squared
  adj_r_squared <- summary(model)$adj.r.squared
  aic <- AIC(model)
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals(model)^2))
  
  # Calculate Residual Standard Error (RSE)
  rse <- summary(model)$sigma
  
  # Return results
  c(Model = name, R_squared = r_squared, Adj_R_squared = adj_r_squared, AIC = aic, RMSE = rmse, RSE = rse)
}

# Collect model results
results <- data.frame(
  rbind(
    summarize_model(model1, "Linear"),
    summarize_model(model2, "Log-Transformed"),
    summarize_model(model3, "Quadratic"),
    summarize_model(model4, "Interaction"),
    summarize_model(model5, "Polynomial"),
    summarize_model(model6, "Complex")
  ),
  stringsAsFactors = FALSE
)

# Convert numeric columns to numeric
results$R_squared <- as.numeric(results$R_squared)
results$Adj_R_squared <- as.numeric(results$Adj_R_squared)
results$AIC <- as.numeric(results$AIC)
results$RMSE <- as.numeric(results$RMSE)
results$RSE <- as.numeric(results$RSE)

# Print results table
cat("\nMODEL COMPARISON:\n")
print(results)

# Save results to CSV
write.csv(results, "output/nonlinear/model_comparison.csv", row.names = FALSE)

# Find the best model based on adjusted R-squared
best_model_idx <- which.max(results$Adj_R_squared)
best_model_name <- results$Model[best_model_idx]
cat("\nBest model by adjusted R-squared:", best_model_name, "\n")

# Get the actual best model object
best_model <- switch(best_model_idx,
                     model1,
                     model2, 
                     model3,
                     model4,
                     model5,
                     model6)

# =============================================
# PART 7: Analyze Best Model
# =============================================

cat("\n==== BEST MODEL ANALYSIS ====\n")
cat("\nBEST MODEL SUMMARY:\n")
print(summary(best_model))

# NEW: Test for normality of best model residuals
best_residuals <- residuals(best_model)
cat("\nShapiro-Wilk test for normality of best model residuals:\n")
best_shapiro_test <- shapiro.test(best_residuals)
print(best_shapiro_test)
cat("p-value < 0.05 indicates residuals are not normally distributed\n")

# =============================================
# PART 8: Visualize Results
# =============================================

cat("\n==== CREATING VISUALIZATIONS ====\n")

# Plot residuals
png("output/nonlinear/best_model_residuals.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(best_model)
dev.off()

# Create a dataframe with actual and fitted values
fitted_df <- data.frame(
  Date = richemont_data$Date,
  Actual = richemont_data$Close,
  Fitted = fitted(best_model),
  Residuals = residuals(best_model)
)

# Plot actual vs fitted
png("output/nonlinear/actual_vs_fitted.png", width = 1000, height = 600)
plot(fitted_df$Date, fitted_df$Actual, type = "l", col = "blue", 
     xlab = "Date", ylab = "Stock Price (CHF)",
     main = paste("Actual vs Fitted Values -", best_model_name, "Model"))
lines(fitted_df$Date, fitted_df$Fitted, col = "red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("blue", "red"), lty = 1)
dev.off()

# Plot residuals over time
png("output/nonlinear/residuals_time.png", width = 1000, height = 600)
plot(fitted_df$Date, fitted_df$Residuals, type = "l", col = "darkgreen",
     xlab = "Date", ylab = "Residuals",
     main = paste("Residuals Over Time -", best_model_name, "Model"))
abline(h = 0, lty = 2, col = "red")
dev.off()

# NEW: Create QQ plot and histogram for best model residuals
png("output/nonlinear/best_model_residuals_qq_plot.png", width = 800, height = 600)
qqnorm(best_residuals, main = paste("Q-Q Plot of", best_model_name, "Model Residuals"))
qqline(best_residuals, col = "red", lwd = 2)
dev.off()

png("output/nonlinear/best_model_residuals_histogram.png", width = 800, height = 600)
hist(best_residuals, breaks = 30, 
     main = paste("Histogram of", best_model_name, "Model Residuals"), 
     xlab = "Residuals", 
     prob = TRUE,
     col = "lightblue")
# Add normal density curve
curve(dnorm(x, mean = mean(best_residuals), sd = sd(best_residuals)), 
      add = TRUE, col = "red", lwd = 2)
dev.off()

# Comment on scatter plots
cat("\nSkipping individual scatter plots due to synthetic data limitations.\n")
cat("The relationships shown would not be meaningful with synthetic currency data.\n")
cat("For a real analysis, use actual currency data and then create relationship visualizations.\n")

# =============================================
# PART 9: Save Model Results
# =============================================

# Save coefficients
coefs <- data.frame(
  Variable = names(coef(best_model)),
  Coefficient = coef(best_model),
  stringsAsFactors = FALSE
)
write.csv(coefs, "output/nonlinear/best_model_coefficients.csv", row.names = FALSE)

# Save best model formula
sink("output/nonlinear/best_model_formula.txt")
cat("BEST MODEL:", best_model_name, "\n\n")
cat("Formula:\n")
print(formula(best_model))
cat("\nCoefficients:\n")
print(coef(best_model))
cat("\nShapiro-Wilk Normality Test for Residuals:\n")
print(best_shapiro_test)
sink()

# =============================================
# PART 10: Validation Summary
# =============================================

cat("\n==== VALIDATION SUMMARY ====\n")
cat("1. Basic data validation:\n")
cat("   - Stock price summary statistics calculated\n")
cat("   - Linear time trend model analyzed (R-squared:", round(summary(lm_model)$r.squared, 4), ")\n")
cat("   - Return autocorrelations examined\n")
cat("   - Residual normality assessed (Shapiro-Wilk p-value:", format.pval(shapiro_test$p.value, digits=4), ")\n")
cat("   - Results in output/validation/\n\n")

cat("2. Advanced model validation:\n")
cat("   - Nonlinear transformations of currency variables tested\n")
cat("   - Multiple model forms compared\n")
cat("   - Best model:", best_model_name, "(R-squared:", round(max(results$R_squared), 4), ")\n") 
cat("   - Residual normality assessed (Shapiro-Wilk p-value:", format.pval(best_shapiro_test$p.value, digits=4), ")\n")
cat("   - Results in output/nonlinear/\n\n")

cat("Unified validation process complete!\n") 