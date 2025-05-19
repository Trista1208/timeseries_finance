# =============================================
# Nonlinear Currency Relationships
# =============================================

# Load basic packages
library(stats)
library(graphics)

# Create output directory
dir.create("output/nonlinear", recursive = TRUE, showWarnings = FALSE)

# =============================================
# Load Data
# =============================================

# Load Richemont stock data
cat("Loading Richemont data...\n")
richemont_data <- read.csv("data/richemont_historical_data.csv")

# Check if the data was loaded correctly
cat("Data dimensions:", dim(richemont_data), "\n")

# Convert Date to proper format
richemont_data$Date <- as.Date(richemont_data$Date)

# =============================================
# Generate Synthetic Currency Data (if needed)
# =============================================

# For demonstration purposes, we'll create synthetic currency data
# In a real scenario, you'd use actual currency exchange rate data
cat("\nCreating synthetic currency data for demonstration...\n")

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
# Create Nonlinear Transformations
# =============================================

cat("\nCreating nonlinear transformations of currency variables...\n")

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

# Exponential terms
richemont_data$exp_EUR <- exp(richemont_data$EUR_CHF / 10)  # Scaled to avoid overflow
richemont_data$exp_USD <- exp(richemont_data$USD_CHF / 10)
richemont_data$exp_CNY <- exp(richemont_data$CNY_CHF / 100)

# =============================================
# Fit Multiple Models with Different Transformations
# =============================================

cat("\nFitting various models with different transformations...\n")

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
# Compare Models
# =============================================

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
# Analyze Best Model
# =============================================

cat("\nBEST MODEL SUMMARY:\n")
print(summary(best_model))

# =============================================
# Visualize Residuals
# =============================================

# Plot residuals
png("output/nonlinear/best_model_residuals.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(best_model)
dev.off()

# =============================================
# Visualize Model Fit
# =============================================

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

# =============================================
# Visualize Relationships
# =============================================

# Remove the individual scatter plots section and replace with a more focused approach
# Comment explaining why we're not creating individual scatter plots
cat("\nSkipping individual scatter plots due to synthetic data limitations.\n")
cat("The relationships shown would not be meaningful with synthetic currency data.\n")
cat("For a real analysis, use actual currency data and then create relationship visualizations.\n")

# =============================================
# Save Model Results
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
sink()

cat("\nAnalysis complete. Results saved to output/nonlinear/ directory.\n") 