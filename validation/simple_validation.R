# Create output directory
dir.create("output/validation", recursive = TRUE, showWarnings = FALSE)

# =============================================
# Load Data
# =============================================

# Load Richemont stock data
cat("Loading Richemont data...\n")
richemont_data <- read.csv("data/richemont_historical_data.csv")

# Check if the data was loaded correctly
cat("Data dimensions:", dim(richemont_data), "\n")
cat("First few rows of Richemont data:\n")
print(head(richemont_data))

# =============================================
# Basic Data Analysis
# =============================================

# Convert Date to proper format
richemont_data$Date <- as.Date(richemont_data$Date)

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

# =============================================
# Basic Correlation Analysis
# =============================================

# Calculate returns (percentage change)
richemont_data$Returns <- c(NA, diff(richemont_data$Close) / richemont_data$Close[-nrow(richemont_data)])

# Calculate lagged returns
richemont_data$Lag1 <- c(NA, richemont_data$Returns[-nrow(richemont_data)])
richemont_data$Lag2 <- c(NA, NA, richemont_data$Returns[-c(nrow(richemont_data), nrow(richemont_data)-1)])

# Remove NA values
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
sink()

# =============================================
# Basic Plot Creation
# =============================================

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

cat("\nSimple validation complete. Results saved to output/validation directory.\n") 