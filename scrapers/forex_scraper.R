library(tidyquant)
library(tidyverse)
library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(rugarch)

# Set date range
from_date <- "2015-01-01"
to_date <- "2024-12-31"

# Get FX rates
getSymbols(c("CHFEUR=X", "CHFUSD=X", "CHFCNY=X"), src = "yahoo", from = from_date, to = to_date) # nolint
getSymbols("LUXU.PA", src = "yahoo", from = from_date, to = to_date)

# Rename for readability
chfeur <- `CHFEUR=X`
chfusd <- `CHFUSD=X`
chfcny <- `CHFCNY=X`
combined <- merge(chfeur, chfusd, chfcny)

luxury_index <- `LUXU.PA`

# change here if you want to write to csv or plot
currency_name <- "LUXU.PA"
currency_data <- luxury_index

file_path <- "forex_data.csv"
filename <- paste0(currency_name, "_", from_date, "_", to_date, ".csv")

if (file.exists(file_path)) {
  print("The file already exists")
} else {
  write.csv(currency_data, filename, row.names = TRUE)
}

richemont_data <- read.csv("C:\\Users\\VincentmobileUp\\PycharmProjects\\Timeseries_finance\\timeseries_finance\\data\\richemont_historical_data.csv") # nolint


#prepare the data
ts_richemont <- xts(richemont_data$close, order.by = as.Date(richemont_data$Date))
ts_richemont$Date <- as.Date(richemont_data$Date, format = "%Y-%m-%d")
ts_richemont$Close <- as.numeric(richemont_data$Close)

ts_forex_chfeur <- chfeur[, c("CHFEUR=X.Close")]
ts_forex_chfusd <- chfusd[, c("CHFUSD=X.Close")]
ts_forex_chfcny <- chfcny[, c("CHFCNY=X.Close")]
ts_luxury_index <- luxury_index[, c("LUXU.PA.Close")]

log_returns_richemont <- diff(log(ts_richemont))
log_returns_chfeur <- diff(log(ts_forex_chfeur))
log_returns_chfusd <- diff(log(ts_forex_chfusd))
log_returns_chfcny <- diff(log(ts_forex_chfcny))
log_returns_luxury_index <- diff(log(ts_luxury_index))

log_returns_chfcny <- na.omit(log_returns_chfcny)
log_returns_chfeur <- na.omit(log_returns_chfeur)
log_returns_chfusd <- na.omit(log_returns_chfusd)
log_returns_luxury_index <- na.omit(log_returns_luxury_index)

plot(log_returns_chfeur, main = "Forex Log Returns")
plot(log_returns_richemont, main = "Richemont Log Returns")

# align all ts to the same date and combine in one array
merged_data <- Reduce(function(x, y) merge(x, y, join = "inner"),
                      list(log_returns_richemont, log_returns_chfeur, log_returns_chfusd, log_returns_chfcny, log_returns_luxury_index)) # nolint

colnames(merged_data) <- c("Richemont", "CHFEUR", "CHFUSD", "CHFCNY", "LUX_INDEX")
merged_data <- na.omit(merged_data)

#check for stationarity
adf.test(log_returns_richemont)
adf.test(log_returns_chfeur)
adf.test(log_returns_chfusd)
adf.test(log_returns_chfcny)
adf.test(log_returns_luxury_index)

#prepare train/test split
split_date <- "2022-12-31"
train_data <- merged_data[paste0("/", split_date)]
test_data  <- merged_data[paste0(as.Date(split_date) + 1, "/")]


#fit linear model to see if forex data can explain richemont data
lm_model <- lm(Richemont ~ CHFEUR + CHFUSD + CHFCNY, data = train_data)
plot(residuals(lm_model), main = "Residuals of LM Model")

lm_pred <- predict(lm_model, newdata = test_data)
summary(lm_pred)

lm_luxindex <- lm(Richemont ~ LUX_INDEX + CHFUSD, data = train_data)
lm_luxindex_pred <- predict(lm_luxindex, newdata = test_data)
summary(lm_luxindex)

plot(index(test_data), test_data$Richemont, type = "l", col = "black", lwd = 2,
     ylab = "Richemont", main = "Actual vs Predicted")
lines(index(test_data), lm_luxindex_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "blue"), lty = 1)

#arima
model <- auto.arima(na.omit(log_returns_richemont))
forecast(model, h = 10)

#Build ARIMAX Model to explain Richemont returns based on Forex Rates
y <- train_data$Richemont
x <- train_data[, c("CHFEUR", "CHFUSD", "CHFCNY", "LUX_INDEX")]

model_arimax <- auto.arima(y, xreg = x)

#model residuals and fit garch model to model volatility of residuals
res <- residuals(model_arimax)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

garch_fit <- ugarchfit(spec, res)
volatility <- sigma(garch_fit)
vol_xts <- xts(volatility, order.by = index(train_data))

plot(vol_xts, type = "l", col = "blue", lwd = 2,
     main = "Conditional Volatility from GARCH Model",
     ylab = "Volatility", xlab = "Time")

#try to force the model to explore other combinations than (0,0,0)
model_arimax_2 <- auto.arima(
  y,
  xreg = x,
  stepwise = FALSE,    # explore more combinations
  approximation = FALSE  # use exact maximum likelihood
)

summary(model_arimax_2)

#try weekly returns to get a smoother signal
weekly_returns <- apply.weekly(train_data, colMeans)
y_weekly <- weekly_returns$Richemont
x_weekly <- weekly_returns[, c("CHFEUR", "CHFUSD", "CHFCNY", "LUX_INDEX")]

arimax_weekly <- auto.arima(y_weekly, xreg = x_weekly)
summary(arimax_weekly)

#try monthly returns
monthly_returns <- apply.monthly(train_data, colMeans)
y_monthly <- monthly_returns$Richemont
x_monthly <- monthly_returns[, c("CHFEUR", "CHFUSD", "CHFCNY", "LUX_INDEX")]

arimax_monthly <- auto.arima(y_monthly, xreg = x_monthly)
summary(arimax_monthly)

monthly_test <- apply.monthly(test_data, colMeans)
x_monthly_test <- monthly_test[, c("CHFEUR", "CHFUSD", "CHFCNY", "LUX_INDEX")]

forecast_obj <- forecast(arimax_monthly, xreg = x_monthly_test)
plot(forecast_obj)


#try a lagged model to capture forex influence
lag1 <- lag.xts(merged_data[, c("CHFEUR", "CHFUSD", "CHFCNY")], k = 1)
lag2 <- lag.xts(merged_data[, c("CHFEUR", "CHFUSD", "CHFCNY")], k = 2)
lag5 <- lag.xts(merged_data[, c("CHFEUR", "CHFUSD", "CHFCNY")], k = 5)

x_lagged <- Reduce(function(x, y) merge(x, y, join = "inner"),
                   list(lag1, lag2, lag5))

x_lagged <- na.omit(x_lagged)

colnames(x_lagged) <- c("CHFEUR_1", "CHFUSD_1", "CHFCNY_1",
                        "CHFEUR_2", "CHFUSD_2", "CHFCNY_2",
                        "CHFEUR_5", "CHFUSD_5", "CHFCNY_5")

y_aligned <- merged_data$Richemont[index(x_lagged)]

model_lagged <- auto.arima(y_aligned, xreg = x_lagged)
summary(model_lagged)

#weekly lagged returns
weekly_lagged_returns <- apply.weekly(x_lagged, colMeans)
y_lagged_weekly <- apply.weekly(y_aligned, colMeans)

weekly_lagged_model <- auto.arima(y_lagged_weekly, xreg = weekly_lagged_returns)
summary(weekly_lagged_model)

# Convert xts to data frame with date as a column
df <- data.frame(date = index(currency_data), coredata(currency_data))

close_df <- df %>%
  select(date, contains("Close"))


normalized_df <- close_df %>%
  mutate(across(where(is.numeric), ~ . / first(., na_rm = TRUE) * 100)) %>%
  mutate(date = as.Date(date))  # Make sure date column is Date class

# Reshape to long format for plotting
long_df <- normalized_df %>%
  pivot_longer(-date, names_to = "series", values_to = "value")

# Plot it
ggplot(long_df, aes(x = date, y = value, color = series)) +
  geom_line(size = 1) +
  labs(title = "Normalized Price & FX Trends",
       x = "Date", y = "Indexed Value (100 = start)",
       color = "Series") +
  theme_minimal() +
  theme(aspect.ratio = 0.5)