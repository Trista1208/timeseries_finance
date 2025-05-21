library(tidyquant)
library(tidyverse)
library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(rugarch)
library(FinTS)

# load data
richemont_data <- read.csv("C:\\Users\\vincent schreyer\\PycharmProjects\\Timeseries_Analysis\\timeseries_finance\\data\\richemont_historical_data.csv") # nolint
load("yahoo_data.RData")

chfeur_close <- chfeur[, "CHFEUR=X.Close"]
chfusd_close <- chfusd[, "CHFUSD=X.Close"]
chfcny_close <- chfcny[, "CHFCNY=X.Close"]
lux_index <- lux_index[, "LUXU.PA.Close"]
gold <- gold[, "GC=F.Close"]

#check structure of data
str(chfeur_close)
str(chfusd_close)
str(chfcny_close)
str(lux_index)
str(gold)

#prepare the data
<<<<<<< HEAD
ts_richemont <- xts(richemont_data$Close, order.by = as.Date(richemont_data$Date)) # nolint
richemont_data$Date <- as.Date(richemont_data$Date, format = "%Y-%m-%d")
richemont_data$Close <- as.numeric(richemont_data$Close)
=======
start_date = as.Date("2015-01-01")
dates_gold <- seq(from = start_date, by = "days", length.out = nrow(gold_data))
dates_fx   <- seq(from = start_date, by = "days", length.out = nrow(forex_data))
dates_lux  <- seq(from = start_date, by = "days", length.out = nrow(luxury_index))

ts_richemont <- xts(richemont_data$Close, order.by = as.Date(richemont_data$Date))
ts_chfeur <- xts(forex_data$CHFEUR.X.Close, order.by = dates_fx)
ts_chfusd <- xts(forex_data$CHFUSD.X.Close, order.by = dates_fx)
ts_chfcny <- xts(forex_data$CHFCNY.X.Close, order.by = dates_fx)
ts_lux <- xts(luxury_index$LUXU.PA.Close, order.by = dates_lux)
ts_gold <- xts(gold_data$GC.F.Close, order.by = dates_gold)
>>>>>>> 89465b8be949297fb15057913171a97deff2b363

log_returns_richemont <- diff(log(ts_richemont))
log_returns_chfeur <- diff(log(chfeur_close))
log_returns_chfusd <- diff(log(chfusd_close))
log_returns_chfcny <- diff(log(chfcny_close))
log_returns_luxury_index <- diff(log(lux_index))
log_returns_gold <- diff(log(gold))

log_returns_richemont <- na.omit(log_returns_richemont)
log_returns_chfcny <- na.omit(log_returns_chfcny)
log_returns_chfeur <- na.omit(log_returns_chfeur)
log_returns_chfusd <- na.omit(log_returns_chfusd)
log_returns_luxury_index <- na.omit(log_returns_luxury_index)
log_returns_gold <- na.omit(log_returns_gold)

plot(log_returns_gold, main = "Gold Log Returns")
plot(log_returns_richemont, main = "Richemont Log Returns")

# align all ts to the same date and combine in one array
merged_data <- Reduce(function(x, y) merge(x, y, join = "inner"),
                      list(log_returns_richemont, log_returns_chfeur, log_returns_chfusd, log_returns_chfcny, log_returns_luxury_index, log_returns_gold)) # nolint

colnames(merged_data) <- c("Richemont", "CHFEUR", "CHFUSD", "CHFCNY", "LUX_INDEX", "GOLD")

#check for stationarity
adf.test(log_returns_richemont)
adf.test(log_returns_chfeur)
adf.test(log_returns_chfusd)
adf.test(log_returns_chfcny)
adf.test(log_returns_luxury_index)
adf.test(log_returns_gold)

#check for correlation in between time series
cor(merged_data$Richemont, merged_data$GOLD, use = "complete.obs")


#prepare train/test split
split_date <- "2022-12-31"
train_data <- merged_data[paste0("/", split_date)]
test_data  <- merged_data[paste0(as.Date(split_date) + 1, "/")]

# Filter for a specific range
data_2020 <- merged_data["2020-01-01/2020-12-31"]
split_date <- "2020-11-01"
train_2020 <- data_2020[paste0("/", split_date)]
test_2020  <- data_2020[paste0(as.Date(split_date) + 1, "/")]

#fit linear model to see if forex data can explain richemont data
<<<<<<< HEAD
lm_model <- lm(Richemont ~ CHFEUR + CHFUSD, data = train_2020)
=======
lm_model <- lm(Richemont ~ CHFEUR + CHFUSD + LUX_INDEX, data = train_data)
>>>>>>> 89465b8be949297fb15057913171a97deff2b363
summary(lm_model)
plot(residuals(lm_model), main = "Residuals of LM Model")

lm_pred <- predict(lm_model, newdata = test_2020)
summary(lm_pred)

plot(index(test_2020), test_2020$Richemont, type = "l", col = "black", lwd = 2,
     ylab = "Richemont", main = "Actual vs Predicted")
lines(index(test_2020), lm_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "blue"), lty = 1)

#arima
model <- auto.arima(na.omit(log_returns_richemont))
forecast(model, h = 10)
tsdisplay(residuals(model))

#Build ARIMAX Model to explain Richemont returns based on Forex Rates
y <- train_2020$Richemont
x <- train_2020[, c("CHFEUR", "CHFUSD", "CHFCNY")]

model_arimax <- auto.arima(y, xreg = x)

#collect model residuals
res <- residuals(model_arimax)

#check for heteroskedasticity
ArchTest(res)

#apply garch model to model varying volatility in residuals
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
)

garch_fit <- ugarchfit(spec, res)
volatility <- sigma(garch_fit)
vol_xts <- xts(volatility, order.by = index(train_2020))

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
weekly_returns <- apply.weekly(merged_data, colMeans)
y_weekly <- weekly_returns$Richemont
x_weekly <- weekly_returns[, c("CHFEUR", "CHFUSD", "CHFCNY", "GOLD")]

lm_weekly <- lm(y_weekly ~ x_weekly, data = weekly_returns)

arimax_weekly <- auto.arima(y_weekly, xreg = x_weekly)
summary(arimax_weekly)

#try monthly returns
monthly_returns <- apply.monthly(merged_data, colMeans)
y_monthly <- monthly_returns$Richemont
x_monthly <- monthly_returns[, c("CHFEUR", "CHFUSD", "CHFCNY", "GOLD")]

lm_monthly <- lm(y_monthly ~ x_monthly, data = monthly_returns)
summary(lm_monthly)

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

# Calculate p values via t statistic and normal distribution
coefs <- coef(model_lagged)
se <- sqrt(diag(vcov(model_lagged)))  

t_values <- coefs / se

p_values <- 2 * (1 - pnorm(abs(t_values)))

results <- data.frame(
  Coefficient = coefs,
  Std_Error = se,
  t_value = t_values,
  p_value = p_values
)

print(round(results, 4))

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