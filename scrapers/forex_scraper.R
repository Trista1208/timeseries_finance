library(tidyquant)
library(tidyverse)
library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(rugarch)

# Set date range
from_date <- "2020-01-01"
to_date <- "2024-12-31"

# Get FX rates
getSymbols(c("CHFEUR=X", "CHFUSD=X", "CHFCNY=X"), src = "yahoo", from = from_date, to = to_date) # nolint
getSymbols("LUXU.PA", src = "yahoo", from = from_date, to = to_date)
getSymbols("RIT1.F", src = "yahoo", from = from_date, to = to_date)

richemont_data <- read.csv("C:\\Users\\VincentmobileUp\\PycharmProjects\\Timeseries_finance\\timeseries_finance\\data\\richemont_historical_data.csv") # nolint
forex_data <- read.csv("C:\\Users\\VincentmobileUp\\PycharmProjects\\Timeseries_finance\\timeseries_finance\\data\\forex_data.csv")

# Rename for readability
chfeur <- `CHFEUR=X`
chfusd <- `CHFUSD=X`
chfcny <- `CHFCNY=X`

chfeur <- forex_data[, c("CHFEUR.X.Close")]
chfusd <- forex_data[, c("CHFUSD.X.Close")]
chfcny <- forex_data[, c("CHFCNY.X.Close")]
combined <- merge(chfeur, chfusd, chfcny)

luxury_index <- `LUXU.PA`

gold <- `GC=F`

richemont <- `RIT1.F`

# change here if you want to write to csv or plot
currency_name <- "RIT1.F"
currency_data <- richemont

file_path <- "forex_data.csv"
filename <- paste0(currency_name, "_", from_date, "_", to_date, ".csv")

if (file.exists(file_path)) {
  print("The file already exists")
} else {
  write.csv(currency_data, filename, row.names = TRUE)
}

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