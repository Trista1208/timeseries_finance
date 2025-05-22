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
getSymbols(c("CHFEUR=X", "CHFUSD=X", "CHFCNY=X", "LUXU.PA", "GC=F"), src = "yahoo", from = from_date, to = to_date) # nolint

# Rename for readability
chfeur <- `CHFEUR=X`
chfusd <- `CHFUSD=X`
chfcny <- `CHFCNY=X`
lux_index <- `LUXU.PA`
gold <- `GC=F`

save(chfeur, chfusd, chfcny, lux_index, gold, file = "yahoo_data_2015.RData")

chfeur <- forex_data[, c("CHFEUR.X.Close")]
chfusd <- forex_data[, c("CHFUSD.X.Close")]
chfcny <- forex_data[, c("CHFCNY.X.Close")]
combined <- merge(chfeur, chfusd, chfcny)

# change here if you want to write to csv or plot
currency_name <- "RIT1.F"
currency_data <- richemont

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