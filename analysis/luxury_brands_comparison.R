# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Read the data for all three companies
richemont_data <- read.csv("data/richemont_historical_data.csv")
kering_data <- read.csv("data/kering_historical_data.csv")
lvmh_data <- read.csv("data/lvmh_historical_data.csv")

# Convert date columns to Date type
richemont_data$Date <- as.Date(richemont_data$Date)
kering_data$Date <- as.Date(kering_data$Date)
lvmh_data$Date <- as.Date(lvmh_data$Date)

# Print date ranges to verify data availability
print("Date ranges for stock data:")
print(paste("Richemont data from", min(richemont_data$Date), "to", max(richemont_data$Date)))
print(paste("Kering data from", min(kering_data$Date), "to", max(kering_data$Date)))
print(paste("LVMH data from", min(lvmh_data$Date), "to", max(lvmh_data$Date)))

# Create consistent datasets with company identifiers
richemont_data <- richemont_data %>% 
  select(Date, Close) %>% 
  rename(Richemont = Close)

kering_data <- kering_data %>% 
  select(Date, Close) %>% 
  rename(Kering = Close)

lvmh_data <- lvmh_data %>% 
  select(Date, Close) %>% 
  rename(LVMH = Close)

# Merge all datasets
# Start with an inner join to include only dates common to all three
common_data <- richemont_data %>%
  inner_join(kering_data, by = "Date") %>%
  inner_join(lvmh_data, by = "Date")

print(paste("Number of common trading days:", nrow(common_data)))

# Create a function to normalize data for better comparison
normalize <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if(is.infinite(min_x) || is.infinite(max_x) || max_x == min_x) {
    return(rep(0, length(x)))
  }
  return((x - min_x) / (max_x - min_x))
}

# Normalize the stock prices for comparison
common_data_normalized <- common_data %>%
  mutate(
    Richemont_norm = normalize(Richemont),
    Kering_norm = normalize(Kering),
    LVMH_norm = normalize(LVMH)
  )

# Create a long format dataset for plotting
common_data_long <- common_data %>%
  pivot_longer(cols = c(Richemont, Kering, LVMH),
               names_to = "Company",
               values_to = "Price")

common_data_normalized_long <- common_data_normalized %>%
  select(Date, Richemont_norm, Kering_norm, LVMH_norm) %>%
  pivot_longer(cols = c(Richemont_norm, Kering_norm, LVMH_norm),
               names_to = "Company",
               values_to = "Normalized_Price") %>%
  mutate(Company = gsub("_norm", "", Company))

# Create the raw price plot
p1 <- ggplot(common_data_long, aes(x = Date, y = Price, color = Company)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "Richemont" = "#1f77b4",
    "Kering" = "#ff7f0e",
    "LVMH" = "#2ca02c"
  )) +
  labs(
    title = "Stock Price Comparison of Luxury Brands",
    subtitle = "Raw Closing Prices",
    x = "Date",
    y = "Price (in local currency)",
    color = "Company"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Create the normalized price plot
p2 <- ggplot(common_data_normalized_long, aes(x = Date, y = Normalized_Price, color = Company)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "Richemont" = "#1f77b4",
    "Kering" = "#ff7f0e",
    "LVMH" = "#2ca02c"
  )) +
  labs(
    title = "Normalized Stock Price Comparison of Luxury Brands",
    subtitle = "Prices Normalized to Range [0,1] for Comparison",
    x = "Date",
    y = "Normalized Price",
    color = "Company"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Save the plots
ggsave("charts/luxury_brands_raw_price_comparison.png", p1, width = 12, height = 6, dpi = 300)
ggsave("charts/luxury_brands_normalized_comparison.png", p2, width = 12, height = 6, dpi = 300)

# Calculate relative performance
# Set a common start date and normalize prices to that date (index = 100)
indexed_data <- common_data %>%
  arrange(Date) %>%
  mutate(
    Richemont_index = (Richemont / first(Richemont)) * 100,
    Kering_index = (Kering / first(Kering)) * 100,
    LVMH_index = (LVMH / first(LVMH)) * 100
  )

# Convert to long format for plotting
indexed_data_long <- indexed_data %>%
  select(Date, Richemont_index, Kering_index, LVMH_index) %>%
  pivot_longer(cols = c(Richemont_index, Kering_index, LVMH_index),
               names_to = "Company",
               values_to = "Index") %>%
  mutate(Company = gsub("_index", "", Company))

# Create the indexed price plot
p3 <- ggplot(indexed_data_long, aes(x = Date, y = Index, color = Company)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "Richemont" = "#1f77b4",
    "Kering" = "#ff7f0e",
    "LVMH" = "#2ca02c"
  )) +
  labs(
    title = "Indexed Stock Performance of Luxury Brands",
    subtitle = "First Trading Day = 100",
    x = "Date",
    y = "Index Value",
    color = "Company"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave("charts/luxury_brands_indexed_performance.png", p3, width = 12, height = 6, dpi = 300)

# Calculate correlations between stock prices
correlations <- cor(common_data[, c("Richemont", "Kering", "LVMH")], 
                  use = "pairwise.complete.obs")
print("Correlations between luxury brand stock prices:")
print(correlations)

# Calculate monthly returns
monthly_returns <- common_data %>%
  arrange(Date) %>%
  mutate(
    month = floor_date(Date, "month"),
    Richemont_prev = lag(Richemont),
    Kering_prev = lag(Kering),
    LVMH_prev = lag(LVMH)
  ) %>%
  filter(!is.na(Richemont_prev)) %>%
  mutate(
    Richemont_return = (Richemont - Richemont_prev) / Richemont_prev * 100,
    Kering_return = (Kering - Kering_prev) / Kering_prev * 100,
    LVMH_return = (LVMH - LVMH_prev) / LVMH_prev * 100
  ) %>%
  group_by(month) %>%
  summarize(
    Richemont_monthly_return = last(Richemont_return),
    Kering_monthly_return = last(Kering_return),
    LVMH_monthly_return = last(LVMH_return)
  )

# Calculate correlation of monthly returns
return_correlations <- cor(monthly_returns[, c("Richemont_monthly_return", "Kering_monthly_return", "LVMH_monthly_return")], 
                          use = "pairwise.complete.obs")
print("Correlations between monthly returns:")
print(return_correlations)

# Calculate volatility (standard deviation of returns)
volatility <- monthly_returns %>%
  summarize(
    Richemont_volatility = sd(Richemont_monthly_return, na.rm = TRUE),
    Kering_volatility = sd(Kering_monthly_return, na.rm = TRUE),
    LVMH_volatility = sd(LVMH_monthly_return, na.rm = TRUE)
  )
print("Monthly return volatility (standard deviation):")
print(volatility)

# Save results
write.csv(correlations, "output/luxury_brands_price_correlations.csv")
write.csv(return_correlations, "output/luxury_brands_return_correlations.csv")
write.csv(volatility, "output/luxury_brands_volatility.csv")
write.csv(monthly_returns, "output/luxury_brands_monthly_returns.csv")

# Summary statistics - Calculate annual returns
annual_returns <- common_data %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(
    Richemont_start = first(Richemont),
    Richemont_end = last(Richemont),
    Kering_start = first(Kering),
    Kering_end = last(Kering),
    LVMH_start = first(LVMH),
    LVMH_end = last(LVMH)
  ) %>%
  mutate(
    Richemont_annual_return = (Richemont_end - Richemont_start) / Richemont_start * 100,
    Kering_annual_return = (Kering_end - Kering_start) / Kering_start * 100,
    LVMH_annual_return = (LVMH_end - LVMH_start) / LVMH_start * 100
  )

print("Annual returns by year:")
print(annual_returns)
write.csv(annual_returns, "output/luxury_brands_annual_returns.csv") 