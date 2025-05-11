# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(quantmod) # For downloading financial data

# Set the date range for data retrieval
start_date <- as.Date("2015-01-01")
end_date <- Sys.Date()  # Today's date

# Download the latest currency exchange rates data using quantmod
# Swiss Franc to Euro
getSymbols("CHFEUR=X", src = "yahoo", from = start_date, to = end_date)
# Swiss Franc to USD
getSymbols("CHFUSD=X", src = "yahoo", from = start_date, to = end_date)
# Swiss Franc to CNY
getSymbols("CHFCNY=X", src = "yahoo", from = start_date, to = end_date)

# Read the Richemont stock data
richemont_data <- read.csv("data/richemont_historical_data.csv")

# Convert date columns to Date type
richemont_data$Date <- as.Date(richemont_data$Date)

# Create a dataframe with the updated currency data
chf_eur <- data.frame(
  Date = index(`CHFEUR=X`),
  CHF_EUR = as.numeric(`CHFEUR=X`[, 4])  # Use closing price
)

chf_usd <- data.frame(
  Date = index(`CHFUSD=X`),
  CHF_USD = as.numeric(`CHFUSD=X`[, 4])  # Use closing price
)

chf_cny <- data.frame(
  Date = index(`CHFCNY=X`),
  CHF_CNY = as.numeric(`CHFCNY=X`[, 4])  # Use closing price
)

# Merge all currency dataframes
currency_data <- chf_eur %>%
  left_join(chf_usd, by = "Date") %>%
  left_join(chf_cny, by = "Date")

# Print date range information
print("Date ranges:")
print(paste("Richemont data from", min(richemont_data$Date), "to", max(richemont_data$Date)))
print(paste("Currency data from", min(currency_data$Date), "to", max(currency_data$Date)))
print(paste("Number of rows in currency data:", nrow(currency_data)))

# Filter Richemont data to only include real historical dates (excluding future projections)
current_date <- Sys.Date()
richemont_historical <- richemont_data %>%
  filter(Date <= current_date)

print(paste("Historical Richemont data (excluding future projections) from", 
            min(richemont_historical$Date), "to", max(richemont_historical$Date)))

# Merge the datasets with matching dates
combined_data <- merge(richemont_historical, currency_data, by = "Date", all = FALSE)
print(paste("Number of rows after merging:", nrow(combined_data)))

# Create a function to normalize data
normalize <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if(is.infinite(min_x) || is.infinite(max_x) || max_x == min_x) {
    return(rep(0, length(x)))
  }
  return((x - min_x) / (max_x - min_x))
}

# Normalize the data for comparison
combined_data <- combined_data %>%
  mutate(
    normalized_price = normalize(Close),
    normalized_chf_eur = normalize(CHF_EUR),
    normalized_chf_usd = normalize(CHF_USD),
    normalized_chf_cny = normalize(CHF_CNY)
  )

# Create the combined plot
p <- ggplot(combined_data, aes(x = Date)) +
  geom_line(aes(y = normalized_price, color = "Richemont Stock Price"), linewidth = 1) +
  geom_line(aes(y = normalized_chf_eur, color = "CHF/EUR"), linewidth = 1) +
  geom_line(aes(y = normalized_chf_usd, color = "CHF/USD"), linewidth = 1) +
  geom_line(aes(y = normalized_chf_cny, color = "CHF/CNY"), linewidth = 1) +
  scale_color_manual(values = c(
    "Richemont Stock Price" = "#1f77b4",
    "CHF/EUR" = "#ff7f0e",
    "CHF/USD" = "#2ca02c",
    "CHF/CNY" = "#d62728"
  )) +
  labs(
    title = "Richemont Stock Price vs Currency Exchange Rates",
    subtitle = "Normalized Values for Comparison (Up to Current Date)",
    x = "Date",
    y = "Normalized Value",
    color = "Metric"
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

# Save the plot
ggsave("charts/richemont_currency_comparison_updated.png", p, width = 12, height = 6, dpi = 300)

# Calculate correlations
print("Correlations between Richemont stock price and currency rates:")
correlations <- cor(combined_data[, c("Close", "CHF_EUR", "CHF_USD", "CHF_CNY")], 
                     use = "pairwise.complete.obs")
print(correlations)

# Save correlations to a CSV file
write.csv(correlations, "output/richemont_currency_correlations_updated.csv")

# Also create correlation plot for year-by-year analysis
combined_data <- combined_data %>%
  mutate(Year = year(Date))

yearly_correlations <- combined_data %>%
  group_by(Year) %>%
  summarize(
    Correlation_EUR = cor(Close, CHF_EUR, use = "pairwise.complete.obs"),
    Correlation_USD = cor(Close, CHF_USD, use = "pairwise.complete.obs"),
    Correlation_CNY = cor(Close, CHF_CNY, use = "pairwise.complete.obs")
  )

print("Year-by-year correlations:")
print(yearly_correlations)

# Save the yearly correlations
write.csv(yearly_correlations, "output/richemont_currency_yearly_correlations.csv")

# Create a long format for yearly correlations plot
yearly_correlations_long <- yearly_correlations %>%
  pivot_longer(cols = starts_with("Correlation_"),
               names_to = "Currency",
               values_to = "Correlation") %>%
  mutate(Currency = gsub("Correlation_", "", Currency))

# Create the yearly correlation plot
p2 <- ggplot(yearly_correlations_long, aes(x = Year, y = Correlation, color = Currency, group = Currency)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "EUR" = "#ff7f0e",
    "USD" = "#2ca02c",
    "CNY" = "#d62728"
  )) +
  labs(
    title = "Year-by-Year Correlation: Richemont Stock vs Currency Exchange Rates",
    subtitle = "How the relationship has changed over time",
    x = "Year",
    y = "Correlation Coefficient",
    color = "Currency"
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

# Save the yearly correlation plot
ggsave("charts/richemont_currency_yearly_correlation.png", p2, width = 12, height = 6, dpi = 300) 