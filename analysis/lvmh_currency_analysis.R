# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Read the data
lvmh_data <- read.csv("data/lvmh_historical_data.csv")
currency_data <- read.csv("data/forex_data.csv")

# Display date information for debugging
print("Date columns:")
print(paste("lvmh_data first row date:", lvmh_data$Date[1]))
print(head(currency_data))

# Convert date columns to Date type
lvmh_data$Date <- as.Date(lvmh_data$Date)

# For currency data, create dates
start_date <- as.Date("2015-01-01")  # Starting point - adjust based on data
currency_data$Date <- start_date + 0:(nrow(currency_data)-1)

# Print date range information
print("Date ranges:")
print(paste("LVMH data from", min(lvmh_data$Date), "to", max(lvmh_data$Date)))
print(paste("Currency data from", min(currency_data$Date), "to", max(currency_data$Date)))

# Rename the currency columns for easier access
currency_data <- currency_data %>%
  rename(
    CHF_EUR = CHFEUR.X.Close,
    CHF_USD = CHFUSD.X.Close,
    CHF_CNY = CHFCNY.X.Close
  )

# Merge the datasets with matching dates
combined_data <- merge(lvmh_data, currency_data, by = "Date", all = FALSE)
print(paste("Number of rows after merging:", nrow(combined_data)))

# If we have no exact date matches, we need to adjust one of the datasets
if(nrow(combined_data) == 0) {
  print("No matching dates found. Trying an alternative approach...")
  
  # Let's adjust the currency data dates forward by a few days to try to match
  for(offset in 1:10) {
    currency_data$Date <- start_date + offset + 0:(nrow(currency_data)-1)
    combined_data <- merge(lvmh_data, currency_data, by = "Date", all = FALSE)
    if(nrow(combined_data) > 0) {
      print(paste("Found matches with offset:", offset))
      break
    }
  }
  
  # If we still have no matches, try the other direction
  if(nrow(combined_data) == 0) {
    for(offset in 1:10) {
      currency_data$Date <- start_date - offset + 0:(nrow(currency_data)-1)
      combined_data <- merge(lvmh_data, currency_data, by = "Date", all = FALSE)
      if(nrow(combined_data) > 0) {
        print(paste("Found matches with negative offset:", -offset))
        break
      }
    }
  }
}

# Final check
print(paste("Final number of rows:", nrow(combined_data)))

# If we have data to work with
if(nrow(combined_data) > 0) {
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
    geom_line(aes(y = normalized_price, color = "LVMH Stock Price"), linewidth = 1) +
    geom_line(aes(y = normalized_chf_eur, color = "CHF/EUR"), linewidth = 1) +
    geom_line(aes(y = normalized_chf_usd, color = "CHF/USD"), linewidth = 1) +
    geom_line(aes(y = normalized_chf_cny, color = "CHF/CNY"), linewidth = 1) +
    scale_color_manual(values = c(
      "LVMH Stock Price" = "#1f77b4",
      "CHF/EUR" = "#ff7f0e",
      "CHF/USD" = "#2ca02c",
      "CHF/CNY" = "#d62728"
    )) +
    labs(
      title = "LVMH Stock Price vs Currency Exchange Rates",
      subtitle = "Normalized Values for Comparison",
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
  ggsave("charts/lvmh_currency_comparison.png", p, width = 12, height = 6, dpi = 300)
  
  # Calculate correlations
  print("Correlations between LVMH stock price and currency rates:")
  # Only calculate if we have at least some complete cases
  if(sum(complete.cases(combined_data[, c("Close", "CHF_EUR", "CHF_USD", "CHF_CNY")])) > 1) {
    correlations <- cor(combined_data[, c("Close", "CHF_EUR", "CHF_USD", "CHF_CNY")], 
                         use = "pairwise.complete.obs")
    print(correlations)
    
    # Save correlations to a CSV file
    write.csv(correlations, "output/lvmh_currency_correlations.csv")
  } else {
    print("Not enough complete data pairs to calculate correlations")
  }
} else {
  print("No overlapping data found between the datasets - cannot continue analysis")
} 