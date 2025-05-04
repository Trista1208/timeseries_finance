library(tidyquant)

# Set your preferred date range
from_date <- "2020-01-01"
to_date <- "2024-12-31"

# Get FX rates
getSymbols(c("EURUSD=X", "USDCHF=X", "USDCNY=X"), src = "yahoo", from = from_date, to = to_date)

# Rename for readability
eurusd <- `EURUSD=X`
usdchf <- `USDCHF=X`
usdcny <- `USDCNY=X`

