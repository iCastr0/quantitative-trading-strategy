# ────────────────────────────────
# PREDICT_TODAY.R - Daily Signals
# ────────────────────────────────
library(tidyquant)
library(tidyverse)
library(tidymodels)
library(ranger)
library(glue)

set.seed(67)

# Load trained model
rf_fit <- readRDS("rf_fit.rds")

# Use fixed tickers or all S&P 500
tickers <- tq_index("SP500")$symbol

# Get recent prices
prices_raw <- tq_get(tickers,
                     from = Sys.Date() - 60,
                     to   = Sys.Date(),
                     get  = "stock.prices")

# Filter symbols with sufficient data
prices <- prices_raw %>%
  group_by(symbol) %>%
  filter(n() >= 30) %>%
  ungroup()

# Feature Engineering
features_today <- prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    SMA5         = TTR::SMA(adjusted, 5),
    SMA10        = TTR::SMA(adjusted, 10),
    RSI14        = TTR::RSI(adjusted, 14),
    MACD_macd    = TTR::MACD(adjusted, 12, 26, 9)[,1],
    MACD_signal  = TTR::MACD(adjusted, 12, 26, 9)[,2],
    MACD_diff    = MACD_macd - MACD_signal,
    Volatility10 = TTR::runSD(adjusted, 10),
    Momentum5    = TTR::momentum(adjusted, 5)
  ) %>%
  ungroup() %>%
  drop_na(SMA5:Momentum5)

# Use only the most recent date
latest_date <- max(features_today$date)

data_latest <- features_today %>%
  filter(date == latest_date) %>%
  select(symbol, date, SMA5, SMA10, RSI14, MACD_diff, Volatility10, Momentum5)

# Predict probabilities
preds_today <- predict(rf_fit, data_latest, type = "prob") %>%
  bind_cols(data_latest)

# Calculate signals
signals_today <- preds_today %>%
  mutate(signal = ifelse(.pred_1 > 0.7, 1, 0)) %>%
  filter(signal == 1) %>%
  arrange(desc(.pred_1)) %>%
  mutate(weight = 1 / n()) %>%
  select(symbol, date, .pred_1, signal, weight)

# Save signals
saveRDS(signals_today, "signals_today.rds")

# Display in console
glue("Generated {nrow(signals_today)} signals for {latest_date}") %>% print()
print(signals_today, n = Inf)
