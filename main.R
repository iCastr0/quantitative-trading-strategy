# ────────────────────────────────
# 0. PACKAGES AND PARALLELIZATION
# ────────────────────────────────
library(tidyquant)
library(tidyverse)
library(tidymodels)
library(themis)
library(future)
library(ranger)
library(yardstick)

plan(multisession, workers = parallel::detectCores() - 1)
set.seed(67)
gc()

# ────────────────────────────────
# 1. DATA INGESTION
# ────────────────────────────────
tickers <- tq_index("SP500")$symbol
tickers_sample <- sample(tickers, 50)

prices_raw <- tq_get(tickers_sample,
                     from = "2020-01-01",
                     to = Sys.Date(),
                     get = "stock.prices")

prices <- prices_raw %>%
  group_by(symbol) %>%
  filter(n() >= 500) %>%
  ungroup()

rm(prices_raw); gc()

# ────────────────────────────────
# 2. FEATURE ENGINEERING
# ────────────────────────────────
features <- prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    SMA5        = TTR::SMA(adjusted, 5),
    SMA10       = TTR::SMA(adjusted, 10),
    RSI14       = TTR::RSI(adjusted, 14),
    MACD_macd   = TTR::MACD(adjusted, 12, 26, 9)[,1],
    MACD_signal = TTR::MACD(adjusted, 12, 26, 9)[,2],
    MACD_diff   = MACD_macd - MACD_signal,
    Volatility10= TTR::runSD(adjusted, 10),
    Momentum5   = TTR::momentum(adjusted, 5),
    Return_5d   = lead(adjusted, 5) / adjusted - 1
  ) %>%
  ungroup() %>%
  drop_na(SMA5:Momentum5, Return_5d)

rm(prices); gc()

data_ml <- features %>%
  mutate(target = factor(ifelse(Return_5d > 0.02, 1, 0), levels = c(0, 1))) %>%
  select(symbol, date, target, SMA5, SMA10, RSI14, MACD_diff,
         Volatility10, Momentum5)

gc()

# ────────────────────────────────
# 3. TRAIN / TEST SPLIT
# ────────────────────────────────
cutoff_date <- as.Date(quantile(as.numeric(data_ml$date), 0.8), origin = "1970-01-01")
train <- data_ml %>% filter(date <= cutoff_date)
test  <- data_ml %>% filter(date >  cutoff_date)
rm(data_ml); gc()

# ────────────────────────────────
# 4. RECIPE
# ────────────────────────────────
recipe_rf <- recipe(target ~ SMA5 + SMA10 + RSI14 + MACD_diff + Volatility10 + Momentum5,
                    data = train) %>%
  step_zv(all_predictors()) %>%
  step_downsample(target)

# ────────────────────────────────
# 5. MODEL AND WORKFLOW
# ────────────────────────────────
rf_spec <- rand_forest(trees = 100, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

workflow_rf <- workflow() %>%
  add_recipe(recipe_rf) %>%
  add_model(rf_spec)

# ────────────────────────────────
# 6. TEMPORAL CROSS VALIDATION
# ────────────────────────────────
folds <- rolling_origin(
  data        = train,
  initial     = 300,
  assess      = 60,
  skip        = 90,
  cumulative  = FALSE
)

grid <- grid_random(
  mtry(range   = c(2, 5)),
  min_n(range  = c(2, 10)),
  size         = 4
)

# ────────────────────────────────
# 7. HYPERPARAMETER TUNING
# ────────────────────────────────
tuned_rf <- tune_grid(
  workflow_rf,
  resamples = folds,
  grid      = grid,
  metrics   = metric_set(roc_auc, pr_auc, accuracy),
  control   = control_grid(
    save_pred  = FALSE,
    verbose    = TRUE,
    allow_par  = FALSE
  )
)

best_params <- select_best(tuned_rf, metric = "roc_auc")
final_rf <- finalize_workflow(workflow_rf, best_params)
rf_fit <- fit(final_rf, data = train)
saveRDS(rf_fit, "rf_fit.rds")

# ────────────────────────────────
# 8. PREDICTIONS AND EXPORT FOR SHINY
# ────────────────────────────────

preds <- predict(rf_fit, test, type = "prob") %>%
  bind_cols(test) %>%
  mutate(pred_class = factor(ifelse(.pred_1 > 0.6, 1, 0), levels = c(0, 1)))

test_returns <- features %>%
  filter(date > cutoff_date) %>%
  select(symbol, date, Return_5d)

test_signals <- preds %>%
  left_join(test_returns, by = c("symbol", "date")) %>%
  mutate(
    signal = ifelse(.pred_1 > 0.7, 1, 0)
  ) %>%
  group_by(date) %>%
  mutate(
    n_signals = sum(signal),
    weight = ifelse(signal == 1 & n_signals > 0, 1 / n_signals, 0)
  ) %>%
  ungroup()

saveRDS(test_signals, "test_signals_base.rds")

df <- readRDS("test_signals_base.rds")
glimpse(df)