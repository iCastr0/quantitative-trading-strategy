# Quantitative Trading Strategy

A machine learning-based quantitative trading strategy using Random Forest to predict stock price movements in the S&P 500.

## Overview

This project implements a quantitative trading strategy that:
- Uses technical indicators to predict 5-day stock returns
- Applies machine learning (Random Forest) for signal generation
- Includes an interactive Shiny app for backtesting
- Provides daily trading signals

## Files

### Core Scripts
- **`main.R`** - Main training script that builds the machine learning model
- **`app.R`** - Interactive Shiny application for strategy backtesting
- **`predict_today.R`** - Generates daily trading signals

### Data Files
- **`rf_fit.rds`** - Trained Random Forest model
- **`test_signals_base.rds`** - Historical signals for backtesting
- **`signals_today.rds`** - Current day's trading signals

## Features

### Technical Indicators
- Simple Moving Averages (SMA5, SMA10)
- Relative Strength Index (RSI14)
- MACD (Moving Average Convergence Divergence)
- Volatility (10-day rolling standard deviation)
- Momentum (5-day price momentum)

### Machine Learning
- Random Forest classifier
- Temporal cross-validation
- Hyperparameter tuning
- Binary classification (positive vs negative returns)

### Strategy Components
- Probability threshold for signal generation
- Equal weight allocation among signals
- Stop loss and take profit implementation
- Performance metrics (CAGR, Sharpe ratio, Max drawdown)

## Usage

### 1. Train the Model
```r
source("main.R")
```

### 2. Generate Daily Signals
```r
source("predict_today.R")
```

### 3. Run Interactive Backtest
```r
shiny::runApp("app.R")
```

## Requirements

### R Packages
```r
install.packages(c(
  "tidyquant",
  "tidyverse", 
  "tidymodels",
  "themis",
  "future",
  "ranger",
  "yardstick",
  "shiny",
  "DT",
  "glue"
))
```

## Strategy Logic

1. **Data Collection**: Downloads S&P 500 stock prices from 2020 onwards
2. **Feature Engineering**: Calculates technical indicators for each stock
3. **Target Creation**: Labels stocks as positive (1) if 5-day return > 2%
4. **Model Training**: Uses Random Forest with temporal cross-validation
5. **Signal Generation**: Predicts probability of positive returns
6. **Portfolio Construction**: Equal weights among high-probability signals

## Performance Metrics

The strategy is evaluated using:
- **CAGR**: Compound Annual Growth Rate
- **Volatility**: Annualized standard deviation
- **Sharpe Ratio**: Risk-adjusted returns
- **Maximum Drawdown**: Largest peak-to-trough decline
- **Calmar Ratio**: CAGR / Maximum Drawdown

## Interactive Features

The Shiny app allows users to:
- Adjust probability thresholds
- Set stop loss and take profit levels
- View real-time performance metrics
- Compare strategy vs buy-and-hold

## Notes

- The model is trained on historical data and should be retrained periodically
- Past performance does not guarantee future results
- Always perform proper risk management
- Consider transaction costs in real implementation

## License

This project is for educational purposes. Use at your own risk. 