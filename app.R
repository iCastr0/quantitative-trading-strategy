# app.R

library(shiny)
library(tidyverse)
library(DT)

# ────────────────────────────────
# STRATEGY EVALUATION FUNCTION
# ────────────────────────────────
eval_strategy <- function(data, threshold = 0.7, tp = 0.05, sl = -0.05) {
  if (nrow(data) == 0) return(NULL)

  # Step 1: Signals and weights
  data <- data %>%
    mutate(
      signal = ifelse(.pred_1 > threshold, 1, 0)
    ) %>%
    group_by(date) %>%
    mutate(
      n_signals = sum(signal),
      weight = ifelse(signal == 1 & n_signals > 0, 1 / n_signals, 0)
    ) %>%
    ungroup() %>%
    mutate(
      strat_ret_raw = weight * Return_5d,
      strat_ret = case_when(
        strat_ret_raw >= tp ~ tp,
        strat_ret_raw <= sl ~ sl,
        TRUE ~ strat_ret_raw
      )
    )

  # Step 2: Daily returns
  daily_returns <- data %>%
    group_by(date) %>%
    summarise(
      strategy_return = sum(strat_ret, na.rm = TRUE),
      bh_return       = mean(Return_5d, na.rm = TRUE)
    ) %>%
    mutate(
      strategy_cum = cumprod(1 + coalesce(strategy_return, 0)),
      bh_cum       = cumprod(1 + coalesce(bh_return, 0))
    )

  # Step 3: Risk metrics
  n_periods <- nrow(daily_returns)
  n_annual <- 252 / 5  # 5 days = 1 period

  cagr <- function(x) (last(x) / first(x))^(n_annual / n_periods) - 1
  vol  <- function(x) sd(x, na.rm = TRUE) * sqrt(n_annual)
  sharpe <- function(x) mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(n_annual)
  max_dd <- function(x) min((x - cummax(x)) / cummax(x), na.rm = TRUE)

  metrics <- tibble(
    Strategy     = c("Model", "Buy & Hold"),
    CAGR         = c(cagr(daily_returns$strategy_cum), cagr(daily_returns$bh_cum)),
    Volatility   = c(vol(daily_returns$strategy_return), vol(daily_returns$bh_return)),
    Sharpe       = c(sharpe(daily_returns$strategy_return), sharpe(daily_returns$bh_return)),
    Max_Drawdown = c(max_dd(daily_returns$strategy_cum), max_dd(daily_returns$bh_cum))
  ) %>%
    mutate(
      Calmar = CAGR / abs(Max_Drawdown)
    ) %>%
    mutate(across(where(is.numeric), round, 4))

  list(df = daily_returns, metrics = metrics)
}

# ────────────────────────────────
# USER INTERFACE
# ────────────────────────────────
ui <- fluidPage(
  titlePanel("Interactive Backtest with Stop Loss and Take Profit"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold", "Probability Threshold", min = 0.5, max = 0.9, value = 0.7, step = 0.01),
      sliderInput("tp", "Take Profit (%)", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      sliderInput("sl", "Stop Loss (%)", min = -0.2, max = -0.01, value = -0.05, step = 0.01)
    ),
    mainPanel(
      plotOutput("equity_plot"),
      dataTableOutput("metrics_table")
    )
  )
)

# ────────────────────────────────
# SERVER
# ────────────────────────────────
server <- function(input, output, session) {

  data_input <- reactive({
    req(file.exists("test_signals_base.rds"))
    df <- readRDS("test_signals_base.rds")
    validate(
      need(all(c("date", "Return_5d", ".pred_1") %in% colnames(df)),
           "Missing required columns in test_signals_base.rds")
    )
    df
  })

  results <- reactive({
    eval_strategy(
      data = data_input(),
      threshold = input$threshold,
      tp = input$tp,
      sl = input$sl
    )
  })

  output$equity_plot <- renderPlot({
    res <- results()
    ggplot(res$df, aes(x = date)) +
      geom_line(aes(y = strategy_cum, color = "Model (SL/TP)")) +
      geom_line(aes(y = bh_cum, color = "Buy & Hold")) +
      labs(
        title = "Cumulative Growth",
        y = "Cumulative Capital",
        x = "Date"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Model (SL/TP)" = "blue", "Buy & Hold" = "gray"))
  })

  output$metrics_table <- renderDataTable({
    res <- results()
    datatable(res$metrics, options = list(dom = 't'))
  })
}

# ────────────────────────────────
# RUN APP
# ────────────────────────────────
shinyApp(ui, server)