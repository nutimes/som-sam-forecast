
## ---- Split 80% training, 20% testing ----------------------------------------
pasto_split <- diff_ts |> 
  filter(lsystems == "Pastoral") |> 
  initial_time_split(
  prop = 0.8
)

### ------------------------------------------ Extract training and testing ----
pasto_train_data <- training(pasto_split)
pasto_test_data <- testing(pasto_split)

## ---- Fit a SARIMA model -----------------------------------------------------

### -------------------------------------------- Pastoral Livelihood system ----
pasto_fit <- pasto_train_data |> 
  model(
    arima010210 = ARIMA(formula = .admissions ~ pdq(0,1,0) + PDQ(1,1,0)),
    auto = ARIMA(
      formula = .admissions,
      stepwise = FALSE,
      approximation = FALSE
    )
  )
### --------- Check candidate ARIMA models vs automatically selected models ----
pasto_fit |> 
  pivot_longer(
    cols = 2:3,
    names_to = "model_name",
    values_to = "orders"
  )

### ----------------------- Identify the best model-fit amongst the others -----
  glance(pasto_fit) |> 
  arrange(AICc) |> 
  select(.model:BIC)

### -------------------------- Diganose residuals (white noise?) using plot ----
pasto_fit |> 
  select(auto) |> 
  gg_tsresiduals(lag = 36)

### --- Diagnose residuals (white noise?) using a formal hypothesis testing ----
augment(pasto_fit) |> 
  filter(.model == "auto") |> 
  features(.innov, ljung_box, lag = 36, def = 1)

### ------------------------------- Forecast: h-steps = the test set period ----
pasto_forecast <- pasto_fit |> 
  forecast(h = nrow(pasto_test_data))

### ------------------------------------ Evaluate in-sample forecast errors ----
pasto_fit |> 
  select(auto) |> 
  accuracy()

### ----------------------------------- Evaluate out-sample forecast errors ----
pasto_forecast |> 
  filter(.model == "auto") |> 
  accuracy(pasto_test_data)
