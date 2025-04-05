################################################################################
#                TRAINING AND TEST - PASTORAL LIVELIHOOD SYSTEM                #
################################################################################


## ---- Split 80% training, 20% testing ----------------------------------------

pasto_split <- grouped_admissions |>
  mutate(
    .admissions = do.call(
      what = row_wise_box_cox,
      args = list(admissions = sam_admissions, lsystems = lsystems)
    )
  ) |> 
  filter(lsystems == "Pastoral") |> 
  initial_time_split(prop = 0.839)

### ------------------------------------------ Extract training and testing ----

pasto_train_data <- training(pasto_split)
pasto_test_data <- testing(pasto_split)

## ---- Fit a SARIMA model -----------------------------------------------------

### -------------------------------------------- Pastoral Livelihood system ----

pasto_fit <- pasto_train_data |> 
  model(
    arima010011 = ARIMA(formula = .admissions ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima010110 = ARIMA(formula = .admissions ~ pdq(1,1,0) + PDQ(1,1,0)), 
    snaive = SNAIVE(.admissions)
  )

### --------- Check candidate ARIMA models vs automatically selected models ----

pasto_fit |> 
  pivot_longer(
    cols = 2:4,
    names_to = "model_name",
    values_to = "orders"
  )

### ----------------------- Identify the best model-fit amongst the others -----

  glance(pasto_fit) |> 
  arrange(AICc) |> 
  select(.model:BIC)

### -------------------------- Diganose residuals (white noise?) using plot ----

pasto_fit |> 
  select(arima010011) |> 
  gg_tsresiduals(lag = 36)

### --- Diagnose residuals (white noise?) using a formal hypothesis testing ----

augment(pasto_fit) |> 
  filter(.model == "arima010011") |> 
  features(.innov, ljung_box, lag = 36, def = 1)

### ------------------------------- Forecast: h-steps = the test set period ----
pasto_forecast <- pasto_fit |> 
  forecast(h = nrow(pasto_test_data))

### ------------------------------------ Evaluate in-sample forecast errors ----

pasto_fit |> 
  select(arima010011) |> 
  accuracy()

### ----------------------------------- Evaluate out-sample forecast errors ----

pasto_forecast |> 
  filter(.model == "arima010011") |> 
  accuracy(pasto_test_data)

### ------------------- Forecast 
forecast(pasto_fit, h = 6) |> 
  filter(.model == "arima010011") |> 
  autoplot(pasto_train_data)
