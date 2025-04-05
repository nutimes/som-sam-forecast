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
    arima010011 = ARIMA(formula = .admissions ~ pdq(0, 1, 1) + PDQ(0, 1, 1)),
    arima010110 = ARIMA(formula = .admissions ~ pdq(1, 1, 0) + PDQ(1, 1, 0)),
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


## ---- Forecast future admissions cases into program --------------------------

pasto_forecast <- pasto_fit |>
  forecast(h = 6) |>
  filter(.model == "arima010011")


### ---------- Reverse box-cox transformation to original admissions scales ----

pasto_forecast_reverse <- pasto_forecast |>
  hilo(level = c(80, 95)) |>
  unpack_hilo("80%") |>
  unpack_hilo("95%") |>
  mutate(
    mean_inv = inv_box_cox(mean(.admissions), lambda_pastoral),
    across(ends_with(c("_lower", "_upper")),
      ~ inv_box_cox(
        .x,
        lambda = lambda_pastoral
      ),
      .names = "{.col}"
    )
  )


## ---- Visualize forecasts ----------------------------------------------------

pasto_forecast_reverse |>
  filter(.model == "arima010011") |>
  pivot_longer(
    cols = c(`80%_lower`, `80%_upper`, `95%_lower`, `95%_upper`),
    names_to = "interval",
    values_to = "value"
  ) |>
  separate(
    col = interval,
    into = c("level", "bound"),
    sep = "_"
  ) |>
  pivot_wider(
    names_from = bound,
    values_from = value
  ) |>
  ggplot() +
  geom_ribbon(
    aes(x = Monthly, ymin = lower, ymax = upper, fill = level),
    alpha = 0.5
  ) +
  geom_line(
    aes(x = Monthly, y = mean_inv),
    color = "blue", alpha = 0.6
  ) +
  geom_line(
    data = pasto_train_data,
    aes(x = Monthly, y = sam_admissions),
    color = "black"
  ) +
  scale_fill_manual(
    name = "Confidence Interval",
    values = c("80%" = "#1F77B4", "95%" = "#AEC7E8")
  ) +
  labs(
    title = "Future SAM admission cases by June 2025 in pastoral livelihood systems",
    y = "Number of cases",
    x = "Monthly"
  ) +
  theme_minimal()

################################ End of workflow ###############################
