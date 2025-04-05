################################################################################
#          TRAIN, TEST MODEL AND FORECAST - PASTORAL LIVELIHOOD SYSTEM         #
################################################################################


## ---- Check assumptions for stationarity and select candidate models ---------

### ----------------------------------- ACF plot of the original admissions ----

grouped_admissions |>
  filter(lsystems == "Pastoral") |>
  ACF(
    y = sam_admissions,
    type = "correlation",
    lag_max = 36
  ) |>
  autoplot() +
  labs(
    title = "First order autocorrelation by livelihood system",
    subtitle = "Time series show non-stationarity: it has a trend and seasonal pattern",
    y = "Autocorrelation coeficient"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

## ------------------------ Apply seasonal differencing using training data ----

pasto_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  autoplot(.vars = .admissions) +
  labs(
    title = "Seasonal differenced time series",
    subtitle = "It shows constant variance across the series"
  ) +
  theme(
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    plot.subtitle = element_text(colour = "#706E6D")
  )

### ---------------------------------------------------- ACF and PACF plots ----

pasto_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
  labs(title = "Pastoral livelihood system")

# Candidate models selected based on ACF (MA) and PACF (AR):
# ARIMA(0,1,1)(0,1,1)[12] and ARIMA(1,1,0)(1,1,0)[12]

### ------------------------------ Test if the time series is a white noise ----

pasto_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  features(.var = .admissions, ljung_box, lag = 10)


## ---- Fit a Seasonal ARIMA model ---------------------------------------------

### -------------------------------------------- Pastoral Livelihood system ----

pasto_fit <- pasto_train_data |>
  model(
    sets = ETS(
      formula = .admissions ~ error("A") + trend("A") + season("A")
    ),
    arima010011 = ARIMA(
      formula = .admissions ~ pdq(0, 1, 1) + PDQ(0, 1, 1)
    ),
    arima010110 = ARIMA(
      formula = .admissions ~ pdq(1, 1, 0) + PDQ(1, 1, 0)
    )
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

pasto_forecast <- pasto_forecast |>
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

pasto_forecast |>
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
