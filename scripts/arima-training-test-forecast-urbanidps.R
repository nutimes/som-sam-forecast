################################################################################
#         TRAIN, TEST MODEL AND FORECAST - URBAN/IDP LIVELIHOOD SYSTEM         #
################################################################################


## ---- Check assumptions for stationarity and select candidate models ---------

### ----------------------------------- ACF plot of the original admissions ----


grouped_admissions |>
  filter(lsystems == "Urban/IDPs") |>
  ACF(
    y = sam_admissions,
    type = "correlation",
    lag_max = 36
  ) |>
  autoplot() +
  labs(
    title = "First order autocorrelation",
    subtitle = "Time series show non-stationarity: it has a trend and seasonal pattern",
    y = "Autocorrelation coefficient"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )


### --------------------- Apply seasonal differencing using training dataset ---

urbanidps_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  autoplot(.vars = .admissions) +
  labs(
    title = "Seasonal differenced time serie",
    subtitle = "It shows a constant variance across the series"
  ) +
  theme(
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    plot.subtitle = element_text(colour = "#706E6D")
  )


### ---------------------------------------------------- ACF and PACF plots ----

urbanidps_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(2)
  ) |>
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
  labs(title = "Riverine livelihood system")

# Candidate models selected based on ACF (MA) and PACF (AR):
# ARIMA(0,1,1)(0,1,2)[12] or ARIMA(1,1,0)(0,1,0)


### ------------------------------ Test if the time series is a white noise ----

urbanidps_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(2)
  ) |>
  features(.var = .admissions, ljung_box, lag = 10)


## ---- Fit a Seasonal ARIMA model ---------------------------------------------

urbanidps_fit <- urbanidps_train_data |>
  model(
    sets = ETS(
      formula = .admissions ~ error("M") + trend("Ad") + season("M")
    ),
    arima012010 = ARIMA(
      formula = .admissions ~ pdq(0, 1, 2) + PDQ(0, 1, 0)
    ),
    arima110010 = ARIMA(
      formula = .admissions ~ pdq(1, 1, 0) + PDQ(0, 1, 0)
    ),
    arima013010 = ARIMA(
      formula = .admissions ~ pdq(0, 1, 3) + PDQ(0, 1, 0)
    ),
    auto = ARIMA(
      formula = .admissions, stepwise = FALSE, approximation = FALSE
    )
  )


### ----------------------- Identify the best model-fit amongst the others -----

glance(urbanidps_fit) |>
  arrange(AICc) |>
  select(.model:BIC)

# .model auto had the lowest AICc - best model.

### -------------------------- Diganose residuals (white noise?) using plot ----

urbanidps_fit |>
  select(auto) |>
  gg_tsresiduals(lag = 36)


### --- Diagnose residuals (white noise?) using a formal hypothesis testing ----

augment(urbanidps_fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag = 36, def = 1)


### ------------------------------- Forecast: h-steps = the test set period ----

urbanidps_forecast <- urbanidps_fit |>
  forecast(h = nrow(urbanidps_test_data))


### ------------------------------------ Evaluate in-sample forecast errors ----

urbanidps_fit |>
  select(auto) |>
  accuracy()


### ----------------------------------- Evaluate out-sample forecast errors ----

urbanidps_forecast |>
  filter(.model == "auto") |>
  accuracy(urbanidps_test_data)


## ---- Forecast future admissions cases into program --------------------------

urbanidps_forecast <- urbanidps_fit |>
  forecast(h = 6) |>
  filter(.model == "auto")


### ---------- Reverse box-cox transformation to original admissions scales ----

urbanidps_forecast <- urbanidps_forecast |>
  hilo(level = c(80, 95)) |>
  unpack_hilo("80%") |>
  unpack_hilo("95%") |>
  mutate(
    mean_inv = inv_box_cox(.mean, lambda_urban_idps),
    across(ends_with(c("_lower", "_upper")),
      ~ inv_box_cox(
        .x,
        lambda = lambda_urban_idps
      ),
      .names = "{.col}"
    )
  )


## ---- Visualize forecasts ----------------------------------------------------

### ------------------------------------------------------------- Tidy data ----

urbanidps_forecast <- urbanidps_forecast |>
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
  )


### ------------------------------------------------------ Plot forecasts ----

urbanidps_forecast |>
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
    data = urbanidps_train_data,
    aes(x = Monthly, y = sam_admissions),
    color = "black"
  ) +
  scale_fill_manual(
    name = "Confidence Interval",
    values = c("80%" = "#1F77B4", "95%" = "#AEC7E8")
  ) +
  labs(
    title = "Forecasted SAM admissions into the program in the Urban/IDPs livelihood systems",
    subtitle = "Time horizon: from January to June 2025",
    y = "Number of cases",
    x = "Monthly"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9, colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

################################ End of workflow ###############################



