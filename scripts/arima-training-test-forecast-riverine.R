################################################################################
#       TRAIN, TEST MODEL AND FORECAST - RIVERINE LIVELIHOOD SYSTEM        #
################################################################################


## ---- Check assumptions for stationarity and select candidate models ---------

### ----------------------------------- ACF plot of the original admissions ----


grouped_admissions |>
  filter(lsystems == "Riverine") |>
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

train_data_riverine |>
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

train_data_riverine |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
  labs(title = "Riverine livelihood system")

# Candidate models selected based on ACF (MA) and PACF (AR):
# ARIMA(0,1,1)(0,1,2)[12] or ARIMA(1,1,0)(0,1,0)


### ------------------------------ Test if the time series is a white noise ----

train_data_riverine |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  features(.var = .admissions, ljung_box, lag = 10)


## ---- Fit a Seasonal ARIMA model ---------------------------------------------

fit_riverine <- train_data_riverine |>
  model(
    sets = ETS(
      formula = .admissions ~ error("A") + trend("Ad") + season("A")
    ),
    arima011010 = ARIMA(
      formula = .admissions ~ pdq(0, 1, 1) + PDQ(0, 1, 0)
    ),
    arima110010 = ARIMA(
      formula = .admissions ~ pdq(1, 1, 0) + PDQ(0, 1, 0)
    ),
    auto = ARIMA(
      formula = .admissions, stepwise = FALSE, approximation = FALSE
    )
  )

### ----------------------- Identify the best model-fit amongst the others -----

glance(fit_riverine) |>
  arrange(AICc) |>
  select(.model:BIC)

# .model arima110010 had the lowest AICc - best model.

### -------------------------- Diganose residuals (white noise?) using plot ----

fit_riverine |>
  select(arima110010) |>
  gg_tsresiduals(lag = 36)


### --- Diagnose residuals (white noise?) using a formal hypothesis testing ----

augment(fit_riverine) |>
  filter(.model == "arima110010") |>
  features(.innov, ljung_box, lag = 36, def = 1)

### ------------------------------- Forecast: h-steps = the test set period ----

forecast_riverine <- fit_riverine |>
  forecast(h = nrow(test_data_riverine))


### ------------------------------------ Evaluate in-sample forecast errors ----

fit_riverine |>
  select(arima110010) |>
  accuracy()


### ----------------------------------- Evaluate out-sample forecast errors ----

forecast_riverine |>
  filter(.model == "arima110010") |>
  accuracy(test_data_riverine)


## ---- Forecast future admissions cases into program --------------------------

forecast_riverine <- fit_riverine |>
  forecast(h = 6) |>
  filter(.model == "arima110010")


### ---------- Reverse box-cox transformation to original admissions scales ----

forecast_riverine <- forecast_riverine |>
  hilo(level = c(80, 95)) |>
  unpack_hilo("80%") |>
  unpack_hilo("95%") |>
  mutate(
    mean_inv = inv_box_cox(.mean, lambda_riverine),
    across(ends_with(c("_lower", "_upper")),
      ~ inv_box_cox(
        .x,
        lambda = lambda_riverine
      ),
      .names = "{.col}"
    )
  )


## ---- Visualize forecasts ----------------------------------------------------

### ------------------------------------------------------------- Tidy data ----

forecast_riverine <- forecast_riverine |>
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

forecast_riverine |>
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
    data = train_data_riverine,
    aes(x = Monthly, y = sam_admissions),
    color = "black"
  ) +
  scale_fill_manual(
    name = "Confidence Interval",
    values = c("80%" = "#1F77B4", "95%" = "#AEC7E8")
  ) +
  labs(
    title = "Forecasted SAM admissions into the program in the riverine livelihood systems",
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
