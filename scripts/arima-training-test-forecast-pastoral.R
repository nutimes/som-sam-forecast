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

## --------------------- Apply seasonal differencing using training dataset ----

train_data_pasto |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  autoplot(.vars = .admissions) +
  labs(
    title = "Seasonal differenced time serie",
    subtitle = "It shows constant variance across the series"
  ) +
  theme(
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    plot.subtitle = element_text(colour = "#706E6D")
  )

### ---------------------------------------------------- ACF and PACF plots ----

train_data_pasto |>
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

train_data_pasto |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  features(.var = .admissions, ljung_box, lag = 10)


## ---- Fit a Seasonal ARIMA model ---------------------------------------------

fit_pasto <- train_data_pasto |>
  model(
    sets = ETS(
      formula = .admissions ~ error("A") + trend("Ad") + season("A")
    ),
    arima011011 = ARIMA(
      formula = .admissions ~ pdq(0, 1, 1) + PDQ(0, 1, 1)
    ),
    arima010110 = ARIMA(
      formula = .admissions ~ pdq(1, 1, 0) + PDQ(1, 1, 0)
    ), 
    auto = ARIMA(
      formula = .admissions, stepwise = FALSE, approximation = FALSE
  )
)

### ----------------------- Identify the best model-fit amongst the others -----

glance(fit_pasto) |>
  arrange(AICc) |>
  select(.model:BIC)

# .model arima010011 had the lowest AICc - best model.

### -------------------------- Diganose residuals (white noise?) using plot ----

fit_pasto |>
  select(arima011011) |>
  gg_tsresiduals(lag = 36)


### --- Diagnose residuals (white noise?) using a formal hypothesis testing ----

augment(fit_pasto) |>
  filter(.model == "arima011011") |>
  features(.innov, ljung_box, lag = 36, def = 1)

### ------------------------------- Forecast: h-steps = the test set period ----

forecast_pasto <- fit_pasto |>
  forecast(h = nrow(test_data_pasto))


### ------------------------------------ Evaluate in-sample forecast errors ----

fit_pasto |>
  select(arima011011) |>
  accuracy()


### ----------------------------------- Evaluate out-sample forecast errors ----

forecast_pasto |>
  filter(.model == "arima011011") |>
  accuracy(test_data_pasto)



## ---- Refit model on full data -----------------------------------------------

fit_pasto_full <- grouped_admissions |> 
  subset(lsystems == "Pastoral") |> 
  model(
    arima011011 = ARIMA(.admissions ~ pdq(0,1,1) + PDQ(0,1,1))
  )


### --- Forecast future admissions cases into program: January to December 2025 

forecast_pasto <- forecast(
  object = fit_pasto_full,
  h = 12
)


### ---------- Reverse box-cox transformation to original admissions scales ----

forecast_pasto <- forecast_pasto |>
  hilo(level = c(80, 95)) |>
  unpack_hilo("80%") |>
  unpack_hilo("95%") |>
  mutate(
    mean_inv = inv_box_cox(mean(.admissions), lambda_pasto),
    across(ends_with(c("_lower", "_upper")),
      ~ inv_box_cox(
        .x,
        lambda = lambda_pasto
      ),
      .names = "{.col}"
    )
  )


## ---- Visualize forecasts ----------------------------------------------------

### ------------------------------------------------------------- Tidy data ----

forecast_pasto <- forecast_pasto |>
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

forecast_pasto |>
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
    data = grouped_admissions |> 
      subset(lsystems == "Pastoral"),
    aes(x = Monthly, y = sam_admissions),
    color = "black"
  ) +
  scale_fill_manual(
    name = "Confidence Interval",
    values = c("80%" = "#1F77B4", "95%" = "#AEC7E8")
  ) +
  labs(
    title = "Future SAM admission cases by June 2025 in pastoral livelihood systems",
    subtitle = "Time horizon: from January to December 2025",
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
