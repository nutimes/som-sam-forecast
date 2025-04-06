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
