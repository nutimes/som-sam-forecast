################################################################################
#       TRAIN, TEST MODEL AND FORECAST - AGROPASTORAL LIVELIHOOD SYSTEM        #
################################################################################


## ---- Check assumptions for stationarity and select candidate models ---------

### ----------------------------------- ACF plot of the original admissions ----


grouped_admissions |>
  filter(lsystems == "Agropastoral") |>
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

agropasto_train_data |>
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

agropasto_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
  labs(title = "Agropastoral livelihood system")

# Candidate models selected based on ACF (MA) and PACF (AR):
# ARIMA(0,1,0)(0,1,0)[12]

### ------------------------------ Test if the time series is a white noise ----

agropasto_train_data |>
  mutate(
    .admissions = do.call(
      what = difference,
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1)
  ) |>
  features(.var = .admissions, ljung_box, lag = 10)


## ---- Fit a Seasonal ARIMA model ---------------------------------------------

agropasto_fit <- agropasto_train_data |>
  model(
    sets = ETS(
      formula = .admissions ~ error("A") + trend("Ad") + season("A")
    ),
    arima010010 = ARIMA(
      formula = .admissions ~ pdq(0, 1, 0) + PDQ(0, 1, 0)
    ),
    auto = ARIMA(
      formula = .admissions, stepwise = FALSE, approximation = FALSE
    )
  )


### ----------------------- Identify the best model-fit amongst the others -----

glance(agropasto_fit) |>
  arrange(AICc) |>
  select(.model:BIC)

# .model arima010011 had the lowest AICc - best model.

### -------------------------- Diganose residuals (white noise?) using plot ----

agropasto_fit |>
  select(auto) |>
  gg_tsresiduals(lag = 36)


### --- Diagnose residuals (white noise?) using a formal hypothesis testing ----

augment(agropasto_fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag = 36, def = 1)


### ------------------------------- Forecast: h-steps = the test set period ----

agropasto_forecast <- agropasto_fit |>
  forecast(h = nrow(agropasto_test_data))


### ------------------------------------ Evaluate in-sample forecast errors ----

agropasto_fit |>
  select(auto) |>
  accuracy()


### ----------------------------------- Evaluate out-sample forecast errors ----

agropasto_forecast |>
  filter(.model == "auto") |>
  accuracy(agropasto_test_data)


## ---- Forecast future admissions cases into program --------------------------

agropasto_forecast <- agropasto_fit |>
  forecast(h = 6) |>
  filter(.model == "auto")


### ---------- Reverse box-cox transformation to original admissions scales ----

agropasto_forecast <- agropasto_forecast |>
  hilo(level = c(80, 95)) |>
  unpack_hilo("80%") |>
  unpack_hilo("95%") |>
  mutate(
    mean_inv = inv_box_cox(.mean, lambda_agropastoral),
    across(ends_with(c("_lower", "_upper")),
      ~ inv_box_cox(
        .x,
        lambda = lambda_agropastoral
      ),
      .names = "{.col}"
    )
  )


## ---- Visualize forecasts ----------------------------------------------------

### ------------------------------------------------------------- Tidy data ----

agropasto_forecast <- agropasto_forecast |>
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

agropasto_forecast |>
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
    data = agropasto_train_data,
    aes(x = Monthly, y = sam_admissions),
    color = "black"
  ) +
  scale_fill_manual(
    name = "Confidence Interval",
    values = c("80%" = "#1F77B4", "95%" = "#AEC7E8")
  ) +
  labs(
    title = "Forecasted SAM admissions into the program in the agropastoral livelihood systems",
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
