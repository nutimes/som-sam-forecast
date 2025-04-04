################################################################################
#                     ENFORCING TIME SERIES STATIONARITY                       #
################################################################################

## ---- ACF plot of the original admissions ------------------------------------
acf_plot <- grouped_admissions |> 
  ACF(
    y = sam_admissions, 
    type = "correlation", 
    lag_max = 36
  ) |> 
  autoplot() +
  facet_wrap(vars(lsystems)) +
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

## ---- Apply seasonal differencing --------------------------------------------
diff_ts <- grouped_admissions |> 
  mutate(
    .admissions = do.call(
      what = row_wise_box_cox,
      args = list(admissions = sam_admissions, lsystems = lsystems)
    ), 
    .admissions = do.call(
      what = difference, 
      args = list(x = .admissions, lag = 12, differences = 1)
    ) |> difference(1) 
  )

### ------------------------------------- Visualise differenced time series ----
diff_ts |> 
  autoplot(.vars = .admissions) +
  labs(
    title = "Seasonal differenced time series"
  ) +
    theme(
      plot.caption = element_text(colour = "#706E6D"),
      axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    )

### -------------------------------------------------------------- ACF plot ----
diff_ts |> 
  ACF(y = .admissions) |> 
  autoplot() +
  labs(
    title = "Autocorrelation coefficient after seasonal differencing",
    subtitle = "Time series show stationarity: white noise", 
    y = "Autocorrelation coeficient"
    ) + 
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = "#706E6D"),
      axis.title.y = element_text(size = 10, margin = margin(r = 5)),
      axis.title.x = element_text(size = 10, margin = margin(r = 5))
    )

### ------------------------------ Test if the time series is a white noise ----
diff_ts |> 
  features(.var = .admissions, ljung_box, lag = 10)

### ----------------------------------- Plot Partial Autocorrelation (PACF) ----

#### Pastoral livelihood system ----
diff_ts |> 
  filter(lsystems == "Pastoral") |> 
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
  labs(title = "Pastoral livelihood system")

#### Agropastoral livelihood system ----
diff_ts |> 
  filter(lsystems == "Agropastoral") |> 
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
    labs(title = "Agropastoral livelihood system")

#### Riverine livelihood system ----
diff_ts |> 
  filter(lsystems == "Riverine") |> 
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
    labs(title = "Riverine livelihood system")

#### Urban/IDPs livelihood system ----
diff_ts |> 
  filter(lsystems == "Urban/IDPs") |> 
  gg_tsdisplay(y = .admissions, plot_type = "partial", lag_max = 36) +
    labs(title = "Urban/IDPs livelihood system")

############################## End of workflow #################################
