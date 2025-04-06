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
