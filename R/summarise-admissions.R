#'
#'
#' Summarise time series data
#'
#' @param ts A time series object of class `tsibble`.
#'
#' @param .group Logical. Whether the `tsibble` should be grouped or not, as
#'    it would be required in subsequent analysis.
#'
#' @param time A choice of the time series interval of occurrence.
#' `"M"` for monthly and `"Q"` for quarterly.
#'
#'

summarise_admissions <- function(ts, .group = TRUE, time = c("M", "Q")) {
  ## Enforce options in `time` ----
  time <- match.arg(time)

  ## Grouped time series ----
  if (.group) {
    if (time == "M") {
      ts <- ts |>
        select(lsystems, Monthly, admissions, region) |>
        group_by(region, lsystems, Monthly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          index = Monthly,
          key = c(region, lsystems)
        )
    }
    if (time == "Q") {
      ts <- ts |>
        select(lsystems, Quarterly, admissions, region) |>
        group_by(region, lsystems, Quarterly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          key = c(region, lsystems),
          index = Quarterly
        )
    }
  } else {
    if (time == "M") {
      ts <- ts |>
        select(Monthly, admissions, region) |>
        group_by(region, Monthly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          index = Monthly,
          key = region
        )
    }

    if (time == "Q") {
      ts <- ts |>
        select(Quarterly, admissions, region) |>
        group_by(region, Quarterly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          index = Quarterly,
          key = region
        )
    }
  }
  ## Return ----
  ts
}
