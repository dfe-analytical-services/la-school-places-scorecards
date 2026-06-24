format_perc <- function(x, output = "dashboard") {
  if (output == "dashboard") {
    paste0(sprintf("%+.1f", 100. * round_half_up(x, 3)), "%")
  } else if (output == "latex") {
    paste0(sprintf("%+.1f", 100. * round_half_up(x, 3)), "\\%")
  }
}

forecast_summary_stats <- function(data, years, phase) {
  forecast <- paste0("For_", years)
  summary_stats <- data %>%
    filter(name == forecast, Phase == phase, LA_name != "England") %>%
    summarise(
      p0 = quantile(value, 0.0, na.rm = TRUE),
      p25 = quantile(value, 0.25, na.rm = TRUE),
      p50 = quantile(value, 0.5, na.rm = TRUE),
      p75 = quantile(value, 0.75, na.rm = TRUE),
      p100 = quantile(value, 1.0, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(
      cols = c("p0", "p25", "p50", "p75", "p100"),
      names_to = "percentile",
      values_to = "accuracy"
    ) |>
    mutate(quantile = c("Min", "25th centile", "Median", "75th centile", "Max"))
  return(summary_stats)
}


forecast_accuracy_desclabel <- function(forecast_accuracy, la, summary_stats) {
  # Adds overestimate/underestimate descriptors.
  case_when(
    la == "City of London" ~
      "No forecast accuracy score due to smaller numbers of pupils in City of London",
    la == "Isles Of Scilly" ~
      "No forecast accuracy score due to smaller numbers of pupils in Isles of Scilly",
    la != "England" ~
      case_when(
        la != "England" &
          forecast_accuracy > 0 &
          forecast_accuracy > summary_stats$accuracy[4] ~
          "Overestimate of pupil numbers, larger overestimate than at least 75\\% of local authorities",
        la != "England" &
          forecast_accuracy > 0 &
          forecast_accuracy < summary_stats$accuracy[4] ~
          "Overestimate of pupil numbers, within the middle 25-75\\% of local authorities' forecast accuracy scores",
        la != "England" &
          forecast_accuracy < 0 &
          forecast_accuracy < summary_stats$accuracy[2] ~
          "Underestimate of pupil numbers, larger underestimation than at least 75\\% of local authorities",
        la != "England" &
          forecast_accuracy < 0 &
          forecast_accuracy > summary_stats$accuracy[2] ~
          "Underestimate of pupil numbers, within the middle 25-75\\% of local authorities' forecast accuracy scores",
        TRUE ~ "No overestimate/underestimate therefore accurate"
      ),
    la == "England" ~
      case_when(
        la == "England" & forecast_accuracy > 0 ~
          "Overestimate of pupil numbers",
        la == "England" & forecast_accuracy < 0 ~
          "Underestimate of pupil numbers",
        TRUE ~ "No overestimate/underestimate therefore accurate"
      ),
    TRUE ~ "No overestimate/underestimate therefore accurate"
  )
}


roundFiveUp <- function(number, dp = 0) {
  if (!is.numeric(number) && !is.numeric(dp)) {
    stop("both input arguments must be numeric")
  }
  if (!is.numeric(number)) {
    stop("the input number to be rounded must be numeric")
  }
  if (!is.numeric(dp)) {
    stop("the decimal places input must be numeric")
  }

  z <- abs(number) * 10^dp
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^dp
  return(z * sign(number))
}

vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash = "solid")
  )
}
