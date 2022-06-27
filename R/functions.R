format_perc <- function(x, output = "dashboard") {
  if (output == "dashboard") {
    paste0(sprintf("%+.1f", 100. * roundFiveUp(x, 3)), "%")
  } else if (output == "latex") {
    paste0(sprintf("%+.1f", 100. * roundFiveUp(x, 3)), "\\%")
  }
}

forecast_summary_stats <- function(data, years, phase) {
  forecast <- paste0("For_", years)
  summary_stats <- data %>%
    filter(name == forecast, Phase == phase, LA_name != "England") %>%
    summarise(
      quantile = c("Min", "25th centile", "Median", "75th centile", "Max"),
      accuracy = quantile(value, c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
    ) %>%
    as.data.frame()
  return(summary_stats)
}


forecast_accuracy_desclabel <- function(forecast_accuracy, la, summary_stats) {
  # Adds overestimate/underestimate descriptors.
  case_when(
    la == "City of London" ~ "No forecast accuracy score due to smaller numbers of pupils in City of London",
    la == "Isles Of Scilly" ~ "No forecast accuracy score due to smaller numbers of pupils in Isles of Scilly",
    la != "England" ~
      case_when(
        la != "England" & forecast_accuracy > 0 & forecast_accuracy > summary_stats$accuracy[4] ~ "Overestimate of pupil numbers, larger overestimate than at least 75\\% of local authorities",
        la != "England" & forecast_accuracy > 0 & forecast_accuracy < summary_stats$accuracy[4] ~ "Overestimate of pupil numbers, within the middle 25-75\\% of local authorities' forecast accuracy scores",
        la != "England" & forecast_accuracy < 0 & forecast_accuracy < summary_stats$accuracy[2] ~ "Underestimate of pupil numbers, larger underestimation than at least 75\\% of local authorities",
        la != "England" & forecast_accuracy < 0 & forecast_accuracy > summary_stats$accuracy[2] ~ "Underestimate of pupil numbers, within the middle 25-75\\% of local authorities' forecast accuracy scores",
        TRUE ~ "No overestimate/underestimate therefore accurate"
      ),
    la == "England" ~
      case_when(
        la == "England" & forecast_accuracy > 0 ~ "Overestimate of pupil numbers",
        la == "England" & forecast_accuracy < 0 ~ "Underestimate of pupil numbers",
        TRUE ~ "No overestimate/underestimate therefore accurate"
      ),
    TRUE ~ "No overestimate/underestimate therefore accurate"
  )
}
