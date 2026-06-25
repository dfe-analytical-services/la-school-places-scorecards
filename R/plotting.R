plot_forecast <- function(
    dfScorecards,
    dfScorecardsPivot,
    la_choice,
    phase,
    years) {
  forecast <- paste0("For_", years)
  # Calculate the data range - for this we're using Primary and Secondary together.
  range_allphases <- dfScorecardsPivot %>%
    filter(name == forecast)
  max_scale <- max(c(
    abs(min(range_allphases$value, na.rm = TRUE)),
    max(range_allphases$value, na.rm = TRUE)
  ))
  max_scale <- ceiling(100. * max_scale) / 100.

  # Derive the tickmark positions based on the scale.
  scale_increment <- floor(100. * abs(max_scale)) / 100. / 2.
  x_scale <- seq(-2. * scale_increment, 2.1 * scale_increment, scale_increment)

  # Now get the percentile values for plotting on the chart.
  percentiles <- dfScorecardsPivot %>%
    filter(name == forecast, Phase == phase) %>%
    summarise(
      p25 = quantile(value, 0.25, na.rm = TRUE),
      p75 = quantile(value, 0.75, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(
      cols = c("p25", "p75"),
      names_to = "percentile",
      values_to = "accuracy"
    )

  # Now grab the actual data point (note that this has already been filtered for phase and LA).
  forecast_accuracy <- dfScorecards %>%
    filter(name == forecast)
  forecast_accuracy$value <- forecast_accuracy$value %>% roundFiveUp(., 3)

  p <- ggplot(
    forecast_accuracy,
    aes(
      name,
      value,
      fill = value,
      tooltip = paste0(la_choice, ": ", format_perc(value))
    )
  ) +
    geom_col_interactive(stat = "identity", width = 100) +
    scale_fill_gradientn(
      colors = divergent_gradient,
      space = "Lab",
      limits = c(-0.75 * abs(max_scale), 1.08 * abs(max_scale)),
    ) +
    scale_y_continuous(
      breaks = x_scale,
      labels = format_perc(x_scale),
      limits = c(-abs(max_scale), max_scale)
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
      text = element_text(size = 42)
      # font size is scaled to the size of the container so this is set larger than standard
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1.5) +
    geom_hline_interactive(
      yintercept = percentiles$accuracy[1],
      linetype = "dashed",
      color = "Grey",
      size = 1.5,
      tooltip = "25th percentile"
    ) +
    geom_hline_interactive(
      yintercept = percentiles$accuracy[2],
      linetype = "dashed",
      color = "Grey",
      size = 1.5,
      tooltip = "75th percentile"
    ) +
    geom_hline(yintercept = forecast_accuracy$value, size = 2) +
    labs(x = "", y = "Accuracy") +
    coord_flip()
  return(p)
}
