format_perc <- function(x) {
  paste0(sprintf("%+.1f", 100. * roundFiveUp(x, 3)), "%")
}
