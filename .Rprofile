Sys.setenv(no_proxy = "*")

source("renv/activate.R")

shhh <- suppressPackageStartupMessages # It's a library, so shhh!

tidy_code <- function() {
  shhh(source("global.r"))
  shhh(tidy_code_function())
}
