Sys.setenv(no_proxy = "*")

source("renv/activate.R")

shhh <- suppressPackageStartupMessages # It's a library, so shhh!

run_tests_locally <- function() {
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  shhh(source("global.R"))
  # message("================================================================================")
  # message("== testthat ====================================================================")
  # message("")
  # shhh(testthat::test_dir("tests/testthat"))
  # message("")
  message("================================================================================")
  message("== shinytest ===================================================================")
  message("")
  shhh(shinytest::testApp())
  message("")
  message("================================================================================")
}


# Install commit-hooks locally
statusWriteCommit <- file.copy(".hooks/pre-commit.R", ".git/hooks/pre-commit", overwrite = TRUE)
