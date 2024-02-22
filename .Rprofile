source("renv/activate.R")

shhh <- suppressPackageStartupMessages # It's a library, so shhh!

# Install commit-hooks locally
statusWriteCommit <- file.copy(".hooks/pre-commit.R", ".git/hooks/pre-commit", overwrite = TRUE)
