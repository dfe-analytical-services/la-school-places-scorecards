# ---------------------------------------------------------------------------
# Library calls
# ----------------------------------------------------------------------------
library(shinyGovstyle)
library(shiny)
library(dplyr)
library(data.table)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(flexdashboard)
library(shinydashboard)
library(scales)
library(forcats)
library(ggbeeswarm)
library(shinyjs)
library(openxlsx)
library(kableExtra)
library(metathis)
library(shinyWidgets)
library(styler)
library(rsconnect)
library(bit64)
library(webshot)
webshot::install_phantomjs()

# tidy_code_function -------------------------------------------------------------------------------

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  return(app_scripts)
}

source("0_variable_change.R")

# ----------------------------------------------------------------------------
# Setup loading screen and spinner
# ----------------------------------------------------------------------------

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

options(spinner.type = 5)
options(spinner.color = "#c8c8c8")
options(spinner.size = .5)

seq_gradient <- c("#8c2d04", "#cc4c02", "#ec7014", "#fe9929", "#fec44f", "#d9f0a3")
divergent_gradient <- c(seq_gradient, rev(seq_gradient))


# Enable bookmarking ---------------------------------------------------------

enableBookmarking(store = "url")


# ----------------------------------------------------------------------------
# Reading and manipulating data
# ----------------------------------------------------------------------------

scorecards_data <- fread("data/scorecards_data.csv")

# pivot data around to long format
scorecards_data_pivot <- scorecards_data %>%
  mutate_at(vars(-c("Region","LA_name")), as.numeric) %>%
  pivot_longer(cols = !starts_with(c("Region","LA"))) %>%
  # assign phase based on names of columns
  mutate(
    Phase = ifelse(
      str_detect(name, "_S_") | str_detect(name, "KS4") | name %in% c("For_3_S", "For_1_S"), "Secondary", "Primary"
    ),
    # remove the _S_, _P_ name identifiers so we can instead use the phase column to get data
    name = str_replace_all(name, "_S_", ""),
    name = str_replace_all(name, "_P_", ""),
    name = str_replace_all(name, "_S$", ""),
    name = str_replace_all(name, "_P$", "")
  )


# LA options - reordered
LA_options <- sort(unique(scorecards_data$LA_name)) %>%
  as.factor() %>%
  relevel("England")


# Functions ---------------------------------------------------------------

# Create rounding function as baseR one rounds fives down

roundFiveUp <- function(x, n) {
  z <- abs(x) * 10^n
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^n
  positiveNegative <- sign(x)
  return(z * positiveNegative)
}

# Comma separating

cs_num <- function(x) {
  format(x, big.mark = ",", trim = TRUE)
}

# Create colour palette

dfe_colours <- c(
  "#12436D", # `blue`
  "#f47738", # `orange`
  "#801650", # `bright purple`
  "#28A197", # `turquoise`
  "#4C2C92" # `light-purple`
)

# Notes tables----------------------------------

notesTable <- read.xlsx(
  xlsxFile = "data/tech_guidance.xlsx",
  sheet = "Overall"
) # %>%
# fill(everything()) #collapse_Rows currently broken in kable. When fixed we can add back in.

notesTableQuant <- read.xlsx(
  xlsxFile = "data/tech_guidance.xlsx",
  sheet = "Quantity"
)

notesTablePref <- read.xlsx(
  xlsxFile = "data/tech_guidance.xlsx",
  sheet = "Preference"
)

notesTableQual <- read.xlsx(
  xlsxFile = "data/tech_guidance.xlsx",
  sheet = "Quality"
)

notesTableCost <- read.xlsx(
  xlsxFile = "data/tech_guidance.xlsx",
  sheet = "Cost"
)


# File download -----------------------------------------------------------

metadata <- fread("data/metadata.csv", encoding = "UTF-8")

# Create clean versions of the file for download--------------

scorecards_data_clean <- scorecards_data %>% dplyr::rename(`LA Name` = LA_name)

# this line renames the columns of the old dataset using the lookup table
scorecards_data_clean <- data.table::setnames(scorecards_data_clean, old = metadata$programmer_friendly_names, new = metadata$user_friendly_name, skip_absent = TRUE)

# Primary data
primary_data_clean <- scorecards_data_clean %>%
  select(`LA Name`, `LA Number`, contains(c("primary", "Primary")))

# Secondary data
secondary_data_clean <- scorecards_data_clean %>%
  select(`LA Name`, `LA Number`, contains(c("secondary", "Secondary")))

# Create download button without the icon
myDownloadButton <- function(outputId, label = "Download") {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label
  )
}