shhh <- suppressPackageStartupMessages # It's a library, so shhh!


# ---------------------------------------------------------------------------
# Library calls
# ----------------------------------------------------------------------------
shhh(library(shinyGovstyle))
shhh(library(shiny))
shhh(library(dplyr))
shhh(library(data.table))
# library(shinya11y)
shhh(library(shinycssloaders))
shhh(library(shinyalert))
shhh(library(tidyr))
shhh(library(stringr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(flexdashboard))
shhh(library(shinydashboard))
shhh(library(scales))
shhh(library(forcats))
shhh(library(ggbeeswarm))
shhh(library(shinyjs))
shhh(library(openxlsx))
shhh(library(kableExtra))
shhh(library(metathis))
shhh(library(shinyWidgets))
shhh(library(styler))
shhh(library(rsconnect))
shhh(library(bit64))
shhh(library(webshot))
shhh(library(checkmate))
shhh(library(dfeshiny))
shhh(library(shinytest2))
shhh(library(diffviewer))
shhh(library(janitor))

# Phantom js needed for pdf compile to work. Note this works even on the shinyapps server.
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs(force = FALSE)
}

source("0_variable_change.R")
source("R/functions.R")
source("R/plotting.R")

site_title <- "Local Authority School Places Scorecards"
site_primary <- "https://department-for-education.shinyapps.io/la-school-places-scorecards/"
site_overflow <- NA
sites_list <- c(site_primary) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "Statistical publication" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/local-authority-school-places-scorecards" # Update with parent publication link
google_analytics_key <- "1RK7T205RS"

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


scorecards_data <- fread("data/scorecards_data_2023_dummy.csv")


# pivot data around to long format
scorecards_data_pivot <- scorecards_data %>%
  mutate_at(vars(-c("Region", "LA_name")), as.numeric) %>%
  pivot_longer(cols = !starts_with(c("Region", "LA"))) %>%
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

# LA options - for benchmarking
La_data_benchmark <- scorecards_data %>%
  filter(LA_name != c("England"))

# La_data_benchmark2 <- reactive({
# scorecards_data %>%
# filter (LA_name != input$LA_choice)
# })

LA_benchmark_options <-
  sort(unique(La_data_benchmark$LA_name)) %>%
  as.factor() %>%
  relevel("Barking and Dagenham")

LA_benchmark_options_pref <-
  sort(unique(scorecards_data$LA_name)) %>%
  as.factor() %>%
  relevel("England")

LA_benchmark_options_quality <-
  sort(unique(scorecards_data$LA_name)) %>%
  as.factor() %>%
  relevel("England")


# Functions ---------------------------------------------------------------

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


notesTableQuant <- read.xlsx(
  xlsxFile = "data/tech_guidance.xlsx",
  sheet = "Quantity"
)

notesTableforacc <- read.xlsx(
  xlsxFile = "data/tech_guidance.xlsx",
  sheet = "ForecastAccuracy"
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
