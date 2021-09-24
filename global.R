# ---------------------------------------------------------------------------
# Library calls
# ----------------------------------------------------------------------------

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


# ----------------------------------------------------------------------------
# Reading and manipulating data
# ----------------------------------------------------------------------------

scorecards_data <- fread("data/scorecards_data.csv") 

#pivot data around to long format
  scorecards_data_pivot <- scorecards_data %>%  
  mutate_at(vars(-("LA_name")),as.numeric) %>% 
  pivot_longer(cols = !starts_with("LA")) %>% 
    # assign phase based on names of columns
  mutate(Phase = ifelse(
    str_detect(name,"_S_")|str_detect(name,"KS4")| name %in% c("For_3_S","For_1_S"),"Secondary","Primary"),
  # remove the _S_, _P_ name identifiers so we can instead use the phase column to get data
   name =  str_replace_all(name,"_S_",""),
   name =  str_replace_all(name,"_P_",""),
  name =  str_replace_all(name,"_S$",""),
  name =  str_replace_all(name,"_P$","")
  )
  
  
  #LA options - reordered
  LA_options <- sort(unique(scorecards_data$LA_name)) %>% 
    as.factor() %>% 
    relevel("England") 
  
# Functions ---------------------------------------------------------------

#Create rounding function as baseR one rounds fives down

roundFiveUp <- function(x, n){ 
  z = abs(x)*10^n 
  z = z + 0.5 + sqrt(.Machine$double.eps) 
  z = trunc(z) 
  z = z/10^n 
  positiveNegative = sign(x) 
  return(z * positiveNegative) 
}

# Comma separating 
  
cs_num <- function(x){format(x,big.mark=",", trim=TRUE)}


# Notes tables----------------------------------

notesTable <- read.xlsx(
  xlsxFile="data/tech_guidance.xlsx",
  sheet="Overall")# %>% 
  #fill(everything()) #collapse_Rows currently broken in kable. When fixed we can add back in.

notesTableQuant <- read.xlsx(
  xlsxFile="data/tech_guidance.xlsx",
  sheet="Quantity")

notesTablePref <- read.xlsx(
  xlsxFile="data/tech_guidance.xlsx",
  sheet="Preference")

notesTableQual <- read.xlsx(
  xlsxFile="data/tech_guidance.xlsx",
  sheet="Quality")

notesTableCost <- read.xlsx(
  xlsxFile="data/tech_guidance.xlsx",
  sheet="Cost")


# File download -----------------------------------------------------------

metadata <- fread("data/metadata.csv")

#Create clean versions of the file for download--------------

scorecards_data_clean<-scorecards_data %>% dplyr::rename(`LA Name` = LA_name)

# this line renames the columns of the old dataset using the lookup table
scorecards_data_clean<-data.table::setnames(scorecards_data_clean, old=metadata$programmer_friendly_names,new=metadata$user_friendly_name,skip_absent = TRUE)

#Primary data
primary_data_clean <- scorecards_data_clean %>% 
  select(`LA Name`, `LA Number`, contains(c("primary","Primary")))

#Secondary data
secondary_data_clean <- scorecards_data_clean %>% 
  select(`LA Name`, `LA Number`, contains(c("secondary","Secondary")))
