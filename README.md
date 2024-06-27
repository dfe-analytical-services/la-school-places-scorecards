<h1 align="center">
  <br>
  Local Authority Scorecards 
  <br>
</h1>

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction 

A shiny app to provide a way to view LA-level school places scorecard data and compare to England. This replaces the old excel workbook scorecards. This is deployed via the DfE visual studio and rsconnect subscriptions. There are three environments only accessible to DfE AD:

 - More information on environments/deploying to follow here

---

## Requirements


### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 3.6.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)
  
---

## How to use

### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.

### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest2 that test the app loads, that content appears correctly when different inputs are selected, and that tab content displays as expected. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The function shinytest2::test_app() will run any unit and ui tests available in the `tests/` directory.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions, to [https://department-for-education.shinyapps.io/la-school-places-scorecards](https://department-for-education.shinyapps.io/la-school-places-scorecards). The yaml file for this can be found in the .github/workflows folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function tidy_code() is provided in the `dfeshiny` package to tidy code according to tidyverse styling using the `styler` package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.

---

## Contact

SCAP.PPP@education.gov.uk
