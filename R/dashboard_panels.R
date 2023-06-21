panel_homepage <- function() {
  tabPanel(
    "Homepage",
    h2("Local authority school places scorecards: academic year 2021/22"),
    br("Scorecards display a snapshot of the progress each local authority across England is making towards ensuring there are sufficient, good quality, school places."),
    br("You can view the progress for England as a whole or for an individual local authority.
         To do this click on", actionLink("linklascorecardsTab", "LA scorecards"), "after reading all the information on this homepage.
         Use the drop-down list, or type in the box, to select England or your chosen local authority. "),
    br("You can view primary or secondary places by using the adjacent drop down. The figures and charts in the scorecard will automatically update when these drop downs are changed."),
    br("You can download a PDF version of the complete scorecard, by clicking the ‘download report’. Use the ‘download data’ button on the left of the scorecard to
         download underlying data for all local authorities and England."),
    br("There are five scorecard metrics and more information on each metric can be found below. Each scorecard metric is shown on a different tab within 'LA scorecards'
        and can be selected by clicking on the name of the metric."), 
    br(),
    div(
      class = "panel panel-info",
      div(
        class = "panel-heading",
        style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
        h2("Scorecard Contents"),
      ),
      div(
        class = "panel-body",
        h3("School Place Metrics Included"),
        br(),
        h4(actionLink("linkQuantityTab", "Quantity")),
        #  h4("Quantity"),
        br("This measure shows how much progress the chosen local authority is making in providing sufficient school places for academic year 2024/25. This is based on published
             ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-capacity", "school capacity data"), "as collected in the 2022 the School Capacity Survey (SCAP22)."),
        br("The chart shows the:"),
        br("•places already added since the academic year 2009/10;",
        "•additional places planned to be added up to the 2024/25 academic year; and",
        "•estimated number of additional places still needed to meet demand in 2024/25, based on the local authority’s forecast demand."),
        br("The ‘pupils in place’s headline boxes show the:"),
        br("•the percentage increase in pupil numbers in primary or secondary state-funded mainstream provision between 2009/10 to 2022/23; and",
        "•the percentage of unfilled places in 2021/22; based on reported school capacity and pupil number on roll."),
        br("The ‘estimated future school place demand’ headline boxes are based on the local authority’s forecast demand. They show the:"),
        br(" •	estimated number of additional places still needed to meet demand in 2024/25 (same figure shown in the chart);",
        " •	estimated percentage of spare places in 2024/25. It is common for a local authority to have both a need for additional places and spare capacity,
             reflecting pockets of localised need for places or pockets of localised spare places;",
        " •	the total amount of", a(href = "https://www.gov.uk/government/publications/basic-need-allocations", "basic need capital funding"), "allocated to each local authority to create new places from 2011 to 2024; and",
        " •	the anticipated percentage increase in pupil numbers in primary or secondary state-funded mainstream provision between 2022/23 to 2024/25."),
        br(),
        br("In a box above the chart, users can select up to three additional local authorities to view their data in the chart, alongside their chosen local authority. 
           Please note the headline boxes will not change when selecting local authorities to benchmark against."),
        br(),
        br("It is important to take care when making comparisons using the quantity measure. Local authorities greatly vary in size and population and therefore the number of schools and school places. Some local authorities have long-standing place pressure,
           whereas for others it has emerged more recently. Those experiencing long-standing place pressures will have had more chance to demonstrate that they can add large quantities of places."),
        br(),
        br("Local authorities with relatively fewer places still needed are making good progress in delivering the places they anticipate are needed, however they may be good reasons for a local authority showing a significant number of places still needed to be created.
           For example, places from the department’s centrally funded free schools due to open in September 24 onwards are not included in the calculations. Projects to add school places would not have been included in these calculations if they were only recently planned and/or confirmed. "),
        br(),
        br("Unfilled places can be evidence of local authorities having planned ahead for future need. Unfilled places can also be attributed to the building of whole new schools, which fill up from the bottom, leaving space in the upper years until those year groups work their way through. 
           In some areas, low or declining need for places will also contribute to the number of unfilled places."),
        br(),
        h4(actionLink("linkForecastTab", "Forecast Accuracy")),
        br("Estimating place pressure in future years relies on the forecasts of pupil numbers made by local authorities, as provided in the annual school capacity survey. The scorecard illustrates the forecasting accuracy of the selected local authority for forecasts made one year ago and provided in SCAP22, 
           and two years ago in SCAP21. These pupil forecasts have been compared with pupils on roll from the January 2023 school census numbers to produce a forecast accuracy score for one and two years ahead. A negative forecast accuracy score indicates an underestimation of pupils, 
           a positive score indicates an overestimation."),
        br(),
        h4(actionLink("linkPreferenceTab", "Preference")),
        br(
          "You can use the scorecard to see how well the chosen local authority is able to meet parental preference,
             based on published", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/secondary-and-primary-school-applications-and-offers", "school applications and offers data."),
          "The headline box shows the percentage of applicants, in the local authority, who received an offer of a place in one of their top three preferences.
             The adjacent headline box shows the same percentage for England."
        ),
        br("The chart breaks this down further into to those who received an offer of their first, second or third preferences.
             ‘Other’ offers include pupils who received an offer of a lower preference (where a local authority allows four or more preferences) or did not receive a preferred offer.
             The latter can include applicants who were made an alternative offer and those who were not made any offer."),
        br("In a box above the chart, users can select England or another local authority to view their data in the chart, alongside their chosen local authority. Please note the headline boxes will not change when selecting local authorities to benchmark against."),
        br(),
        h4(actionLink("linkQualityTab", "Quality")),
        br("You can use this measure to see the quality of the schools where the chosen local authority has added school places, based on the change between the 2020/21 and the 2021/22 published school capacity data and", a(href = "https://www.gov.uk/government/statistics/state-funded-schools-inspections-and-outcomes-as-at-31-august-2022", "published school Ofsted rating."), ),
        br("The chart shows the number of new places added in the local authority, between May 2021 and May 2022, according to the Ofsted rating of the school in which they have been added. There are 4 possible Ofsted ratings: outstanding, good, requires improvement and inadequate."),
        br("It is important to take care when making comparisons using the quality measure as:"),
        br("•	many Ofsted inspections have been delayed due to the pandemic, therefore local authorities may use other data/own local knowledge when deciding which schools to expand;"),
        "•	the starting position for local authorities is different and some have more good or outstanding schools to add places to; which is why the overall distribution of existing school places by Ofsted rating in the local authority chosen is shown;",
        br("•	when deciding upon which schools to expand, some good or outstanding schools may be on sites that are not suitable for expansion;"),
        "•	the Ofsted rating used was available at 31 August 2022 and places may have been added before or after that rating was given: ",
        br(),
        br("In a box above the chart, users can select England or another local authority to view their data in the chart, alongside their chosen local authority. Please note the headline boxes will not change when selecting local authorities to benchmark against."),
        br(),
        h4(actionLink("linkCostTab", "Cost")),
        br("The Capital Spend Survey that replaced the SCAP Capital Spend Data now collects data on project costs, however due to incomplete coverage it has not yet been incorporated into the scorecard.  The cost data used in the scorecard remains the Capital Spend Data from SCAP18. This was used in the 2018, 2019 and 2021 scorecards; there has been no change in the sample of projects. As done in 2021, for the 2022 Scorecard, the data has been adjusted for inflation (uprated to 1st quarter 2023 prices)."),
        br(strong("You can use the scorecard to view the national average cost per place for both primary and secondary school places. These are shown for permanent expansions, temporary expansions and new schools separately. You can also view, the national averages adjusted for 2022 regional location factors. ")),
        br("Because the sample of projects are from 2018, the local authority average costs are not shown in the 2022 scorecard. However, you can find these in the underlying data on explore education statistics if needed."),
        br("There is further guidance on converting costs into current or future prices in the scorecard ‘Technical notes’.")
      )
)
  )
    
  
}


# Scorecard panel code ----------------------------------------------------

panel_scorecard <- function() {
  tabPanel(
    value = "la_scorecards",
    title = "LA scorecards",
    # Sidebar---------------------------------------------------------------------
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              column(
                width = 12,
                p("View a local authority school places scorecard using the drop downs below and switch between different school place metrics using the tabs on the underneath."),
              )
            ),
            gov_row(
              column(
                width = 6,
                selectInput("LA_choice",
                  label = p(strong("Choose a local authority")),
                  choices = levels(LA_options),
                  selected = "England"
                )
              ),
              column(
                width = 6,
                selectInput("phase_choice",
                  label = p(strong("Choose between primary or secondary school places")),
                  choices = c("Primary", "Secondary"),
                  selected = "Primary"
                )
              )
            ),
            gov_row(
              column(
                width = 6,
                p(strong("Download data for all local authorities", style = "color:white")),
                myDownloadButton(
                  "download_ud",
                  "Download data"
                )
              ),
              column(
                width = 6,
                p(strong("Download summary pdf for chosen local authority", style = "color:white")),
                myDownloadButton(
                  "pdfDownload",
                  "Download report"
                )
              )
            )
          )
        )
      ), # end of panel

      # Create the main content-----------------
      gov_row(
        column(
          width = 12,
          h2(textOutput("data_description")),
          tabsetPanel(
            id = "tabs",
            tabPanel(
              value = "quantity",
              title = "Quantity",
              gov_main_layout(
                gov_row(
                  column(
                    6,
                    p(strong("School places created, planned future places, additional places still needed, as at May", SCAP_ref)),
                    plotlyOutput("places_chart") %>% withSpinner(),
                    uiOutput("quantity.bartext")
                    
                  ),
                  column(
                    6,
                    gov_row(
                      column(
                        12,
                        strong(textOutput("pupil_subtitle")),
                        br(),
                        valueBoxOutput("pupil_growth", width = 6),
                        valueBoxOutput("current_unfilled_places", width = 6),
                        p(strong(paste0("Estimated future school place demand"))),
                        p("A local authority can have both ‘spare places’ and ‘additional places needed’ due to localised or specific year group demand"),
                        valueBoxOutput("estimated_additional_places", width = 6),
                        valueBoxOutput("estimated_spare_places", width = 6)
                      )
                    ),
                    gov_row(
                      column(
                        12,
                        valueBoxOutput("total_funding_box", width = 6),
                        valueBoxOutput("pupil_anticipated_growth", width = 6)#,
                        #uiOutput("quantity.bartext")
                      )
                    )
                  )
                )
              )
            ),
            tabPanel(
              value = "forecast",
              title = "Pupil forecast accuracy",
              gov_main_layout(
                gov_row(
                  column(
                    width = 12,
                    p(strong("Forecast accuracy of pupil projections for", forecast_year, ", made one year and two years previously")),
                    uiOutput("forecasting.bartext"),
                    column(
                      6,

                      # details(
                      #  inputId = "faccuracyhelp",
                      #  label = "How to benchmark using the charts",
                      # help_text = "
                      # The thick vertical line shows the chosen LA's
                      # average forecasting accuracy, whilst the dashed lines show the
                      # 25th and 75th percentiles across all LAs (i.e. half of all LAs were
                      # found to have a forecasting accuracy falling between the two dashed lines)."),
                      htmlOutput("label_estimate_y1"),
                      plotlyOutput("forecast_1y_bar", height = "120px"),
                      br(),
                      p("One year ahead: range of forecast accuracy scores"),
                      dataTableOutput("for1year_table"),
                    ),
                    column(
                      6,
                      htmlOutput("label_estimate_y2"),
                      plotlyOutput("forecast_2y_bar", height = "120px"),
                      br(),
                      p("Two year ahead: range of forecast accuracy scores"),
                      dataTableOutput("for2year_table")
                    )
                  ),
                  br(),
                  p(em("Caution should be taken with forecast accuracy scores, due to unforseen impacts on pupil numbers after the forecasts were made. See Homepage for more information.")),
                )
              )
            ),
            tabPanel(
              value = "preference",
              title = "Preference",
              gov_main_layout(
                gov_row(
                  column(
                    width = 12,
                    p(strong(paste0("School applications and offers for September ", preference_current_year, " and " , preference_next_year, " entry"))),
                    # preference content to go here
                    valueBoxOutput("prefT3_NY_ENG", width = 3),
                    valueBoxOutput("PrefT3_NY_LA", width = 3),
                    valueBoxOutput("prefT3_CY_ENG", width = 3),
                    valueBoxOutput("PrefT3_CY_LA", width = 3)
                  )
                ),
                gov_row(
                  column(
                    12,
                    p(strong(paste0("Proportion of applicants who received an offer of a school place in their first, second and third preferences"))),
                    plotlyOutput("preference_p") %>% withSpinner()
                  )
                )
              )
            ),
            tabPanel(
              value = "quality",
              title = "Quality",
              gov_main_layout(
                gov_row(
                  column(
                    width = 12,
                    strong(textOutput("quality_description")),
                    br(),
                    valueBoxOutput("England_GO_places", width = 4),
                    valueBoxOutput("LA_GO_places", width = 4),
                    valueBoxOutput("LA_GO_ran", width = 4)
                  )
                ),
                gov_row(
                  column(
                    12,
                    p(strong(paste0("Number of new places created in schools of each category and number of existing school places in each category"))),
                    plotlyOutput("quality_chart") %>% withSpinner()
                  )
                ),
                textOutput("no_rating_line"),
                br(),
                p(em("Caution should be taken with quality data as many Ofsted inspections have been delayed due to Covid-19."))
              )
            ),
            tabPanel(
              value = "cost",
              title = "Cost",
              gov_main_layout(
                gov_row(
                  column(
                    width = 12,
                    p(strong("Average cost of additional mainstream school places")),
                    p("Based on local authority reported projects between ", cost_year_1, " and ", cost_year_2, ", adjusted for inflation and regional variation"),
                    p(em("Local authority average costs are not shown because there is no new data. Only national average costs and number of projects for England are shown."))
                  )
                ),
                gov_row(
                  column(
                    width = 12,
                    valueBoxOutput("perm_box", width = 4),
                    valueBoxOutput("temp_box", width = 4),
                    valueBoxOutput("new_box", width = 4),
                  ),
                  p(strong("Average cost per place for permanent, temporary and new school projects")),
                  uiOutput("cost.bartext"),
                  br()
                ),
                gov_row(
                  column(
                    4,
                    tableOutput("cost_table")
                  )
                )
              )
            )
          ) # end of tabset
        )
      )
    )
  )
}


# Technical panel ---------------------------------------------------------

panel_technical <- function() {
  tabPanel(
    value = "technical_notes",
    title = "Technical notes",
    h2("Technical notes"),
    br("Use this dashboard to view school places scorecards for local authorities in England"),
    br("All dates refer to the academic year, apart from basic need funding years which refer to the financial year."),
    br("There is no scorecard for North Northamptonshire and West Northamptonshire as they are new local authorities, following the split from one local authority into two in April 2021. As these two new local authorities are not directly comparable with the pre LGR 2021 Northamptonshire, we were unable to produce complete figures for the majority of individual indictors included in the school places scorecard,
                           however the relevant data for these pre and post LGR 2021 local authorities are included in the England data and can be found in the summary data on explore education statistics."),
    br(),
    tabBox(
      title = "",
      id = "tabs_tech_notes", width = "12",
      tabPanel(
        "Quantity",
        tableOutput("notesTableQuant") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        "Pupil forecast accuracy",
        tableOutput("notesTableforacc") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        "Preference",
        tableOutput("notesTablePref") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        "Quality",
        tableOutput("notesTableQual") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        value = "cost",
        title = "Cost",
        tableOutput("notesTableCost") # made in global.R file
      ) # end of tabPanel
    ) # end of tabBox
  )
}
