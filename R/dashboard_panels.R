panel_homepage <- function() {
  tabPanel(
    "Homepage",
    h1("Local authority school places scorecards"),
    br(
      "You can view the scorecard for England as a whole or for an individual
       local authority. To do this click on",
      actionLink("linklascorecardsTab", "LA scorecards"),
      "after reading all the information on this homepage. Use the drop-down list,
       or type in the box, to select England or your chosen local authority."
    ),
    p(
      "You can view primary or secondary places by using the right drop down.
       The figures and charts in the scorecard will automatically update when these drop downs are changed."
    ),
    p(
      "To select which quality measure you want to use click on the drop down on the quality tab.
       The figures and charts in the scorecard will automatically update when these
       drop downs are changed."
    ),
    p(
      "Use the ‘download data’ button to download underlying data
       for all local authorities and England."
    ),
    p(
      "There are five scorecard metrics and more information on each metric can
    be found below. Each scorecard metric is shown on a different tab within 'LA scorecards'
    and can be selected by clicking on the name of the metric."
    ),
    br(strong("The scorecard now conatins a drop-down list allowing you to view 2022/23 or 2023/24 data.
              The contents section below relates to the 2023/24 data, but would be applicable to the 2022/23 data if all stated dates and SCAP surveys were a year earlier.
              If you have any questions about the content of the 2022/23 scorecard, please get in touch.")),
    br(),
    div(
      class = "panel panel-info",
      div(
        class = "panel-heading",
        style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
        h2("Scorecard Contents (2023/24)"),
      ),
      div(
        class = "panel-body",
        h3(actionLink("linkQuantityTab", "Quantity")),
        p(
          "This measure shows how many places the chosen local authority has provided
        up to 2023/24 and has plans to provide for academic year 2026/27.  This is based on published
           ",
          a(
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-capacity",
            "school capacity data"
          ),
          "as collected in the 2024 the School Capacity Survey (SCAP24)."
        ),
        p("The measures shown are:"),
        tags$ul(
          tags$li("places already added since the academic year 2009/10;"),
          tags$li(
            "additional places planned to be added up to the 2026/27 academic year; and"
          ),
          tags$li(
            "estimated number of additional places still needed to meet demand in 2026/27,
           based on the local authority’s forecast demand."
          ),
          tags$li(
            "the percentage change in pupil numbers in primary or secondary state-funded
           mainstream provision between 2009/10, 2014/15 (for secondary), 2018/19 (for primary) and 2024/25; and"
          ),
          tags$li(
            "the percentage of unfilled places in 2023/24; based on reported school
          capacity and pupil number on roll."
          )
        ),
        p(
          "The ‘estimated future school place demand’ headline boxes are based on
           the local authority’s forecast demand. They show the:"
        ),
        tags$ul(
          tags$li(
            "estimated number of additional places still needed to meet demand in 2026/27 (same figure shown in the chart);"
          ),
          tags$li(
            "estimated percentage of spare places in 2026/27. It is common for a local authority to have both a need for additional places and spare capacity, reflecting pockets of localised need for places or pockets of localised spare places;"
          ),
          tags$li(
            "the total amount of ",
            external_link(
              href = "https://www.gov.uk/government/publications/basic-need-allocations",
              "basic need capital funding"
            ),
            " allocated to each local authority to create new places from 2011 to 2026; and"
          ),
          tags$li(
            "the anticipated percentage change in pupil numbers in primary or secondary state-funded mainstream provision between 2024/25 to 2026/27."
          )
        ),
        p(
          "In the box above the chart, users can select up to two additional
        local authorities to view their data in the chart, alongside their chosen local authority.
        Please note the headline boxes will not change when selecting local authorities to benchmark against,
        however the table at the bottom of the page will update to contain England and all LAs selected.
        Users may wish to benchmark local authorities against their statistical neighbours found in the ",
          external_link(
            href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/975117/Childrens_services_statistical_neighbour_benchmarking_tool_-_LGR_Version__April_2021_.xlsx",
            "Children's services statistical neighbour benchmarking tool"
          ),
          "."
        ),
        p(
          "It is important to take care when making comparisons using the quantity measure. Local authorities vary in size and population and therefore vary in the number of schools and school places. Some local authorities have long-standing place pressure,
           whereas for others it has emerged more recently. Those experiencing long-standing place pressures will have had more chance to demonstrate that they can add large quantities of places."
        ),
        p(
          "Local authorities with relatively fewer places still needed are making good progress in delivering the places they anticipate are needed, however they may be good reasons for a local authority showing a significant number of places still needed to be created.
           For example, places from the department’s centrally funded free schools due to open in September 2026 onwards are not included in the calculations. Projects to add school places would not have been included in these calculations if they were only recently planned and/or confirmed. "
        ),
        p(
          "Unfilled places can be evidence of local authorities having planned ahead for future need. Unfilled places can also be attributed to the building of whole new schools, which fill up from the bottom, leaving space in the upper years until those year groups work their way through.
           In some areas, low or declining need for places will also contribute to the number of unfilled places."
        ),
        h3(actionLink("linkForecastTab", "Forecast Accuracy")),
        p(
          "Estimating place pressure in future years relies on the forecasts of pupil numbers made by local authorities, as provided in the annual school capacity survey. The scorecard illustrates the forecasting accuracy of the selected local authority for forecasts made one year ago and provided in SCAP24,
           and three years ago in SCAP22. These pupil forecasts have been compared with pupils on roll from the January 2025 school census numbers to produce a forecast accuracy score for one and three years ahead. A negative forecast accuracy score indicates an underestimation of pupils,
           a positive score indicates an overestimation."
        ),
        h3(actionLink("linkPreferenceTab", "Preference")),
        p(
          "You can use the scorecard to see how well the chosen local authority is able to meet parental preference, based on published ",
          external_link(
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/secondary-and-primary-school-applications-and-offers",
            "school applications and offers data"
          ),
          ". The headline box shows the percentage of applicants, in the local authority, who received an offer of a place in one of their top three preferences for September 2024 and 2025 entry. The adjacent headline box shows the same percentage for England."
        ),
        p(
          "The chart breaks this down further into to those who received an offer of their first, second or third preferences for September 2024 and 2025 entry.
             ‘Other’ offers include pupils who received an offer of a lower preference (where a local authority allows four or more preferences) or did not receive a preferred offer.
             The latter can include applicants who were made an alternative offer and those who were not made any offer."
        ),
        p(
          "In the box above the chart, users can select England or another local authority to view their data in the chart, alongside their chosen local authority. Please note the headline boxes will not change when selecting a local authority to benchmark against."
        ),
        h3(actionLink("linkQualityTab", "Quality")),
        p(
          "The quality measures you can choose from are Ofsted rating, key stage 2 reading and maths progress or Progress 8."
        ),
        h4("Ofsted rating"),
        p(
          "You can use this measure to see the quality of the schools where the chosen ",
          "local authority has added school places, based on the change between the 2022/23",
          " and the 2023/24 published school capacity data and ",
          external_link(
            href = "https://www.gov.uk/government/statistics/state-funded-schools-inspections-and-outcomes-as-at-31-august-2024",
            "published school Ofsted rating"
          ),
          "."
        ),
        p(
          "The chart shows the number of new places added in the local authority, between May 2023 and May 2024, according to the Ofsted rating of the school in which they have been added. There are 4 possible Ofsted ratings: outstanding, good, requires improvement and inadequate."
        ),
        p(
          "It is important to take care when making comparisons using the quality measure as:"
        ),
        tags$ul(
          tags$li(
            "the starting position for local authorities is different and some have more good or outstanding schools to add places to; which is why the overall distribution of existing school places by Ofsted rating in the local authority chosen is shown;"
          ),
          tags$li(
            "when deciding upon which schools to expand, some good or outstanding schools may be on sites that are not suitable for expansion;"
          ),
          tags$li(
            "the Ofsted rating used was available at 31 August 2024 and places may have been added before or after that rating was given."
          ),
        ),
        p(
          "In the box above the chart, users can select England or another local authority to view their data in the chart, alongside their chosen local authority. Please note the headline boxes will not change when selecting a local authority to benchmark against."
        ),
        h4("Progress 8"),
        p(
          "For secondary you can also change the view to look at the number of new places added according to ",
          external_link(
            href = "https://www.compare-school-performance.service.gov.uk/",
            "key stage 4 Progress 8 performance"
          ),
          ". There are 5 possible Progress 8 bandings: well above average, above average, average, below average and well below average."
        ),
        tags$ul(
          tags$li(
            "the progress 8 rating used is for the 2023/24 academic year and places may have been added before or after that rating was given."
          )
        ),
        h4("Key stage 2 reading and maths progress"),
        p(
          "For primary you can also change the view to look at the number of new places added according to either ",
          external_link(
            href = "https://www.compare-school-performance.service.gov.uk/",
            "key stage 2 reading or key stage 2 maths progress"
          ),
          ". The 5 possible progress bandings are the same as listed above for Progress 8."
        ),
        tags$ul(
          tags$li(
            "the reading and maths progress rating used is for the 2022/23 academic year as it was not possible to calculate KS1-KS2 progress measures",
            "for 2023/24, as primary tests and assessments were cancelled in academic years 2019/20 and 2020/21 due to COVID-19 disruption (see ",
            external_link(
              href = "https://assets.publishing.service.gov.uk/media/67b6fdf19ae06ef4a71cf312/Primary_school_accountability_in_2024_technical_guide_-_December_2024.pdf",
              "Primary school accountability in 2024: technical guide"
            ),
            " for more information). Places may have been added before or after that rating was given."
          )
        ),
        h3(actionLink("linkCostTab", "Cost")),
        p(
          "The Capital Spend Survey that replaced the SCAP Capital Spend Data now collects data on project costs, however due to incomplete coverage it has not yet been incorporated into the scorecard. The cost data used in the scorecard remains the Capital Spend Data from SCAP18. This was used in the 2018, 2019, 2021 and 2022 scorecards; there has been no change in the sample of projects. As done in 2022, for the 2023 Scorecard, the data has been adjusted for inflation (uprated to 1st quarter 2025 prices)."
        ),
        p(strong(
          "You can use the scorecard to view the national average cost per place for both primary and secondary school places. These are shown for permanent expansions, temporary expansions and new schools separately. You can also view, the national averages adjusted for 2023 regional location factors."
        )),
        p(
          "As coverage and completeness of the Capacity Spend Survey is still improving, the local authority average costs are not shown in the 2024 scorecard. However, you can find details of all the projects included in the Cost metric in the underlying data on explore education statistics if needed."
        ),
        p(
          "There is further guidance on converting costs into current or future prices in the scorecard",
          actionLink("linkTechnicalnotesTab", "Technical Notes")
        ), # ‘Technical notes’.")
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
                p(
                  "View a local authority school places scorecard using the drop downs below and switch between different school place metrics using the tabs on the underneath."
                ),
              )
            ),
            gov_row(
              column(
                width = 6,
                selectInput(
                  "LA_choice",
                  label = p(strong("Choose a local authority")),
                  choices = levels(LA_options),
                  selected = "England"
                )
              ),
              column(
                width = 6,
                selectInput(
                  "phase_choice",
                  label = p(strong(
                    "Choose between primary or secondary school places"
                  )),
                  choices = c("Primary", "Secondary"),
                  selected = "Primary"
                )
              )
            ),
            gov_row(
              column(
                width = 6,
                selectInput("year_choice",
                  label = p(strong("Choose year")),
                  choices = unique(scorecards_data$Year),
                  selected = max(scorecards_data$Year)
                )
              )
            ),
            gov_row(
              column(
                width = 6,
                shinyGovstyle::download_button(
                  "download_ud",
                  "Download data for all LAs",
                  file_type = "CSV",
                  file_size = "155 KB"
                )
              )
              # column(
              #  width = 6,
              #  p(strong("Download summary pdf for chosen local authority", style = "color:white")),
              #  myDownloadButton(
              #    "pdfDownload",
              #    "Download report"
              #  )
              # )
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
                    12,
                    uiOutput("quantity.bartext"),
                    br()
                  )
                ),
                gov_row(
                  column(
                    6,
                    p(strong(uiOutput("quantity_plot_title"))),
                    conditionalPanel(
                      condition = "input.LA_choice != 'England'",
                      selectizeInput(
                        "selectBenchLAs",
                        "Select up to two benchmark LAs in box below",
                        choices = levels(LA_benchmark_options),
                        multiple = TRUE,
                        options = list(maxItems = 2)
                      )
                    ),
                    plotlyOutput("places_chart"),
                    plotlyOutput("places_chart_england")
                  ),
                  column(
                    6,
                    gov_row(
                      column(
                        12,
                        strong(textOutput("pupil_subtitle")),
                        br(),
                        valueBoxOutput("pupil_growth", width = 6)
                      )
                    ),
                    gov_row(
                      column(
                        12,
                        textOutput("pupil_growth_breakpoint_text"),
                        br(),
                        valueBoxOutput("pupil_growth_10_to_break", width = 6),
                        valueBoxOutput(
                          "pupil_growth_break_to_current",
                          width = 6
                        )
                      )
                    ),
                    gov_row(
                      column(
                        12,
                        valueBoxOutput("pupil_anticipated_growth", width = 6)
                      )
                    )
                  )
                ),
                gov_row(
                  column(
                    12,
                    br(),
                  )
                ),
                gov_row(
                  column(
                    12,
                    strong(textOutput("quantitysubtitle")),
                    p(
                      "A local authority can have both ‘spare places’ and ‘additional places needed’ due to localised or specific year group demand"
                    ),
                    valueBoxOutput("estimated_additional_places", width = 3),
                    valueBoxOutput("current_unfilled_places", width = 3),
                    valueBoxOutput("estimated_spare_places", width = 3),
                    valueBoxOutput("total_funding_box", width = 3)
                    # uiOutput("quantity.bartext")
                  )
                ),
                gov_row(
                  column(
                    12,
                    br(),
                  )
                ),
                gov_row(
                  column(
                    12,
                    dataTableOutput("quantity_table")
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
                    p(strong(uiOutput("forecast_title"))),
                    uiOutput("forecasting.bartext"),
                    p(
                      "The tables show forecast accuracy measures for all Local Authorities. Of these, only the 25th and 75th percentiles are shown in the charts."
                    ),
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
                      htmlOutput("label_estimate_y3"),
                      plotlyOutput("forecast_3y_bar", height = "120px"),
                      br(),
                      p("Three years ahead: range of forecast accuracy scores"),
                      dataTableOutput("for3year_table")
                    )
                  ),
                )
              )
            ),
            tabPanel(
              value = "preference",
              title = "Preference",
              gov_main_layout(
                gov_row(
                  column(
                    width = 6,
                    strong(textOutput("preference_eng_subtitle")),
                    # preference content to go here
                    br(),
                    valueBoxOutput("prefT3_NY_ENG", width = 6),
                    valueBoxOutput("prefT3_CY_ENG", width = 6)
                  ),
                  column(
                    width = 6,
                    strong(textOutput("preference_la_subtitle")),
                    # preference content to go here
                    br(),
                    valueBoxOutput("PrefT3_NY_LA", width = 6),
                    valueBoxOutput("PrefT3_CY_LA", width = 6)
                  )
                ),
                gov_row(
                  column(
                    12,
                    p(strong(paste0(
                      "Proportion of applicants who received an offer of a school place in their first, second and third preferences"
                    ))),
                    conditionalPanel(
                      condition = "input.LA_choice != 'England'",
                      selectizeInput(
                        "selectBenchLAspref",
                        "Select England or a LA to benchmark",
                        choices = levels(LA_benchmark_options_pref),
                        multiple = TRUE,
                        options = list(maxItems = 1)
                      )
                    ),
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
                    selectInput(
                      "chart_choice",
                      label = p(strong("Choose a quality measure")),
                      choices = c(
                        "Ofsted Rating",
                        "Progress 8",
                        "Reading Progress",
                        "Maths Progress"
                      ),
                      selected = "Ofsted Rating"
                    ),
                    br(),
                    strong(textOutput("quality_description")),
                    br(),
                    valueBoxOutput("England_GO_places", width = 6),
                    valueBoxOutput("LA_GO_places", width = 6)
                    # valueBoxOutput("LA_GO_ran", width = 4)
                  )
                ),
                gov_row(
                  column(
                    12,
                    p(strong(paste0(
                      "Number of new places created in schools of each category and number of existing school places in each category"
                    ))),
                    conditionalPanel(
                      condition = "input.LA_choice != 'England'",
                      selectizeInput(
                        "selectBenchLAsquality",
                        "Select England or a LA to benchmark",
                        choices = levels(LA_benchmark_options_pref),
                        multiple = TRUE,
                        options = list(maxItems = 1)
                      )
                    ),
                    plotlyOutput("quality_chart") %>% withSpinner()
                  )
                ),
                textOutput("no_rating_line"),
                br(),
                p(em(
                  "Caution should be taken with quality data as many Ofsted inspections have been delayed due to Covid-19."
                ))
              )
            ),
            tabPanel(
              value = "cost",
              title = "Cost",
              gov_main_layout(
                gov_row(
                  column(
                    width = 12,
                    p(strong(
                      "Average cost of additional mainstream school places"
                    )),
                    p(textOutput("cost_title")),
                    p(em(
                      "Local authority average costs are not shown due to incomplete coverage and completeness. Only national average costs and number of projects for England are shown."
                    ))
                  )
                ),
                gov_row(
                  column(
                    width = 12,
                    valueBoxOutput("perm_box", width = 4),
                    valueBoxOutput("temp_box", width = 4),
                    valueBoxOutput("new_box", width = 4),
                  ),
                  p(strong(
                    "Average cost per place for permanent, temporary and new school projects"
                  )),
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
    h1("Technical notes"),
    p(
      "Use this dashboard to view school places scorecards for local authorities in England"
    ),
    p(
      "All dates refer to the academic year, apart from basic need funding years which refer to the financial year."
    ),
    br(),
    selectInput("technical_year", "Select Year:", choices = unique(notesTableQuant$Publication), selected = max(notesTableQuant$Publication)),
    tabBox(
      title = "",
      id = "tabs_tech_notes",
      width = "12",
      tabPanel(
        "Quantity",
        uiOutput("notesTableQuant") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        "Pupil forecast accuracy",
        uiOutput("notesTableforacc") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        "Preference",
        uiOutput("notesTablePref") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        "Quality",
        uiOutput("notesTableQual") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        value = "cost",
        title = "Cost",
        uiOutput("notesTableCost") # made in global.R file
      ) # end of tabPanel
    ) # end of tabBox
  )
}
