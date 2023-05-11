panel_homepage <- function(){
tabPanel(
  "Homepage",
  h2("Local authority school places scorecards: academic year 2020/21"),
  br("Scorecards display a snapshot of the progress each local authority across England is making towards ensuring there are sufficient, good quality, school places."),
  br("You can view the progress for England as a whole or for an individual local authority.
         To do this click on", actionLink("linklascorecardsTab", "LA scorecards"), "after reading all the information on this homepage.
         Use the drop-down list at the top left of the ‘LA scorecards’, or type in the box, to select the national option or your chosen local authority.
         You can view primary or secondary places by using the drop-down further below. "),
  br("When a different local authority is selected, or the primary or secondary option is changed, the figures and charts in the scorecard will automatically update.
         This means that you can compare the position in selected local authorities by switching between authorities."),
  br("There are five scorecard metrics and more information on each metric can be found in the 'Scorecard Contents' section below.
      Each scorecard metric is shown on a different tab within 'LA scorecards' and can be selected by clicking on the name of the metric near the top of the scorecard."),
  br("You can download a PDF version of the complete scorecard, by clicking the ‘download report’ button on the left of the 'LA scorecards'. Use the ‘download data’ button on the left of the scorecard to
         download underlying data for all local authorities and England."),
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
      br("This measure shows how much progress the chosen local authority is making in providing sufficient school places for academic year 2023/24. This is based on published
             ", a(href = "https://www.gov.uk/government/statistics/school-capacity-in-england-academic-year-2020-to-2021", "school capacity 20/21 data"), "as collected in the 2021 the School Capacity Survey (SCAP21)."),
      br("The chart shows the:"),
      br("•places already added since the academic year 2009/10;"),
      "•additional places planned to be added up to the 2023/24 academic year; and",
      br("•estimated number of additional places still needed to meet demand in 2023/24, based on the local authority’s forecast demand."),
      br("The four headline boxes show the:"),
      br(" •	estimated number of additional places still needed to meet demand in 2023/24, based on the local authority’s forecast demand (same figure shown in the chart);"),
      " •	estimated percentage of spare places. It is common for a local authority to have both a need for additional places and spare capacity,
             reflecting pockets of localised need for places or pockets of localised spare places;",
      br(" •	the total amount of", a(href = "https://www.gov.uk/government/publications/basic-need-allocations", "basic need capital funding"), "allocated to each local authority to create new places from 2011 to 2024; and"),
      " •	the anticipated percentage increase in pupil numbers in primary or secondary provision between 2009/10 to 2023/24.",
      "This is important context when looking at places that have been and are still to be delivered.",
      br(),
      br("It is important to take care when making comparisons using the quantity measure. Some local authorities have long-standing place pressure,
            whereas for others it has emerged more recently. Those experiencing long-standing place pressures will have had more chance to demonstrate
            that they can add large quantities of places."),
      br(" Local authorities with relatively fewer places still needed are making good progress in delivering the places they anticipate are needed,
              however they may be good reasons for a local authority showing a significant number of places still needed to be created.
              For example, a new school due to open in September 2023 would not be included in these calculations,
              or projects which have only recently been planned and/or confirmed would not have been included in the latest school capacity survey."),
      br(),
      h4(actionLink("linkForecastTab", "Forecast Accuracy")),
      br(" Estimating place pressure in future years relies on the forecasts of pupil numbers made by local authorities, as provided in the annual school capacity survey.
             The scorecard illustrates the forecasting accuracy of the selected local authority for forecasts made one year ago and provided in SCAP21, and three years ago in SCAP19.
             These pupil forecasts have been compared with pupils on roll from the January 2022 school census numbers to produce a forecast accuracy score for one and three years ahead.
             A negative forecast accuracy score indicates an underestimation of pupils, a positive score indicates an overestimation."),
      br("   Many factors have impacted pupil numbers in January 2022.
             Local authorities would have taken account of these factors in their forecasts for SCAP21 as far as possible,
             some with more certainty than others. However, because local authorities submitted their forecasts in July 2021,
             there has been and/or will be significant changes in some areas due to the increased inward migration from Hong Kong,
             Afghanistan, Ukraine, and Asylum Seekers. This may affect one year ahead forecast accuracy scores for some local authorities
             and will mean the scorecards are not a representation of the current state of play. "),
      br("Some factors affecting both pupil numbers in January 2022 and SCAP21 forecasts are:"),
      br("•	impacts from the pandemic such as a decrease in 2021 primary enrolment numbers, which is expected to be temporary;"),
      "•	decrease in 2021 primary enrolment numbers that is expected to be temporary;",
      br("•	delays to housing developments in some areas; and"),
      "•	changes in trends in international migration and movement between areas of the country. ",
      br(),
      br("Caution should also be taken with three year ahead accuracy, as forecasts were submitted in summer 2019, before the Covid-19 pandemic and prior to the unforeseen faster decline in birth rates. Because of this, there was a significant shift in forecast trends between SCAP19 and SCAP21, so you would expect there to be more divergence than usual between the 1 year ahead score and the 3 year ahead score."),
      br(),
      h4(actionLink("linkPreferenceTab", "Preference")),
      br(
        "You can use the scorecard to see how well the chosen local authority is able to meet parental preference,
             based on published", a(href = "https://www.gov.uk/government/statistics/secondary-and-primary-school-applications-and-offers-2021", "school applications and offers 2021 data."),
        "The headline box shows the percentage of applicants, in the local authority, who received an offer of a place in one of their top three preferences for entry in September 2021.
             The adjacent headline box shows the same percentage for England."
      ),
      br("The chart breaks this down further into to those who received an offer of their first, second or third preferences.
             ‘Other’ offers include pupils who received an offer of a lower preference (where a local authority allows four or more preferences) or did not receive a preferred offer.
             The latter can include applicants who were made an alternative offer and those who were not made any offer."),
      br(),
      h4(actionLink("linkQualityTab", "Quality")),
      br("You can use this measure to see the quality of the schools where the chosen local authority has added school places, based on the change between the 2018/19 and the 2020/21 published school capacity data and", a(href = "https://www.gov.uk/government/statistics/state-funded-schools-inspections-and-outcomes-as-at-31-august-2021", "published school Ofsted rating."), ),
      br("The chart shows the number of new places added in the local authority, between May 2019 and May 2021, according to the Ofsted rating of the school in which they have been added. There are 4 possible Ofsted ratings: outstanding, good, requires improvement and inadequate."),
      br("It is important to take care when making comparisons using the quality measure as:"),
      br("•	many Ofsted inspections have been delayed due to the pandemic, therefore local authorities may use other data/own local knowledge when deciding which schools to expand;"),
      "•	the starting position for local authorities is different and some have more good or outstanding schools to add places to; which is why the overall distribution of existing school places by Ofsted rating in the local authority chosen is shown;",
      br("•	when deciding upon which schools to expand, some good or outstanding schools may be on sites that are not suitable for expansion;"),
      "•	the Ofsted rating used was available at 31 August 2021 and places may have been added before or after that rating was given; and ",
      br("•	where schools have amalgamated, the Ofsted rating is only used when we can be sure the rating is for the post-amalgamation school."),
      br(),
      h4(actionLink("linkCostTab", "Cost")),
      br("The Capital Spend Survey that replaced the SCAP Capital Spend Data now collects data on project costs, however due to incomplete coverage it has not yet been incorporated into the scorecard.  The cost data used in the scorecard remains the Capital Spend Data from SCAP18. This was used in the 2018 and 2019 scorecards; there has been no change in the sample of projects. As done in 2019, for the 2021 Scorecard, the data has been adjusted for inflation (uprated to 1st quarter 2022 prices)."),
      br(strong("You can use the scorecard to view the national average cost per place for both primary and secondary school places. These are shown for permanent expansions, temporary expansions and new schools separately. You can also view, for the first time, the national averages adjusted for 2021 regional location factors. ")),
      br("Because the sample of projects are from 2018, the local authority average costs are not shown in the 2021 scorecard. However, you can find these in the underlying data on explore education statistics if needed."),
      br("There is further guidance on converting costs into current or future prices in the scorecard ‘Technical notes’.")
    )
  )
)
}


# Scorecard panel code ----------------------------------------------------

panel_scorecard <- function(){
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
            width=12,
            p("View a local authority school places scorecard using the drop downs below and switch between different school place metrics using the tabs on the underneath."),
      )),
      gov_row(
      column(
        width=6,
      selectInput("LA_choice",
                  label = p(strong("Choose a local authority")),
                  choices = levels(LA_options),
                  selected = "England"
      )
      ),
      column(
        width=6,
        selectInput("phase_choice",
                  label = p(strong("Choose between primary or secondary school places")),
                  choices = c("Primary", "Secondary"),
                  selected = "Primary"
      )
      )),
      gov_row(
      column(
        width=6,
      p(strong("Download data for all local authorities", style = "color:white")),
      myDownloadButton(
        "download_ud",
        "Download data"
      )
      ),
      column(
        width=6,
      p(strong("Download summary pdf for chosen local authority", style = "color:white")),
      myDownloadButton(
        "pdfDownload",
        "Download report"
      )
      )
    ))
    )
    ), # end of panel
    
    # Create the main content-----------------
    gov_row(
      column(
        width=12,
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
                plotlyOutput("places_chart") %>% withSpinner()
              ),
              column(
                6,
                gov_row(
                  column(
                    12,
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
                    valueBoxOutput("pupil_growth", width = 6),
                    uiOutput("quantity.bartext")
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
                width=12,
              p(strong("Forecast accuracy of pupil projections for", forecast_year, ", made one year and three years previously")),
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
                htmlOutput("label_estimate_y3"),
                plotlyOutput("forecast_3y_bar", height = "120px"),
                br(),
                p("Three year ahead: range of forecast accuracy scores"),
                dataTableOutput("for3year_table")
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
                width=12,
              p(strong(paste0("School applications and offers for September ", preference_year, " entry"))),
              # preference content to go here
              valueBoxOutput("prefT3_ENG", width = 6),
              valueBoxOutput("PrefT3_LA", width = 6)
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
                width=12,
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
                width=12,
              p(strong("Average cost of additional mainstream school places")),
              p("Based on local authority reported projects between ", cost_year_1, " and ", cost_year_2, ", adjusted for inflation and regional variation"),
              p(em("Local authority average costs are not shown because there is no new data. Only national average costs and number of projects for England are shown."))
            )
            ),
            gov_row(
              column(
                width=12,
              valueBoxOutput("perm_box", width = 4),
              valueBoxOutput("temp_box", width = 4),
              valueBoxOutput("new_box", width = 4),
            ),
            p(strong("Average cost per place for permanent, temporary and new school projects")),
            uiOutput("cost.bartext"),
            br()),
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

panel_technical <- function(){
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