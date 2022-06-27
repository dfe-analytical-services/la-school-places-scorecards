fluidPage(
  useShinyjs(),
  useShinydashboard(),
  includeCSS("www/dfe_shiny_gov_style.css"),
  title = "LA School Places Scorecards",
  # use_tota11y(), # accessibility layer for local testing

  # Set metadata for browser ==================================================

  tags$html(lang = "en"),
  meta_general(
    application_name = "School places scorecards",
    description = "Scorecards for school places by local authority in England",
    robots = "index,follow",
    generator = "R-Shiny",
    subject = "School places in England",
    rating = "General",
    referrer = "no-referrer"
  ),

  # Set title for search engines
  HTML("<title>LA School Places Scorecards</title>"),

  # Navbar ====================================================================

  tagList(
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(5) {
                           float: right;
                           }
                           ")))
  ),
  navbarPage(
    id = "navbar",
    title = "",
    tabPanel(
      "Homepage",
      h2("Local authority school places scorecards: academic year 2020/21"),
      br("Scorecards display a snapshot of the progress each local authority across England is making towards ensuring there are sufficient, good quality, school places."),
      br("You can view the progress for England as a whole or for an individual local authority. 
         To do this click on ‘LA scorecards’ at the top, after reading all the information on this homepage. 
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
          br("The chart shows"),
          br("•the places already added since the academic year 2009/10;"), 
          "•the additional places planned to be added up to the 2023/24 academic year; and", 
          br("•the estimated number of additional places still needed to meet demand in 2023/24, based on the local authority’s forecast demand."), 
          br("The four headline boxes show:"),
          br(" •	the estimated number of additional places still needed to meet demand in 2023/24, based on the local authority’s forecast demand (same figure shown in the chart);"), 
          " •	the estimated percentage of spare places. It is common for a local authority to have both a need for additional places and spare capacity, 
             reflecting pockets of localised need for places or pockets of localised spare places;",
          br(" •	the total amount of basic need capital funding allocated to each local authority to create new places from 2011 to 2024; and"),
          " •	the anticipated percentage increase in pupil numbers in primary or secondary provision between 2009/10 to 2023/24.", 
          "This is important context when looking at places that have been and are still to be delivered.",  
          br(),
         br("It is important to take care when making comparisons using the quantity measure. Some local authorities have long-standing place pressure, 
            whereas for others it has emerged more recently. Those experiencing long-standing place pressures will have had more chance to demonstrate 
            that they can add large quantities of places."),
           br(" Local authorities with relatively fewer places still needed are making good progress in delivering the places they anticipate are needed,
              however they may be good reasons for a local authority showing a significant number of places still needed to be created 
              (such as a new school due to open in September 2023, which is not included in these calculations, 
              or projects that have since been planned that were not in the latest school capacity survey."),
                 
        br(),
          h4(actionLink("linkForecastTab", "Forecast Accuracy")),
       
          br(" Estimating place pressure in future years relies on the forecasts of pupil numbers made by local authorities, as provided in the annual school capacity survey. 
             The scorecard illustrates the forecasting accuracy of the selected local authority for forecasts made one year ago and provided in SCAP21, and three years ago in SCAP19.  
             These pupil forecasts have been compared with pupils on roll from the January 2022 school census numbers to produce a forecast accuracy score for one and three years ahead.
             A negative forecast accuracy score indicates an underestimation of pupils, a positive score indicates an overestimation."),
          br("   Many factors have impacted pupil numbers in January 2022, due to causes such as the pandemic, the UK’s exit from the EU,
             and arrivals in the UK from places such as Hong Kong and Afghanistan (arrivals from Ukraine will not yet be in the school census data).
             Local authorities would have taken account of these factors in their forecasts for SCAP21 as far as possible, some with more certainty than others.  Some factors affecting both pupil numbers in January 2022 and SCAP21 forecasts are:"),
          br("•	faster than expected declines in birth rates;"), 
          "•	decrease in 2021 primary enrolment numbers that is expected to be temporary;",
         br( "•	delays to housing developments in some areas; and"),
        "•	faster than expected declines in birth rates.",
        br(),
       br("Caution should also be taken with three year ahead accuracy, as forecasts were made in summer 2019, before the Covid pandemic and prior to the unforeseen faster decline in birth rates. Because of this, there was a significant shift in forecast trends between SCAP19 and SCAP21, so you would expect there to be more divergence than usual  between the 1 year ahead score and the 3 year ahead score."),
           br(),
          h4(actionLink("linkPreferenceTab", "Preference")),
          br("You can use the scorecard to see how well the chosen local authority is able to meet parental preference, 
             based on published",  a(href = "https://www.gov.uk/government/statistics/secondary-and-primary-school-applications-and-offers-2021",  "school applications and offers 2021 data."), 
          "The headline box shows the percentage of applicants who received an offer of a place in one of their top three preferences for entry in September 2021, in the local authority. 
             The adjacent headline box shows the same percentage for England."),
          br("The chart breaks this down further into to those who received an offer of their first, second or third preferences. 
             ‘Other’ offers include  pupils who received an offer of a lower preference (where a local authority allows four or more preferences) or did not receive a preferred offer. 
             The latter can include applicants who were made an alternative offer and those who were not made any offer."),
          br(),
          h4(actionLink("linkQualityTab", "Quality")),
          br("You can use this measure to see the quality of the schools where the chosen local authority has added school places, based on published school capacity data for 2018/19 and 2020/21 and school Ofsted rating."),
          br("The chart shows the number of new places added in the local authority, between May 2019 and May 2021, according to the Ofsted rating of the school in which they have been added. There are 4 possible Ofsted ratings: outstanding, good, requires improvement and inadequate."),
          br("It is important to take care when making comparisons using the quality measure as:"),
       "•	Ofsted inspections may have been delayed due to the pandemic, therefore local authorities may use other data/own local knowledge when deciding which schools to expand;",
          br("•	The starting position for local authorities is different and some have more good or outstanding schools to add places to - which is why the overall distribution of existing school places by Ofsted rating in the local authority chosen is shown;"),
          "•	When deciding upon which schools to expand, some good or outstanding schools may be on sites that are not suitable for expansion;",
          br("•	The Ofsted rating used was available at 31 August 2021 and places may have been added before or after that rating was given; and "),
          "•	Where schools have amalgamated, the Ofsted rating is only used when we can be sure the rating is for the post-amalgamation school.",
          br(),
       br(),
          h4(actionLink("linkCostTab", "Cost")),
          br("The Capital Spend Survey that replaced the SCAP Capital Spend Data now collects data on project costs, however due to incomplete coverage it has not yet been incorporated into the scorecard.  The cost data used in the scorecard remains the Capital Spend Data from SCAP18. This was used in the 2018 and 2019 scorecards – there has been no change in the sample of projects. As done in 2019, for the 2021 Scorecard, the data has been adjusted for inflation (uprated to 1st quarter 2022 prices – see technical notes for details)."),
          br("You can use the scorecard to view the national average cost per place for both primary and secondary school places. These are shown for permanent expansions, temporary expansions and new schools separately. You can also view, for the first time, the national averages adjusted for 2021 regional location factors. "),
          br("Because the sample of projects are from 2018, the local authority average costs are not shown in the 2021 scorecard. However, you can find these in the underlying data if needed."),
          br("There is further guidance on converting costs into current or future prices in the scorecard ‘technical notes’.")
                 )
      )
    ),
    # scorecard---------------------------------------------------------------------
    tabPanel(
      value = "la_scorecards",
      title = "LA scorecards",
      # Sidebar---------------------------------------------------------------------
      sidebarLayout(
        sidebarPanel(
          width = 2,
          p("View a local authority school places scorecard using the drop downs below."),
          p("Switch between different school place metrics using the tabs on the right."),
          selectInput("LA_choice",
            label = p(strong("Choose a local authority")),
            choices = levels(LA_options),
            selected = "Darlington"
          ),
          br(),
          selectInput("phase_choice",
            label = p(strong("Choose between primary or secondary school places")),
            choices = c("Primary", "Secondary"),
            selected = "Primary"
          ),
          #  br(),
          #  selectInput("chart_choice",
          #   label = p(strong("Choose a quality measure")),
          #  choices = c("Ofsted", "Reading Progress", "Maths Progress")
          #  ),
          br(),
          br(),
          p(strong("Download data for all local authorities", style = "color:white")),
          myDownloadButton(
            "download_ud",
            "Download data"
          ),
          br(),
          br(),
          p(strong("Download summary pdf for chosen local authority", style = "color:white")),
          myDownloadButton(
            "pdfDownload",
            "Download report"
          )
        ), # end of panel

        # fluidRow(
        # valueBoxOutput("total_funding_box", width = 6),
        # valueBoxOutput("pupil_growth", width = 6)
        # ),


        # Create the main content-----------------
        mainPanel(
          width = 10,
          h2(textOutput("data_description")),
          br(),
          tabsetPanel(
            id = "tabs",
            tabPanel(
              "Quantity",
              fluidPage(
                fluidRow(
                  column(width = 12, br()),
                  column(
                    6,
                    p(strong("School places created, planned future places, additional places still needed, as at May", SCAP_ref)),
                    plotlyOutput("places_chart") %>% withSpinner()
                  ),
                  column(
                    6,
                    fluidRow(
                      column(
                        12,
                        p(strong(paste0("Estimated future school place demand"))),
                        p("A local authority can have both ‘spare places’ and ‘additional places needed’ due to localised or specific year group demand"),
                        valueBoxOutput("estimated_additional_places", width = 6),
                        valueBoxOutput("estimated_spare_places", width = 6)
                      )
                    ),
                    fluidRow(
                      column(
                        12,
                       )
                    ),
                    fluidRow(
                      column(
                        12,
                        valueBoxOutput("total_funding_box", width = 6),
                        valueBoxOutput("pupil_growth", width = 6)
                      )
                    )
                  )
                ),
                uiOutput("quantity.bartext")
              )
            ),
            tabPanel(
              value = "forecast",
              title = "Pupil forecast accuracy",
              fluidPage(
                fluidRow(
                  br(),
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
                p(em("Caution should be taken with forecast accuracy scores due to unforseen circumstances. See Homepage for more information.")),
              )
            ),
            tabPanel(
              value = "preference",
              title = "Preference",
              fluidPage(
                fluidRow(
                  br(),
                  p(strong(paste0("School applications and offers for September ", preference_year, " entry"))),
                  # preference content to go here
                  valueBoxOutput("prefT3_ENG", width = 6),
                  valueBoxOutput("PrefT3_LA", width = 6)
                ),
                fluidRow(
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
              fluidPage(
                fluidRow(
                  br(),
                  strong(textOutput("quality_description")),
                  br(),
                  valueBoxOutput("England_GO_places", width = 4),
                  valueBoxOutput("LA_GO_places", width = 4),
                  valueBoxOutput("LA_GO_ran", width = 4)
                ),
                fluidRow(
                  column(
                    12,
                    p(strong(paste0("Number of new places created in schools of each category and number of existing school places in each category"))),
                    plotlyOutput("quality_chart") %>% withSpinner()
                  )
                ),
                textOutput("no_rating_line"),
                br(),
                p(em("Caution should be taken with quality data as Ofsted inspections may have been delayed due to Covid-19."))
              )
            ),
            tabPanel(
              value = "cost",
              title = "Cost",
              fluidPage(
                fluidRow(
                  br(),
                  p(strong("Average cost of additional mainstream school places")),
                  p("Based on local authority reported projects between ", cost_year_1, " and ", cost_year_2, ", adjusted for inflation and regional variation"),
                  p("Local authority average costs are not shown because there is no new data. Only national average costs and number of projects for England are shown.")
                ),
                fluidRow(
                  valueBoxOutput("perm_box", width = 4),
                  valueBoxOutput("temp_box", width = 4),
                  valueBoxOutput("new_box", width = 4),
                ),
                p(strong("Average cost per place for permanent, temporary and new school projects")),
                uiOutput("cost.bartext"),
                br(),
                fluidRow(
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
    ),
    # Create the tech notes-----------------
    tabPanel(
      "Technical notes",
      meta_general(
        application_name = "LA scorecards",
        description = "Scorecards for school places by local authority in England",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "School places in England",
        rating = "General",
        referrer = "no-referrer"
      ),
      h2("Technical notes"),
      br("Use this dashboard to view school places scorecards for local authorities in England"),
      br("All dates refer to the academic year, apart from basic need funding years which refer to financial year."),
      br("There is no scorecard for North Northamptonshire and West Northamptonshire as they are new local authorities, following the split from one local authority into two in April 2021. As these two new local authorities are not directly comparable with the pre LGR 2021 Northamptonshire, we were unable to produce complete figures for the majority of individual indictors included in the School Places Scorecard,
                           however the relevant data for these pre and post LGR 2021 local authorities are included in the England data and Summary data tabs on explore education statistics."),
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
          "Cost",
          tableOutput("notesTableCost") # made in global.R file
        ) # end of tabPanel
      ) # end of tabBox
    ),

    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      meta_general(
        application_name = "LA scorecards",
        description = "Scorecards for school places by local authority in England",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "School places in England",
        rating = "General",
        referrer = "no-referrer"
      ),
      h2("Accessibility statement"),
      br("This accessibility statement applies to the Local Authority (LA) scorecards application.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
      h3("WCAG 2.1 compliance"),
      br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
      tags$div(tags$ul(
        tags$li("uses colours that have sufficient contrast"),
        tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
        tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
      )),
      h3("Limitations"),
      br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
      tags$div(tags$ul(
        tags$li("Keyboard navigation through the interactive charts is currently limited"),
        tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
      )),
      h3("Feedback"),
      br(
        "If you have any feedback on how we could further improve the accessibility of this application, please contact ",
        a(href = "mailto:SCAP.PPP@education.gov.uk", "SCAP.PPP@education.gov.uk")
      )
    ),
    tabPanel(
      "Support & Feedback",
      div(
        h2("Give us feedback"),
        "This dashboard is hosted on a new platform that will continue to be developed. If you have any feedback or suggestions for improvements, please submit them using this ",
        a(
          href = "https://forms.office.com/r/wEWr5KCrTQ",
          "feedback form", .noWS = c("after")
        ), ".", br(),
        "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
        a(href = "mailto:scap.ppp@education.gov.uk", "scap.ppp@education.gov.uk", .noWS = c("after")), ".",
        br(),
        h2("Find more information on the data"),
        "The data used to produce the dashboard, along with methodological information can be found on ",
        a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics", .noWS = c("after")),
        ".",
        br(),
        h2("Contact us"),
        "If you have questions about the dashboard or data within it, please contact ",
        a(href = "mailto:scap.ppp@education.gov.uk", "scap.ppp@education.gov.uk", .noWS = c("after")),
        br(),
        h2("See the source code"),
        "The source code for this dashboard is available in our ",
        a(href = "https://github.com/dfe-analytical-services/la-school-places-scorecards", "GitHub repository", .noWS = c("after")),
        ".",
        br(),
        br(),
        br(),
        br(),
        br(),
        br()
      )
    ),
    shinyGovstyle::footer(TRUE)
  )
)
