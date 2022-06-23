
function(request) {
  source("0_variable_change.R") ##
  #Homepage----------------------------------------------------------------------
  navbarPage(
    id="navbar",
    title = "",
    tabPanel("Homepage",
    h2("Local authority school places scorecards academic year 2020/21"),
    br("Scorecards display a snapshot of the progress each local authority across England is making towards ensuring there are sufficient good quality school places."),
    br("You can use the school places scorecards to look at the school place situation for England or any individual local authority (in 2021 there is no scorecard for North Northamptonshire and West Northamptonshire due to their recent boundary changes, although you can find relevant data for these local authorities in the summary data ). To do this, use the drop-down option at the top left of the scorecard to select the national option or your chosen local authority. You can also type the name of a local authority to select it. You can look at primary or secondary places by using the adjacent drop-down option."),
    br("When a different local authority is selected, or the education phase is changed, the figures and charts in the scorecard will automatically update to reflect the chosen local authority and phase. This means that you can compare the position in selected local authorities by switching between authorities (they are not shown side by side)."),
    br("Each scorecard metric is shown on  a different tab, select each tab by clicking on the name of the metric near the top of the scorecard, to see the relevant figures and chart for the chosen metric. You can download a PDF version of the scorecard which shows all the metrics, by clicking the ‘download report’ button on the left. Use the ‘download data’ button on the left to download underlying data for all geographies and phases."),
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
      h3 ("School Place Metrics Included"),
       br(),
    h4(actionLink("linkQuantityTab", "Quantity")),
  #  h4("Quantity"),
    br("You can see how much progress the chosen local authority is making in providing sufficient school places by looking at the quantity measure. The quantity chart is based on published", a(href = "https://www.gov.uk/government/statistics/school-capacity-in-england-academic-year-2020-to-2021", "school capacity 20/21 data.")),
    br("The blue portion of the bar chart shows the places already added since the academic year 2009/10, the orange portion shows the places planned up to the 2023/24 academic year and the purple portion shows the estimated number of additional places needed to meet demand in the 2023/24 academic year (the number for this portion is also shown in the box next to  the bar chart). Local authorities with relatively small or no purple bars are making the best progress."),
    br("It is important to take care when making comparisons using the quantity measure. Some local authorities have long-standing place pressure, whereas for others it has emerged more recently. Those experiencing long-standing place pressures will have had more chance to demonstrate that they can add large quantities of places."),
    br("The estimated percentage of spare places is also shown in the blue box next to  the bar chart. It is common for a local authority to have both a need for additional places and spare capacity, reflecting pockets of localised need for places or pockets of localised spare places."),
    br("The basic need funding box shows the total amount of basic need capital funding allocated to each local authority to create new places from 2011 to 2024."),
    br("The percentage growth in pupil numbers box shows the anticipated percentage increase in pupil numbers in primary or secondary provision  between 2009/10 to 2023/24."),
    br(),
    h4(actionLink("linkForecastTab", "Forecast Accuracy")),
    br("Estimating place pressure in future years relies upon the forecasts of pupil numbers made by local authorities, provided in the annual school capacity survey. We have included two graphics, which illustrate the forecasting accuracy of the selected local authority for forecasts made one year ago in SCAP21 and three years ago in SCAP19.  These pupil forecasts have been compared with pupils on roll from the January 22 school census numbers to produce a forecast accuracy score for one and three years ahead. The bar shows underestimates for number of pupils  (a minus figure) and over estimates for numbers of pupils  (a positive figure) respectively. The bar extends to the position of the local authority’s forecast accuracy score in the range of all forecast accuracy scores"),
    br(),
  h4(actionLink("linkPreferenceTab","Preference")),
    br("You can use the scorecard to see how well the chosen local authority is able to meet parents’ school preferences, based on published school applications and offers 2021 data. The scorecard shows the percentage of applicants who received an offer of a place in one of their top three preferences for entry in September 2021, in the local authority. This is presented alongside the same percentage for England."),
    br("The chart breaks down the percentage of applicants who received an offer of one of their top three preferences to those who received an offer of their first, second or third preferences. The turquoise section represents the proportion of pupils made an offer of a lower preference (where a local authority allows four or more preferences) and the proportion not made a preferred offer. The latter can include applicants who were made an alternative offer and those who were not, made any offer."),
    br(),
  h4(actionLink("linkQualityTab","Quality")),
    br("You can check the quality measure to see where the chosen local authority has added school places, based on published school capacity 20/21 data and school Ofsted rating"),
    br("The bar chart shows the number of new places added in the local authority, between May 2019 and May 2021, according to the Ofsted rating of the school in which they have been added. There are 4 possible Ofsted ratings: outstanding, good, requires improvement and inadequate."),
    br("It is important to take care when making comparisons using the quality measure as:"),
    ("•	The starting position for local authorities is different and some have more good or outstanding schools to add places to - which is why we also provide the overall distribution of school places by Ofsted rating in the local authority chosen, and for England"),
    br("•	When deciding upon which schools to expand, some good or outstanding schools may be on sites that are not suitable for expansion"),
    ("•	We have used the Ofsted rating available at 31 August 2021 and places may have been added before or after that rating was given"),
    br("•	Where schools have amalgamated, we have only used an Ofsted rating when we can be sure the rating is for the post-amalgamation school."),
    br(),
  h4(actionLink("linkCostTab","Cost Metric")),
    br("No cost data was collected in 2021 as the Capital Spend data collection was removed from the SCAP survey pending the introduction of the Capital Spend Survey. The most recent cost data available is the 2018 Capital Spend data as used in the 2018 Scorecard. For the 2021 Scorecard, this data has been adjusted for inflation (rebased to 1st quarter 2022 prices)."),
    br("You can use the scorecard to see whether the average amount spent on each school place is relatively high or low compared with other local authorities."),
    br("The types of projects local authorities reported in the academic years 2015/16, 2016/17 and 2017/18 have been split into permanent expansions, temporary expansions and new schools. They have been separated so that you can compare more similar groups of projects between local authorities."),
    br("Cost figures have been adjusted to take location factors and inflation into account when average cost per place is calculated. There is further guidance on converting costs into current or future prices and/or regional prices in the scorecard ‘technical notes’."),
    br("It is important to take care when making comparisons. Some local authorities have small numbers of projects to add places, so cost comparisons become very dependent upon the nature of individual projects. Some additional but limited benchmark information for similar capital programme schemes carried out by the DfE is available in the National School Delivery Cost Benchmarking study.")))),
    #scorecard---------------------------------------------------------------------
    tabPanel(
      value="la_scorecards",
      title="LA scorecards",
      includeCSS("www/dfe_shiny_gov_style.css"),
      useShinyjs(),
      useShinydashboard(),
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
     
       #Sidebar---------------------------------------------------------------------
      sidebarLayout(
        sidebarPanel(
          width = 2,
          p("View a LA School Places Scorecard using the drop downs below."),
          p("Switch between different school place metrics using the tabs on the right."),
          selectInput("LA_choice",
            label = p(strong("Choose a geography")),
            choices = levels(LA_options),
            selected = "Darlington"
          ),
          br(),
          selectInput("phase_choice",
            label = p(strong("Choose a phase")),
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
          p(strong("Download data for all geographies and phases using the button below",style = "color:white")),
          myDownloadButton(
            "download_ud",
            "Download data"
                      ),
        br(),
        br(),
        p(strong("Download summary pdf for the chosen geography and phase using the button below",style = "color:white")),
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
            fluidRow(
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
                    valueBoxOutput("estimated_additional_places", width =6),
                    valueBoxOutput("estimated_spare_places", width = 6)
                  )),
                fluidRow(
                  column(
                    12,
                    p(strong("Funding allocated for creation of new places, anticipated increase in pupils"))
                  )),
                fluidRow(
                  column(
                    12,
                    valueBoxOutput("total_funding_box", width = 6),
                    valueBoxOutput("pupil_growth", width = 6)
                  )))),
            uiOutput("quantity.bartext")
                         ),
                     tabPanel(
               value="forecast",
             title="Pupil forecast accuracy",
               fluidRow(
               p(strong("Forecast accuracy of pupil projections for", forecast_year, ", made one year and three years previously")),
               uiOutput("forecasting.bartext"),
               column(
                 6,
             
                # details(
                 #  inputId = "faccuracyhelp",
                 #  label = "How to benchmark using the charts",
                  # help_text = "
#The thick vertical line shows the chosen LA's
 # average forecasting accuracy, whilst the dashed lines show the
 # 25th and 75th percentiles across all LAs (i.e. half of all LAs were
  # found to have a forecasting accuracy falling between the two dashed lines)."),
                 htmlOutput("label_estimate_y1"),
                 plotlyOutput("forecast_1y_bar", height = "120px"),
                   br(),
               p("One year ahead: range of forecast accuracy scores"),
                  tableOutput("for1year_table"),
                 # plotlyOutput("forecast_3y_bar", height = "120px"),

               ),
               column(
                 6,
                 htmlOutput("label_estimate_y3"),
                   plotlyOutput("forecast_3y_bar", height = "120px"),
                 br(),
                 p("Three year ahead: range of forecast accuracy scores"),
             tableOutput("for3year_table"))),
           br(),
          p(em("Caution should be taken with forecast accuracy scores due to unforseen circumstances. See Homepage for more information.")),
             ),
            tabPanel(
              value="preference",
              title="Preference",
              p(strong(paste0("School applications and offers for September ", preference_year, " entry"))),
              # preference content to go here
              valueBoxOutput("prefT3_ENG", width = 6),
              valueBoxOutput("PrefT3_LA", width = 6),
              fluidRow(
                column(
                  12,
                  p(strong(paste0("Proportion of applicants who received an offer of a school place in their first, second and third preferences"))),
                  plotlyOutput("preference_p") %>% withSpinner()
                )
              )
            ),
            tabPanel(
              value="quality",
              title="Quality",
              strong(textOutput("quality_description")),
              br(),
              valueBoxOutput("England_GO_places", width = 4),
              valueBoxOutput("LA_GO_places", width = 4),
              valueBoxOutput("LA_GO_ran", width = 4),
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
            ),
tabPanel(
    value="cost",
  title="Cost",
    p(strong("Average cost of additional mainstream school places")),
  p("Based on local authority reported projects between ", cost_year_1, " and ", cost_year_2, ", adjusted for inflation and regional variation"),
  p("Not new data: see technical notes"),
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
      br("All dates refer to the academic year."),
      br("There is no scorecard for Dorset (838) and Bournemouth, Christchurch and Poole (839) as they are new local authorities, following changes to LA boundaries in this region in April 2019. As these two new local authorities are not directly comparable with their pre LGR 2019 local authorities, we were unable to produce complete figures for the majority of individual indictors included in the School Places Scorecard,
                           however the relevant data for these pre and post LGR 2019 local authorities are included in the England data and Summary data tabs."),
      br("The source code for this application can be found in our ", a(href = "https://github.com/dfe-analytical-services/la-school-places-scorecards", "GitHub repository.")),
      br(),
      tabBox(
        title = "",
        id = "tabs_tech_notes", width = "12",
        tabPanel(
          "Overall",
          tableOutput("notesTable") # made in global.R file
        ), # end of tabPanel
        tabPanel(
          "Quantity",
          tableOutput("notesTableQuant") # made in global.R file
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
        "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
        a(href = "mailto:SCAP.PPP@education.gov.uk", "SCAP.PPP@education.gov.uk")
      )
    ),
tabPanel(
  "Support & Feedback"),
    shinyGovstyle::footer(TRUE)
  )
}
