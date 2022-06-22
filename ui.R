
function(request) {
  source("0_variable_change.R") ##
  navbarPage(
    id="navbar",
    title = "",
    tabPanel("Homepage"),
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
          p("Download data for all geographies and phases using the button below."),
          myDownloadButton(
            "download_ud",
            "Download data"
                      ),
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
                         tabBox(
            title = "",
            id = "tabs", width = "12",
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
             "Pupil forecast accuracy",
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
              "Preference",
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
              "Quality",
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
  "Cost",
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
