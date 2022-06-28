
function(request) {
  source("0_variable_change.R") ##



  navbarPage(
    title = "School places scorecards",
    tags$head(includeHTML(("google-analytics.html"))),
    tabPanel(
      "LA scorecards",
      includeCSS("www/shiny_gov_style.css"),
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
          selectInput("LA_choice",
            label = p(strong("Choose a geography")),
            choices = levels(LA_options)
          ),
          br(),
          selectInput("phase_choice",
            label = p(strong("Choose a phase")),
            choices = c("Primary", "Secondary")
          ),
          br(),
          selectInput("chart_choice",
            label = p(strong("Choose a quality measure")),
            choices = c("Ofsted", "Reading Progress", "Maths Progress")
          ),
          br(),
          br(),
          myDownloadButton(
            "download_ud",
            "Download data"
          )
        ), # end of panel


        # Create the main content-----------------
        mainPanel(
          width = 10,
          h2(textOutput("data_description")),
          fluidRow(
            valueBoxOutput("total_funding_box", width = 6),
            valueBoxOutput("pupil_growth", width = 6)
          ),
          br(),
          tabBox(
            title = "",
            id = "tabs", width = "12",
            tabPanel(
              "Quantity",
              p(strong(paste0("Places created since 2009-10, places planned to ", plan_year, " and estimated place pressure in ", plan_year))),
              p("A local authority can have both ‘spare places’ and ‘additional places needed’ due to localised or specific year group demand"),
              valueBoxOutput("estimated_additional_places", width = 6),
              valueBoxOutput("estimated_spare_places", width = 6),
              fluidRow(
                column(
                  6,
                  plotlyOutput("places_chart") %>% withSpinner()
                ),
                column(
                  6,
                  p(strong("Forecast accuracy of pupil projections (values closer to 0 are more accurate)")),
                  htmlOutput("label_estimate_y1"),
                  br(),
                  uiOutput("forecast_1y_proxy"),
                  htmlOutput("label_estimate_y3"),
                  br(),
                  uiOutput("forecast_3y_proxy")
                )
              )
            ),
            tabPanel(
              "Preference",
              p(strong(paste0("Proportion of applicants who received an offer of one of their top three preference schools for September ", preference_year, " entry"))),
              # preference content to go here
              valueBoxOutput("prefT3_ENG", width = 6),
              valueBoxOutput("PrefT3_LA", width = 6),
              fluidRow(
                column(
                  12,
                  plotlyOutput("preference_p") %>% withSpinner()
                )
              )
            ),
            tabPanel(
              "Quality",
              p(strong(paste0("Quality of places created between ", last_year, " and ", this_year))),
              valueBoxOutput("England_GO_places", width = 4),
              valueBoxOutput("LA_GO_places", width = 4),
              valueBoxOutput("LA_GO_ran", width = 4),
              fluidRow(
                column(
                  12,
                  plotlyOutput("quality_chart") %>% withSpinner()
                )
              ),
              textOutput("no_rating_line")
            ),
            tabPanel(
              "Cost",
              p(strong(paste0("Average cost of additional mainstream place from local authority reported projects between ", last_year_1, " and ", last_year, ", adjusted for inflation and regional variation"))),
              p("(Not new data: see technical notes)"),
              valueBoxOutput("perm_box", width = 4),
              valueBoxOutput("temp_box", width = 4),
              valueBoxOutput("new_box", width = 4),
              fluidRow(
                column(
                  8,
                  plotlyOutput("cost_plot") %>% withSpinner()
                ),
                column(
                  4,
                  tableOutput("cost_table")
                )
              )
              # Cost content to go here
            )
          ), # end of tabset

          br(),
          br(),
          img(src = "dfe_logo.svg", height = 100, width = 150, alt = "Department for Education logo"),
          br(),
          br(),
          p("This is a development of our excel-based scorecards - if you would like to provide feedback on this dashboard, please complete our ", a(href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-fim2cxpeIFFus-loZuDRkhUN0ZPMlhCSDgzQjlVTjNVTFpXRTdWMlo0MC4u", "online survey"))
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
      ), # end of tabBox


      br(),
      br(),
      img(src = "dfe_logo.svg", height = 100, width = 150, alt = "Department for Education logo"),
      br(),
      br(),
      p("This is a development of our excel-based scorecards - if you would like to provide feedback on this dashboard, please complete our ", a(href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-fim2cxpeIFFus-loZuDRkhUN0ZPMlhCSDgzQjlVTjNVTFpXRTdWMlo0MC4u", "online survey"))
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
      ),
      br(),
      img(src = "dfe_logo.svg", height = 100, width = 150, alt = "Department for Education logo"),
      br(),
      br(),
      p("This is a development of our excel-based scorecards - if you would like to provide feedback on this dashboard, please complete our ", a(href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-fim2cxpeIFFus-loZuDRkhUN0ZPMlhCSDgzQjlVTjNVTFpXRTdWMlo0MC4u", "online survey"))
    )
  )
}
