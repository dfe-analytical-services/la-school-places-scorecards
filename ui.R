


dashboardPage(
    dashboardHeader(title = "LA scorecards"),
    dashboardSidebar(
        sidebarMenu(
        selectInput("LA_choice",
                    label = p(strong("Choose a geography")),
                    choices = unique(scorecards_data$LA_name)),
        br(),
        selectInput("phase_choice",
                    label = p(strong("Choose a phase")),
                    choices = c("Primary","Secondary"))

    )
    ),
    dashboardBody(
        h2(textOutput("data_description")),
           fluidRow(
               valueBoxOutput("total_funding_box"),
               valueBoxOutput("pupil_growth")
           ),
        br(),
        h3("QUANTITY"),
        fluidRow(
         column(6,
        plotlyOutput("places_chart")),
        column(6,
        p("Forecast accuracy one year ahead"),
        br(),
        gaugeOutput("forecast_1y")
        ))
        )
    )
