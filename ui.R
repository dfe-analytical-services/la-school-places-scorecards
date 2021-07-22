


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
        
        tabBox(
            title = "",        
            id = "tabs", width ="12",
            tabPanel("Quantity", 
               p(strong("Places created since 2009/10, places planned to 2021/22 and estimated place pressure in 2021/22")),
                     fluidRow(
                column(6,
                       plotlyOutput("places_chart")),
                column(6,
                       p("Forecast accuracy one year ahead"),
                       br(),
                       gaugeOutput("forecast_1y"),
                column(12,
                       p("Forecast accuracy three years ahead"),
                       br(),
                       gaugeOutput("forecast_3y"))
                ))),
            tabPanel("Preference", 
                     p(strong("Proportion of applicants who received an offer of one of their top three preferences for September 2019 entry")),
                     #preference content to go here
                     
                     ),
            tabPanel("Quality", 
                     p(strong("Quality of places created between 2017/18 and 2018/19")),
                     fluidRow(
                     column(12,
                            plotlyOutput("ofsted_chart")))
                     #Quality content to go here
                     
            ),
            tabPanel("Cost", 
                     p(strong("Local authority reported projects between 2015/16 and 2017/18, adjusted for inflation and regional variation")),
                     p("(Not new data: see technical notes)"),
                     fluidRow(
                         column(9,
                                plotlyOutput("cost_plot")),
                         column(3,
                                tableOutput("cost_table")))
                     #Cost content to go here
                     
            )
            
            
            
            
            
        )#end of tabset
        )#end of dashboard body
    )
