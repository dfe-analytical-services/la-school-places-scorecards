source("0_variable_change.R")


dashboardPage(
    dashboardHeader(title = "LA scorecards"),
    dashboardSidebar(
        sidebarMenu(
        selectInput("LA_choice",
                    label = p(strong("Choose a geography")),
                    choices = levels(LA_options)),
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
               p(strong(paste0("Places created since 2009/10, places planned to ", plan_year," and estimated place pressure in ",plan_year))),
               valueBoxOutput("estimated_additional_places", width = 6),
               valueBoxOutput("estimated_spare_places", width = 6),
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
                       gaugeOutput("forecast_3y")
                              

                )))),
            tabPanel("Preference", 
                     p(strong(paste0("Proportion of applicants who received an offer of one of their top three preferences for September ", preference_year," entry"))),
                     #preference content to go here
                     fluidRow(
                         valueBoxOutput("PrefT3_LA"),
                         valueBoxOutput("prefT3_ENG")
                     
                     )),
            tabPanel("Quality", 
                     p(strong(paste0("Quality of places created between ", last_year," and ",this_year))),
                     selectInput("chart_choice",
                                 label = p(strong("Choose a quality measure")),
                                 choices =  c("Ofsted","Reading Progress", "Maths Progress")
                     ),
                     fluidRow(
                     column(12,
                            plotlyOutput("quality_chart")%>% withSpinner())
                     ),
                     textOutput("no_rating_line")
                     column(12,
                            valueBoxOutput("LA_GO_places")),
                     column(12,
                            valueBoxOutput("England_GO_places")))
                     #Quality content to go here

                     
            ),
            tabPanel("Cost", 
                     p(strong(paste0("Local authority reported projects between ", last_year_1," and ", last_year,", adjusted for inflation and regional variation"))),
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
