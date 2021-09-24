source("0_variable_change.R")##


    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "shiny_gov_style.css")
)
    
#sidebar---------------------
sidebar<- dashboardSidebar(
        sidebarMenu(
        selectInput("LA_choice",
                    label = p(strong("Choose a geography")),
                    choices = levels(LA_options)),
        br(),
        selectInput("phase_choice",
                    label = p(strong("Choose a phase")),
                    choices = c("Primary","Secondary")),
        br(),
        selectInput("chart_choice",
                    label = p(strong("Choose a quality measure")),
                    choices =  c("Ofsted","Reading Progress", "Maths Progress")
        ),
        menuItem("LA scorecards", tabName = "scorecards", icon = icon("fas fa-bookmark")),
        menuItem("Technical notes", tabName = "cover", icon = icon("book-open"))

       

    )# end of sidebar menu
    )
    
    
 #body--------------------   
        body<-dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "shiny_gov_style.css")
            ),
            useShinyjs(),
           # theme = "shiny_gov_style.css",
            #tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
          
            
            tabItems(
            # scorecards tab --------------------------------------------------------------
            
            tabItem(tabName = "scorecards",
                    
                    
                    h2(textOutput("data_description")),
                    fluidRow(
                        valueBoxOutput("total_funding_box", width = 6),
                        valueBoxOutput("pupil_growth", width = 6)
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
                                            plotlyOutput("places_chart")%>% withSpinner()),
                                     column(6,
                                            p(strong("Forecast accuracy - values closer to 0 are more accurate.")),
                                            htmlOutput("label_estimate_y1"),
                                            br(),
                                            gaugeOutput("forecast_1y"),
                                            htmlOutput("label_estimate_y3"),
                                            br(),
                                            gaugeOutput("forecast_3y")
                                                   
                                            ))),
                        tabPanel("Preference", 
                                 p(strong(paste0("Proportion of applicants who received an offer of one of their top three preferences for September ", preference_year," entry"))),
                                 #preference content to go here
                                 valueBoxOutput("prefT3_ENG", width = 6),
                                 valueBoxOutput("PrefT3_LA", width = 6),
                                 
                                 fluidRow(
                                     column(12,
                                            plotlyOutput("preference_p")%>% withSpinner())
                                 ))     
                        
                        ,
                        tabPanel("Quality", 
                                 p(strong(paste0("Quality of places created between ", last_year," and ",this_year))),
                                 valueBoxOutput("England_GO_places", width = 4),
                                 valueBoxOutput("LA_GO_places",width = 4),
                                 valueBoxOutput("LA_GO_ran",width = 4),
                                 fluidRow(
                                     column(12,
                                            plotlyOutput("quality_chart")%>% withSpinner())
                                 ),
                                 textOutput("no_rating_line")
                                 
                        ),
                        tabPanel("Cost", 
                                 p(strong(paste0("Local authority reported projects between ", last_year_1," and ", last_year,", adjusted for inflation and regional variation"))),
                                 p("(Not new data: see technical notes)"),
                                 valueBoxOutput("perm_box", width = 4),
                                 valueBoxOutput("temp_box", width = 4),
                                 valueBoxOutput("new_box", width = 4),
                                 fluidRow(
                                     column(6,
                                            plotlyOutput("cost_plot")%>% withSpinner()),
                                     column(3,
                                            tableOutput("cost_table")))
                                 #Cost content to go here
                                 
                        )
                        
                        
                        
                        
                        
                    ),#end of tabset
                    
                    br(),
                    br(),
                    img(src = "Department_for_Education.png", height = 100, width = 150),
                    br(),
                    br(),
                    p("This is a development of our excel-based scorecards - if you would like to provide feedback on this tool please contact ",a(href="mailto:SCAP.PPP@education.gov.uk","SCAP.PPP@education.gov.uk"))
                    
                    
            ),#end of tab item
            
 # technical notes --------------------------------------------------------------

                
                tabItem(tabName = "cover",
                        h2("Technical notes"),
                        br("Use this dashboard to view school places scorecards for local authorities in England"),
                        br("All dates refer to the academic year."),
                        br("There is no scorecard for Dorset (838) and Bournemouth, Christchurch and Poole (839) as they are new local authorities, following changes to LA boundaries in this region in April 2019. As these two new local authorities are not directly comparable with their pre LGR 2019 local authorities, we were unable to produce complete figures for the majority of individual indictors included in the School Places Scorecard,
                           however the relevant data for these pre and post LGR 2019 local authorities are included in the England data and Summary data tabs."),
                        br(),
                        
                        tabBox(
                            title = "",        
                            id = "tabs", width ="12",
                            tabPanel("Overall", 
                                     tableOutput("notesTable") # made in global.R file
                                     
                            ),#end of tabPanel
                            tabPanel("Quantity", 
                                     tableOutput("notesTableQuant")  # made in global.R file
                                     
                            ),#end of tabPanel
                            tabPanel("Preference", 
                                     tableOutput("notesTablePref")  # made in global.R file
                                     
                            ),#end of tabPanel
                            tabPanel("Quality", 
                                     tableOutput("notesTableQual")  # made in global.R file
                                     
                            ),#end of tabPanel
                            tabPanel("Cost", 
                                     tableOutput("notesTableCost")  # made in global.R file
                                     
                            )#end of tabPanel
                            
                            
                        ),#end of tabBox

                        
br(),
br(),
img(src = "Department_for_Education.png", height = 100, width = 150),
br(),
br(),
p("This is a development of our excel-based scorecards - if you would like to provide feedback on this tool please contact ",a(href="mailto:SCAP.PPP@education.gov.uk","SCAP.PPP@education.gov.uk"))

                ) #end of tab item


)#end of tab items
)#end of body
        
#build page -------------------

dashboardPage(
    
    dashboardHeader(title = "LA scorecards"),
    sidebar,
    body
    
    )
