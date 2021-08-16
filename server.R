function(input, output, session) {


# Data calculations - reactive --------------------------------------------

  # Scorecard data, filtered on user input
    live_scorecard_data  <- reactive({

        scorecards_data_pivot %>% filter(LA_name == input$LA_choice,
                                         Phase == input$phase_choice)
        
    })
    
    # Scorecard data, filtered on user input AND including England as a comparison
    live_scorecard_data_england_comp  <- reactive({
      
      scorecards_data_pivot %>% filter(LA_name %in% c(input$LA_choice, "England"),
                                       Phase == input$phase_choice) %>% 
        mutate(LA_name = as.factor(LA_name),
               # This step just makes sure that the LA is FIRST when it comes to plots/tables
               LA_name = relevel(LA_name, "England"),
               LA_name = factor(LA_name, levels=rev(levels(LA_name))))
      
    })
    
    # Scorecard data for ALL LAs, filtered only on phase choice
    live_scorecard_data_all_la  <- reactive({
      scorecards_data_pivot %>% filter(Phase == input$phase_choice) 
      
    })

    
        
# Top lines -------------------------
## create header so users know what the data is showing

output$data_description <- renderText({
    paste0("Data for ",str_to_lower(input$phase_choice), " schools in ", input$LA_choice, ": ")
})

## Total funding

output$total_funding_box <- renderValueBox({
    
    #Take data, get total funding and divide by billion if it's England, million if it's not
    total_funding <- scorecards_data %>% 
        filter(LA_name == input$LA_choice) %>% 
        select(Funding) %>% 
        mutate(Funding = 
                   ifelse(input$LA_choice =="England", roundFiveUp(Funding/1000000000,2), 
                          roundFiveUp(Funding/1000000,0))) %>% 
        as.numeric()
    
    #Create the actual output here. Use if statement so we display "bn" if it's England, "m" if not.
    if(input$LA_choice =="England") {
        shinydashboard::valueBox(
            paste0("£", total_funding, "bn"), 
            "Total primary and secondary basic need funding 2021-22",
            #get different icons for background here: https://fontawesome.com/v5.15/icons?d=gallery&p=2
            icon = icon("fas fa-pound-sign"),
            color = "purple"
        )   
    } else {
        shinydashboard::valueBox(
            paste0("£", total_funding, "m"),
            "Total primary and secondary basic need funding 2021-22",
            icon = icon("fas fa-pound-sign"),
            color = "purple"
        )
    }
})

## Growth in pupil numbers

output$pupil_growth <- renderValueBox({
    
    #Take filtered data, search for growth rate, pull the value and tidy the number up
  growth_perc <- live_scorecard_data() %>% 
      filter(name=="Bangro") %>% 
      pull(value) %>% 
      roundFiveUp(.,2)*100
    
  #Put value into box to plug into app
  shinydashboard::valueBox(
            paste0(growth_perc, "%"),
            paste0("Growth in ", str_to_lower(input$phase_choice)," pupil numbers 2009/10 to 2021/22"),
            icon = icon("fas fa-chart-line"),
            color = "blue"
        )
})


# Quantity ----------------------------------------------------------------

## Estimated additional places - use QUAN_P_RP and QUAN_S_RP

# Box to go here (use pupil growth as template) 

output$estimated_additional_places <- renderValueBox({

  #Take filtered data, search for growth rate, pull the value and tidy the number up
  additional_places_perc <- live_scorecard_data() %>%
    filter(name=="QuanRP") %>%
    pull(value)

  #Put value into box to plug into app
  shinydashboard::valueBox(
    paste0(additional_places_perc),
    paste0("Estimated number of additional " , str_to_lower(input$phase_choice), " places required to meet demand in 2021/22"),
    icon = icon("fas fa-signal"),
    color = "red"
  )
})

## Estimated spare places

# Box to go here (use pupil growth as template)

output$estimated_spare_places <- renderValueBox({

  #Take filtered data, search for growth rate, pull the value and tidy the number up
  spare_places_per <- live_scorecard_data() %>%
    filter(name=="QuanSu") %>%
    pull(value) %>%
    roundFiveUp(.,2)*100

  #Put value into box to plug into app
  shinydashboard::valueBox(
    paste0(spare_places_per, "%"),
    paste0("Estimated percentage of spare ", str_to_lower(input$phase_choice)," places in 2021/22"),
    icon = icon("fas fa-school"),
    color = "orange"
  )
})

## Places stacked bar

output$places_chart<- renderPlotly({
    
    #Take filtered data, filter for the variables we want to plot and pivot data round
    places_chart_data <- live_scorecard_data() %>% 
        filter(name %in% c("QuanIn","QuanPP","QuanRP")) %>% 
        select(LA_name,name,value) %>% 
        pivot_wider()
    
    #create interactive stacked bar chart
  plot_ly(
        places_chart_data, x = ~LA_name, y = ~QuanIn, type = 'bar', name = "Total places created between 2009/10 and 2018/19",
        text = ~QuanIn, textposition = 'inside') %>%
        add_trace(y = ~QuanPP, name = 'New places planned for delivery between 2019/20 and 2021/21', text = ~QuanPP, textposition = 'inside') %>% 
        add_trace(y = ~QuanRP, name = 'Estimated additional places needed to meet demand in 2021/22', text = ~QuanRP, textposition = 'inside') %>% 
        layout(yaxis = list(title = ''),
               xaxis = list(title =''),
               barmode = 'stack',
               uniformtext=list(minsize=12, mode='hide'),
               legend = list(orientation = 'h'))
    

})

## Forecast accuracy one year ahead

output$forecast_1y <- renderGauge({
    #live_scorecard_data<- scorecards_data_pivot %>% filter(LA_name =="Sheffield",Phase =="Secondary")
    
    forecast_accuracy <- live_scorecard_data() %>% 
        filter(name == "For_1") %>% 
        pull(value)%>% 
        roundFiveUp(.,3)*100
    
    lowest_accuracy <- scorecards_data_pivot %>% 
        filter(name == "For_1",
               Phase == input$phase_choice) %>% 
        slice(which.min(value)) %>% 
        pull(value)%>% 
        roundFiveUp(.,3)*100
        
    highest_accuracy <- scorecards_data_pivot %>% 
        filter(name == "For_1",
               Phase == input$phase_choice) %>% 
        slice(which.max(value)) %>% 
        pull(value)%>% 
        roundFiveUp(.,3)*100
    
    #Get medians/quartiles to set the sectors in the gauge
    mid_accuracy <-  median(c(highest_accuracy,lowest_accuracy))
    low_mid_accuracy <-  median(c(mid_accuracy,lowest_accuracy))
    high_mid_accuracy <-  median(c(mid_accuracy,highest_accuracy))
    mid_low_accuracy <- median(c(low_mid_accuracy, lowest_accuracy))
    mid_high_accuracy <- median(c(high_mid_accuracy, highest_accuracy))
        
    
    gauge(forecast_accuracy, 
          min = lowest_accuracy, 
          max = highest_accuracy, 
          symbol = '%',
          sectors = gaugeSectors(success = c(low_mid_accuracy, high_mid_accuracy), 
                                 warning = c(mid_low_accuracy, mid_high_accuracy),
                                 danger = c(lowest_accuracy, highest_accuracy)))
    
    
})

## Forecast accuracy three years ahead 

output$forecast_3y <- renderGauge({
  #live_scorecard_data<- scorecards_data_pivot %>% filter(LA_name =="Sheffield",Phase =="Secondary")
  

  forecast_accuracy <- live_scorecard_data() %>% 

    filter(name == "For_3") %>% 
    pull(value)%>% 
    roundFiveUp(.,3)*100
  

  lowest_accuracy <- scorecards_data_pivot %>% 

    filter(name == "For_3",
           Phase == input$phase_choice) %>% 
    slice(which.min(value)) %>% 
    pull(value)%>% 
    roundFiveUp(.,3)*100
  

  highest_accuracy <- scorecards_data_pivot %>% 

    filter(name == "For_3",
           Phase == input$phase_choice) %>% 
    slice(which.max(value)) %>% 
    pull(value)%>% 
    roundFiveUp(.,3)*100
  
  #Get medians/quartiles to set the sectors in the gauge

  mid_accuracy <-  median(c(highest_accuracy,lowest_accuracy))
  low_mid_accuracy <-  median(c(mid_accuracy,lowest_accuracy))
  high_mid_accuracy <-  median(c(mid_accuracy,highest_accuracy))
  
  
  gauge(forecast_accuracy, 
        min = lowest_accuracy, 
        max = highest_accuracy, 
        symbol = '%',
        sectors = gaugeSectors(success = c(high_mid_accuracy, highest_accuracy), 
                               warning = c(low_mid_accuracy, high_mid_accuracy),
                               danger = c(lowest_accuracy, low_mid_accuracy)))
  
  
})


#Code to go here using above template


output$forecast_3y <- renderGauge({
  #live_scorecard_data<- scorecards_data_pivot %>% filter(LA_name =="Sheffield",Phase =="Secondary")
  
  forecast_accuracy <- live_scorecard_data() %>% 
    filter(name == "For_3") %>% 
    pull(value)%>% 
    roundFiveUp(.,3)*100
  
  lowest_accuracy <- scorecards_data_pivot %>% 
    filter(name == "For_3",
           Phase == input$phase_choice) %>% 
    slice(which.min(value)) %>% 
    pull(value)%>% 
    roundFiveUp(.,3)*100
  
  highest_accuracy <- scorecards_data_pivot %>% 
    filter(name == "For_3",
           Phase == input$phase_choice) %>% 
    slice(which.max(value)) %>% 
    pull(value)%>% 
    roundFiveUp(.,3)*100
  
  #Get medians/quartiles to set the sectors in the gauge
  mid_accuracy <-  median(c(highest_accuracy,lowest_accuracy))
  low_mid_accuracy <-  median(c(mid_accuracy,lowest_accuracy))
  high_mid_accuracy <-  median(c(mid_accuracy,highest_accuracy))
  mid_low_accuracy <- median(c(low_mid_accuracy, lowest_accuracy))
  mid_high_accuracy <- median(c(high_mid_accuracy, highest_accuracy))
 
  
  
  gauge(forecast_accuracy, 
        min = lowest_accuracy, 
        max = highest_accuracy, 
        symbol = '%',
        sectors = gaugeSectors(success = c(low_mid_accuracy, high_mid_accuracy), 
                               warning = c(mid_low_accuracy, mid_high_accuracy),
                               danger = c(lowest_accuracy, highest_accuracy)))
  
  
})


# Preference -------------------------------------------------------------

# to fill in here - use the output$pupil_growth as a template :)

# Box for England % 

output$prefT3_ENG <- renderValueBox({

  #Take filtered data, search for growth rate, pull the value and tidy the number up
  PrefT3_E <- live_scorecard_data_all_la() %>%
    filter(name=="PrefT3") %>%
    filter(LA_name=="England") %>%
    pull(value) %>%
    roundFiveUp(.,2)

  #Put value into box to plug into app
  shinydashboard::valueBox(
    paste0(PrefT3_E, "%"),
    paste0("Percentage of ", str_to_lower(input$phase_choice)," pupils who recieved an offer of one of their top three preferences in England"),
    icon = icon("fas fa-chart-line"),
    color = "blue"
  )
})

# Box for LA % preference

output$PrefT3_LA <- renderValueBox({

  #Take filtered data, search for growth rate, pull the value and tidy the number up
  PrefT3 <- live_scorecard_data() %>%
    filter(name=="PrefT3") %>%
    pull(value) %>%
    roundFiveUp(.,2)

  if (input$LA_choice =="England") {
    shinydashboard::valueBox(
      paste0("-"),
      paste0("Percentage of ", str_to_lower(input$phase_choice)," pupils who recieved an offer of one of their top three preferences in England"),
      icon = icon("fas fa-sort-amount-up"),
      color = "green"
    )
    
  } else {
  
  #Put value into box to plug into app
  shinydashboard::valueBox(
    paste0(PrefT3, "%"),
    paste0("Percentage of ", str_to_lower(input$phase_choice)," pupils who recieved an offer of one of their top three preferences in " , str_to_lower(input$LA_choice)),
    icon = icon("fas fa-sort-amount-up"),
    color = "green"
  )}
})

# Stacked bar instead of pie here for preference? 
# Easier for users to interpret
# can use output$ofsted_chart further down for a template


# Quality -----------------------------------------------------------------

# box for % of new places in good and outstanding schools - LA

output$LA_GO_places <- renderValueBox({
  
  #Take filtered data, search for growth rate, pull the value and tidy the number up
  LA_GO_per <- live_scorecard_data() %>%
    filter(name=="QualProp") %>%
    pull(value) %>%
    roundFiveUp(.,2)*100
  
  #Put value into box to plug into app
  shinydashboard::valueBox(
    paste0(LA_GO_per, "%"),
    paste0("Percentage of new places in good and outstanding ", str_to_lower(input$phase_choice)," schools"),
    icon = icon("fas fa-boxes"),
    color = "aqua"
  )
})

# box for % of new places in good and outstanding schools - England

output$England_GO_places <- renderValueBox({
  
  #Take filtered data, search for growth rate, pull the value and tidy the number up
  England_GO_per <- live_scorecard_data_all_la() %>%
    filter(name=="QualProp") %>%
    filter(LA_name=="England") %>%
    pull(value) %>%
    roundFiveUp(.,2)*100
  
  #Put value into box to plug into app
  shinydashboard::valueBox(
    paste0(England_GO_per, "%"),
    paste0("Percentage of new places in good and outstanding ", str_to_lower(input$phase_choice)," schools"),
    icon = icon("fas fa-equals"),
    color = "maroon"
  )
})

# box for % of new places in good and outstanding schools - LA Ranking
# This one might take a bit more thinking but give it a go! 
# the arrange() function might come in handy here to rank data items
# the mutate() function will be needed to create a new column of rankings.


# Quality - charts --------------------------------------------------------


# Final step once charts are ready - making this bit reactive with a dropdown

# charts_reactive <- reactiveValues()

# Bar chart comparison - Ofsted

output$ofsted_chart<- renderPlotly({
  
  #reshape the data so it plots neatly!
ofsted_data <- live_scorecard_data_england_comp() %>% 
  #select only the ofsted values
  filter(name %in% c("Qual1_N","Qual2_N","Qual3_N","Qual4_N","Qual0_N",
                     "Qual1_E","Qual2_E","Qual3_E","Qual4_E","Qual0_E")) %>% 
  #Create groups for "new" and "existing" places based on names
  mutate(place_type = case_when (str_detect(name, "N") ~ "New",
                                 str_detect(name, "E") ~ "Existing"))%>% 
  #Create Ofsted ratings out of the names
  mutate(rating = case_when (str_detect(name, "1") ~ "Oustanding",
                             str_detect(name, "2") ~ "Good",
                             str_detect(name, "3") ~ "Requires Improvement",
                             str_detect(name, "4") ~ "Inadequate",
                             str_detect(name, "0") ~ "No rating" )) %>% 
  #Create new variable called places, replace 0s with NAs so it plots neatly
  mutate(places = if_else(value==0, NA_integer_, as.integer(value)))


p <- ofsted_data %>% 
  filter(rating != "No rating") %>% 
  ggplot(aes(y=value, x=place_type, 
             fill = factor(rating, levels=c("Oustanding","Good","Requires Improvement","Inadequate")),
             text = paste(rating, ": ", places, " places"))) + 
  geom_bar(stat="identity", position = position_fill(reverse = TRUE))+

  coord_flip() +
  facet_wrap(~LA_name,nrow = 2) + 
  geom_text( aes(label = scales::comma(places)),size = 3, position = position_fill(reverse = TRUE,vjust = 0.5))+
  labs( x ="", y = "")+
  guides(fill=guide_legend(title=""))+
   theme_minimal()+
  theme(legend.position="bottom")

  ggplotly(p,
           tooltip = c("text")) %>% 
    layout(legend = list(orientation = "h",
                         y =-0.1, x = 0.25)) })



# Bar chart comparison - Progress 8 -- TO ADD


output$progress8_chart<- renderPlotly({
  
  #reshape the data so it plots neatly!
  progress_8_data <- live_scorecard_data_england_comp() %>% 
    #select only the progress 8 values
    filter(name %in% c("KS4_WAA_N",	"KS4_AA_N",	"KS4_A_N", "KS4_BA_N", "KS4_WBA_N",	"KS4_NR_N",
                       "KS4_WAA_E",	"KS4_AA_E",	"KS4_A_E", "KS4_BA_E", "KS4_WBA_E",	"KS4_NR_E")) %>% 
    #Create groups for "new" and "existing" places based on names
    mutate(place_type = case_when (str_detect(name, "N") ~ "New",
                                   str_detect(name, "E") ~ "Existing"))%>% 
    #Create Ofsted ratings out of the names
    mutate(rating = case_when (str_detect(name, "_WAA_") ~ "Well above average",
                               str_detect(name, "_A_") ~ "Average",
                               str_detect(name, "_BA_") ~ "Below average",
                               str_detect(name, "_WBA_") ~ "Well below average",
                               str_detect(name, "NR") ~ "No rating" )) %>% 
    #Create new variable called places, replace 0s with NAs so it plots neatly
    mutate(places = if_else(value==0, NA_integer_, as.integer(value)))
  
  
  p <- progress_8_data %>% 
    filter(rating != "No rating") %>% 
    ggplot(aes(y=value, x=place_type, 
               text = paste(rating, ": ", places, " places"),
               fill = factor(rating, levels=c("Well above average","Average","Below average","Well below average")))) + 
    geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
    
    coord_flip() +
    facet_wrap(~LA_name,nrow = 2) + 
    geom_text( aes(label = scales::comma(places)),size = 3, position = position_fill(reverse = TRUE,vjust = 0.5))+
    labs( x ="", y = "")+
    guides(fill=guide_legend(title=""))+
    theme_minimal()+
    theme(legend.position="bottom")
  
  ggplotly(p,
           tooltip = c("text")) %>% 
    layout(legend = list(orientation = "h",
                         y =-0.1, x = 0.25)) })



# Bar chart comparison - Progress Reading

output$progressreading_chart<- renderPlotly({
  
  #reshape the data so it plots neatly!
  progress_reading_data <- live_scorecard_data_england_comp() %>% 
    #select only the reading values
    filter(name %in% c("KS2Read_WAA_N",	"KS2Read_AA_N",	"KS2Read_A_N", "KS2Read_BA_N", "KS2Read_WBA_N",	"KS2Read_NR_N",
                       "KS2Read_WAA_E",	"KS2Read_AA_E",	"KS2Read_A_E", "KS2Read_BA_E", "KS2Read_WBA_E",	"KS2Read_NR_E")) %>% 
    #Create groups for "new" and "existing" places based on names
    mutate(place_type = case_when (str_detect(name, "N") ~ "New",
                                   str_detect(name, "E") ~ "Existing"))%>% 
    #Create Ofsted ratings out of the names
    mutate(rating = case_when (str_detect(name, "_WAA_") ~ "Well above average",
                               str_detect(name, "_A_") ~ "Average",
                               str_detect(name, "_BA_") ~ "Below average",
                               str_detect(name, "_WBA_") ~ "Well below average",
                               str_detect(name, "NR") ~ "No rating" )) %>% 
    #Create new variable called places, replace 0s with NAs so it plots neatly
    mutate(places = if_else(value==0, NA_integer_, as.integer(value)))
  
  
  p <- progress_reading_data %>% 
    filter(rating != "No rating") %>% 
    ggplot(aes(y=value, x=place_type, 
               text = paste(rating, ": ", places, " places"),
               fill = factor(rating, levels=c("Well above average","Average","Below average","Well below average")))) + 
    geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
    
    coord_flip() +
    facet_wrap(~LA_name,nrow = 2) + 
    geom_text( aes(label = scales::comma(places)),size = 3, position = position_fill(reverse = TRUE,vjust = 0.5))+
    labs( x ="", y = "")+
    guides(fill=guide_legend(title=""))+
    theme_minimal()+
    theme(legend.position="bottom")
  
  ggplotly(p,
           tooltip = c("text")) %>% 
    layout(legend = list(orientation = "h",
                         y =-0.1, x = 0.25)) })


# Bar chart comparison - Progress Maths -- TO ADD

output$progressmaths_chart<- renderPlotly({
  
  #reshape the data so it plots neatly!
  progress_maths_data <- live_scorecard_data_england_comp() %>% 
    #select only the maths values
    filter(name %in% c("KS2Mat_WAA_N",	"KS2Mat_AA_N",	"KS2Mat_A_N", "KS2Mat_BA_N", "KS2Mat_WBA_N",	"KS2Mat_NR_N",
                       "KS2Mat_WAA_E",	"KS2Mat_AA_E",	"KS2Mat_A_E", "KS2Mat_BA_E", "KS2Mat_WBA_E",	"KS2Mat_NR_E")) %>% 
    #Create groups for "new" and "existing" places based on names
    mutate(place_type = case_when (str_detect(name, "N") ~ "New",
                                   str_detect(name, "E") ~ "Existing"))%>% 
    #Create Ofsted ratings out of the names
    mutate(rating = case_when (str_detect(name, "_WAA_") ~ "Well above average",
                               str_detect(name, "_A_") ~ "Average",
                               str_detect(name, "_BA_") ~ "Below average",
                               str_detect(name, "_WBA_") ~ "Well below average",
                               str_detect(name, "NR") ~ "No rating" )) %>% 
    #Create new variable called places, replace 0s with NAs so it plots neatly
    mutate(places = if_else(value==0, NA_integer_, as.integer(value)))
  
  
  p <- progress_maths_data %>% 
    filter(rating != "No rating") %>% 
    ggplot(aes(y=value, x=place_type, 
               text = paste(rating, ": ", places, " places"),
               fill = factor(rating, levels=c("Well above average","Average","Below average","Well below average")))) + 
    geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
    
    coord_flip() +
    facet_wrap(~LA_name,nrow = 2) + 
    geom_text( aes(label = scales::comma(places)),size = 3, position = position_fill(reverse = TRUE,vjust = 0.5))+
    labs( x ="", y = "")+
    guides(fill=guide_legend(title=""))+
    theme_minimal()+
    theme(legend.position="bottom")
  
  ggplotly(p,
           tooltip = c("text")) %>% 
    layout(legend = list(orientation = "h",
                         y =-0.1, x = 0.25)) })


# Cost --------------------------------------------------------------------

# Comparison table - average cost of projects per place
output$cost_table <- renderTable({
  
  live_scorecard_data_england_comp() %>%
    #Filter for Cost, places and project data
  filter(str_detect(name,"Cost|Places|Projects")) %>%
    #Create new column called data_type, based on the name of the data
  mutate(data_type = case_when (str_detect(name, "Cost") ~ "Cost",
                                str_detect(name, "Place") ~ "Place",
                                str_detect(name, "Project") ~ "Project")) %>%
  mutate(exp_type = case_when (str_detect(name, "EP") ~ "Permanent",
                               str_detect(name, "ET") ~ "Temporary",
                               str_detect(name, "NS") ~ "New school")
         ) %>%
  select(LA_name,data_type,exp_type,value) %>%
    #pivot the data wider
  pivot_wider(names_from = data_type, values_from = value) %>%
    #calculate cost per place
  mutate(cost_per_place = roundFiveUp(Cost/Place,0),
         #format it nicely with £ sign
         cost_per_place = paste0("£",cs_num(cost_per_place))) %>% 
  select(LA_name,Type = exp_type ,cost_per_place) %>%
  pivot_wider(names_from = LA_name, values_from = cost_per_place)
    

})


#Comparison charts - average cost per place

output$cost_plot <- renderPlotly({
  
  all_LA_cost <- live_scorecard_data_all_la() %>% 
    #live_scorecard_data_all_la() %>%
    filter(str_detect(name,"Cost|Places|Projects")) %>%
    mutate(data_type = case_when (str_detect(name, "Cost") ~ "Cost",
                                  str_detect(name, "Place") ~ "Place",
                                  str_detect(name, "Project") ~ "Project")) %>%
    mutate(exp_type = case_when (str_detect(name, "EP") ~ "Permanent",
                                 str_detect(name, "ET") ~ "Temporary",
                                 str_detect(name, "NS") ~ "New school")
           
    ) %>%
    select(LA_name,data_type,exp_type,value) %>%
    pivot_wider(names_from = data_type, values_from = value) %>%
    mutate(cost_per_place = roundFiveUp(Cost/Place,0),
           grouping = case_when(!LA_name %in% c("England",input$LA_choice) ~ "Other LA",
                                TRUE ~ as.character(LA_name)),
           x = 1,
           group_higlight = if_else(grouping =="Other LA",0,1)
    ) %>%
    arrange(group_higlight)
  
p<-ggplot() +
  geom_beeswarm(data=all_LA_cost %>% filter(group_higlight==0), mapping = aes(x, cost_per_place,color = grouping,
                                                                              text = paste(LA_name, ": £", cost_per_place, " per place")),
                 groupOnX=TRUE, na.rm = TRUE) +
  geom_beeswarm(data=all_LA_cost %>% filter(group_higlight==1), aes(x, cost_per_place, color = grouping,
                                                                    text = paste(LA_name, ": £", cost_per_place, " per place")),
                groupOnX=TRUE, na.rm = TRUE) +
  facet_grid(~factor(exp_type, levels=c('Permanent',"Temporary","New school")))+
  scale_color_manual(breaks = c( input$LA_choice, "England","Other LA"),
                     values=c( "#d95f02", "#1b9e77","#f0f0f0"))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
  


 ggplotly(p,tooltip = c("text"))   %>% 
   layout(legend = list(orientation = "h",
                        y =-0.1, x = 0.33))
 

})

# NEED TO ADD:

# Table to show number of projects - can sit under the other table in this tab?


#change 

}