function(input, output, session) {


# Data calculations - reactive --------------------------------------------

    live_scorecard_data  <- reactive({

        scorecards_data_pivot %>% filter(LA_name == input$LA_choice,
                                         Phase == input$phase_choice)
        

        
    })
    
# Recreate that "caluclations" sheet you have, based on the user inputs
    
 # rv <- reactiveValues()
 # 
 # calculated_data <- live_scorecard_data %>% 
     

    
        
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

## Estimated additional places

# Box to go here (use pupil growth as template)


## Estimated spare places

# Box to go here (use pupil growth as template)

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
               legend = list(orientation = 'h'),
               plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
              fig_bgcolor   = "rgba(0, 0, 0, 0)")
    

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
        
    
    gauge(forecast_accuracy, 
          min = lowest_accuracy, 
          max = highest_accuracy, 
          sectors = gaugeSectors(success = c(high_mid_accuracy, highest_accuracy), 
                                 warning = c(low_mid_accuracy, high_mid_accuracy),
                                 danger = c(lowest_accuracy, low_mid_accuracy)))
    
    
})

}