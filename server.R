function(input, output, session) {
  #Define font family for charts
  font_choice <- list(
    family = "Arial",
    size = 14)
  
  # Data calculations - reactive --------------------------------------------
  
  #LA options - reordered
  LA_options <- sort(unique(scorecards_data$LA_name)) %>% 
    as.factor() %>% 
    relevel("England") 
  
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
  
  # Options for chart choice - dependent on phase choice
  
  chart_options <- reactive({
    if(input$phase_choice == "Primary"){c("Ofsted","Reading Progress", "Maths Progress")}
    else {c("Ofsted","Progress 8")}
  })
  
  observe({
    updateSelectInput(session, "chart_choice",
                      choices = chart_options(),
                      selected = "Ofsted"
    )})
  
  

  
  
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
        paste0("Total primary and secondary basic need funding ",funding_year),
        #get different icons for background here: https://fontawesome.com/v5.15/icons?d=gallery&p=2
        icon = icon("fas fa-pound-sign"),
        color = "purple"
      )   
    } else {
      shinydashboard::valueBox(
        paste0("£", total_funding, "m"),
        paste0("Total primary and secondary basic need funding ",funding_year),
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
      paste0("Growth in ", str_to_lower(input$phase_choice)," pupil numbers 2009/10 to ", plan_year),
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
      paste0(scales::comma(additional_places_perc)),
      paste0("Estimated additional " , str_to_lower(input$phase_choice), " places required to meet demand in ",plan_year),
      icon = icon("fas fa-signal"),
      color = "blue"
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
      paste0("Estimated percentage of spare ", str_to_lower(input$phase_choice)," places in ",plan_year),
      icon = icon("fas fa-school"),
      color = "green"
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
      places_chart_data, x = ~LA_name, y = ~QuanIn,
      marker = list( color = c("#1d70b8")),
      type = 'bar', name = paste0("Total places created between 2009/10 and ", this_year),
      text = ~scales::comma(QuanIn), textposition = 'inside', textfont = list(color = '#FFF')) %>%
      add_trace(y = ~QuanPP,  marker = list( color = c("#f47738")),name = paste0('New places planned for delivery between ', next_year,' and ', plan_year), text = ~scales::comma(QuanPP), textposition = 'inside') %>% 
      add_trace(y = ~QuanRP,  marker = list( color = c("#28a197")),name = paste0('Estimated additional places needed to meet demand in ', plan_year), text = ~scales::comma(QuanRP), textposition = 'inside') %>% 
      layout(yaxis = list(title = ''),
             xaxis = list(title =''),
             barmode = 'stack',
             uniformtext=list(minsize=12, mode='hide'),
             legend = list(orientation = 'h'),
             font = font_choice)
    
    
  })
  
  
  
  ## Forecast accuracy labels
  
  output$label_estimate_y1 <- renderText({
    
    forecast_accuracy <- live_scorecard_data() %>% 
      filter(name == "For_1") %>% 
      pull(value)%>% 
      roundFiveUp(.,3)*100
    
 label<- case_when(forecast_accuracy > 0 ~ "Overestimate",
              forecast_accuracy < 0 ~ "Underestimate",
              TRUE ~ "Accurate")
  
  paste("<b>One year ahead :</b>", label)

    
  })
  
  
  output$label_estimate_y3 <- renderText({
    
    forecast_accuracy <- live_scorecard_data()%>% 
      filter(name == "For_3") %>% 
      pull(value)%>% 
      roundFiveUp(.,3)*100
    
    label<- case_when(forecast_accuracy > 0 ~ "Overestimate",
                      forecast_accuracy < 0 ~ "Underestimate",
                      TRUE ~ "Accurate")
    
    paste("<b>Three years ahead :</b>", label)
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
  
    mid_low_accuracy <- median(c(-1, lowest_accuracy))
    mid_high_accuracy <- median(c(1, highest_accuracy))

    gauge(forecast_accuracy, 
          min = lowest_accuracy, 
          max = highest_accuracy, 
          symbol = '%',
          sectors = gaugeSectors(success = c(-1, 1), 
                                 warning = c(mid_low_accuracy, mid_high_accuracy),
                                 danger = c(lowest_accuracy, highest_accuracy),
          colors = c("#00703c",	"#ffdd00", "#d4351c")))
    
    
  })
  
  ## Forecast accuracy three years ahead 
  
  #Code to go here using above template
  
  output$forecast_3y <- 
    
    
    renderGauge({
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
    
    mid_low_accuracy <- median(c(-1, lowest_accuracy))
    mid_high_accuracy <- median(c(1, highest_accuracy))

    gauge(forecast_accuracy, 
          min = lowest_accuracy, 
          max = highest_accuracy, 
          symbol = '%',
          sectors = gaugeSectors(success = c(-1, 1), 
                                 warning = c(mid_low_accuracy, mid_high_accuracy),
                                 danger = c(lowest_accuracy, highest_accuracy),
          colors = c("#00703c",	"#ffdd00", "#d4351c")))
    
    
  })
  
  
  # Preference -------------------------------------------------------------
  
  # to fill in here - use the output$pupil_growth as a template :)
  
  # Box for England % preference
  
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
    
      #Put value into box to plug into app
      shinydashboard::valueBox(
        paste0(PrefT3, "%"),
        paste0("Percentage of ", str_to_lower(input$phase_choice)," pupils who recieved an offer of one of their top three preferences in " , (input$LA_choice)),
        icon = icon("fas fa-sort-amount-up"),
        color = "green")

  })
  
  # Stacked bar instead of pie here for preference? 
  # Easier for users to interpret
  output$preference_p<- renderPlotly({  
   
   
    #reshape the data so it plots neatly!
    preference_data <- live_scorecard_data_england_comp() %>% 
      #select only preference values
      filter(name %in% c("Pref1","Pref2","Pref3")) %>% 

      #Create ratings out of the names
      mutate(rating = case_when (str_detect(name, "1") ~ "First",
                                 str_detect(name, "2") ~ "Second",
                                 str_detect(name, "3") ~ "Third" ))
    
    #Get % not getting 1st 2nd or 3rd preference
    preference_data_sum <- preference_data %>% 
      group_by(LA_name,LANumber,Phase) %>% 
      summarise(value = 100- sum(value)) %>% 
      mutate(rating = "Other")
    
    
    preference_data <- preference_data %>% 
      select(-name) %>% 
      bind_rows(preference_data_sum) %>% 
      #sort levels out so plots in correct order
      mutate(rating=factor(rating, levels=c("First","Second","Third","Other")),
      #Neaten up percs
      value = as.numeric(roundFiveUp(value,1)),
      value_label = if_else(value > 2, paste0(value,"%"), NA_character_))
      
    
    
    preference_p <- preference_data %>% 
       
      ggplot(aes(y=value, x="", 
                 fill = factor(rating),
                 text = paste(rating, ": ", value, "%"))) + 
      geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
      
      coord_flip() +
      facet_wrap(~LA_name,nrow = 2) + 
      geom_text( aes(label = value_label),colour ="#ffffff",size = 4, position = position_fill(reverse = TRUE,vjust = 0.5))+
      labs( x ="", y = "")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values = dfe_colours)+
      theme_minimal()+
      theme(legend.position="bottom",
            text=element_text(size=14,  family="Arial"))
    
    
    ggplotly(preference_p,
             tooltip = c("text")) %>% 
      layout(  uniformtext=list(minsize=12, mode='hide'),
               legend = list(orientation = "h",
                           y =-0.1, x = 0.33,
               font = font_choice))
    

})
  
  # Quality -----------------------------------------------------------------
  
  # Change name of what "better than average" is depending on chart choice:
  school_description <- reactive({
    if(input$chart_choice =="Ofsted"){"good and outstanding "}
    else {"well above and above average "}
    
  })
  
  # Calculate LA % depending on chart choice:
  LA_comp <- reactive({
    
    if(input$chart_choice =="Ofsted"){
   
      live_scorecard_data() %>% 
        filter(name=="QualProp") %>%
        pull(value) %>%
        roundFiveUp(.,2)*100
      
    }else if (input$chart_choice =="Progress 8"){

      live_scorecard_data() %>% 
        filter(name=="Qual_KS4_Prop") %>%
        pull(value) %>%
        roundFiveUp(.,2)*100
      
       }
    
    else if (input$chart_choice =="Reading Progress"){
      live_scorecard_data() %>% 
        filter(name=="Qual_KS2Read_Prop") %>%
        pull(value) %>%
        roundFiveUp(.,2)*100
      }
    
    else if (input$chart_choice =="Maths Progress"){
      live_scorecard_data() %>% 
        filter(name=="Qual_KS2Mat_Prop") %>%
        pull(value) %>%
        roundFiveUp(.,2)*100
      }
    
  })
  
  
  
  # box for % of new places in good and outstanding schools
  output$LA_GO_places <- renderValueBox({
 
    #Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(LA_comp(), "%"),
      paste0("Percentage of new places in ", school_description(), str_to_lower(input$phase_choice)," schools in ", input$LA_choice),
      icon = icon("fas fa-boxes"),
      color = "green"
    )
  })
  
# Calculate England comparator depending on chart choice:
  england_comp <- reactive({
    
    if(input$chart_choice =="Ofsted"){
      numerator<- live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("Qual1_N", "Qual2_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      denominator<-live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("Qual1_N", "Qual2_N","Qual3_N","Qual4_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      #calculate percentage
      roundFiveUp(numerator/denominator*100,1)}
    
    else if (input$chart_choice =="Progress 8"){
      numerator<- live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("KS4_WAA_N", "KS4_AA_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      denominator<-live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("KS4_WAA_N", "KS4_AA_N","KS4_A_N","KS4_BA_N","KS4_WBA_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      #calculate percentage
      roundFiveUp(numerator/denominator*100,1)}
      
    else if (input$chart_choice =="Reading Progress"){
      numerator<- live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("KS2Read_WAA_N", "KS2Read_AA_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      denominator<-live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("KS2Read_WAA_N", "KS2Read_AA_N","KS2Read_A_N","KS2Read_BA_N","KS2Read_WBA_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      #calculate percentage
      roundFiveUp(numerator/denominator*100,1)}
    
    else if (input$chart_choice =="Maths Progress"){
      numerator<- live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("KS2Mat_WAA_N", "KS2Mat_AA_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      denominator<-live_scorecard_data_all_la() %>% 
        filter(LA_name=="England" &
                 name %in% c("KS2Mat_WAA_N", "KS2Mat_AA_N","KS2Mat_A_N","KS2Mat_BA_N","KS2Mat_WBA_N")) %>%
        summarise(sum(value)) %>%
        as.numeric()
      
      #calculate percentage
      roundFiveUp(numerator/denominator*100,1)}
    
  })
  
  
  
  # box for % of new places in top schools - England
  output$England_GO_places <- renderValueBox({

    #Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(england_comp(), "%"),
      paste0("Percentage of new places in ",school_description(), str_to_lower(input$phase_choice)," schools in England"),
      icon = icon("fas fa-equals"),
      color = "blue"
    )
  })
  
  
  # Calculate % ranking depending on chart choice:
  LA_ranking <- reactive({
    
    if(input$chart_choice =="Ofsted"){
      
       live_scorecard_data() %>%
        filter(name=="QualPropranks") %>%
        pull(value) 
    
    }else if (input$chart_choice =="Progress 8"){
      
      live_scorecard_data() %>%
        filter(name=="Qual_KS4_Propranks") %>%
        pull(value) 

    }else if (input$chart_choice =="Reading Progress"){
   
      live_scorecard_data() %>%
        filter(name=="Qual_KS2Read_Propranks") %>%
        pull(value) 
    
    }else if (input$chart_choice =="Maths Progress"){
      
      live_scorecard_data() %>%
        filter(name=="Qual_KS2Mat_Propranks") %>%
        pull(value) 
      
      }
    
  })
  
  
  
  # Calculate ranking denominator depending on chart choice:
  LA_denom <- reactive({
    
    if(input$chart_choice =="Ofsted"){
      
      live_scorecard_data_all_la() %>% 
      filter(name=="QualPropranks"  & !is.na(value)) %>% 
      nrow()
      
    }else if (input$chart_choice =="Progress 8"){
      
      live_scorecard_data_all_la() %>% 
        filter(name=="Qual_KS4_Propranks"  & !is.na(value)) %>% 
      nrow()

      
    }else if (input$chart_choice =="Reading Progress"){
      
      live_scorecard_data_all_la() %>% 
        filter(name=="Qual_KS2Read_Propranks"  & !is.na(value)) %>% 
      nrow()
      
    }else if (input$chart_choice =="Maths Progress"){
      
      live_scorecard_data_all_la() %>% 
        filter(name=="Qual_KS2Mat_Propranks"  & !is.na(value)) %>% 
      nrow()
      
    }
    
  })
  
  # box for % of new places in top schools - LA Ranking

  output$LA_GO_ran <- renderValueBox({

    #Put value into box to plug into app
    shinydashboard::valueBox(
      LA_ranking(),
      paste0("LA Rank out of ", LA_denom()) ,
      icon = icon("fas fa-bars"),
      color = "purple"
    )
  })
  
  # Quality - charts --------------------------------------------------------
  
  
  # Final step once charts are ready - making this bit reactive with a dropdown
  
  rv <- reactiveValues()
  
  output$quality_chart <- renderPlotly({
    
    ggplotly(rv$quality_chart_choice,
             tooltip = c("text")) %>% 
      layout(legend = list(orientation = "h",
                           y =-0.1, x = 0.33,
                           font = font_choice))
    
    
  })
  
  
  output$no_rating_line <- renderText({
    paste0("New places with no rating = ", scales::comma(rv$no_rating) )
    
  })
  
  
  observe({
    
    # Bar chart comparison - Ofsted
    
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
        mutate(
          places = if_else(value==0, NA_integer_, as.integer(roundFiveUp(value,0)))) %>% 
          #Give NA for label if it's too small
        group_by(LA_name,place_type) %>% 
        mutate(places_perc = places/sum(places,na.rm=TRUE),
          value_label = if_else(places_perc > 0.02, places, NA_integer_))
      
      
    
    
    ofsted_p <- ofsted_data %>% 
      filter(rating != "No rating") %>% 
      ggplot(aes(y=value, x=place_type, 
                 fill = factor(rating, levels=c("Oustanding","Good","Requires Improvement","Inadequate")),
                 text = paste(rating, ": ", places, " places"))) + 
      geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
      coord_flip() +
      facet_wrap(~LA_name,nrow = 2) + 
      geom_text( aes(label = scales::comma(value_label)),size = 4,colour = "#FFFFFF", position = position_fill(reverse = TRUE,vjust = 0.5))+
      labs( x ="", y = "")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values = dfe_colours)+
      theme_minimal()+
      theme(legend.position="bottom",
            text=element_text(size=14,  family="Arial"))

    if(input$LA_choice =="England" & input$chart_choice =="Ofsted"){
      ofsted_no_rating <- ofsted_data %>%
        filter(rating =="No rating" & place_type =="New") %>%
        pull(places)
    } else if(input$chart_choice =="Ofsted") {
      ofsted_no_rating <- ofsted_data %>%
        filter(LA_name != "England" & rating =="No rating" & place_type =="New") %>%
        pull(places)}

    # Bar chart comparison - Progress 8 
    
    #reshape the data so it plots neatly!
    progress_8_data <- live_scorecard_data_england_comp() %>% 
      #select only the progress 8 values
      filter(name %in% c("KS4_WAA_N",	"KS4_AA_N",	"KS4_A_N", "KS4_BA_N", "KS4_WBA_N",	"KS4_NR_N",
                         "KS4_WAA_E",	"KS4_AA_E",	"KS4_A_E", "KS4_BA_E", "KS4_WBA_E",	"KS4_NR_E")) %>% 
      #Create groups for "new" and "existing" places based on names
      mutate(place_type = case_when (str_detect(name, "N$") ~ "New",
                                     str_detect(name, "E$") ~ "Existing"))%>% 
      #Create Ofsted ratings out of the names
      mutate(rating = case_when (str_detect(name, "_WAA_") ~ "Well above average",
                                 str_detect(name, "_AA_") ~ "Above average",
                                 str_detect(name, "_A_") ~ "Average",
                                 str_detect(name, "_BA_") ~ "Below average",
                                 str_detect(name, "_WBA_") ~ "Well below average",
                                 str_detect(name, "NR") ~ "No rating" )) %>% 
      mutate(rating = factor(rating, levels=c("Well above average","Above average","Average","Below average","Well below average"))) %>% 
      #Create new variable called places, replace 0s with NAs so it plots neatly
      mutate(places = if_else(value==0, NA_integer_, as.integer(roundFiveUp(value,0)))) %>% 
    #Give NA for label if it's too small
    group_by(LA_name,place_type) %>% 
      mutate(places_perc = places/sum(places,na.rm=TRUE),
             value_label = if_else(places_perc > 0.02, places, NA_integer_))
    
    
    
    progress_8_p <- progress_8_data %>% 
      filter(rating != "No rating") %>% 
      ggplot(aes(y=value, x=place_type, 
                 text = paste(rating, ": ", places, " places"),
                 group = rating,
                 fill = rating)) + 
      geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
      
      coord_flip() +
      facet_wrap(~LA_name,nrow = 2) + 
      geom_text( aes(label = scales::comma(value_label)),size = 4,colour = "#FFFFFF", position = position_fill(reverse = TRUE,vjust = 0.5))+
      labs( x ="", y = "")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values = dfe_colours)+
      theme_minimal()+
      theme(legend.position="bottom",
            text=element_text(size=14,  family="Arial"))
    
    if(input$LA_choice =="England" & input$chart_choice =="Progress 8"){
      progress_8_no_rating <- progress_8_data %>%
        filter(rating =="No rating" & place_type =="New") %>%
        pull(places)
    } else if(input$chart_choice =="Progress 8") {
      progress_8_no_rating <- progress_8_data %>%
        filter(LA_name != "England" & rating =="No rating" & place_type =="New") %>%
        pull(places)}
    
    # Bar chart comparison - Progress Reading
    
    #reshape the data so it plots neatly!
    progress_reading_data <- live_scorecard_data_england_comp() %>% 
      #select only the reading values
      filter(name %in% c("KS2Read_WAA_N",	"KS2Read_AA_N",	"KS2Read_A_N", "KS2Read_BA_N", "KS2Read_WBA_N",	"KS2Read_NR_N",
                         "KS2Read_WAA_E",	"KS2Read_AA_E",	"KS2Read_A_E", "KS2Read_BA_E", "KS2Read_WBA_E",	"KS2Read_NR_E")) %>% 
      #Create groups for "new" and "existing" places based on names
      mutate(place_type = case_when (str_detect(name, "N$") ~ "New",
                                     str_detect(name, "E$") ~ "Existing"))%>% 
      #Create Ofsted ratings out of the names
      mutate(rating = case_when (str_detect(name, "_WAA_") ~ "Well above average",
                                 str_detect(name, "_AA_") ~ "Above average",
                                 str_detect(name, "_A_") ~ "Average",
                                 str_detect(name, "_BA_") ~ "Below average",
                                 str_detect(name, "_WBA_") ~ "Well below average",
                                 str_detect(name, "NR") ~ "No rating" )) %>% 
      mutate(rating = factor(rating, levels=c("Well above average","Above average","Average","Below average","Well below average"))) %>% 
      #Create new variable called places, replace 0s with NAs so it plots neatly
      mutate(places = if_else(value==0, NA_integer_, as.integer(roundFiveUp(value,0))))%>% 
      #Give NA for label if it's too small
      group_by(LA_name,place_type) %>% 
      mutate(places_perc = places/sum(places,na.rm=TRUE),
             value_label = if_else(places_perc > 0.02, places, NA_integer_))
    
    
    progress_reading_p <- progress_reading_data %>% 
      filter(rating != "No rating") %>% 
      ggplot(aes(y=value, x=place_type, 
                 text = paste(rating, ": ", places, " places"),
                 group = rating,
                 fill = rating)) + 
      geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
      
      coord_flip() +
      facet_wrap(~LA_name,nrow = 2) + 
      geom_text( aes(label = scales::comma(value_label)),size = 4,colour = "#FFFFFF", position = position_fill(reverse = TRUE,vjust = 0.5))+
      labs( x ="", y = "")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values = dfe_colours)+
      theme_minimal()+
      theme(legend.position="bottom",
            text=element_text(size=14,  family="Arial"))
    
    
    if(input$LA_choice =="England"& input$chart_choice =="Reading Progress"){
      progress_reading_no_rating <-  progress_reading_data %>%
        filter(rating =="No rating" & place_type =="New") %>%
        pull(places)
    } else if(input$chart_choice =="Reading Progress"){
      progress_reading_no_rating <-  progress_reading_data %>%
        filter(LA_name != "England" & rating =="No rating" & place_type =="New") %>%
        pull(places)}

    # Bar chart comparison - Progress Maths 
    
    #reshape the data so it plots neatly!
    progress_maths_data <- live_scorecard_data_england_comp() %>% 
      #select only the maths values
      filter(name %in% c("KS2Mat_WAA_N",	"KS2Mat_AA_N",	"KS2Mat_A_N", "KS2Mat_BA_N", "KS2Mat_WBA_N",	"KS2Mat_NR_N",
                         "KS2Mat_WAA_E",	"KS2Mat_AA_E",	"KS2Mat_A_E", "KS2Mat_BA_E", "KS2Mat_WBA_E",	"KS2Mat_NR_E")) %>% 
      #Create groups for "new" and "existing" places based on names
      mutate(place_type = case_when (str_detect(name, "N$") ~ "New",
                                     str_detect(name, "E$") ~ "Existing"))%>% 
      #Create Ofsted ratings out of the names
      mutate(rating = case_when (str_detect(name, "_WAA_") ~ "Well above average",
                                 str_detect(name, "_AA_") ~ "Above average",
                                 str_detect(name, "_A_") ~ "Average",
                                 str_detect(name, "_BA_") ~ "Below average",
                                 str_detect(name, "_WBA_") ~ "Well below average",
                                 str_detect(name, "NR") ~ "No rating" )) %>% 
      mutate(rating = factor(rating, levels=c("Well above average","Above average","Average","Below average","Well below average"))) %>% 
      #Create new variable called places, replace 0s with NAs so it plots neatly
      mutate(places = if_else(value==0, NA_integer_, as.integer(roundFiveUp(value,0)))) %>% 
      #Give NA for label if it's too small
      group_by(LA_name,place_type) %>% 
      mutate(places_perc = places/sum(places,na.rm=TRUE),
             value_label = if_else(places_perc > 0.02, places, NA_integer_))
    
    
    progress_maths_p <- progress_maths_data %>% 
      filter(rating != "No rating") %>% 
      ggplot(aes(y=value, x=place_type, 
                 text = paste(rating, ": ", places, " places"),
                 group = rating,
                 fill = rating)) + 
      geom_bar(stat="identity", position = position_fill(reverse = TRUE))+
      
      coord_flip() +
      facet_wrap(~LA_name,nrow = 2) + 
      geom_text( aes(label = scales::comma(value_label)),size = 4,colour = "#FFFFFF", position = position_fill(reverse = TRUE,vjust = 0.5))+
      labs( x ="", y = "")+
      guides(fill=guide_legend(title=""))+
      scale_fill_manual(values = dfe_colours)+
      theme_minimal()+
      theme(legend.position="bottom",
            text=element_text(size=14,  family="Arial"))
    
    if(input$LA_choice =="England" & input$chart_choice =="Maths Progress"){
      progress_maths_no_rating <- progress_maths_data %>%
        filter(rating =="No rating" & place_type =="New") %>%
        pull(places)
    } else if(input$chart_choice =="Maths Progress"){
      progress_maths_no_rating <- progress_maths_data %>%
        filter(LA_name != "England" & rating =="No rating" & place_type =="New") %>%
        pull(places)}
    
    # Pick chart to plot based on user input
    if(input$chart_choice =="Ofsted"){
      rv$quality_chart_choice <- ofsted_p
      rv$no_rating <- ofsted_no_rating 
    } else if (input$chart_choice =="Reading Progress"){
      rv$quality_chart_choice <- progress_reading_p
      rv$no_rating <- progress_reading_no_rating 
    } else if (input$chart_choice =="Maths Progress"){
      rv$quality_chart_choice <- progress_maths_p
      rv$no_rating <- progress_maths_no_rating 
    } else if (input$chart_choice =="Progress 8"){
      rv$quality_chart_choice <- progress_8_p
      rv$no_rating <- progress_8_no_rating }
    
    
  })
  
  
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
             cost_per_place = paste0("£",cs_num(cost_per_place)),
             #Nicely format any NA
             cost_per_place = str_replace(cost_per_place,"£NaN","-")
             ) %>% 
      select(LA_name,Type = exp_type ,cost_per_place) %>%
      pivot_wider(names_from = LA_name, values_from = cost_per_place) 
      
    
    
  })
  
  
  #Comparison charts - average cost per place
  
  output$cost_plot <- renderPlotly({
    
    all_LA_cost <- live_scorecard_data_all_la() %>% 
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
                                                                                  text = paste(LA_name, ": £", scales::comma(cost_per_place), " per place")),
                    groupOnX=TRUE, na.rm = TRUE) +
      geom_beeswarm(data=all_LA_cost %>% filter(group_higlight==1), aes(x, cost_per_place, color = grouping,
                                                                        text = paste(LA_name, ": £", scales::comma(cost_per_place), " per place")),
                    groupOnX=TRUE, na.rm = TRUE) +
      facet_grid(~factor(exp_type, levels=c('Permanent',"Temporary","New school")))+
      scale_color_manual(breaks = c( input$LA_choice, "England","Other LA"),
                         values=c( "#f47738", "#1d70b8","#f3f2f1"))+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.title = element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            text=element_text(size=14,  family="Arial"))
    
    
    
    ggplotly(p,tooltip = c("text"))   %>% 
      layout(legend = list(orientation = "h",
                           y =-0.1, x = 0.33,
                           font = font_choice))
    
    
  })
  
  # Comparison boxes - number of projects
  output$perm_box <- renderValueBox({
    
    perm_fig <- live_scorecard_data()%>%
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
      filter(data_type=="Project" & exp_type=="Permanent") %>%
      pull(value)
    
    
    shinydashboard::valueBox(
      paste0( perm_fig, " project(s)"),
      paste0("Permanent " ,str_to_lower(input$phase_choice), " expansion projects in ", input$LA_choice),
      icon = icon("fas fa-school"),
      color = "blue"
    )
    
})
  
  output$temp_box <- renderValueBox({
    
    temp_fig <- live_scorecard_data()%>%
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
      filter(data_type=="Project" & exp_type=="Temporary") %>%
      pull(value)
    
    
    shinydashboard::valueBox(
      paste0( temp_fig, " project(s)"),
      paste0("Temporary ",str_to_lower(input$phase_choice)," projects in ", input$LA_choice),
      icon = icon("fas fa-campground"),
      color = "green"
    )
    
  })

  
  output$new_box <- renderValueBox({
    
    new_fig <- live_scorecard_data()%>%
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
      filter(data_type=="Project" & exp_type=="New school") %>%
      pull(value)
    
    
    shinydashboard::valueBox(
      paste0( new_fig, " project(s)"),
      paste0("New ",str_to_lower(input$phase_choice), " schools projects in ", input$LA_choice),
      icon = icon("fas fa-plus"),
      color = "purple"
    )
    
  })
    
# Files for download ------------------------------------------------------
  
  filename_choice <- reactive({
    
    if(input$phase_choice =="Primary"){
      "Primary_scorecards_data.csv"}else{ "Secondary_scorecards_data.csv" }
    
  })
  
  file_choice <- reactive({
    
    if(input$phase_choice =="Primary"){
      primary_data_clean} else{ secondary_data_clean }
    
  })
  
  
  output$download_ud <- downloadHandler(
    
    
   filename = function() {
     filename_choice()
    },
    content = function(file) {
      write.csv(file_choice() , file, row.names = FALSE)
    }
)
   
    
    
# Tech guidance tables ----------------------------------------------------

  output$notesTable<- function(){
  
    notesTable[is.na(notesTable)] <- " "
    
  kable(notesTable, "html", align = "l",escape=FALSE) %>%
    kable_styling(full_width = F) %>%
    #collapse_rows(columns = 1:2) %>%
    column_spec(1, bold = T, width = "20em",  extra_css = 'vertical-align: top !important;')%>% 
    column_spec(2,  width = "20em")
  }
  
  output$notesTableQuant<- function(){
    
    notesTableQuant[is.na(notesTableQuant)] <- " "
    
    kable(notesTableQuant, "html", align = "l",escape=FALSE) %>%
      kable_styling(full_width = F) %>%
      #collapse_rows(columns = 1:2) %>%
      column_spec(1, bold = T, extra_css = 'vertical-align: top !important;')%>% 
      column_spec(2,  width_min = "20em") %>% 
      column_spec(5,  width_max = "40em")
  }
  
  output$notesTablePref<- function(){
    
    notesTablePref[is.na(notesTablePref)] <- " "
    
    kable(notesTablePref, "html", align = "l",escape=FALSE) %>%
      kable_styling(full_width = F) %>%
      #collapse_rows(columns = 1:2) %>%
      column_spec(1, bold = T,width = "20em",  extra_css = 'vertical-align: top !important;')%>% 
      column_spec(2,  width = "20em")
  }
  
  output$notesTableQual<- function(){
    
    notesTableQual[is.na(notesTableQual)] <- " "
    
    kable(notesTableQual, "html", align = "l",escape=FALSE) %>%
      kable_styling(full_width = F) %>%
      #collapse_rows(columns = 1:2) %>%
      column_spec(1, bold = T, width = "20em",  extra_css = 'vertical-align: top !important;')%>% 
      column_spec(2,  width = "20em")
  }
  
  output$notesTableCost<- function(){
    
    notesTableCost[is.na(notesTableCost)] <- " "
    
    kable(notesTableCost, "html", align = "l",escape=FALSE) %>%
      kable_styling(full_width = F) %>%
      #collapse_rows(columns = 1:2) %>%
      column_spec(1, bold = T,  width = "20em", extra_css = 'vertical-align: top !important;') %>% 
     column_spec(2,  width = "20em")
  }
  
  
  
  # Hide details if Eng--------
  observe({
    if (input$LA_choice =="England") {
      shinyjs::hide("LA_GO_places")
      shinyjs::hide("LA_GO_ran")
      shinyjs::hide("PrefT3_LA")
      shinyjs::hide("new_box")
      shinyjs::hide("temp_box")
      shinyjs::hide("perm_box")
    }
    
    else {
      shinyjs::show("LA_GO_places")
      shinyjs::show("LA_GO_ran")
      shinyjs::show("PrefT3_LA")
      shinyjs::show("new_box")
      shinyjs::show("temp_box")
      shinyjs::show("perm_box")
    }
  })
  

}