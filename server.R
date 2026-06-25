function(input, output, session) {
  # Define font family for charts
  font_choice <- list(
    family = "Arial",
    size = 14
  )

  # Cookie control ----------------------------------------------------------
  output$cookie_status <- dfeshiny::cookies_banner_server(
    input_cookies = reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

  # Dashboard control -------------------------------------------------------

  output$pdfDownload <- downloadHandler(
    filename = paste0("dashboard_output.pdf"),
    content = function(file) {
      # Add a loading modal, can probably make this prettier at a later date
      showModal(modalDialog("Preparing PDF report...", footer = NULL))
      on.exit(removeModal())

      # List of parameters to pass from shiny to the report
      params <- list(
        input_la_choice = input$LA_choice,
        input_phase_choice = input$phase_choice
      )

      # Render the pdf file from the rmarkdown template
      rmarkdown::render(
        "Summary_scorecard.Rmd",
        output_file = file,
        params = params,
        output_format = "pdf_document",
        envir = new.env(parent = globalenv())
      )
    }
  )

  # actionLinks
  observeEvent(input$linkQuantityTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "la_scorecards")
    updateTabsetPanel(session, "tabs", selected = "quantity")
  })
  observeEvent(input$linkForecastTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "la_scorecards")
    updateTabsetPanel(session, "tabs", selected = "forecast")
  })
  observeEvent(input$linkPreferenceTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "la_scorecards")
    updateTabsetPanel(session, "tabs", selected = "preference")
  })
  observeEvent(input$linkQualityTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "la_scorecards")
    updateTabsetPanel(session, "tabs", selected = "quality")
  })
  observeEvent(input$linkCostTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "la_scorecards")
    updateTabsetPanel(session, "tabs", selected = "cost")
  })
  observeEvent(input$linkTechnicalnotesTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "technical_notes")
    updateTabsetPanel(session, "tabs_tech_notes", selected = "cost")
  })
  observeEvent(input$linklascorecardsTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "la_scorecards")
  })

  # Technical section - reactive ----------------------
  notes_quant <- reactive({
    req(input$technical_year)
    notesTableQuant
  })

  notes_foracc <- reactive({
    req(input$technical_year)
    notesTableforacc
  })

  notes_pref <- reactive({
    req(input$technical_year)
    notesTablePref
  })

  notes_qual <- reactive({
    req(input$technical_year)
    notesTableQual
  })

  notes_cost <- reactive({
    req(input$technical_year)
    notesTableCost
  })


  # Data calculations - reactive --------------------------------------------

  # LA options - reordered
  LA_options <- sort(unique(scorecards_data$LA_name)) %>%
    as.factor() %>%
    relevel("England")

  La_data_benchmark <- scorecards_data %>%
    filter(LA_name != c("England"))

  # La_data_benchmark2 <- reactive({
  # scorecards_data %>%
  # filter (LA_name != input$LA_choice)
  # })

  LA_benchmark_options <- sort(unique(La_data_benchmark$LA_name)) %>%
    factor()

  LA_benchmark_options_pref <-
    sort(unique(scorecards_data$LA_name)) %>%
    as.factor() %>%
    relevel("England")

  # Scorecard data, filtered on user input
  live_scorecard_data <- reactive({
    scorecards_data_pivot %>%
      filter(
        LA_name == input$LA_choice,
        Year == input$year_choice,
        Phase == input$phase_choice
      )
  })

  # Scorecard data, filtered on user input AND including England as a comparison
  live_scorecard_data_england_comp_pref <- reactive({
    scorecards_data_pivot %>%
      filter(
        case_when(
          input$LA_choice != "England" ~
            LA_name %in% c(input$LA_choice, input$selectBenchLAspref),
          TRUE ~ LA_name %in% c(input$LA_choice)
        ),
        Phase == input$phase_choice,
        Year == input$year_choice,
      ) %>%
      mutate(
        # This step just makes sure that the LA is FIRST when it comes to
        # plots / tables
        LA_name = factor(LA_name) %>% relevel(input$LA_choice)
      )
  })

  # Scorecard data, filtered on user input AND including England as a comparison
  live_scorecard_data_england_comp_quality <- reactive({
    scorecards_data_pivot %>%
      filter(
        case_when(
          input$LA_choice != "England" ~
            LA_name %in% c(input$LA_choice, input$selectBenchLAsquality),
          TRUE ~ LA_name %in% c(input$LA_choice)
        ),
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      mutate(
        # This step just makes sure that the LA is FIRST when it comes to
        # plots / tables
        LA_name = factor(LA_name) %>% relevel(input$LA_choice)
      )
  })

  # Scorecard data for ALL LAs, filtered on phase and year choice
  live_scorecard_data_all_la <- reactive({
    scorecards_data_pivot %>% filter(Phase == input$phase_choice, Year == input$year_choice)
  })

  # Scorecard data, filtered on user input AND including England as a comparison
  # for cost data
  live_scorecard_data_england_comp <- reactive({
    scorecards_data_pivot %>%
      filter(
        LA_name %in% c(input$LA_choice, "England"),
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      mutate(
        LA_name = as.factor(LA_name),
        # This step just makes sure that the LA is FIRST when it comes to
        # plots / tables
        LA_name = relevel(LA_name, "England"),
        LA_name = factor(LA_name, levels = rev(levels(LA_name)))
      )
  })

  # scorecard data, filtered on user input and benchmarking choice Las as a
  # comparison
  live_scorecard_data_reactive_benchmark <- reactive({
    scorecards_data_pivot %>%
      filter(
        LA_name %in% c(input$LA_choice, input$selectBenchLAs),
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      mutate(
        LA_name = factor(
          LA_name,
          levels = c(input$LA_choice, input$selectBenchLAs)
        )
      )
  })

  # scorecard data, filtered on user input and benchmarking choice Las as a
  # comparison
  live_quantity_england_all_selected <- reactive({
    scorecards_data_pivot %>%
      filter(
        LA_name %in% c(input$LA_choice, input$selectBenchLAs, "England"),
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      mutate(
        LA_name = factor(
          LA_name,
          levels = c(input$LA_choice, input$selectBenchLAs)
        )
      )
  })

  # scorecard data, filtered on user input and benchmarking choice Las as a
  # comparison
  live_quantity_england_all_selected <- reactive({
    scorecards_data_pivot %>%
      filter(
        LA_name %in% c(input$LA_choice, input$selectBenchLAs, "England"),
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      mutate(LA_name = factor(LA_name,
        levels = c(input$LA_choice, input$selectBenchLAs)
      ))
  })

  # Options for chart choice - dependent on phase choice
  chart_options <- reactive({
    if (input$phase_choice == "Primary") {
      c("Ofsted Rating", "Reading Progress", "Maths Progress")
    } else {
      c("Ofsted Rating", "Progress 8")
    }
  })
  observe({
    updateSelectInput(
      session,
      "chart_choice",
      choices = chart_options(),
      selected = "Ofsted Rating"
    )
  })

  #  Reactive year variables
  numeric_year <- function(academic_year) {
    as.numeric(paste0("20", substr(academic_year, 6, 7)))
  }
  year_vars <- reactive({
    selected_year <- input$year_choice
    base_year <- numeric_year(selected_year)

    list(
      this_year = selected_year,
      last_year = paste0(base_year - 2, "/", substr(base_year - 1, 3, 4)),
      last_year_minus_1 = paste0(base_year - 3, "/", substr(base_year - 2, 3, 4)),
      next_year = paste0(base_year, "/", substr(base_year + 1, 3, 4)),
      plan_year = paste0(base_year + 2, "/", substr(base_year + 3, 3, 4)),
      SCAP_ref = as.character(base_year),
      plannedplaces_ref = as.character(base_year + 2),
      funding_year = case_when(
        base_year == 2023 ~ "2025",
        base_year == 2024 ~ "2026",
        base_year == 2025 ~ "2028"
      ),
      cost_year_1 = "2015/16", # static
      cost_year_2 = "2017/18", # static
      preference_current_year = as.character(base_year),
      preference_next_year = as.character(base_year + 1),
      forecast_year = paste0(base_year, "/", substr(base_year + 1, 3, 4)),
      chart_choice = "Ofsted Rating"
    )
  })

  # Top lines -------------------------
  ## create header so users know what the data is showing

  output$data_description <- renderText({
    paste0(
      "Data for ",
      str_to_lower(input$phase_choice),
      " state-funded school places in ",
      input$LA_choice
    )
  })

  output$quantitysubtitle <- renderText({
    paste0("Estimated future school place demand for ", input$LA_choice)
  })

  ## create quality heading
  output$quality_description <- renderText({
    ifelse(
      input$chart_choice %in% c("Maths Progress", "Reading Progress"),
      paste0(
        "Quality of school places created between ",
        year_vars()$last_year,
        " and ",
        year_vars()$this_year,
        " based on ",
        input$chart_choice
      ),
      paste0(
        "Quality of school places created between ",
        year_vars()$last_year,
        " and ",
        year_vars()$this_year,
        " based on ",
        input$chart_choice
      )
    )
  })

  # Quantity ----------------------------------------------------------------

  ## create quantity
  output$quantity_plot_title <- renderUI({
    strong(
      paste0("School places created, planned future places, additional places still needed, as at May ", year_vars()$SCAP_ref)
    )
  })

  output$pupil_subtitle <- renderText({
    paste0("Pupils in places in ", input$LA_choice)
  })

  output$pupil_growth_breakpoint_text <- renderText({
    ifelse(
      input$phase_choice == "Primary",
      "In England, the number of primary pupils increased in each academic year between 2009/10 and 2018/19, before beginning to decrease after 2018/19.",
      "In England, the number of secondary pupils decreased in each academic year between 2009/10 and 2014/15, before beginning to increase after 2014/15."
    )
  })

  ## Total funding

  output$total_funding_box <- renderUI({
    # Take data, get total funding and divide by billion if it's England, million if it's not
    total_funding <- scorecards_data %>%
      filter(LA_name == input$LA_choice) %>%
      filter(Year == input$year_choice) %>%
      select(Funding) %>%
      mutate(Funding = unlist(Funding)) %>%
      mutate(Funding = as.numeric(Funding)) %>%
      mutate(
        Funding = ifelse(
          input$LA_choice == "England",
          round_half_up(Funding / 1000000000, 2),
          round_half_up(Funding / 1000000, 0)
        )
      ) %>%
      pull(Funding)

    total_funding <- sum(total_funding, na.rm = TRUE)

    # Create the actual output here. Use if statement so we display "bn" if it's
    # England, "mm" if not.
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = paste0("£", total_funding, "bn"),
        title = paste(
          "Total primary and secondary basic need funding to create new places",
          year_vars()$funding_year
        ),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = paste0("£", total_funding, "m"),
        title = (paste(
          "Total primary and secondary basic need funding to create new places",
          year_vars()$funding_year
        )),
        theme = "primary"
      )
    }
  })

  ## Growth in pupil numbers (actual)

  output$pupil_growth <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    growth_perc <- live_scorecard_data() %>%
      filter(name == "Bangro") %>%
      pull(value) # %>%
    # round_half_up(., 3) * 100

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = paste0(
          "Actual change in ",
          str_to_lower(input$phase_choice),
          " pupil numbers 2009/10 to ",
          year_vars()$next_year
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = paste0(
          "Actual change in ",
          str_to_lower(input$phase_choice),
          " pupil numbers 2009/10 to ",
          year_vars()$next_year
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "primary"
      )
    }
  })

  ## Growth in pupil numbers (actual) 10 to 19
  output$pupil_growth_10_to_break <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    growth_perc <- live_scorecard_data() %>%
      filter(name == "NoR_10_to_breakpercent") %>%
      pull(value) # %>%
    # round_half_up(., 3) * 100

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(
          input$phase_choice == "Primary",
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2009/10 to 2018/19"
          ),
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2009/10 to 2014/15"
          )
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(
          input$phase_choice == "Primary",
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2009/10 to 2018/19"
          ),
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2009/10 to 2014/15"
          )
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "primary"
      )
    }
  })

  ## Growth in pupil numbers (actual) 19 to current
  output$pupil_growth_break_to_current <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    growth_perc <- live_scorecard_data() %>%
      filter(name == "NoR_break_to_currentpercent") %>%
      pull(value) # %>%
    # round_half_up(., 3) * 100

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(
          input$phase_choice == "Primary",
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2018/19 to ",
            year_vars()$next_year
          ),
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2014/15 to ",
            year_vars()$next_year
          )
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(
          input$phase_choice == "Primary",
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2018/19 to ",
            year_vars()$next_year
          ),
          paste0(
            "Actual change in ",
            str_to_lower(input$phase_choice),
            " pupil numbers 2014/15 to ",
            year_vars()$next_year
          )
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "primary"
      )
    }
  })

  ## Growth in pupil numbers (actual) 10 to 19
  output$pupil_growth_10_to_break <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    growth_perc <- live_scorecard_data() %>%
      filter(name == "NoR_10_to_breakpercent") %>%
      pull(value) # %>%
    # round_half_up(., 3) * 100

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(input$phase_choice == "Primary",
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2009/10 to 2018/19"),
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2009/10 to 2014/15")
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(input$phase_choice == "Primary",
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2009/10 to 2018/19"),
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2009/10 to 2014/15")
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "primary"
      )
    }
  })

  ## Growth in pupil numbers (actual) 19 to current
  output$pupil_growth_break_to_current <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    growth_perc <- live_scorecard_data() %>%
      filter(name == "NoR_break_to_currentpercent") %>%
      pull(value) # %>%
    # round_half_up(., 3) * 100

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(input$phase_choice == "Primary",
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2018/19 to ", year_vars()$next_year),
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2014/15 to ", year_vars()$next_year)
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = format_perc(growth_perc),
        title = ifelse(input$phase_choice == "Primary",
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2018/19 to ", year_vars()$next_year),
          paste0("Actual change in ", str_to_lower(input$phase_choice), " pupil numbers 2014/15 to ", year_vars()$next_year)
        ),
        # icon = icon("fas fa-chart-line"),
        theme = "primary"
      )
    }
  })

  # Anticipated pupil growth

  output$pupil_anticipated_growth <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    anticipated_growth_perc <- live_scorecard_data() %>%
      filter(name == "Angro") %>%
      pull(value) # %>%
    # round_half_up(., 3) * 100

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = format_perc(anticipated_growth_perc),
        title = paste0(
          "Anticipated change in ",
          str_to_lower(input$phase_choice),
          " pupil numbers ",
          year_vars()$next_year,
          " to ",
          year_vars()$plan_year,
          " (consider alongside forecast accuracy. See the technical notes for more information)"
        ),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = format_perc(anticipated_growth_perc),
        title = paste0(
          "Anticipated change in ",
          str_to_lower(input$phase_choice),
          " pupil numbers ",
          year_vars()$next_year,
          " to ",
          year_vars()$plan_year
        ),
        theme = "primary"
      )
    }
  })

  ## Estimated additional places - use QUAN_P_RP and QUAN_S_RP

  ## Caveats for BCP and Dorset and Northamptonshire
  output$quantity.bartext <- renderUI({
    if (
      input$LA_choice %in% c("Dorset", "Bournemouth, Christchurch and Poole")
    ) {
      paste0(
        "Some historical data is not comparable because of 2019 boundary changes.
             Therefore total places created growth in pupil numbers may not shown for this LA."
      )
    } else if (
      input$LA_choice %in% c("West Northamptonshire", "North Northamptonshire")
    ) {
      paste0(
        "Some historical data is not comparable because of 2021 boundary changes.
             Therefore total places created growth in pupil numbers may not shown for this LA."
      )
    } else if (
      input$LA_choice %in%
        c("City of London") &
        input$phase_choice == "Secondary"
    ) {
      paste0("City of London does not contain any secondary places")
    } else if (
      input$LA_choice %in%
        c("Isles of Scilly") &
        input$phase_choice == "Primary"
    ) {
      paste0("City of London does not contain any secondary places")
    } else if (
      input$LA_choice %in% c("Cumberland", "Westmorland and Furness")
    ) {
      paste0(
        "Some historical data is not comparable because of 2023 boundary changes.
             Therefore total places created growth in pupil numbers may not shown for this LA."
      )
    }
  })
  # Current unfilled places

  output$current_unfilled_places <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    unfilled_places_perc <- live_scorecard_data() %>%
      filter(name == "QuanUP") %>%
      pull(value) %>%
      round_half_up(., 1)

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = paste0(unfilled_places_perc, "%"),
        title = paste0(
          "Percentage of unfilled ",
          str_to_lower(input$phase_choice),
          " places ",
          year_vars()$this_year
        ),
        # icon = icon("fas fa-signal"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = paste0(unfilled_places_perc, "%"),
        title = paste0(
          "Percentage of unfilled ",
          str_to_lower(input$phase_choice),
          " places ",
          year_vars()$this_year
        ),
        # icon = icon("fas fa-signal"),
        theme = "primary"
      )
    }
  })

  # Estimatd additional places

  output$estimated_additional_places <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    additional_places_perc <- live_scorecard_data() %>%
      filter(name == "QuanRP") %>%
      pull(value)

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = paste0(scales::comma(additional_places_perc)),
        title = paste0(
          "Estimated additional ",
          str_to_lower(input$phase_choice),
          " places needed to meet demand in ",
          year_vars()$plan_year
        ),
        # icon = icon("fas fa-signal"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = paste0(scales::comma(additional_places_perc)),
        title = paste0(
          "Estimated additional ",
          str_to_lower(input$phase_choice),
          " places needed to meet demand in ",
          year_vars()$plan_year
        ),
        # icon = icon("fas fa-signal"),
        theme = "primary"
      )
    }
  })

  ## Estimated spare places

  output$estimated_spare_places <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    spare_places_per <- live_scorecard_data() %>%
      filter(name == "QuanSu") %>%
      pull(value) %>%
      round_half_up(., 3) *
      100

    # Put value into box to plug into app
    if (input$LA_choice == "England") {
      bslib::value_box(
        value = paste0(spare_places_per, "%"),
        title = paste0(
          "Estimated percentage of spare ",
          str_to_lower(input$phase_choice),
          " places in ",
          year_vars()$plan_year
        ),
        # icon = icon("fas fa-school"),
        theme = "info"
      )
    } else {
      bslib::value_box(
        value = paste0(spare_places_per, "%"),
        title = paste0(
          "Estimated percentage of spare ",
          str_to_lower(input$phase_choice),
          " places in ",
          year_vars()$plan_year
        ),
        # icon = icon("fas fa-school"),
        theme = "primary"
      )
    }
  })

  # Stop users being able to select chosen LA as benchmark LA for quantity chart
  observeEvent(input$LA_choice, {
    updateSelectizeInput(
      session,
      "selectBenchLAs",
      choices = levels(
        LA_benchmark_options %>% droplevels(exclude = input$LA_choice)
      )
    )
  })
  # Stop users being able to select chosen LA as benchmark LA for quality chart
  observeEvent(input$LA_choice, {
    updateSelectizeInput(
      session,
      "selectBenchLAsquality",
      choices = levels(
        LA_benchmark_options_pref %>% droplevels(exclude = input$LA_choice)
      )
    )
  })

  test_count <- reactive({
    length(c(input$LA_choice, input$selectBenchLAs))
  })

  # Stop users being able to select chosen LA as benchmark LA for preference chart
  observeEvent(input$LA_choice, {
    updateSelectizeInput(
      session,
      "selectBenchLAspref",
      choices = levels(
        LA_benchmark_options_pref %>% droplevels(exclude = input$LA_choice)
      )
    )
  })

  ## Places bar

  output$places_chart <- renderGirafe({
    # Take filtered data, filter for the variables we want to plot
    places_chart_data <- live_scorecard_data_reactive_benchmark() %>%
      filter(name %in% c("QuanIn", "QuanPP", "QuanRP")) %>%
      select(LA_name, name, value) %>%
      mutate(LA_name = factor(LA_name) %>% relevel(input$LA_choice))

    places_chart_data <- places_chart_data %>%
      mutate(
        tooltip_text = paste0(
          case_when(
            name == "QuanIn" ~ paste0("Total places created between May 2010 and May ", year_vars()$SCAP_ref),
            name == "QuanPP" ~ paste0(
              "New places planned for delivery between May ",
              year_vars()$SCAP_ref, " and September ",
              year_vars()$plannedplaces_ref
            ),
            name == "QuanRP" ~ paste0(
              "Estimated additional places still needed to meet demand in ",
              year_vars()$plan_year
            )
          ),
          "\n", LA_name, ": ", scales::comma(value)
        ),
      )


    p <- ggplot(places_chart_data, aes(
      x = LA_name,
      y = value,
      fill = name,
      tooltip = tooltip_text,
      data_id = paste0(LA_name, "_", name)
    )) +
      geom_bar_interactive(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
      geom_text(
        aes(label = scales::comma(value)),
        position = position_dodge(width = 0.8),
        vjust = 0,
        colour = "black",
        size = 6
      ) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_fill_manual(
        values = c("QuanIn" = "#08519c", "QuanPP" = "#3182bd", "QuanRP" = "#6baed6"),
        labels = c(
          paste0("Total places created between May 2010 and May ", year_vars()$SCAP_ref),
          paste0(
            "New places planned for delivery between May ",
            year_vars()$SCAP_ref, " and September ",
            year_vars()$plannedplaces_ref
          ),
          paste0(
            "Estimated additional places still needed to meet demand in ",
            year_vars()$plan_year
          )
        )
      ) +
      labs(
        x = NULL,
        y = NULL,
        title = "Chart showing total places created, new places
        planned for delivery and estimated additional places needed
        to meet demand, by Local Authority compared to England",
        fill = NULL
      ) +
      # theme_minimal(base_family = font_choices$family) +
      theme(
        plot.title = element_text(color = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        legend.direction = "vertical",
        panel.grid.major.y = element_line(color = "grey80", size = 0.5),
        text = element_text(size = 20, family = "Arial")
      )

    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 10,
      options = list(
        opts_tooltip(css = "background-color:white; color:black; padding: 5px; border: 1px solid #ccc;"),
        opts_hover(css = "fill-opacity:0.7;cursor:pointer;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })


  ## Places bar
  output$places_chart_england <- renderGirafe({
    # Take filtered data, filter for the variables we want to plot
    places_chart_england_data <- live_scorecard_data() %>%
      filter(name %in% c("QuanIn", "QuanPP", "QuanRP")) %>%
      select(LA_name, name, value)

    places_chart_england_data <- places_chart_england_data %>%
      mutate(
        tooltip_text = paste0(
          case_when(
            name == "QuanIn" ~ paste0("Total places created between May 2010 and May ", year_vars()$SCAP_ref),
            name == "QuanPP" ~ paste0(
              "New places planned for delivery between May ",
              year_vars()$SCAP_ref, " and September ",
              year_vars()$plannedplaces_ref
            ),
            name == "QuanRP" ~ paste0(
              "Estimated additional places still needed to meet demand in ",
              year_vars()$plan_year
            )
          ),
          "\n", LA_name, ": ", scales::comma(value)
        ),
      )


    e <- ggplot(places_chart_england_data, aes(
      x = LA_name,
      y = value,
      fill = name,
      tooltip = tooltip_text,
      data_id = paste0(LA_name, "_", name)
    )) +
      geom_bar_interactive(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
      geom_text(
        aes(label = scales::comma(value)),
        position = position_dodge(width = 0.8),
        vjust = 0,
        colour = "black",
        size = 6
      ) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_fill_manual(
        values = c("QuanIn" = "#08519c", "QuanPP" = "#3182bd", "QuanRP" = "#6baed6"),
        labels = c(
          paste0("Total places created between May 2010 and May ", year_vars()$SCAP_ref),
          paste0(
            "New places planned for delivery between May ",
            year_vars()$SCAP_ref, " and September ",
            year_vars()$plannedplaces_ref
          ),
          paste0(
            "Estimated additional places still needed to meet demand in ",
            year_vars()$plan_year
          )
        )
      ) +
      labs(
        x = NULL,
        y = NULL,
        title = "Chart showing total places created, new places
        planned for delivery and estimated additional places needed
        to meet demand, by Local Authority compared to England",
        fill = NULL
      ) +
      # theme_minimal(base_family = font_choices$family) +
      theme(
        plot.title = element_text(color = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major.y = element_line(color = "grey80", size = 0.5),
        legend.position = "bottom",
        legend.direction = "vertical",
        text = element_text(size = 20, family = "Arial")
      )

    girafe(
      ggobj = e,
      width_svg = 10,
      height_svg = 10,
      options = list(
        opts_tooltip(css = "background-color:white; color:black; padding: 5px; border: 1px solid #ccc;"),
        opts_hover(css = "fill-opacity:0.7;cursor:pointer;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })

  # Comparison table - average cost of projects per place
  output$quantity_table <- renderDataTable({
    live_quantity_england_all_selected() %>%
      filter(
        name %in%
          c(
            "Bangro",
            "Angro",
            "QuanUP",
            "QuanRP",
            "QuanSu",
            "Funding",
            "NoR_10_to_breakpercent",
            "NoR_break_to_currentpercent"
          )
      ) %>%
      select(LA_name, name, value) %>%
      mutate(
        value = case_when(
          name == "Bangro" ~ as.character(paste0(round(value * 100, 1), "%")),
          name == "Angro" ~ as.character(paste0(round(value * 100, 1), "%")),
          name == "NoR_10_to_breakpercent" ~
            as.character(paste0(round(value * 100, 1), "%")),
          name == "NoR_break_to_currentpercent" ~
            as.character(paste0(round(value * 100, 1), "%")),
          name == "QuanUP" ~ as.character(paste0(round(value, 1), "%")),
          name == "QuanRP" ~ as.character(scales::comma(round(value, 0))),
          name == "QuanSu" ~ as.character(paste0(round(value * 100, 1), "%")),
          name == "Funding" ~
            paste0(as.character(scales::comma(round(value / 1000000, 0)))),
          TRUE ~ as.character(value)
        )
      ) %>%
      mutate(
        name = case_when(
          name == "Bangro" ~
            paste0(
              "Actual change in ",
              str_to_lower(input$phase_choice),
              " pupil numbers 2009/10 to ",
              year_vars()$next_year
            ),
          name == "NoR_10_to_breakpercent" ~
            paste0(
              "Actual change in ",
              str_to_lower(input$phase_choice),
              " pupil numbers 2009/10 to 2018/19"
            ),
          name == "NoR_break_to_currentpercent" ~
            paste0(
              "Actual change in ",
              str_to_lower(input$phase_choice),
              " pupil numbers 2018/19 to ",
              year_vars()$next_year
            ),
          name == "Angro" ~
            paste0(
              "Anticipated change in ",
              str_to_lower(input$phase_choice),
              " pupil numbers ",
              year_vars()$next_year,
              " to ",
              year_vars()$plan_year
            ),
          name == "QuanUP" ~
            paste0(
              "Percentage of unfilled ",
              str_to_lower(input$phase_choice),
              " places ",
              year_vars()$this_year
            ),
          name == "QuanRP" ~
            paste0(
              "Estimated additional ",
              str_to_lower(input$phase_choice),
              " places needed to meet demand in ",
              year_vars()$plan_year
            ),
          name == "QuanSu" ~
            paste0(
              "Estimated percentage of spare ",
              str_to_lower(input$phase_choice),
              " places in ",
              year_vars()$plan_year
            ),
          name == "Funding" ~
            paste0(
              "Total primary and secondary basic need funding to create new places (£millions)"
            ),
          TRUE ~ name
        ),
        LA_name = case_when(
          is.na(LA_name) ~ "England",
          TRUE ~ LA_name
        )
      ) %>%
      # pivot the data wider
      pivot_wider(names_from = name, values_from = value) %>%
      select(
        LA_name,
        starts_with("Actual"),
        starts_with("Anticipated"),
        starts_with("Percentage"),
        starts_with("Estimated additional"),
        starts_with("Estimated percentage"),
        starts_with("Total")
      ) %>%
      arrange(LA_name) |>
      datatable(
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          dom = "t",
          style = "bootstrap"
        )
      )
  })

  ## Forecast accuracy labels
  output$forecast_title <- renderUI({
    strong(
      paste0("Forecast accuracy of pupil projections for ", year_vars()$forecast_year, ", made one year and three years previously")
    )
  })


  output$label_estimate_y1 <- renderText({
    forecast_accuracy <- live_scorecard_data() %>%
      filter(name == "For_1") %>%
      pull(value) %>%
      round_half_up(., 3)

    Foracc1year <- scorecards_data_pivot %>%
      filter(
        name == "For_1",
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      pull(value) %>%
      round_half_up(., 3)

    medianaccuracy1 <- median(Foracc1year, na.rm = TRUE)

    Twentyfifthpercentile1 <- quantile(Foracc1year, 0.25, na.rm = TRUE)

    Seventyfifthpercentile1 <- quantile(Foracc1year, 0.75, na.rm = TRUE)

    label <- case_when(
      input$LA_choice != "England" &
        forecast_accuracy > 0 &
        forecast_accuracy > Seventyfifthpercentile1 ~
        "Overestimate of pupil numbers, larger overestimate than at least 75% of local authorities",
      input$LA_choice != "England" &
        forecast_accuracy > 0 &
        forecast_accuracy <= Seventyfifthpercentile1 ~
        "Overestimate of pupil numbers, within the middle 25-75% of local authorities' forecast accuracy scores",
      input$LA_choice != "England" &
        forecast_accuracy < 0 &
        forecast_accuracy < Twentyfifthpercentile1 ~
        "Underestimate of pupil numbers, larger underestimation than at least 75% of local authorities",
      input$LA_choice != "England" &
        forecast_accuracy < 0 &
        forecast_accuracy >= Twentyfifthpercentile1 ~
        "Underestimate of pupil numbers, within the middle 25-75% of local authorities' forecast accuracy scores",
      input$LA_choice == "England" & forecast_accuracy > 0 ~
        "Overestimate of pupil numbers",
      input$LA_choice == "England" & forecast_accuracy < 0 ~
        "Underestimate of pupil numbers",
      input$LA_choice == "City of London" ~
        "No forecast accuracy score due to smaller numbers of pupils in City of London",
      input$LA_choice == "Isles of Scilly" ~
        "No forecast accuracy score due to smaller numbers of pupils in Isles of Scilly",
      TRUE ~ "No overestimate/underestimate therefore accurate"
    )

    if (label != "accurate") {
      paste0(
        "<p><b>One year ahead: ",
        format_perc(forecast_accuracy),
        "</b></p><p>",
        label,
        "</p>"
      )
    } else {
      paste0("<p><b>One year ahead: ", "</b></p><p>", label, "</b></p>")
    }
  })

  output$label_estimate_y3 <- renderText({
    forecast_accuracy <- live_scorecard_data() %>%
      filter(name == "For_3") %>%
      pull(value) %>%
      round_half_up(., 3)

    Foracc3year <- scorecards_data_pivot %>%
      filter(
        name == "For_3",
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      pull(value) %>%
      round_half_up(., 3)

    medianaccuracy3 <- median(Foracc3year, na.rm = TRUE)

    Twentyfifthpercentile2 <- quantile(Foracc3year, 0.25, na.rm = TRUE)

    Seventyfifthpercentile2 <- quantile(Foracc3year, 0.75, na.rm = TRUE)

    label <- case_when(
      input$LA_choice != "England" &
        forecast_accuracy > 0 &
        forecast_accuracy > Seventyfifthpercentile2 ~
        "Overestimate of pupil numbers, larger overestimate than at least 75% of local authorities",
      input$LA_choice != "England" &
        forecast_accuracy > 0 &
        forecast_accuracy <= Seventyfifthpercentile2 ~
        "Overestimate of pupil numbers, within the middle 25-75% of local authorities' forecast accuracy scores",
      input$LA_choice != "England" &
        forecast_accuracy < 0 &
        forecast_accuracy < Twentyfifthpercentile2 ~
        "Underestimate of pupil numbers, larger underestimation than at least 75% of local authorities",
      input$LA_choice != "England" &
        forecast_accuracy < 0 &
        forecast_accuracy >= Twentyfifthpercentile2 ~
        "Underestimate of pupil numbers, within the middle 25-75% of local authorities' forecast accuracy scores",
      input$LA_choice == "England" & forecast_accuracy > 0 ~
        "Overestimate of pupil numbers",
      input$LA_choice == "England" & forecast_accuracy < 0 ~
        "Underestimate of pupil numbers",
      input$LA_choice == "City of London" ~
        "No forecast accuracy score due to smaller numbers of pupils in City of London",
      input$LA_choice == "Isles of Scilly" ~
        "No forecast accuracy score due to smaller numbers of pupils in Isles of Scilly",
      TRUE ~ "No overestimate/underestimate therefore accurate"
    )

    if (label != "accurate") {
      paste0(
        "<p><b>Three years ahead: ",
        format_perc(forecast_accuracy),
        "</b></p><p>",
        label,
        "</p>"
      )
    } else {
      paste("<p><b>Three years ahead: </b></p><p>", label, "</p>")
    }
  })

  output$for1year_table <- renderDataTable({
    scorecards_data_pivot %>%
      filter(
        name == "For_1",
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      mutate(
        Median = format_perc(median(value, na.rm = TRUE)),
        Twentyfifthpercentile = format_perc(quantile(
          value,
          0.25,
          na.rm = TRUE
        )),
        Seventyfifthpercentile = format_perc(quantile(
          value,
          0.75,
          na.rm = TRUE
        )),
        Minimum = format_perc(min(value, na.rm = TRUE)),
        Maximum = format_perc(max(value, na.rm = TRUE)),
      ) %>%
      filter(
        LA_name == "England"
      ) %>%
      select(
        Minimum,
        `25th percentile` = Twentyfifthpercentile,
        Median,
        `75th percentile` = Seventyfifthpercentile,
        Maximum
      ) |>
      datatable(
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          dom = "t",
          style = "bootstrap"
        )
      )
  })

  output$for3year_table <- renderDataTable({
    scorecards_data_pivot %>%
      filter(
        name == "For_3",
        Phase == input$phase_choice,
        Year == input$year_choice
      ) %>%
      mutate(
        Median = format_perc(median(value, na.rm = TRUE)),
        Twentyfifthpercentile = format_perc(quantile(
          value,
          0.25,
          na.rm = TRUE
        )),
        Seventyfifthpercentile = format_perc(quantile(
          value,
          0.75,
          na.rm = TRUE
        )),
        Minimum = format_perc(min(value, na.rm = TRUE)),
        Maximum = format_perc(max(value, na.rm = TRUE)),
      ) %>%
      filter(
        LA_name == "England"
      ) %>%
      select(
        Minimum,
        `25th percentile` = Twentyfifthpercentile,
        Median,
        `75th percentile` = Seventyfifthpercentile,
        Maximum
      ) |>
      datatable(
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          dom = "t",
          style = "bootstrap"
        )
      )
  })

  ## Forecast accuracy three years ahead

  # Code to go here using above template

  output$forecasting.bartext <- renderUI(
    if (input$LA_choice != "England") {
      tagList(
        p(
          paste0(
            "The shaded area ending at the thick vertical line in each chart ",
            "shows the forecasting accuracy for ",
            input$LA_choice,
            ". The starting point is 0, an accurate score, indicated by the thin ",
            "vertical line. A shaded area to the right of 0 indicates an ",
            "overestimate, a shaded area to the left of 0 indicates an ",
            "underestimate. The dashed lines show the 25th and 75th percentiles ",
            "across all local authorities i.e. half of all local authorities were ",
            "found to have a forecasting accuracy falling between the two dashed ",
            "lines."
          )
        )
      )
    } else if (input$LA_choice == "England") {
      tagList(
        p(
          "The shaded area ending at the thick vertical line in each chart ",
          "shows the average forecasting accuracy for local authorities in ",
          "England. The starting point is 0, an accurate score, indicated by ",
          "the thin vertical line. A shaded area to the right of 0 indicates ",
          "an overestimate, a shaded area to the left of 0 indicates an ",
          "underestimate. The dashed lines show the 25th and 75th percentiles ",
          "across all local authorities i.e. half of all local authorities ",
          "were found to have a forecasting accuracy falling between the ",
          "two dashed lines."
        )
      )
    }
  )

  output$forecast_1y_bar <- renderGirafe({
    p <- plot_forecast(
      live_scorecard_data(),
      scorecards_data_pivot,
      input$LA_choice,
      input$phase_choice,
      1
    )
    girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 3,
      options = list(
        opts_tooltip(css = "background-color:white; color:black; padding: 5px; border: 1px solid #ccc;"),
        opts_hover(css = "fill-opacity:0.7;cursor:pointer;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })

  output$forecast_3y_bar <- renderGirafe({
    p <- plot_forecast(
      live_scorecard_data(),
      scorecards_data_pivot,
      input$LA_choice,
      input$phase_choice,
      3
    )
    girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 3,
      options = list(
        opts_tooltip(css = "background-color:white; color:black; padding: 5px; border: 1px solid #ccc;"),
        opts_hover(css = "fill-opacity:0.7;cursor:pointer;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })

  # Preference -------------------------------------------------------------

  # to fill in here - use the output$pupil_growth as a template :)

  # Box for England % preference current year

  output$preference_eng_subtitle <- renderText({
    paste0(
      "Percentage of applicants who received an offer of one of their top ",
      "three preferred ",
      str_to_lower(input$phase_choice),
      " schools in England"
    )
  })

  output$preference_la_subtitle <- renderText({
    ifelse(
      input$LA_choice != "England",
      paste0(
        "Percentage of applicants who received an offer of one of their top ",
        "three preferred ",
        str_to_lower(input$phase_choice),
        " schools in ",
        (input$LA_choice)
      ),
      ""
    )
  })

  output$prefT3_CY_ENG <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    PrefT3_CY_E <- live_scorecard_data_all_la() %>%
      filter(name == "PrefT3_CY") %>%
      filter(LA_name == "England") %>%
      pull(value) %>%
      round_half_up(., 1)

    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(PrefT3_CY_E, "%"),
      title = paste0("in England for ", year_vars()$preference_current_year),
      # icon = icon("fas fa-chart-line"),
      theme = "info"
    )
  })

  # Box for England % preference next year
  output$prefT3_NY_ENG <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    PrefT3_NY_E <- live_scorecard_data_all_la() %>%
      filter(name == "PrefT3_NY") %>%
      filter(LA_name == "England") %>%
      pull(value) %>%
      round_half_up(., 1)

    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(PrefT3_NY_E, "%"),
      title = paste0("in England for ", year_vars()$preference_next_year),
      # icon = icon("fas fa-chart-line"),
      theme = "info"
    )
  })

  # Box for LA % preference current year



  output$PrefT3_CY_LA <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    PrefT3_CY <- live_scorecard_data() %>%
      filter(name == "PrefT3_CY") %>%
      pull(value) %>%
      round_half_up(., 1)

    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(PrefT3_CY, "%"),
      title = paste0("in ", (input$LA_choice), " for ", year_vars()$preference_current_year),
      # icon = icon("fas fa-sort-amount-up"),
      theme = "primary"
    )
  })

  # Box for LA % preference next year

  output$PrefT3_NY_LA <- renderUI({
    # Take filtered data, search for growth rate, pull the value and tidy the number up
    PrefT3_NY <- live_scorecard_data() %>%
      filter(name == "PrefT3_NY") %>%
      pull(value) %>%
      round_half_up(., 1)

    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(PrefT3_NY, "%"),
      title = paste0("in ", (input$LA_choice), " for ", year_vars()$preference_next_year),
      # icon = icon("fas fa-sort-amount-up"),
      theme = "primary"
    )
  })

  # Stacked bar instead of pie here for preference?
  # Easier for users to interpret
  output$preference_p <- renderGirafe({
    # reshape the data so it plots neatly!
    preference_data <- live_scorecard_data_england_comp_pref() %>%
      # select only preference values
      filter(
        name %in%
          c(
            "Pref1_CY",
            "Pref2_CY",
            "Pref3_CY",
            "Pref1_NY",
            "Pref2_NY",
            "Pref3_NY"
          )
      ) %>%
      # Create groups for "current year" and "next year" places based on names
      mutate(
        preference_year = case_when(
          str_detect(name, "CY") ~ year_vars()$preference_current_year,
          str_detect(name, "NY") ~ year_vars()$preference_next_year
        )
      ) %>%
      # Create ratings out of the names
      mutate(
        rating = case_when(
          str_detect(name, "1") ~ "First",
          str_detect(name, "2") ~ "Second",
          str_detect(name, "3") ~ "Third"
        )
      )

    # Get % not getting 1st 2nd or 3rd preference
    preference_data_sum <- preference_data %>%
      group_by(preference_year, LA_name, LANumber, Phase) %>%
      summarise(value = 100 - sum(value)) %>%
      mutate(rating = "Other")

    preference_data <- preference_data %>%
      select(-name) %>%
      bind_rows(preference_data_sum) %>%
      # sort levels out so plots in correct order
      mutate(
        LA_name = factor(LA_name) %>% relevel(input$LA_choice),
      ) %>%
      arrange(preference_year, LA_name, rating) %>%
      # Neaten up percs
      mutate(
        value = as.numeric(round_half_up(value, 1)),
        value_label = if_else(value > 3, paste0(value, "%"), NA_character_),
        tooltip_text = paste(rating, ": ", value, "%")
      )


    preference_data_ordering <- rbind(
      subset(preference_data, rating == "First"),
      subset(preference_data, rating == "Second"),
      subset(preference_data, rating == "Third"),
      subset(preference_data, rating == "Other")
    ) %>%
      pull(tooltip_text) %>%
      unique()


    preference_data$tooltip_text <- factor(preference_data$tooltip_text, levels = preference_data_ordering)

    preference_p <- preference_data %>%
      mutate(rating = factor(rating, levels = c("First", "Second", "Third", "Other"))) %>%
      ggplot(aes(
        y = value,
        x = preference_year,
        fill = rating
      )) +
      geom_bar_interactive(
        aes(
          tooltip = tooltip_text,
          data_id = paste(preference_year, LA_name, rating)
        ),
        stat = "identity",
        position = position_fill(reverse = TRUE)
      ) +
      coord_flip() +
      facet_wrap(~LA_name, nrow = 2) +
      geom_text_interactive(
        aes(
          label = value_label,
          tooltip = tooltip_text,
        ),
        colour = "#ffffff",
        size = 8,
        position = position_fill(reverse = TRUE, vjust = 0.5)
      ) +
      labs(x = "", y = "", fill = NULL) +
      scale_fill_manual(
        values = c("First" = "#08519c", "Second" = "#3182bd", "Third" = "#6baed6", "Other" = "#9ecae1")
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 36, family = "Arial"),
        strip.text.x = element_text(size = 36),
        ggtitle("Chart showing percentage of pupils receiving an offer from their first, second, third or other place schools, by Local Authority compared to England")
      )

    girafe(
      ggobj = preference_p,
      width_svg = 18,
      height_svg = 8,
      options = list(
        opts_tooltip(css = "background-color:white; color:black; padding: 5px; border: 1px solid #ccc;"),
        opts_hover(css = "fill-opacity:0.7;cursor:pointer;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })

  # Quality -----------------------------------------------------------------

  # Change name of what "better than average" is depending on chart choice:
  school_description <- reactive({
    if (input$chart_choice == "Ofsted Rating") {
      "good and outstanding "
    } else {
      "well above and above average "
    }
  })

  # Calculate LA % depending on chart choice:
  LA_comp <- reactive({
    if (input$chart_choice == "Ofsted Rating" & input$year_choice %in% c("2022/23", "2023/24")) {
      live_scorecard_data() %>%
        filter(name == "QualProp") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    } else if (input$chart_choice == "Progress 8") {
      live_scorecard_data() %>%
        filter(name == "Qual_KS4_Prop") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    } else if (input$chart_choice == "Reading Progress") {
      live_scorecard_data() %>%
        filter(name == "Qual_KS2Read_Prop") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    } else if (input$chart_choice == "Maths Progress") {
      live_scorecard_data() %>%
        filter(name == "Qual_KS2Mat_Prop") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    }
  })


  # Calculate LA % depending on chart choice:
  ofsted_LA_comp_QE <- reactive({
    if (input$chart_choice == "Ofsted Rating") {
      live_scorecard_data() %>%
        filter(name == "QualProp_QE") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    }
  })

  ofsted_LA_comp_BA <- reactive({
    if (input$chart_choice == "Ofsted Rating") {
      live_scorecard_data() %>%
        filter(name == "QualProp_BA") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    }
  })

  ofsted_LA_comp_PD <- reactive({
    if (input$chart_choice == "Ofsted Rating") {
      live_scorecard_data() %>%
        filter(name == "QualProp_PD") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    }
  })

  ofsted_LA_comp_LM <- reactive({
    if (input$chart_choice == "Ofsted Rating") {
      live_scorecard_data() %>%
        filter(name == "QualProp_LM") %>%
        pull(value) %>%
        round_half_up(., 3) *
        100
    }
  })


  # box for % of new places in good and outstanding schools
  output$LA_GO_places <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = ifelse(is.na(LA_comp()), "0 new places created", paste0(LA_comp(), "%")),
      title = paste0(
        "Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in ",
        input$LA_choice
      ),
      # icon = icon("fas fa-boxes"),
      theme = "primary"
    )
  })

  output$LA_GO_places_Ofsted_QE <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = ifelse(is.na(ofsted_LA_comp_QE()), "0 new places created", paste0(ofsted_LA_comp_QE(), "%")),
      title = paste0(
        "Quality of Education: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in ",
        input$LA_choice
      ),
      # icon = icon("fas fa-boxes"),
      theme = "primary"
    )
  })

  output$LA_GO_places_Ofsted_BA <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = ifelse(is.na(ofsted_LA_comp_BA()), "0 new places created", paste0(ofsted_LA_comp_BA(), "%")),
      title = paste0(
        "Behaviour and Attitudes: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in ",
        input$LA_choice
      ),
      # icon = icon("fas fa-boxes"),
      theme = "primary"
    )
  })

  output$LA_GO_places_Ofsted_PD <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = ifelse(is.na(ofsted_LA_comp_PD()), "0 new places created", paste0(ofsted_LA_comp_PD(), "%")),
      title = paste0(
        "Personal Development: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in ",
        input$LA_choice
      ),
      # icon = icon("fas fa-boxes"),
      theme = "primary"
    )
  })

  output$LA_GO_places_Ofsted_LM <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = ifelse(is.na(ofsted_LA_comp_LM()), "0 new places created", paste0(ofsted_LA_comp_LM(), "%")),
      title = paste0(
        "Leadership and Management: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in ",
        input$LA_choice
      ),
      # icon = icon("fas fa-boxes"),
      theme = "primary"
    )
  })



  # Calculate England comparator depending on chart choice:
  england_comp <- reactive({
    if (input$chart_choice == "Progress 8") {
      numerator <- live_scorecard_data_all_la() %>%
        filter(
          LA_name == "England" &
            name %in% c("KS4_WAA_N", "KS4_AA_N")
        ) %>%
        summarise(sum(value)) %>%
        as.numeric()

      denominator <- live_scorecard_data_all_la() %>%
        filter(
          LA_name == "England" &
            name %in%
              c("KS4_WAA_N", "KS4_AA_N", "KS4_A_N", "KS4_BA_N", "KS4_WBA_N")
        ) %>%
        summarise(sum(value)) %>%
        as.numeric()

      # calculate percentage
      round_half_up(numerator / denominator * 100, 1)
    } else if (input$chart_choice == "Reading Progress") {
      numerator <- live_scorecard_data_all_la() %>%
        filter(
          LA_name == "England" &
            name %in% c("KS2Read_WAA_N", "KS2Read_AA_N")
        ) %>%
        summarise(sum(value)) %>%
        as.numeric()

      denominator <- live_scorecard_data_all_la() %>%
        filter(
          LA_name == "England" &
            name %in%
              c(
                "KS2Read_WAA_N",
                "KS2Read_AA_N",
                "KS2Read_A_N",
                "KS2Read_BA_N",
                "KS2Read_WBA_N"
              )
        ) %>%
        summarise(sum(value)) %>%
        as.numeric()

      # calculate percentage
      round_half_up(numerator / denominator * 100, 1)
    } else if (input$chart_choice == "Maths Progress") {
      numerator <- live_scorecard_data_all_la() %>%
        filter(
          LA_name == "England" &
            name %in% c("KS2Mat_WAA_N", "KS2Mat_AA_N")
        ) %>%
        summarise(sum(value)) %>%
        as.numeric()

      denominator <- live_scorecard_data_all_la() %>%
        filter(
          LA_name == "England" &
            name %in%
              c(
                "KS2Mat_WAA_N",
                "KS2Mat_AA_N",
                "KS2Mat_A_N",
                "KS2Mat_BA_N",
                "KS2Mat_WBA_N"
              )
        ) %>%
        summarise(sum(value)) %>%
        as.numeric()

      # calculate percentage
      round_half_up(numerator / denominator * 100, 1)
    }
  })

  ofsted_england_comp <- reactive({
    numerator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N", "Qual2_N")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    denominator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N", "Qual2_N", "Qual3_N", "Qual4_N")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    # calculate percentage
    round_half_up(numerator / denominator * 100, 1)
  })

  ofsted_england_comp_QE <- reactive({
    numerator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_QE", "Qual2_N_QE")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    denominator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_QE", "Qual2_N_QE", "Qual3_N_QE", "Qual4_N_QE")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    # calculate percentage
    round_half_up(numerator / denominator * 100, 1)
  })

  ofsted_england_comp_BA <- reactive({
    numerator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_BA", "Qual2_N_BA")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    denominator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_BA", "Qual2_N_BA", "Qual3_N_BA", "Qual4_N_BA")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    # calculate percentage
    round_half_up(numerator / denominator * 100, 1)
  })

  ofsted_england_comp_PD <- reactive({
    numerator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_PD", "Qual2_N_PD")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    denominator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_PD", "Qual2_N_PD", "Qual3_N_PD", "Qual4_N_PD")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    # calculate percentage
    round_half_up(numerator / denominator * 100, 1)
  })


  ofsted_england_comp_LM <- reactive({
    numerator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_LM", "Qual2_N_LM")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    denominator <- live_scorecard_data_all_la() %>%
      filter(
        LA_name == "England" &
          name %in% c("Qual1_N_LM", "Qual2_N_LM", "Qual3_N_LM", "Qual4_N_LM")
      ) %>%
      summarise(sum(value)) %>%
      as.numeric()

    # calculate percentage
    round_half_up(numerator / denominator * 100, 1)
  })

  # box for % of new places in top schools - England - for progress
  output$England_GO_places <- renderUI({
    use_ofsted <- input$chart_choice == "Ofsted Rating"

    value <- if (use_ofsted) {
      ofsted_england_comp()
    } else {
      england_comp()
    }

    bslib::value_box(
      value = paste0(value, "%"),
      title = paste0(
        "Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in England"
      ),
      theme = "info"
    )
  })


  # box for % of new places in top schools - England - for new Ofsted metric
  output$England_GO_places_Ofsted_QE <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(ofsted_england_comp_QE(), "%"),
      title = paste0(
        "Quality of Education: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in England"
      ),
      # icon = icon("fas fa-equals"),
      theme = "info"
    )
  })

  # box for % of new places in top schools - England - for new Ofsted metric
  output$England_GO_places_Ofsted_BA <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(ofsted_england_comp_BA(), "%"),
      title = paste0(
        "Behaviour and Attitudes: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in England"
      ),
      # icon = icon("fas fa-equals"),
      theme = "info"
    )
  })

  # box for % of new places in top schools - England - for new Ofsted metric
  output$England_GO_places_Ofsted_PD <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(ofsted_england_comp_PD(), "%"),
      title = paste0(
        "Personal Development: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in England"
      ),
      # icon = icon("fas fa-equals"),
      theme = "info"
    )
  })

  # box for % of new places in top schools - England - for new Ofsted metric
  output$England_GO_places_Ofsted_LM <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = paste0(ofsted_england_comp_LM(), "%"),
      title = paste0(
        "Leadership and Management: Percentage of new places created in ",
        school_description(),
        str_to_lower(input$phase_choice),
        " schools in England"
      ),
      # icon = icon("fas fa-equals"),
      theme = "info"
    )
  })

  # Calculate % ranking depending on chart choice:
  LA_ranking <- reactive({
    if (input$chart_choice == "Ofsted Rating") {
      live_scorecard_data() %>%
        filter(name == "QualPropranks") %>%
        pull(value)
    } else if (input$chart_choice == "Progress 8") {
      live_scorecard_data() %>%
        filter(name == "Qual_KS4_Propranks") %>%
        pull(value)
    } else if (input$chart_choice == "Reading Progress") {
      live_scorecard_data() %>%
        filter(name == "Qual_KS2Read_Propranks") %>%
        pull(value)
    } else if (input$chart_choice == "Maths Progress") {
      live_scorecard_data() %>%
        filter(name == "Qual_KS2Mat_Propranks") %>%
        pull(value)
    }
  })

  # Calculate ranking denominator depending on chart choice:
  LA_denom <- reactive({
    if (input$chart_choice == "Ofsted Rating") {
      live_scorecard_data_all_la() %>%
        filter(name == "QualPropranks" & !is.na(value)) %>%
        nrow()
    } else if (input$chart_choice == "Progress 8") {
      live_scorecard_data_all_la() %>%
        filter(name == "Qual_KS4_Propranks" & !is.na(value)) %>%
        nrow()
    } else if (input$chart_choice == "Reading Progress") {
      live_scorecard_data_all_la() %>%
        filter(name == "Qual_KS2Read_Propranks" & !is.na(value)) %>%
        nrow()
    } else if (input$chart_choice == "Maths Progress") {
      live_scorecard_data_all_la() %>%
        filter(name == "Qual_KS2Mat_Propranks" & !is.na(value)) %>%
        nrow()
    }
  })

  # box for % of new places in top schools - LA Ranking

  output$LA_GO_ran <- renderUI({
    # Put value into box to plug into app
    bslib::value_box(
      value = LA_ranking(),
      title = paste0(
        "LA Rank out of ",
        LA_denom(),
        " LAs that created new places between ",
        year_vars()$last_year,
        " and ",
        year_vars()$this_year,
        " (ranks can be tied)"
      ),
      # icon = icon("fas fa-bars"),
      theme = "primary"
    )
  })

  output$quality_note <- renderText({
    if (input$chart_choice == "Ofsted Rating" && input$year_choice == "2024/25") {
      "Please note: Schools inspected before September 2023 do not have graded judgements for the four areas reported above. For these schools their overall effectiveness grade has been used for each judgement area in the graphs and value boxes presented above."
    } else if (input$chart_choice != "Ofsted Rating" && input$year_choice == "2024/25") {
      "Please note: The 2025 Scorecard uses reading and maths progress data from 2022/23 and Progress 8 from 2023/24 because primary tests and assessments were cancelled in 2019/20 and 2020/21 due to COVID-19 disruption."
    } else if (input$chart_choice != "Ofsted Rating" && input$year_choice == "2023/24" && input$phase_choice == "Primary") {
      "Please note: The 2024 Scorecard uses reading and maths progress data from 2022/23 because primary tests and assessments were cancelled in 2019/20 and 2020/21 due to COVID-19 disruption."
    }
  })

  output$nca_quality_note <- renderText({
    if (input$year_choice == "2024/25" && input$phase_choice == "Secondary") {
      "New school places for 2024/25 have been affected by the Net Capacity Assessment programme and therefore do not represent school places built between 2023/24 and 2024/25."
    }
  })

  observe({
    shinyjs::hide("England_GO_places_Ofsted_QE")
    shinyjs::hide("England_GO_places_Ofsted_BA")
    shinyjs::hide("England_GO_places_Ofsted_PD")
    shinyjs::hide("England_GO_places_Ofsted_LM")

    shinyjs::hide("LA_GO_places_Ofsted_QE")
    shinyjs::hide("LA_GO_places_Ofsted_BA")
    shinyjs::hide("LA_GO_places_Ofsted_PD")
    shinyjs::hide("LA_GO_places_Ofsted_LM")

    shinyjs::hide("England_GO_places")
    shinyjs::hide("LA_GO_places")
    shinyjs::hide("England_GO_places_Ofsted")

    if (input$chart_choice == "Ofsted Rating" && input$year_choice == "2024/25") {
      if (input$LA_choice == "England") {
        shinyjs::hide("England_GO_places")
        shinyjs::show("England_GO_places_Ofsted_QE")
        shinyjs::show("England_GO_places_Ofsted_BA")
        shinyjs::show("England_GO_places_Ofsted_PD")
        shinyjs::show("England_GO_places_Ofsted_LM")
      } else {
        shinyjs::hide("LA_GO_places")
        shinyjs::show("England_GO_places_Ofsted_QE")
        shinyjs::show("England_GO_places_Ofsted_BA")
        shinyjs::show("England_GO_places_Ofsted_PD")
        shinyjs::show("England_GO_places_Ofsted_LM")
        shinyjs::show("LA_GO_places_Ofsted_QE")
        shinyjs::show("LA_GO_places_Ofsted_BA")
        shinyjs::show("LA_GO_places_Ofsted_PD")
        shinyjs::show("LA_GO_places_Ofsted_LM")
      }
    } else if (input$chart_choice != "Ofsted Rating" && input$year_choice == "2024/25") {
      if (input$LA_choice == "England") {
        shinyjs::show("England_GO_places")
      } else {
        shinyjs::show("England_GO_places")
        shinyjs::show("LA_GO_places")
      }
    } else if (input$year_choice != "2024/25") {
      if (input$LA_choice == "England") {
        shinyjs::show("England_GO_places")
      } else {
        shinyjs::show("England_GO_places")
        shinyjs::show("LA_GO_places")
      }
    }
  })

  # Quality - charts --------------------------------------------------------

  # Final step once charts are ready - making this bit reactive with a dropdown

  rv <- reactiveValues()

  output$quality_chart <- renderGirafe({
    girafe(
      ggobj = rv$quality_chart_choice,
      width_svg = 18,
      height_svg = 8,
      options = list(
        opts_tooltip(css = "background-color:white; color:black; padding: 5px; border: 1px solid #ccc;"),
        opts_hover(css = "fill-opacity:0.7;cursor:pointer;"),
        opts_toolbar(saveaspng = FALSE),
        opts_selection(type = "none")
      )
    )
  })

  output$no_rating_line <- renderUI({
    LA <- paste0(
      input$LA_choice,
      " = ",
      scales::comma(rv$no_rating[1])
    )

    if (!is.null(input$selectBenchLAsquality) &&
      input$selectBenchLAsquality != "") {
      Benchmark <- paste0(
        input$selectBenchLAsquality,
        " = ",
        scales::comma(rv$no_rating[2])
      )

      combined <- paste(LA, Benchmark, sep = "; ")
    } else {
      combined <- LA
    }

    req(!all(is.na(rv$no_rating)))

    warning_text(
      inputId = "no_rating_warning",
      text = paste0("New places with no rating: ", combined)
    )
  })

  observe({
    # Bar chart comparison - Ofsted

    # reshape the data so it plots neatly!
    ofsted_data <- live_scorecard_data_england_comp_quality() %>%
      # select only the ofsted values
      filter(
        name %in%
          c(
            "Qual1_N",
            "Qual2_N",
            "Qual3_N",
            "Qual4_N",
            "Qual0_N",
            "Qual1_E",
            "Qual2_E",
            "Qual3_E",
            "Qual4_E",
            "Qual0_E"
          )
      ) %>%
      # Create groups for "new" and "existing" places based on names
      mutate(
        place_type = case_when(
          str_detect(name, "N") ~ "New",
          str_detect(name, "E") ~ "Existing"
        )
      ) %>%
      # Create Ofsted ratings out of the names
      mutate(
        rating = case_when(
          str_detect(name, "1") ~ "Outstanding",
          str_detect(name, "2") ~ "Good",
          str_detect(name, "3") ~ "Requires Improvement",
          str_detect(name, "4") ~ "Inadequate",
          str_detect(name, "0") ~ "No rating"
        )
      ) %>%
      # Create new variable called places, replace 0s with NAs so it plots neatly
      mutate(
        places = if_else(
          value == 0,
          NA_integer_,
          as.integer(round_half_up(value, 0))
        )
      ) %>%
      # Give NA for label if it's too small
      group_by(LA_name, place_type) %>%
      mutate(
        places_perc = places / sum(places, na.rm = TRUE),
        value_label = if_else(places == 0, NA, places)
      )

    ofsted_p <- ofsted_data %>%
      filter(rating != "No rating") %>%
      mutate(rating = factor(rating, levels = c("Outstanding", "Good", "Requires Improvement", "Inadequate"))) %>%
      ggplot(aes(
        y = value,
        x = place_type,
        fill = rating,
        tooltip = paste(rating, ": ", places, " places")
      )) +
      geom_bar_interactive(
        stat = "identity",
        position = position_fill(reverse = TRUE)
      ) +
      coord_flip() +
      facet_wrap(~LA_name, nrow = 2) +
      geom_text_interactive(
        aes(label = scales::comma(value_label)),
        size = 8,
        colour = "#FFFFFF",
        position = position_fill(reverse = TRUE, vjust = 0.5)
      ) +
      labs(x = "", y = "", fill = NULL) +
      scale_fill_manual(
        values = c("#08519c", "#3182bd", "#6baed6", "#9ecae1")
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 42, family = "Arial"),
        strip.text.x = element_text(size = 42)
      )


    # Stacked Bar - Ofsted
    output$quality_chart_stacked <- renderGirafe({
      girafe(
        ggobj = rv$quality_chart_choice,
        width_svg = 18,
        height_svg = 8,
        options = list(
          opts_tooltip(css = "background-color:white; color:black; padding: 5px; border: 1px solid #ccc;"),
          opts_hover(css = "fill-opacity:0.7;cursor:pointer;"),
          opts_toolbar(saveaspng = FALSE),
          opts_selection(type = "none")
        )
      )
    })


    ofsted_data_stacked <- live_scorecard_data_england_comp_quality() %>%
      # select only the ofsted values
      filter(
        name %in%
          c(
            "Qual1_N_QE",
            "Qual2_N_QE",
            "Qual3_N_QE",
            "Qual4_N_QE",
            "QualNA_N_QE",
            "Qual1_E_QE",
            "Qual2_E_QE",
            "Qual3_E_QE",
            "Qual4_E_QE",
            "QualNA_E_QE",
            "Qual1_N_BA",
            "Qual2_N_BA",
            "Qual3_N_BA",
            "Qual4_N_BA",
            "QualNA_N_BA",
            "Qual1_E_BA",
            "Qual2_E_BA",
            "Qual3_E_BA",
            "Qual4_E_BA",
            "QualNA_E_BA",
            "Qual1_N_PD",
            "Qual2_N_PD",
            "Qual3_N_PD",
            "Qual4_N_PD",
            "QualNA_N_PD",
            "Qual1_E_PD",
            "Qual2_E_PD",
            "Qual3_E_PD",
            "Qual4_E_PD",
            "QualNA_E_PD",
            "Qual1_N_LM",
            "Qual2_N_LM",
            "Qual3_N_LM",
            "Qual4_N_LM",
            "QualNA_N_LM",
            "Qual1_E_LM",
            "Qual2_E_LM",
            "Qual3_E_LM",
            "Qual4_E_LM",
            "QualNA_E_LM"
          )
      ) %>%
      filter(str_detect(name, "^Qual(NA|[0-4])_[NE]_(QE|BA|PD|LM)$")) %>%
      mutate(
        rating = case_when(
          str_detect(name, "^Qual1_") ~ "Outstanding",
          str_detect(name, "^Qual2_") ~ "Good",
          str_detect(name, "^Qual3_") ~ "Requires Improvement",
          str_detect(name, "^Qual4_") ~ "Inadequate",
          str_detect(name, "^QualNA_") ~ "No rating",
          TRUE ~ NA_character_
        ),
        place_type = if_else(str_detect(name, "_N_"), "New", "Existing"),
        metric = str_extract(name, "(QE|BA|PD|LM)$"),
        places = as.integer(value)
      ) %>%
      group_by(LA_name, place_type, metric) %>%
      mutate(
        places_perc = places / sum(places, na.rm = TRUE),
        value_label = if_else(places == 0, NA, places)
      ) %>%
      ungroup()

    ofsted_stacked <- ofsted_data_stacked %>%
      filter(rating != "No rating") %>%
      mutate(
        rating = factor(
          rating,
          levels = c(
            "Outstanding", "Good",
            "Requires Improvement", "Inadequate"
          )
        ),
        metric = factor(metric, levels = c("QE", "BA", "PD", "LM")),
        place_type = factor(place_type, levels = c("Existing", "New")),
        LA_name = factor(
          LA_name,
          levels = unique(c(input$LA_choice, input$selectBenchLAsquality))
        )
      ) %>%
      ggplot(aes(
        x = metric,
        y = value,
        fill = rating,
        tooltip = paste(
          "Metric:",
          recode(metric,
            "QE" = "Quality of Education",
            "BA" = "Behaviour and Attitudes",
            "PD" = "Personal Development",
            "LM" = "Leadership and Management"
          ),
          "<br>",
          rating, ": ", places, " places"
        )
      )) +
      geom_bar_interactive(
        stat = "identity",
        position = position_fill(reverse = TRUE)
      ) +
      facet_wrap(~ LA_name + place_type, ncol = 2) +
      geom_text_interactive(
        aes(label = scales::comma(value_label)),
        position = position_fill(reverse = TRUE, vjust = 0.5),
        colour = "white",
        size = 5
      ) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(labels = c(
        QE = "Quality of Education",
        BA = "Behaviour and Attitudes",
        PD = "Personal Development",
        LM = "Leadership and Management"
      )) +
      scale_fill_manual(
        values = c("#08519c", "#3182bd", "#6baed6", "#9ecae1")
      ) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14)
      )


    if (input$LA_choice == "England" & input$chart_choice == "Ofsted Rating" & input$year_choice == "2024/25") {
      ofsted_no_rating <- ofsted_data_stacked %>%
        filter(rating == "No rating" & place_type == "New" & metric == "QE") %>%
        pull(places)
    } else if (input$chart_choice == "Ofsted Rating" & input$year_choice == "2024/25") {
      ofsted_no_rating <- ofsted_data_stacked %>%
        filter(
          LA_name != "England" & rating == "No rating" & place_type == "New" & metric == "QE"
        ) %>%
        pull(places)
    } else if (input$LA_choice == "England" & input$chart_choice == "Ofsted Rating" & input$year_choice %in% c("2023/24", "2022/23")) {
      ofsted_no_rating <- ofsted_data %>%
        filter(rating == "No rating" & place_type == "New") %>%
        pull(places)
    } else if (input$chart_choice == "Ofsted Rating" & input$year_choice %in% c("2023/24", "2022/23")) {
      ofsted_no_rating <- ofsted_data %>%
        filter(
          LA_name != "England" & rating == "No rating" & place_type == "New"
        ) %>%
        pull(places)
    }

    # Bar chart comparison - Progress 8

    # reshape the data so it plots neatly!
    progress_8_data <- live_scorecard_data_england_comp_quality() %>%
      # select only the progress 8 values
      filter(
        name %in%
          c(
            "KS4_WAA_N",
            "KS4_AA_N",
            "KS4_A_N",
            "KS4_BA_N",
            "KS4_WBA_N",
            "KS4_NR_N",
            "KS4_WAA_E",
            "KS4_AA_E",
            "KS4_A_E",
            "KS4_BA_E",
            "KS4_WBA_E",
            "KS4_NR_E"
          )
      ) %>%
      # Create groups for "new" and "existing" places based on names
      mutate(
        place_type = case_when(
          str_detect(name, "N$") ~ "New",
          str_detect(name, "E$") ~ "Existing"
        )
      ) %>%
      # Create Ofsted ratings out of the names
      mutate(
        rating = case_when(
          str_detect(name, "_WAA_") ~ "Well above average",
          str_detect(name, "_AA_") ~ "Above average",
          str_detect(name, "_A_") ~ "Average",
          str_detect(name, "_BA_") ~ "Below average",
          str_detect(name, "_WBA_") ~ "Well below average",
          str_detect(name, "NR") ~ "No rating"
        )
      ) %>%
      mutate(
        rating = factor(
          rating,
          levels = c(
            "Well above average",
            "Above average",
            "Average",
            "Below average",
            "Well below average",
            "No rating"
          )
        )
      ) %>%
      # Create new variable called places, replace 0s with NAs so it plots neatly
      mutate(
        places = if_else(
          value == 0,
          NA_integer_,
          as.integer(round_half_up(value, 0))
        )
      ) %>%
      # Give NA for label if it's too small
      group_by(LA_name, place_type) %>%
      mutate(
        places_perc = places / sum(places, na.rm = TRUE),
        value_label = if_else(places_perc > 0.05, places, NA_integer_)
      )

    progress_8_p <- progress_8_data %>%
      filter(rating != "No rating") %>%
      ggplot(aes(
        y = value,
        x = place_type,
        tooltip = paste(rating, ": ", places, " places"),
        group = rating,
        fill = rating
      )) +
      geom_bar_interactive(stat = "identity", position = position_fill(reverse = TRUE)) +
      coord_flip() +
      facet_wrap(~LA_name, nrow = 2) +
      geom_text(
        aes(label = scales::comma(value_label)),
        size = 8,
        colour = "#FFFFFF",
        position = position_fill(reverse = TRUE, vjust = 0.5)
      ) +
      labs(x = "", y = "", fill = NULL) +
      scale_fill_manual(
        values = c("#08519c", "#3182bd", "#6baed6", "#9ecae1", "#BFBFBF")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 42, family = "Arial"),
        strip.text.x = element_text(size = 42)
      )

    if (input$LA_choice == "England" & input$chart_choice == "Progress 8") {
      progress_8_no_rating <- progress_8_data %>%
        filter(rating == "No rating" & place_type == "New") %>%
        pull(places)
    } else if (input$chart_choice == "Progress 8") {
      progress_8_no_rating <- progress_8_data %>%
        filter(
          LA_name != "England" & rating == "No rating" & place_type == "New"
        ) %>%
        pull(places)
    }

    # Bar chart comparison - Progress Reading

    # reshape the data so it plots neatly!
    progress_reading_data <- live_scorecard_data_england_comp_quality() %>%
      # select only the reading values
      filter(
        name %in%
          c(
            "KS2Read_WAA_N",
            "KS2Read_AA_N",
            "KS2Read_A_N",
            "KS2Read_BA_N",
            "KS2Read_WBA_N",
            "KS2Read_NR_N",
            "KS2Read_WAA_E",
            "KS2Read_AA_E",
            "KS2Read_A_E",
            "KS2Read_BA_E",
            "KS2Read_WBA_E",
            "KS2Read_NR_E"
          )
      ) %>%
      # Create groups for "new" and "existing" places based on names
      mutate(
        place_type = case_when(
          str_detect(name, "N$") ~ "New",
          str_detect(name, "E$") ~ "Existing"
        )
      ) %>%
      # Create Ofsted ratings out of the names
      mutate(
        rating = case_when(
          str_detect(name, "_WAA_") ~ "Well above average",
          str_detect(name, "_AA_") ~ "Above average",
          str_detect(name, "_A_") ~ "Average",
          str_detect(name, "_BA_") ~ "Below average",
          str_detect(name, "_WBA_") ~ "Well below average",
          str_detect(name, "_NR_") ~ "No rating"
        )
      ) %>%
      mutate(
        rating = factor(
          rating,
          levels = c(
            "Well above average",
            "Above average",
            "Average",
            "Below average",
            "Well below average",
            "No rating"
          )
        )
      ) %>%
      # Create new variable called places, replace 0s with NAs so it plots neatly
      mutate(
        places = if_else(
          value == 0,
          NA_integer_,
          as.integer(round_half_up(value, 0))
        )
      ) %>%
      # Give NA for label if it's too small
      group_by(LA_name, place_type) %>%
      mutate(
        places_perc = places / sum(places, na.rm = TRUE),
        value_label = if_else(places_perc > 0.05, places, NA_integer_)
      )

    progress_reading_p <- progress_reading_data %>%
      filter(rating != "No rating") %>%
      ggplot(aes(
        y = value,
        x = place_type,
        tooltip = paste(rating, ": ", places, " places"),
        group = rating,
        fill = rating
      )) +
      geom_bar_interactive(stat = "identity", position = position_fill(reverse = TRUE)) +
      coord_flip() +
      facet_wrap(~LA_name, nrow = 2) +
      geom_text(
        aes(label = scales::comma(value_label)),
        size = 8,
        colour = "#FFFFFF",
        position = position_fill(reverse = TRUE, vjust = 0.5)
      ) +
      labs(x = "", y = "", fill = NULL) +
      scale_fill_manual(
        values = c("#08519c", "#3182bd", "#6baed6", "#9ecae1", "#BFBFBF")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 36, family = "Arial"),
        strip.text.x = element_text(size = 42)
      )

    if (
      input$LA_choice == "England" & input$chart_choice == "Reading Progress"
    ) {
      progress_reading_no_rating <- progress_reading_data %>%
        filter(rating == "No rating" & place_type == "New") %>%
        pull(places)
    } else if (input$chart_choice == "Reading Progress") {
      progress_reading_no_rating <- progress_reading_data %>%
        filter(
          LA_name != "England" & rating == "No rating" & place_type == "New"
        ) %>%
        pull(places)
    }

    # Bar chart comparison - Progress Maths

    # reshape the data so it plots neatly!
    progress_maths_data <- live_scorecard_data_england_comp_quality() %>%
      # select only the maths values
      filter(
        name %in%
          c(
            "KS2Mat_WAA_N",
            "KS2Mat_AA_N",
            "KS2Mat_A_N",
            "KS2Mat_BA_N",
            "KS2Mat_WBA_N",
            "KS2Mat_NR_N",
            "KS2Mat_WAA_E",
            "KS2Mat_AA_E",
            "KS2Mat_A_E",
            "KS2Mat_BA_E",
            "KS2Mat_WBA_E",
            "KS2Mat_NR_E"
          )
      ) %>%
      # Create groups for "new" and "existing" places based on names
      mutate(
        place_type = case_when(
          str_detect(name, "N$") ~ "New",
          str_detect(name, "E$") ~ "Existing"
        )
      ) %>%
      # Create Ofsted ratings out of the names
      mutate(
        rating = case_when(
          str_detect(name, "_WAA_") ~ "Well above average",
          str_detect(name, "_AA_") ~ "Above average",
          str_detect(name, "_A_") ~ "Average",
          str_detect(name, "_BA_") ~ "Below average",
          str_detect(name, "_WBA_") ~ "Well below average",
          str_detect(name, "NR") ~ "No rating"
        )
      ) %>%
      mutate(
        rating = factor(
          rating,
          levels = c(
            "Well above average",
            "Above average",
            "Average",
            "Below average",
            "Well below average",
            "No rating"
          )
        )
      ) %>%
      # Create new variable called places, replace 0s with NAs so it plots neatly
      mutate(
        places = if_else(
          value == 0,
          NA_integer_,
          as.integer(round_half_up(value, 0))
        )
      ) %>%
      # Give NA for label if it's too small
      group_by(LA_name, place_type) %>%
      mutate(
        places_perc = places / sum(places, na.rm = TRUE),
        value_label = if_else(places_perc > 0.05, places, NA_integer_)
      )

    progress_maths_p <- progress_maths_data %>%
      filter(rating != "No rating") %>%
      ggplot(aes(
        y = value,
        x = place_type,
        tooltip = paste(rating, ": ", places, " places"),
        group = rating,
        fill = rating
      )) +
      geom_bar_interactive(stat = "identity", position = position_fill(reverse = TRUE)) +
      coord_flip() +
      facet_wrap(~LA_name, nrow = 2) +
      geom_text(
        aes(label = scales::comma(value_label)),
        size = 8,
        colour = "#FFFFFF",
        position = position_fill(reverse = TRUE, vjust = 0.5)
      ) +
      labs(x = "", y = "", fill = NULL) +
      scale_fill_manual(
        values = c("#08519c", "#3182bd", "#6baed6", "#9ecae1", "#BFBFBF")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 36, family = "Arial"),
        strip.text.x = element_text(size = 42)
      )

    if (input$LA_choice == "England" & input$chart_choice == "Maths Progress") {
      progress_maths_no_rating <- progress_maths_data %>%
        filter(rating == "No rating" & place_type == "New") %>%
        pull(places)
    } else if (input$chart_choice == "Maths Progress") {
      progress_maths_no_rating <- progress_maths_data %>%
        filter(
          LA_name != "England" & rating == "No rating" & place_type == "New"
        ) %>%
        pull(places)
    }

    # Pick chart to plot based on user input
    if (input$chart_choice == "Ofsted Rating" && input$year_choice == "2024/25") {
      rv$quality_chart_choice <- ofsted_stacked
      rv$no_rating <- ofsted_no_rating
    } else if (input$chart_choice == "Ofsted Rating") {
      rv$quality_chart_choice <- ofsted_p
      rv$no_rating <- ofsted_no_rating
    } else if (input$chart_choice == "Reading Progress") {
      rv$quality_chart_choice <- progress_reading_p
      rv$no_rating <- progress_reading_no_rating
      shinyjs::hide("ofsted_p")
    } else if (input$chart_choice == "Maths Progress") {
      rv$quality_chart_choice <- progress_maths_p
      rv$no_rating <- progress_maths_no_rating
      shinyjs::hide("ofsted_p")
    } else if (input$chart_choice == "Progress 8") {
      rv$quality_chart_choice <- progress_8_p
      rv$no_rating <- progress_8_no_rating
      shinyjs::hide("ofsted_p")
    }
  })

  # Cost --------------------------------------------------------------------
  output$cost_title <- renderText({
    paste0("Based on local authority reported projects between ", year_vars()$cost_year_1, " and ", year_vars()$cost_year_2, ", adjusted for inflation and regional variation")
  })

  output$cost.bartext <- renderUI({
    if (input$LA_choice != "England") {
      paste0(
        "Region column shows England averages, adjusted for regional location factors. See technical notes for more information."
      )
    } else {
      paste("")
    }
  })

  # Comparison table - average cost of projects per place
  output$cost_table <- renderTable(
    {
      live_scorecard_data_england_comp() %>%
        # Filter for Cost, places and project data
        filter(str_detect(name, "Cost|Places|Projects")) %>%
        # Create new column called data_type, based on the name of the data
        mutate(
          data_type = case_when(
            str_detect(name, "Cost") ~ "Cost",
            str_detect(name, "Place") ~ "Place",
            str_detect(name, "Project") ~ "Project"
          )
        ) %>%
        mutate(
          exp_type = case_when(
            str_detect(name, "EP") ~ "Permanent Expansion",
            str_detect(name, "ET") ~ "Temporary Expansion",
            str_detect(name, "NS") ~ "New School"
          )
        ) %>%
        select(Region, data_type, exp_type, value) %>%
        # pivot the data wider
        pivot_wider(names_from = data_type, values_from = value) %>%
        # calculate cost per place
        mutate(
          cost_per_place = round_half_up(Cost / Place, 0),
          # format it nicely with £ sign
          cost_per_place = paste0("£", cs_num(cost_per_place)),
          # Nicely format any NA
          cost_per_place = str_replace(cost_per_place, "£NaN", "-")
        ) %>%
        select(Region, Type = exp_type, cost_per_place) %>%
        pivot_wider(names_from = Region, values_from = cost_per_place)
    },
    align = "r"
  )

  # Comparison charts - average cost per place

  # output$cost_plot <- renderPlotly({
  #   all_LA_cost <- live_scorecard_data_all_la() %>%
  #     filter(str_detect(name, "Cost|Places|Projects")) %>%
  #     mutate(data_type = case_when(
  #       str_detect(name, "Cost") ~ "Cost",
  #       str_detect(name, "Place") ~ "Place",
  #       str_detect(name, "Project") ~ "Project"
  #     )) %>%
  #     mutate(exp_type = case_when(
  #       str_detect(name, "EP") ~ "Permanent",
  #       str_detect(name, "ET") ~ "Temporary",
  #       str_detect(name, "NS") ~ "New School"
  #     )) %>%
  #     select(LA_name, data_type, exp_type, value) %>%
  #     pivot_wider(names_from = data_type, values_from = value) %>%
  #     mutate(
  #       cost_per_place = round_half_up(Cost / Place, 0),
  #       grouping = case_when(
  #         !LA_name %in% c("England", input$LA_choice) ~ "Other LA",
  #         TRUE ~ as.character(LA_name)
  #       ),
  #       x = 1,
  #       group_higlight = if_else(grouping == "Other LA", 0, 1)
  #     ) %>%
  #     arrange(group_higlight)
  #
  #   p <- ggplot() +
  #     geom_beeswarm(
  #       data = all_LA_cost %>% filter(group_higlight == 0), mapping = aes(x, cost_per_place,
  #         color = grouping,
  #         text = paste(LA_name, ": £", scales::comma(cost_per_place), " per place")
  #       ),
  #       groupOnX = TRUE, na.rm = TRUE
  #     ) +
  #     scale_y_continuous(labels = comma) +
  #     labs(x = "", y = "Cost per place (£)") +
  #     geom_beeswarm(
  #       data = all_LA_cost %>% filter(group_higlight == 1), aes(x, cost_per_place,
  #         color = grouping,
  #         text = paste(LA_name, ": £", scales::comma(cost_per_place), " per place")
  #       ),
  #       groupOnX = TRUE, na.rm = TRUE, size = 3.2
  #     ) +
  #     facet_grid(~ factor(exp_type, levels = c("Permanent", "Temporary", "New school"))) +
  #     scale_fill_manual(
  #       breaks = c("Other LA", input$LA_choice, "England"),
  #       values = c("#BFBFBF", "#f47738", "#1d70b8")
  #     ) +
  #     scale_color_manual(
  #       breaks = c(input$LA_choice, "England", "Other LA"),
  #       values = c("#f2590d", "#1c6bb0", "#dcd9d6")
  #     ) +
  #     theme(
  #       axis.line.y = element_line(color = "grey", size = 1),
  #       axis.line.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_text(size = 8),
  #       axis.ticks.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_text(margin = margin(r = 70)),
  #       legend.title = element_blank(),
  #       panel.background = element_blank(),
  #       panel.border = element_rect(color = "grey", size = 1, fill = NA),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       plot.background = element_blank(),
  #       text = element_text(size = 14, family = "Arial")
  #     ) +
  #     labs(y = "Cost per place (£)")
  #
  #
  #
  #   ggplotly(p, tooltip = c("text")) %>%
  #     layout(
  #       legend = list(
  #         orientation = "h",
  #         y = -0.1, x = 0.33,
  #         font = font_choice
  #       ),
  #       title = list(
  #         text = "Chart showing the cost of permanent, temporary and new school projects by local authority",
  #         font = list(color = "#c8c8c8", size = 1)
  #       )
  #     ) %>%
  #     config(displayModeBar = FALSE)
  # })

  # Comparison boxes - number of projects
  output$perm_box <- renderUI({
    perm_fig <- live_scorecard_data_all_la() %>%
      # Filter for Cost, places and project data
      filter(str_detect(name, "Cost|Places|Projects")) %>%
      # Create new column called data_type, based on the name of the data
      mutate(
        data_type = case_when(
          str_detect(name, "Cost") ~ "Cost",
          str_detect(name, "Place") ~ "Place",
          str_detect(name, "Project") ~ "Project"
        )
      ) %>%
      mutate(
        exp_type = case_when(
          str_detect(name, "EP") ~ "Permanent",
          str_detect(name, "ET") ~ "Temporary",
          str_detect(name, "NS") ~ "New school"
        )
      ) %>%
      select(LA_name, data_type, exp_type, value) %>%
      filter(
        LA_name == "England" & data_type == "Project" & exp_type == "Permanent"
      ) %>%
      pull(value)

    bslib::value_box(
      value = paste0(scales::comma(perm_fig)),
      title = paste0(
        "Permanent ",
        str_to_lower(input$phase_choice),
        " expansion projects in England"
      ),
      # icon = icon("fas fa-school"),
      theme = "info"
    )
  })

  output$temp_box <- renderUI({
    temp_fig <- live_scorecard_data_all_la() %>%
      # Filter for Cost, places and project data
      filter(str_detect(name, "Cost|Places|Projects")) %>%
      # Create new column called data_type, based on the name of the data
      mutate(
        data_type = case_when(
          str_detect(name, "Cost") ~ "Cost",
          str_detect(name, "Place") ~ "Place",
          str_detect(name, "Project") ~ "Project"
        )
      ) %>%
      mutate(
        exp_type = case_when(
          str_detect(name, "EP") ~ "Permanent",
          str_detect(name, "ET") ~ "Temporary",
          str_detect(name, "NS") ~ "New school"
        )
      ) %>%
      select(LA_name, data_type, exp_type, value) %>%
      filter(
        LA_name == "England" & data_type == "Project" & exp_type == "Temporary"
      ) %>%
      pull(value)

    bslib::value_box(
      value = paste0(temp_fig),
      title = paste0(
        "Temporary ",
        str_to_lower(input$phase_choice),
        " expansion projects in England "
      ),
      # icon = icon("fas fa-campground"),
      theme = "info"
    )
  })

  output$new_box <- renderUI({
    new_fig <- live_scorecard_data_all_la() %>%
      # Filter for Cost, places and project data
      filter(str_detect(name, "Cost|Places|Projects")) %>%
      # Create new column called data_type, based on the name of the data
      mutate(
        data_type = case_when(
          str_detect(name, "Cost") ~ "Cost",
          str_detect(name, "Place") ~ "Place",
          str_detect(name, "Project") ~ "Project"
        )
      ) %>%
      mutate(
        exp_type = case_when(
          str_detect(name, "EP") ~ "Permanent",
          str_detect(name, "ET") ~ "Temporary",
          str_detect(name, "NS") ~ "New school"
        )
      ) %>%
      select(LA_name, data_type, exp_type, value) %>%
      filter(
        LA_name == "England" & data_type == "Project" & exp_type == "New school"
      ) %>%
      pull(value)

    bslib::value_box(
      value = paste0(new_fig),
      title = paste0(
        "New ",
        str_to_lower(input$phase_choice),
        " schools projects in England"
      ),
      # icon = icon("fas fa-plus"),
      theme = "info"
    )
  })

  # Files for download ------------------------------------------------------

  filename_choice <- reactive({
    if (input$phase_choice == "Primary") {
      "Primary_scorecards_data.csv"
    } else {
      "Secondary_scorecards_data.csv"
    }
  })

  file_choice <- reactive({
    if (input$phase_choice == "Primary") {
      primary_data_clean
    } else {
      secondary_data_clean
    }
  })

  output$download_ud <- downloadHandler(
    filename = function() {
      filename_choice()
    },
    content = function(file) {
      write.csv(file_choice(), file, row.names = FALSE)
    }
  )

  # Tech guidance tables ----------------------------------------------------

  output$notesTableforacc <- renderUI({
    # require this condition
    req(input$technical_year)
    # filter only rows for chosen year
    table_foracc <- notes_foracc() %>%
      filter(Publication == input$technical_year)
    # replace NAs with blanks
    table_foracc[is.na(table_foracc)] <- " "
    # remove publication column from being displayed
    table_foracc <- table_foracc %>% select(-Publication)

    table_html_foracc <- kable(table_foracc, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = F) %>%
      # collapse_rows(columns = 1:2) %>%
      column_spec(
        1,
        bold = T,
        width = "20em",
        extra_css = "vertical-align: top !important;"
      ) %>%
      column_spec(2, width = "20em")

    HTML(table_html_foracc)
  })

  output$notesTableQuant <- renderUI({
    req(input$technical_year)

    table_quant <- notes_quant() %>%
      filter(Publication == input$technical_year)

    table_quant[is.na(table_quant)] <- " "

    table_quant <- table_quant %>% select(-Publication)

    table_html_quant <- kable(table_quant, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = F) %>%
      # collapse_rows(columns = 1:2) %>%
      column_spec(
        1,
        bold = T,
        extra_css = "vertical-align: top !important;"
      ) %>%
      column_spec(2, width_max = "20em") %>%
      column_spec(3, width_max = "20em") %>%
      column_spec(5, width_max = "40em")

    HTML(table_html_quant)
  })

  output$notesTablePref <- renderUI({
    req(input$technical_year)

    table_pref <- notes_pref() %>%
      filter(Publication == input$technical_year)

    table_pref[is.na(table_pref)] <- " "
    table_pref <- table_pref %>% select(-Publication)

    table_html_pref <- kable(table_pref, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = F) %>%
      # collapse_rows(columns = 1:2) %>%
      column_spec(
        1,
        bold = T,
        width = "20em",
        extra_css = "vertical-align: top !important;"
      ) %>%
      column_spec(2, width = "20em")

    HTML(table_html_pref)
  })

  output$notesTableQual <- renderUI({
    req(input$technical_year)

    table_qual <- notes_qual() %>%
      filter(Publication == input$technical_year)

    table_qual[is.na(table_qual)] <- " "
    table_qual <- table_qual %>% select(-Publication)

    table_html_qual <- kable(table_qual, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = F) %>%
      # collapse_rows(columns = 1:2) %>%
      column_spec(
        1,
        bold = T,
        width = "20em",
        extra_css = "vertical-align: top !important;"
      ) %>%
      column_spec(2, width = "20em")

    HTML(table_html_qual)
  })

  output$notesTableCost <- renderUI({
    req(input$technical_year)

    table_cost <- notes_cost() %>%
      filter(Publication == input$technical_year)

    table_cost[is.na(table_cost)] <- " "

    table_cost <- table_cost %>% select(-Publication)

    table_html_cost <- kable(table_cost, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = F) %>%
      # collapse_rows(columns = 1:2) %>%
      column_spec(
        1,
        bold = T,
        width = "20em",
        extra_css = "vertical-align: top !important;"
      ) %>%
      column_spec(2, width = "20em") %>%
      column_spec(3, width = "20em") %>%
      column_spec(4, width = "20em") %>%
      column_spec(5, width_max = "50em")

    HTML(table_html_cost)
  })

  # Hide details if Eng--------
  observe({
    if (input$LA_choice == "England") {
      # shinyjs::hide("LA_GO_places")
      shinyjs::hide("places_chart")
      shinyjs::show("places_chart_england")
      shinyjs::hide("LA_GO_ran")
      shinyjs::hide("PrefT3_CY_LA")
      shinyjs::hide("PrefT3_NY_LA")
    } else {
      # shinyjs::show("LA_GO_places")
      shinyjs::show("LA_GO_ran")
      shinyjs::show("places_chart")
      shinyjs::hide("places_chart_england")
      shinyjs::show("PrefT3_CY_LA")
      shinyjs::show("PrefT3_NY_LA")
    }
  })


  # Automatically bookmark every time an input changes-------------------
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)

  # Set bookmarking to ignore extraneous plotly options
  setBookmarkExclude(c(
    ".clientValue-default-plotlyCrosstalkOpts",
    "plotly_hover-A",
    "plotly_afterplot-A",
    "plotly_relayout-A"
  ))
}
