ui <- function(input, output, session) {
  fluidPage(
    #    use_tota11y(),
    title = tags$head(
      tags$link(
        rel = "shortcut icon",
        href = "dfefavicon.png"
      ),
      # Add title for browser tabs
      tags$title(site_title)
    ),
    tags$html(lang = "en"),
    # Add meta description for search engines
    meta() %>%
      meta_general(
        application_name = site_title,
        description = "DfE Shiny Template",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "stats development",
        rating = "General",
        referrer = "no-referrer"
      ),
    shinyjs::useShinyjs(),
    customDisconnectMessage(),
    useShinydashboard(),
    # Setting up cookie consent based on a cookie recording the consent:
    dfe_cookie_script(),
    cookie_banner_ui("cookies", name = "Local authority school places scorecards"),
    # Adding google analytics script
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    shinyGovstyle::header(
      main_text = "",
      main_link = "https://www.gov.uk/government/organisations/department-for-education",
      secondary_text = site_title,
      logo = "images/DfE_logo_landscape.png",
      logo_width = 150,
      logo_height = 32
    ),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "This Dashboard is in beta phase and we are still reviewing performance and reliability. "
      )
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      panel_homepage(),
      panel_scorecard(),
      panel_technical(),
      panel_accessibility(),
      dfeshiny::support_panel(
        team_email = "scap.ppp@education.gov.uk",
        repo_name = "la-school-places-scorecards",
        publication_name = "Local authority school places scorecards",
        publication_stub = "local-authority-school-places-scorecards",
        form_url = "https://forms.office.com/r/wEWr5KCrTQ"
      )
    ),
    footer(full = TRUE)
  )
}
