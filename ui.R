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
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    shinyjs::useShinyjs(),
    customDisconnectMessage(),
    useShinydashboard(),
    # Setting up cookie consent based on a cookie recording the consent:
    dfe_cookies_script(),
    dfeshiny::cookies_banner_ui(
      name = site_title
    ),
    dfeshiny::custom_disconnect_message(
      publication_name = ees_pub_name,
      publication_link = ees_publication
    ),
    dfeshiny::header(header = site_title),
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
      tabPanel(
        "Accessibility statement",
        dfeshiny::a11y_panel(
          date_tested = "22/06/2023",
          date_reviewed = "25/06/2025",
          date_prepared = "25/06/2025",
          dashboard_url = site_primary,
          issues_contact = "https://github.com/dfe-analytical-services/la-school-places-scorecards/issues",
          dashboard_title = site_title,
          non_accessible_components = c(
            "Charts have non-accessible components that are inaccessible for keyboard users.",
            "Some decorative images are not labelled appropriately as yet."
          ),
          specific_issues = c(
            "An image containing a link in the header banner is not labelled adequately for screen readers, containing neither link text nor alt-text",
            "Keyboard navigation through the interactive charts is currently limited",
            "Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)"
          )
        )
      ),
      tabPanel(
        "Support and feedback",
        dfeshiny::support_panel(
          team_email = "scap.ppp@education.gov.uk",
          repo_name = "https://github.com/dfe-analytical-services/la-school-places-scorecards",
          publication_name = "Local authority school places scorecards",
          publication_slug = "local-authority-school-places-scorecards",
          form_url = "https://forms.office.com/r/wEWr5KCrTQ"
        )
      ),
      tabPanel(
        "Cookie statement",
        dfeshiny::cookies_panel_ui(
          google_analytics_key = google_analytics_key
        )
      )
    ),
    footer(full = TRUE)
  )
}
