# Create the accessibility statement-----------------
panel_accessibility <- function() {
  tabPanel(
    "Accessibility",
    meta_general(
      application_name = "LA scorecards",
      description = "Scorecards for school places by local authority in England",
      robots = "index,follow",
      generator = "R-Shiny",
      subject = "School places in England",
      rating = "General",
      referrer = "no-referrer"
    ),
    h2("Accessibility statement"),
    br("This accessibility statement applies to the Local Authority (LA) scorecards application.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
    h3("WCAG 2.1 compliance"),
    br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
    tags$div(tags$ul(
      tags$li("uses colours that have sufficient contrast"),
      tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
      tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
    )),
    h3("Limitations"),
    br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
    tags$div(tags$ul(
      tags$li("Keyboard navigation through the interactive charts is currently limited"),
      tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
    )),
    h3("Feedback"),
    br(
      "If you have any feedback on how we could further improve the accessibility of this application, please contact ",
      a(href = "mailto:SCAP.PPP@education.gov.uk", "SCAP.PPP@education.gov.uk")
    ),
    br()
  )
}

panel_support <- function() {
  tabPanel(
    "Support & Feedback",
    div(
      h2("Give us feedback"),
      "This dashboard is hosted on a new platform that will continue to be developed. If you have any feedback or suggestions for improvements, please submit them using this ",
      a(
        href = "https://forms.office.com/r/wEWr5KCrTQ",
        "feedback form", .noWS = c("after")
      ), ".", br(),
      "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
      a(href = "mailto:scap.ppp@education.gov.uk", "scap.ppp@education.gov.uk", .noWS = c("after")), ".",
      br(),
      h2("Find more information on the data"),
      "The data used to produce the dashboard, along with methodological information can be found on ",
      a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics", .noWS = c("after")),
      ".",
      br(),
      h2("Contact us"),
      "If you have questions about the dashboard or data within it, please contact ",
      a(href = "mailto:scap.ppp@education.gov.uk", "scap.ppp@education.gov.uk", .noWS = c("after")),
      br(),
      h2("See the source code"),
      "The source code for this dashboard is available in our ",
      a(href = "https://github.com/dfe-analytical-services/la-school-places-scorecards", "GitHub repository", .noWS = c("after")),
      ".",
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    )
  )
}
