#' Run the user interface
#'
#' This function runs a simple example GUI that uses the sdds particle device controller modulre.
#'
#' @param timezone the timezone to user for datetime calculations
#' @inheritParams particle_get_device_info
#' @inheritParams shiny::shinyApp
#' @export
sdds_run_gui <- function(
  token = keyring::key_get("particle"),
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url"
) {
  # startup
  log_info("\n\n========================================================")
  log_info(
    "starting SDDS particle GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )

  # minimalist example ui and server
  ui <- example_ui(timezone = timezone)
  server <- example_server(token = token)

  # generate app
  shinyApp(
    ui = ui,
    server = server,
    # onstart required to read the particle stream!
    onStart = sdds_onstart(token = token),
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}

example_ui <- function(timezone) {
  # ui function
  function(request) {
    fluidPage(
      title = paste0("SDDS Particle GUI v", packageVersion("sddsParticle")),
      sdds_header(),
      selectInput(
        "timezone",
        label = NULL,
        choices = OlsonNames(),
        selected = timezone
      ) |>
        shinydashboard::box(title = "Timezone"),

      # devices
      shinydashboard::box(
        title = span(
          "SDDS Devices",
          div(
            style = "position: absolute; right: 50px; top: 5px;",
            sdds_ui_devices_actions("sdds"),
          )
        ),
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        sdds_ui_devices_table("sdds"),
        footer = tagList("Select the devices you want to work with.")
      ),

      # SDDS structure
      sdds_ui_structures_div(
        id = "sdds",
        shinydashboard::box(
          title = span(
            "Data structures",
            div(
              style = "position: absolute; right: 10px; top: 5px;",
              sdds_ui_structures_actions("sdds")
            )
          ),
          width = 12,
          status = "info",
          solidHeader = TRUE,
          sdds_ui_structures_table("sdds"),
          footer = tagList("Click to change values (if allowed).")
        )
      )
    )
  }
}

example_server <- function(token) {
  # server function
  function(input, output, session) {
    sdds_server("sdds", token, timezone = reactive(input$timezone))
  }
}
