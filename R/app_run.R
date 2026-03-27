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

  # minimalist ui
  ui <- fluidPage(
    title = "SDDS Particle GUI",
    sdds_header(),
    selectInput(
      "timezone",
      label = NULL,
      choices = OlsonNames(),
      selected = timezone
    ) |>
      shinydashboard::box(title = "Timezone"),
    sdds_ui("sdds")
  )

  # minimalist server
  server <- function(input, output, session) {
    sdds_server("sdds", token, reactive(input$timezone))
  }

  # generate app
  shinyApp(
    ui = ui,
    server = server,
    onStart = sdds_onstart(token = token),
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}
