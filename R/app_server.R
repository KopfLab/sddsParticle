# server
app_server <- function(token, timezone) {
  # return server function (input, output, and session parameters required by shiny)
  function(input, output, session) {
    log_info("\n\n========================================================")
    log_info(
      "starting SDDS GUI session ",
      if (shiny::in_devmode()) " in DEV mode"
    )

    # sdds server
    sdds_server("sdds", token, timezone)

    # dev mode
    observeEvent(input$dev_mode_toggle, {
      if (shiny::in_devmode()) {
        shiny::devmode(FALSE)
      } else {
        shiny::devmode(TRUE)
      }
    })
  }
}
