# server
app_server <- function(token) {
  # return server function (input, output, and session parameters required by shiny)
  function(input, output, session) {
    log_info("\n\n========================================================")
    log_info(
      "starting SDDS GUI session ",
      if (shiny::in_devmode()) " in DEV mode"
    )

    # sdds server
    # TODO: make it possible to pass additional modules for the value editing:
    # example in get_structures() in app_module_sdds for Ohm should be coming from here
    # (both converter function and rendeirng module)
    sdds <- sdds_server("sdds", token, reactive(input$timezone))

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
