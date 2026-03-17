#' Run the user interface
#'
#' This function runs the user interface for the sdds particle device controller.
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

  # generate app
  shinyApp(
    ui = app_ui(timezone = timezone),
    server = app_server(token = token, timezone = timezone),
    onStart = sdds_onstart(token = token),
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}
