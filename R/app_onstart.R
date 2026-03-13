# on start function
app_onstart <- function(token) {
  # return onStart function
  function() {
    # ps_connect(
    #   endpoint,
    #   # this is a temporary access token (valid for 10 days) but still, don't commit it!
    #   token = "c8f8da7475e2e3e01a7c16faa37519657bae9f0f",
    #   log = FALSE
    # )
    # onStop(function() {
    #   ps_disconnect()
    #   cat("\nApplication closed.\n")
    # })
  }
}
