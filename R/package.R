#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import cli
#' @import rlang
#' @importFrom tibble tibble
#' @import dplyr
#' @import shiny
## usethis namespace: end
NULL

# make sure the stream pool is created at onload
.onLoad <- function(libname, pkgname) {
  .ps$pool <- curl::new_pool(total_con = 1)
  .ps$monitor_last_event_ts <- lubridate::now(tz = 'UTC')
}
