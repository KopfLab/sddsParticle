#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import cli
#' @import rlang
#' @importFrom tibble tibble
#' @import dplyr
#' @import shiny
#' @importFrom methods getPackageName is
#' @importFrom utils URLencode packageVersion
#' @importFrom prettyunits pretty_sec
## usethis namespace: end
NULL

# column names used in non-standard-evaluation (dplyr) pipelines
utils::globalVariables(c(
  "command",
  "coreid",
  "corename",
  "name",
  "parent",
  "row_id",
  "success",
  "type"
))

# make sure the stream pool is created at onload
.onLoad <- function(libname, pkgname) {
  .ps$pool <- curl::new_pool(total_con = 1)
  .ps$monitor_last_event_ts <- lubridate::now(tz = 'UTC')
}
