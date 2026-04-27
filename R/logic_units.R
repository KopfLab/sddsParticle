# unit converters

# general functions =========

add_units <- function(text, units) {
  if_else(!is.na(text) & !is.na(units), paste0(text, " ", units), text)
}

# FIXME: todo - implement modules, one for each type (text/integer/double/datetime/duration/etc.)
# that defines functions for rendering the UI as well as for returning both the text representation and the actual value (for commands) from the inputs
# registered in the server by the prefix (same as the converter functions) and then called as needed to generate a UI and pull the latest values from it
# make sure to deal with observers that might need to get deinitialized (like an initialize function of some kind)
# maybe this should be an R6 class instead of a module? might make more sense

# standard units converters ======

null_value_to_text <- function(...) "<no value>"

text_value_to_text <- function(value, units) {
  as.character(value) |> add_units(units)
}

integer_value_to_text <- function(value, units) {
  as.character(value) |> add_units(units)
}

double_value_to_text <- function(value, units) {
  as.character(signif(value, 4)) |> add_units(units)
}

enum_value_to_text <- function(value, ...) as.character(value)

version_value_to_text <- function(value, ...) {
  value <- as.integer(value)
  text <- sprintf(
    "%d.%d.%d",
    value %/% 10000,
    value %% 10000 %/% 100,
    value %% 100
  )
  text[is.na(value)] <- NA_character_
  return(text)
}

datetime_value_to_text <- function(value, timezone, ...) {
  dt <- lubridate::ymd_hms(value, quiet = TRUE)
  if (is.na(dt)) {
    return(value)
  }
  dt |>
    lubridate::with_tz(timezone) |>
    format("%b %d %Y %H:%M:%S %Z")
}

HHMM_value_to_datetime <- function(value, timezone, ...) {
  hhmm <- as.integer(value)
  if (is.na(hhmm)) {
    as.POSIXct(NA_integer_)
  }
  now <- lubridate::now(tzone = "UTC")
  lubridate::hour(now) <- hhmm %/% 100
  lubridate::minute(now) <- hhmm %% 100
  now |> lubridate::with_tz(timezone)
}

HHMM_value_to_text <- function(value, timezone, ...) {
  HHMM_value_to_datetime(value, timezone, ...) |> format("%H:%M %Z")
}

# complex units converters ======

duration_value_to_seconds <- function(value, units) {
  switch(
    units,
    ms = lubridate::milliseconds(value),
    sec = value,
    min = value * 60L,
    hour = value * 3600L,
    day = value * 86400L,
    cli_abort("Unknown units {.field {units}}")
  )
}

.duration_converter <- list(
  ms = lubridate::milliseconds,
  sec = lubridate::seconds,
  min = lubridate::minutes,
  hour = lubridate::hours,
  day = lubridate::days
)

duration_to_largest_whole_unit <- function(value, units) {
  out <- purrr::map2(
    value,
    units,
    ~ {
      value <- .x
      units <- .y
      if (!units %in% names(.duration_converter)) {
        cli_abort("Unknown units {.field {units}}")
      }
      secs <- .duration_converter[[units]](value) |> as.numeric("sec")
      if (near(secs %% 86400L, 0)) {
        list(v = secs %/% 86400L, u = "day")
      } else if (near(secs %% 3600L, 0)) {
        list(v = secs %/% 3600L, u = "hour")
      } else if (near(secs %% 60L, 0)) {
        list(v = secs %/% 60L, u = "min")
      } else if (near(secs %% 1L, 0)) {
        list(v = secs, u = "sec")
      } else {
        list(v = secs * 1000, u = "ms")
      }
    }
  )
  if (length(value) == 1L && length(units) == 1L) {
    return(out[[1]])
  }
  return(out)
}

duration_value_to_text <- function(value, units) {
  purrr::map2_chr(
    value,
    units,
    ~ {
      if (!is.na(value)) {
        value <- .x
        units <- .y
        if (!units %in% names(.duration_converter)) {
          cli_abort("Unknown units {.field {units}}")
        }
        secs <- .duration_converter[[units]](value) |> as.numeric("sec")
        if (secs < 1) {
          return(paste0(secs * 1000, " ms"))
        }

        days <- secs %/% 86400L
        secs <- secs %% 86400L

        hours <- secs %/% 3600L
        secs <- secs %% 3600L

        mins <- secs %/% 60L
        secs <- secs %% 60L

        parts <- c(
          if (days >= 2) paste0(days, " days"),
          if (days > 0 && days < 2) paste0(days, " day"),
          if (hours >= 2) paste0(hours, " hours"),
          if (hours > 0 && hours < 2) paste0(hours, " hour"),
          if (mins >= 2) paste0(mins, " mins"),
          if (mins > 0 && mins < 2) paste0(mins, " min"),
          if (secs >= 2) paste0(round(secs, 2), " secs"),
          if (secs > 0 && secs < 2) paste0(round(secs, 2), " sec")
        )

        paste(parts, collapse = " ")
      } else {
        NA_character_
      }
    }
  )
}

.var_intervals_conversion <- tibble(
  value = c(-1L, 0L, 1L, 2L),
  text = c(
    "average over global interval (if recording)",
    "never send",
    "send each change (if recording)",
    "send each change (always)"
  )
)

var_intervals_value_to_text <- function(value, units) {
  tibble(value = as.integer(!!value)) |>
    left_join(.var_intervals_conversion, by = "value") |>
    mutate(
      text = if_else(
        is.na(.data$text),
        .data$value |> duration_value_to_text(!!units),
        .data$text
      )
    ) |>
    pull(.data$text)
}
