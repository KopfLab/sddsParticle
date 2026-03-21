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

# complex units converters ======

duration_value_to_text <- function(value, units) {
  purrr::map2_chr(
    value,
    units,
    ~ {
      value <- .x
      units <- .y
      secs <- switch(
        units,
        ms = value / 1000,
        sec = value,
        min = value * 60,
        hour = value * 3600,
        day = value * 86400,
        cli_abort("Unknown units {.field {units}}")
      )

      if (secs < 1) {
        return(paste0(secs * 1000, " ms"))
      }

      days <- secs %/% 86400
      secs <- secs %% 86400

      hours <- secs %/% 3600
      secs <- secs %% 3600

      mins <- secs %/% 60
      secs <- secs %% 60

      parts <- c(
        if (days >= 2) paste0(days, " days"),
        if (days > 0 && days < 2) paste0(days, " day"),
        if (hours >= 2) paste0(hours, " hours"),
        if (hours > 0 && hours < 2) paste0(hours, " hour"),
        if (mins >= 2) paste0(mins, " mins"),
        if (mins > 0 && mins < 2) paste0(mins, " min"),
        if (secs >= 2) paste0(secs, " secs"),
        if (secs > 0 && secs < 2) paste0(secs, " sec")
      )

      paste(parts, collapse = " ")
    }
  )
}

.var_intervals_conversion <- tibble(
  value = c(-1L, 0L, 1L, 2L),
  text = c(
    "send at global interval (if publishing)",
    "never send",
    "send immediately (if publishing)",
    "send immediately (always)"
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
