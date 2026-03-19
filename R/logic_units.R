# unit converters

# general functions =========

add_units <- function(text, units) {
  if_else(!is.na(text) & !is.na(units), paste0(text, " ", units), text)
}

# variable publishing intervals ========

# special case values
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
        # FIXME: should this use duration_value_to_text instead?
        .data$value |> as.character() |> add_units(!!units),
        .data$text
      )
    ) |>
    pull(.data$text)
}

# duration units ======

duration_value_to_text <- function(value, units) {
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

# Ohm units
