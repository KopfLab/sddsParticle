# logic functions for the app ======

## structures ==================

# convert the simplified tree with values for use in a selector table
prepare_simplified_tree_w_values_for_table <- function(
  simplified_tree_w_valus
) {
  simplified_tree_w_valus |>
    mutate(
      device_info = sprintf(
        "%s (%s)",
        corename,
        .data$published_at |> format("%b %d %H:%M:%S")
      )
    ) |>
    select(
      "device_info",
      "path",
      "parent",
      "label",
      "text"
    ) |>
    tidyr::pivot_wider(
      names_from = "device_info",
      values_from = "text"
    )
}

# simplify the device info
simplify_device_info <- function(devices) {
  devices |> select(where(~ !is.list(.x)))
}

## events ============

# get events for the provided devices
get_stream_events_for_devices <- function(devices) {
  # particle_stream_get_events() |>
  particle_stream_get_events_log() |>
    inner_join(devices |> select(c("coreid", "name")), by = "coreid")
}

# prep events for table
prepare_stream_events_for_table <- function(events, timezone = Sys.timezone()) {
  events |>
    filter(!is.na(.data$published_at)) |>
    arrange(desc(.data$published_at), .data$name) |>
    mutate(
      row_id = row_number(),
      published_at = .data$published_at |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S"),
      data_short = if_else(
        is.na(.data$data),
        .data$error,
        .data$data |> stringr::str_sub(end = 20) |> paste0("...")
      )
    ) |>
    select(
      "row_id",
      "data",
      "Device" = "name",
      "Timestamp" = "published_at",
      "Event" = "event",
      "Data" = "data_short"
    )
}

## commands / value changes ======

prepare_command_queue_entries <- function(tree_w_new_values) {
  tree_w_new_values |>
    mutate(
      command = purrr::map2_chr(
        .data$path,
        .data$new_value,
        ~ sprintf("%s=%s", .x, as.character(.y))
      ),
      status = NA_character_
    )
}

## command logs ===========

# get command logs
get_command_logs_for_devices <- function(
  devices,
  token = keyring::key_get("particle")
) {
  devices |>
    select("coreid", "name") |>
    mutate(
      logs = .data$coreid |>
        purrr::map_chr(particle_get_sdds_command_log, token = token)
    )
}

# prep command logs for table
prepare_command_logs_for_table <- function(
  cmd_logs,
  timezone = Sys.timezone()
) {
  cmd_logs |>
    mutate(
      logs = .data$logs |>
        purrr::map(sdds_parse_command_log, timezone = timezone)
    ) |>
    tidyr::unnest(.data$logs) |>
    arrange(desc(.data$datetime), .data$name) |>
    mutate(
      row_id = row_number(),
      error_code = if_else(
        !is.na(.data$error_code),
        .data$error_code,
        "none"
      ),
      datetime = .data$datetime |> format("%b %d %Y %H:%M:%S")
    ) |>
    select(
      "row_id",
      "Device" = "name",
      "Timestamp" = "datetime",
      "Command" = "cmd",
      "Error" = "error_code"
    )
}
