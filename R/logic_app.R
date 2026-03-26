# logic functions for the app ======

## devices ===================

# get devies in app
get_devices_in_app <- function(token, core_ids) {
  if (in_devmode() && file.exists("cache/sdds_devices.csv")) {
    # take from file in dev mode
    devices <- readr::read_csv(
      "cache/sdds_devices.csv",
      show_col_types = FALSE
    )
  } else {
    # always request from cloud
    devices <- particle_get_device_info(token = token) |>
      simplify_device_info()
    if (in_devmode()) {
      # store in file in devmode
      if (!dir.exists("cache")) {
        dir.create("cache")
      }
      devices |> readr::write_csv("cache/sdds_devices.csv")
    }
  }
  # if only specific coreids are allowed --> filter for them
  if (!is.null(core_ids)) {
    devices <- devices |> filter(.data$id %in% core_ids)
  }
  # subselect info
  devices |>
    mutate(last_heard = lubridate::ymd_hms(.data$last_heard, tz = "UTC"))
}

# get devices for table in app
get_devices_for_table_in_app <- function(devices, timezone) {
  devices |>
    mutate(
      last_heard = .data$last_heard |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S")
    ) |>

    select(
      "coreid",
      Name = "name",
      `Last heard from` = "last_heard",
      Connected = "connected",
      Status = "status",
      Firmware = "system_firmware_version",
      `MAC address` = "mac_wifi"
    )
}

## structures ==================

# get the structures inside the app
get_structures_in_app <- function(
  core_ids,
  devices,
  timezone,
  additional_types,
  additional_converters
) {
  sdds_read_cached_trees_and_values() |>
    filter(.data$coreid %in% core_ids) |>
    sdds_parse_trees_and_values() |>
    sdds_simplify_trees_and_values(
      devices = devices,
      timezone = timezone,
      additional_types = additional_types,
      additional_converters = additional_converters
    )
}

# get structures table in app
get_get_structures_for_table_in_app <- function(
  structs,
  show_system,
  show_hardware
) {
  if (!show_system) {
    structs <- structs |>
      filter(!stringr::str_detect(.data$path, "^SYSTEM"))
  }
  if (!show_hardware) {
    structs <- structs |>
      filter(!stringr::str_detect(.data$path, "^HARDWARE"))
  }
  structs |>
    prepare_simplified_tree_w_values_for_table() |>
    rename(" " = "label")
}

# convert the simplified tree with values for use in a selector table
prepare_simplified_tree_w_values_for_table <- function(
  simplified_tree_w_valus
) {
  simplified_tree_w_valus |>
    mutate(
      device_info = sprintf(
        "%s (%s ago)",
        corename,
        fmt_duration(
          lubridate::now(tzone = lubridate::tz(.data$published_at)) -
            .data$published_at
        )
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
