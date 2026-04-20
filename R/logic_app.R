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

  # make last heards into datetime
  devices <- devices |>
    mutate(last_heard = lubridate::ymd_hms(.data$last_heard, tz = "UTC")) |>
    arrange(.data$name)
  return(devices)
}

# get filtered devices in app
get_filtered_devices_in_app <- function(devices, core_ids) {
  # filter for specific core ids
  if (!is.null(core_ids)) {
    devices <- devices |> filter(.data$coreid %in% core_ids)
  }
  return(devices)
}

# get devices for table in app
get_devices_for_table_in_app <- function(devices, timezone) {
  devices |>
    mutate(
      last_heard = .data$last_heard |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S")
    ) |>
    left_join(sdds_read_cached_trees_and_values(), by = "coreid") |>
    mutate(version = version_value_to_text(.data$version)) |>
    select(
      "coreid",
      Name = "name",
      Type = "type",
      Version = "version",
      `Last heard from` = "last_heard",
      Connected = "connected",
      Status = "status",
      Firmware = "system_firmware_version",
      `MAC address` = "mac_wifi"
    )
}

## structures ==================

get_cached_structures_in_app <- function(core_ids) {
  if (is_empty(core_ids)) {
    return(NULL)
  }
  sdds_read_cached_trees_and_values() |>
    filter(.data$coreid %in% core_ids)
}

# get the structures inside the app
get_structures_in_app <- function(
  structures,
  devices,
  timezone,
  additional_types,
  additional_converters
) {
  if (is_empty(structures) || is_empty(devices)) {
    return(NULL)
  }
  structures |>
    sdds_simplify_trees_and_values(
      devices = devices,
      timezone = timezone,
      additional_types = additional_types,
      additional_converters = additional_converters
    )
}

# get missing trees
get_missing_trees_in_app <- function(structs) {
  structs |>
    filter(.data$tree |> purrr::map_lgl(is_null)) |>
    mutate(
      type_version = paste(.data$type, version_value_to_text(.data$version))
    ) |>
    slice_head(by = c("type", "version"), n = 1L)
}

# get structures table in app
get_structures_for_table_in_app <- function(
  structs,
  show_publishing,
  show_system,
  show_hardware
) {
  if (show_publishing) {
    publish_intervals <- structs |>
      filter(.data$is_var_interval) |>
      mutate(
        path = .data$path |>
          stringr::str_remove(stringr::fixed(
            "SYSTEM.publishing.varIntervals_ms."
          ))
      ) |>
      select("coreid", "path", "interval" = "text")
    structs <- structs |>
      left_join(publish_intervals, by = c("coreid", "path")) |>
      mutate(
        text = if_else(
          !is.na(.data$interval),
          sprintf("%s -> publish: %s", .data$text, .data$interval),
          .data$text
        )
      )
  }

  # var interval is shown with the structure edit boxes, not on its own
  structs <- structs |> filter(!.data$is_var_interval)
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
        "<span style='font-size: 200%%;'>%s</span><br>\n(%s ago)",
        corename,
        format_duration(
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

# request values in app
request_sdds_values_in_app <- function(devices, core_ids, token) {
  out <- devices |>
    filter(.data$coreid %in% core_ids) |>
    select("coreid", "name") |>
    mutate(
      out = .data$coreid |>
        purrr::map(
          ~ particle_request_sdds_values(.x, token = token) |>
            try_catch_cnds(error_value = list(return_value = -1))
        ),
      success = .data$out |>
        purrr::map_lgl(
          ~ identical(.x$result$return_value, 0L)
        ),
      conditions = .data$out |>
        purrr::map(~ .x$conditions)
    )
  out$conditions |>
    bind_rows() |>
    warn_cnds(include_cnd_symbols = FALSE, include_cnd_calls = FALSE)
  return(out)
}

request_sdds_trees_in_app <- function(devices, core_ids, token) {
  devices |>
    filter(.data$coreid %in% core_ids) |>
    select("coreid", "name", "connected") |>
    mutate(
      success = .data$coreid |>
        purrr::map2_lgl(
          .data$connected,
          ~ if (.y) identical(
            particle_request_sdds(.x, token = token)$return_value,
            0L
          ) else FALSE
        )
    )
}

## stream events ============

# stream events log in app
get_stream_events_log_in_app <- function(core_ids) {
  if (is_empty(core_ids)) {
    return(NULL)
  }
  logs <- particle_stream_get_events_log()
  if (nrow(logs) == 0) {
    return(NULL)
  }
  logs |> filter(.data$coreid %in% core_ids)
}

# get streams events for app
get_stream_events_in_app <- function(
  logs,
  devices,
  timezone,
  prepare_for_table = FALSE
) {
  if (is_empty(logs) || is_empty(devices)) {
    return(NULL)
  }
  events <-
    logs |>
    left_join(devices |> select(c("coreid", "name")), by = "coreid")
  if (prepare_for_table) {
    events <- events |> prepare_stream_events_for_table(timezone = timezone)
  }
  return(events)
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

get_pretty_json_event_data_for_app <- function(events) {
  events$data |>
    jsonlite::fromJSON() |>
    jsonlite::toJSON(auto_unbox = TRUE, null = "null", pretty = TRUE)
}

## value edits ======

get_structures_for_path_in_app <- function(structures, path) {
  structures |> filter(.data$path == !!path)
}

prepare_edit_ui_in_app <- function(
  structures,
  gui_id,
  edit_modules,
  changed = FALSE
) {
  # safety checks
  structures |>
    check_arg(
      !missing(structures) &&
        is.list(structures) &&
        all(have_name(structures)),
      "must be a named list of structure tibbles"
    )

  # reset modules
  edit_modules |> purrr::walk(~ .x$reset())

  # create set of widgets for structures
  generate_widgets_set <- function(structure, prefix, changed) {
    if (is.null(structure) || nrow(structure) == 0) {
      return(generate_standard_input_row("no settings available"))
    }
    structure |>
      mutate(
        ui = purrr::pmap(
          list(
            type = .data$type,
            input_id = paste0(prefix, .data$coreid),
            label = .data$corename,
            value = .data$value,
            text = .data$text,
            read_only = .data$readonly,
            units = .data$base_units,
            choices = .data$enum_values
          ),
          function(
            type,
            input_id,
            label,
            value,
            text,
            read_only,
            units,
            choices
          ) {
            if (read_only) {
              type <- "read_only"
            }
            if (!type %in% names(edit_modules)) {
              return(generate_standard_input_row(
                label,
                sprintf("unsupported value type '%s'", type)
              ))
            }
            # generate the ui
            return(edit_modules[[type]]$generate_ui(
              gui_id = gui_id,
              input_id = input_id,
              label = label,
              value = value,
              text = text,
              units = units,
              choices = choices,
              changed = changed
            ))
          }
        )
      ) |>
      pull(.data$ui) |>
      tagList()
  }

  # generate widgets
  widgets <- 
    purrr::pmap(
      list(
        structure = structures,
        prefix = names(structures),
        changed = changed
      ),
      generate_widgets_set
    )

  # activate modules
  edit_modules |> purrr::walk(~ .x$activate())
  return(widgets)
}

prepare_new_values_in_app <- function(structure, prefix, edit_modules) {
  structure |>
    mutate(
      new_value = purrr::pmap(
        list(type = .data$type, ui_id = paste0(prefix, .data$coreid)),
        function(type, ui_id) {
          if (!type %in% names(edit_modules)) {
            return(NULL)
          }
          return(edit_modules[[type]]$get_value(ui_id))
        }
      ),
      has_changed = !purrr::map_lgl(.data$new_value, is.null)
    ) |>
    filter(.data$has_changed) |>
    mutate(
      new_text = purrr::pmap_chr(
        list(
          type = .data$type,
          ui_id = paste0(prefix, .data$coreid),
          units = .data$base_units
        ),
        function(type, ui_id, units) {
          if (!type %in% names(edit_modules)) {
            return(NA_character_)
          }
          return(edit_modules[[type]]$get_text(ui_id, units))
        }
      )
    )
}

update_command_queue_entries_in_app <- function(
  existing_queue,
  tree_w_new_values
) {
  new_queue <- tree_w_new_values |>
    mutate(
      command = purrr::map2_chr(
        .data$path,
        .data$new_value,
        ~ sprintf("%s=%s", .x, format(.y, scientific = FALSE))
      ),
      status = NA_character_
    )
  existing_queue |>
    bind_rows(new_queue) |>
    mutate(row_id = row_number())
}

update_command_queue_status_in_app <- function(existing_queue, cmds_results) {
  existing_queue |>
    left_join(cmds_results |> select("row_id", "success"), by = "row_id") |>
    mutate(
      status = case_when(
        is.na(.data$success) ~ .data$status,
        .data$success ~ "success",
        !.data$success ~ "failed"
      )
    ) |>
    select(-"success") |>
    arrange(.data$row_id)
}


## command logs ===========

# get command logs
get_command_logs_in_app <- function(devices, core_ids, token) {
  out <- devices |>
    filter(.data$coreid %in% core_ids) |>
    select("coreid", "name") |>
    mutate(
      out = .data$coreid |>
        purrr::map(
          ~ {
            particle_get_sdds_command_log(.x, token = token) |>
              try_catch_cnds(error_value = NA_character_)
          }
        ),
      conditions = .data$out |>
        purrr::map(~ .x$conditions),
      logs = .data$out |>
        purrr::map_chr(~ .x$result)
    )
  out$conditions |>
    bind_rows() |>
    warn_cnds(include_cnd_symbols = FALSE, include_cnd_calls = FALSE)
  return(out)
}

# prep command logs for table
prepare_command_logs_for_table_in_app <- function(
  cmd_logs,
  timezone
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
