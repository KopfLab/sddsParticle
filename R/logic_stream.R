# particle stream environment =========

.ps <- rlang::env(
  connections_log = "logs/particle_stream_connections.tsv",
  raw_data_log = "logs/particle_stream_raw_data.tsv",
  errors_log = "logs/particle_stream_errors.tsv",
  events_log = "logs/particle_stream_events.tsv",
  pool = NULL, # set in .onLoad
  endpoint = "",
  log = TRUE,
  active = FALSE,
  connected = FALSE,
  reconnect_delay = 1, # seconds
  reconnect_max_delay = 30, # seconds
  msg_idx = 0L, # number of messages received
  buffer = raw(), # stream buffer
  events = tibble::tibble(timestamp = integer(0) |> as.POSIXct()),
  events_callback = NA_character_, # name of a callback function for events
  monitor_last_event_ts = NULL # set in .onLoad
)

# particle stream exported functions =======

#' Connect to particle event stream
#' @param endpoint which particle endpoint to connect to, by default connects to the sddsData endpoint
#' @param token the particle access token
#' @param log enable log files to store connection, raw data, error, and event information for debugging purposes
#' @param events_callback name of a callback function for events (turn off call back by setting \code{events_callback = NA_character_})
#' @describeIn particle_stream connects to the stream
#' @export
particle_stream_connect <- function(
  endpoint = "events/sddsData",
  token = keyring::key_get("particle"),
  log = FALSE,
  events_callback = "sdds_cache_events"
) {
  # safety
  endpoint |> check_arg(is_scalar_character(endpoint), "must be a string")
  log |> check_arg(is_scalar_logical(log), "must be TRUE or FALSE")
  token |> check_particle_token()
  events_callback |>
    check_arg(
      is_null(events_callback) || is_scalar_character(events_callback),
      "must be the name of a function"
    )

  # disconnect any existing connection
  ps_cleanup()

  # store connection information
  .ps$endpoint <- endpoint
  .ps$log <- log
  Sys.setenv(PARTICLE_EVENT_STREAM_TMP_TOKEN = token)
  if (!is.null(events_callback)) {
    .ps$events_callback <- events_callback
  }

  # create handle for particle endpoint
  sse_url <- sprintf("https://api.particle.io/v1/%s", endpoint)
  handle <- curl::new_handle(url = sse_url)
  handle |>
    curl::handle_setheaders(
      "Authorization" = paste("Bearer", token),
      "Accept" = "text/event-stream",
      "Cache-Control" = "no-cache"
    )

  # start connection
  cli_bullets(
    c(
      "i" = "Connecting to Particle events stream endpoint {.field endpoint} (log files are {.emph {if (log) 'enabled' else 'disabled'}}), events are{if (is.na(.ps$events_callback)) ' not'} processed{if (!is.na(.ps$events_callback)) paste0(' with ', .ps$events_callback, '()')}",
      ">" = "use {.strong particle_stream_monitor()} to automatically printout incoming events",
      ">" = "use {.strong particle_stream_is_connected()} to check on the connection status",
      ">" = "use {.strong particle_stream_get_events()} to get the collected events",
      ">" = "use {.strong particle_stream_disconnect()} to disconnect from the stream"
    )
  )

  # mark stream as active but not yet connected
  .ps$active <- TRUE
  .ps$connected <- FALSE
  .ps$msg_idx <- 0L

  # start stream by adding the handle to the pool
  handle |>
    curl::multi_add(
      # global pool
      pool = .ps$pool,

      # callbacks (note that final is always FALSE in this mode)
      data = function(raw_data, final = FALSE) {
        .ps$buffer <- c(.ps$buffer, raw_data)

        # check if message is complete (ends with an empty line \n\n)
        ndata <- length(.ps$buffer)
        if (
          ndata < 2 ||
            !identical(.ps$buffer[(ndata - 1):ndata], charToRaw("\n\n"))
        ) {
          # not yet complete, wait for more
          return()
        }

        # we reached the end of the message --> proceed
        data <- strsplit(rawToChar(.ps$buffer), "\\r?\\n")[[1]]
        .ps$buffer <- raw()

        # discard empty lines
        data <- data[nzchar(data)]
        if (is_empty(data)) {
          return()
        }

        # log data lines
        ps_update_raw_data_log(data)

        # check on successful connection
        if (any(grepl("^:ok", data))) {
          .ps$connected <- TRUE
          .ps$reconnect_delay <- 1
          ps_update_connections_log(TRUE)
        }

        # parse events
        events <- ps_parse_event_raw_data(data)
        if (nrow(events) == 0L) {
          return()
        }

        # store events
        ps_update_events_log(events)
        .ps$events <- .ps$events |> dplyr::bind_rows(events)

        # process events if there is a callback function
        if (!is.na(.ps$events_callback)) {
          callback <- call2(.ps$events_callback, quote(events))
          tryCatch(
            callback |> eval_tidy(),
            error = function(err) {
              ps_update_error_log(err)
            }
          )
        }
      },

      done = function(response) {
        # server send stream ended --> reconnect to keep receiving data
        ps_reconnect()
      },

      fail = function(err, ...) {
        # encontered an error, reconnect
        ps_update_error_log(err)
        ps_reconnect()
      }
    )

  # start querying the stream
  ps_query_stream()
}

#' @describeIn particle_stream disconnects from the stream
#' @export
particle_stream_disconnect <- function() {
  if (.ps$active) {
    cli_alert_info(
      "Disconnecting from particle events stream endpoint {.field {(.ps$endpoint)}}."
    )
    # cleanup current connection
    ps_cleanup()
    # remove temp token storage
    Sys.unsetenv("PARTICLE_EVENT_STREAM_TMP_TOKEN")
  } else {
    cli_alert_warning(
      "Particle events stream connection is not active."
    )
  }
}

#' @describeIn particle_stream checks if stream is connected
#' @export
particle_stream_is_connected <- function() {
  return(.ps$active && .ps$connected)
}

#' @describeIn particle_stream retrieves the collected events data
#' @export
particle_stream_get_events <- function() {
  return(.ps$events)
}

#' @describeIn particle_stream starts a console event monitor that prints out new evens as they arrive
#' @param poll_interval how many seconds between polling stream
#' @export
particle_stream_monitor <- function(poll_interval = 1) {
  if (!particle_stream_is_connected()) {
    cli_abort(
      "No active particle events stream connection. Please connect first using particle_stream_connect()."
    )
  }

  # start
  cli_h1("Monitoring particle events (stop with Ctrl+C)")

  # start loop
  tryCatch(
    repeat {
      # new events
      new_events <- particle_stream_get_events() |>
        dplyr::filter(.data$timestamp > .ps$monitor_last_event_ts)
      if (nrow(new_events) > 0L) {
        print(new_events)
        .ps$monitor_last_event_ts <- max(new_events$timestamp)
      }

      # check for connection still active
      if (!particle_stream_is_connected()) {
        cli_alert_danger(
          "Particle events stream was disconnected."
        )
        break
      }

      # later does not run automatically in some IDEs --> this makes sure to check it
      later::run_now(timeout = 0)
      Sys.sleep(0.01)
    },
    interrupt = function(e) {
      cli_alert_info("Monitor closed.")
    }
  )
}

# internal functions =============

# reconnect to particle events stream
ps_reconnect <- function() {
  if (.ps$active) {
    # cleanup current connection
    ps_cleanup()

    # schedule reconnect
    cli_alert_info(
      "Reconnecting to particle events stream endpoint {.field {(.ps$endpoint)}} in {.emph {(.ps$reconnect_delay)}} seconds..."
    )
    later::later(
      function() {
        particle_stream_connect(
          .ps$endpoint,
          Sys.getenv("PARTICLE_EVENT_STREAM_TMP_TOKEN")
        )
      },
      .ps$reconnect_delay
    )

    # increase reconnect delay for next time
    .ps$reconnect_delay <- min(
      .ps$reconnect_max_delay,
      .ps$reconnect_delay * 2
    )
  }
}

# cleanup
ps_cleanup <- function() {
  if (.ps$active) {
    if (!is.null(.ps$pool)) {
      # cancel all pending handles before releasing the pool
      curl::multi_list(.ps$pool) |>
        lapply(function(handle) try(curl::multi_cancel(handle)))
      # drain any remaining callbacks
      try(curl::multi_run(pool = .ps$pool, timeout = 0))
    }
    # log the disconnect if the connection was successfully established
    if (.ps$connected) {
      ps_update_connections_log(FALSE)
    }
    # mark connection as inactive and disconnected
    .ps$active <- FALSE
    .ps$connected <- FALSE
  }
}

# query stream
ps_query_stream <- function() {
  if (.ps$active) {
    curl::multi_run(pool = .ps$pool, timeout = 0)
    later::later(ps_query_stream, 0.1)
  }
}

# parse event data
ps_parse_event_raw_data <- function(raw_data) {
  pattern <- "^(event|data): (.*)$"
  events <- tibble(raw_data = !!raw_data) |>
    dplyr::filter(grepl(pattern, .data$raw_data))

  # have any?
  if (nrow(events) == 0L) {
    return(tibble())
  }

  # parse
  events |>
    dplyr::mutate(
      name = gsub(pattern, "\\1", .data$raw_data),
      value = gsub(pattern, "\\2", .data$raw_data),
      idx = cumsum(name == "event")
    ) |>
    dplyr::select(-"raw_data") |>
    tidyr::pivot_wider() |>
    dplyr::select(-"idx") |>
    dplyr::mutate(timestamp = lubridate::now(tz = 'UTC'), .before = 1L) |>
    dplyr::mutate(
      data = .data$data |>
        purrr::map(
          ~ {
            check_json <- jsonlite::validate(.x)
            if (check_json) {
              jsonlite::parse_json(.x) |>
                tibble::as_tibble() |>
                dplyr::select(dplyr::any_of(c(
                  "coreid",
                  "ttl",
                  "published_at",
                  "data"
                ))) |>
                dplyr::mutate(error = NA_character_)
            } else {
              tibble(
                coreid = NA_character_,
                ttl = NA_integer_,
                published_at = NA_character_,
                data = NA_character_,
                error = "invalid json"
              )
            }
          }
        )
    ) |>
    tidyr::unnest("data", keep_empty = TRUE)
}

# logging (logs persist until they are cleared) =======

# clear all logs
ps_clear_logs <- function() {
  if (file.exists(.ps$connections_log)) {
    unlink(.ps$connections_log)
  }
  if (file.exists(.ps$events_log)) {
    unlink(.ps$events_log)
  }
  if (file.exists(.ps$raw_data_log)) {
    unlink(.ps$raw_data_log)
  }
  if (file.exists(.ps$errors_log)) unlink(.ps$errors_log)
}

# update connections log
ps_update_connections_log <- function(status) {
  if (!.ps$log) {
    return()
  }
  log <- tibble(
    timestamp = lubridate::now(tz = 'UTC'),
    status = !!status
  )
  if (!file.exists(.ps$connections_log)) {
    if (!dir.exists(dirname(.ps$connections_log))) {
      dir.create(dirname(.ps$connections_log), recursive = TRUE)
    }
    log |> readr::write_tsv(.ps$connections_log)
  } else {
    log |> readr::write_tsv(.ps$connections_log, append = TRUE)
  }
}

# update raw data log
ps_update_raw_data_log <- function(raw_data) {
  if (!.ps$log) {
    return()
  }
  .ps$msg_idx <- .ps$msg_idx + 1L
  log <- tibble(
    idx = .ps$msg_idx,
    timestamp = lubridate::now(tz = 'UTC'),
    raw_data = !!raw_data
  )
  if (!file.exists(.ps$raw_data_log)) {
    if (!dir.exists(dirname(.ps$raw_data_log))) {
      dir.create(dirname(.ps$raw_data_log), recursive = TRUE)
    }
    log |> readr::write_tsv(.ps$raw_data_log, escape = "none")
  } else {
    log |>
      readr::write_tsv(.ps$raw_data_log, append = TRUE, escape = "none")
  }
}

# update error log
ps_update_error_log <- function(error) {
  # errors are always logged
  log <- tibble(
    timestamp = lubridate::now(tz = 'UTC'),
    error = !!error
  )
  if (!file.exists(.ps$errors_log)) {
    if (!dir.exists(dirname(.ps$errors_log))) {
      dir.create(dirname(.ps$errors_log), recursive = TRUE)
    }
    log |> readr::write_tsv(.ps$errors_log)
  } else {
    log |> readr::write_tsv(.ps$errors_log, append = TRUE)
  }
}

# update events log
ps_update_events_log <- function(event) {
  if (!.ps$log) {
    return()
  }
  if (!file.exists(.ps$events_log)) {
    if (!dir.exists(dirname(.ps$events_log))) {
      dir.create(dirname(.ps$events_log), recursive = TRUE)
    }
    event |> readr::write_tsv(.ps$events_log, escape = "none")
  } else {
    event |>
      readr::write_tsv(.ps$events_log, append = TRUE, escape = "none")
  }
}

# get connection logs
ps_get_connections_log <- function() {
  if (!file.exists(.ps$connections_log)) {
    tibble(timestamp = integer(0) |> as.POSIXct())
  } else {
    readr::read_tsv(.ps$connections_log, col_types = "Tl")
  }
}

# get raw data logs
ps_get_raw_data_log <- function() {
  if (!file.exists(.ps$raw_data_log)) {
    tibble(timestamp = integer(0) |> as.POSIXct())
  } else {
    readr::read_tsv(.ps$raw_data_log, show_col_types = "Tc")
  }
}

# get error logs
ps_get_error_log <- function() {
  if (!file.exists(.ps$errors_log)) {
    tibble(timestamp = integer(0) |> as.POSIXct())
  } else {
    readr::read_tsv(.ps$errors_log, show_col_types = "Tc")
  }
}

# get event logs
particle_stream_get_events_log <- function() {
  if (!file.exists(.ps$events_log)) {
    tibble(timestamp = integer(0) |> as.POSIXct())
  } else {
    readr::read_tsv(.ps$events_log, show_col_types = FALSE)
  }
}
