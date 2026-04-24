# access token ===============

# internatal
check_particle_token <- function(token, call = caller_call()) {
    tryCatch(
        force(token),
        error = function(e) {
            cli_abort(
                "no {.field particle access token} available, use {.strong particle_store_token()}",
                parent = e,
                call = call
            )
        }
    )
    invisible(NULL)
}

#' Interact with the particle API
#'
#' @describeIn particle_api securely stores the access token using your OS keyring
#' @export
particle_store_token <- function() {
    keyring::key_set(service = "particle")
}

# particle functions =============

# helper function to make a particle cloud request
# @param endpoint the url endpoint for the request, typically devices/<device_id>/variable or devices/<device_id>/function
# @param arg request argument --> required for function calls! even if just \code{""}. without this it checks a variable isntead
# @param timeout how long to wait for curl request
send_request <- function(
    endpoint,
    arg = NULL,
    timeout = 3,
    token = keyring::key_get("particle"),
    .call = caller_call()
) {
    # safety checks
    endpoint |>
        check_arg(
            !missing(endpoint) && is_scalar_character(endpoint),
            "must be a string"
        )
    arg |>
        check_arg(is.null(arg) || is_scalar_character(arg), "must be a string")
    token |> check_particle_token()

    # request
    handle <- curl::new_handle(timeout = timeout)
    request <- sprintf("https://api.particle.io/v1/%s", endpoint)
    if (!is.null(arg)) {
        # post
        post <- sprintf("access_token=%s&arg=%s", token, utils::URLencode(arg))
        handle <- handle |> curl::handle_setopt(copypostfields = post)
    } else {
        # get
        request <- sprintf("%s?access_token=%s", request, token)
    }

    # send
    out <- try_catch_cnds({
        fetch <- request |> curl::curl_fetch_memory(handle = handle)
        data <- fetch$content |> rawToChar() |> jsonlite::fromJSON()
        if (!is.null(data$error)) {
            cli_abort("{cli::col_red(data$error)} - {data$error_description}")
        }
        data
    })

    # stop if errors
    abort_cnds(out$conditions, .call = .call)

    # return
    return(out$result)
}

#' @param token particle access token (retrieved from keyring by default), save a token in your keyring with particle_store_token()
#' @param sdds_only whether to only return SDDS devices (default TRUE)
#' @describeIn particle_api retrieve the particle SDDS devices registered to your account
#' @export
particle_get_device_info <- function(
    token = keyring::key_get("particle"),
    sdds_only = TRUE
) {
    # safety checks
    check_arg(sdds_only, is_scalar_logical(sdds_only), "must be TRUE or FALSE")
    # get devices
    send_request("devices", token = token) |>
        tibble::as_tibble() |>
        dplyr::rename("coreid" = "id") |>
        # filter for sdds devices
        dplyr::filter(
            purrr::map_lgl(
                .data$functions,
                ~ if (sdds_only) {
                    all(c("sdds", "sendSdds", "sendSddsValues") %in% .x)
                } else {
                    TRUE
                }
            )
        )
}

#' @param coreid the ID of the particle device to send requests and commands to
#' @describeIn particle_api request the self-describing data structure (sdds) from the device, returns a tibble with `id`, `name`, `connected` and `return_value` (0 = success)
#' @export
particle_request_sdds <- function(
    coreid,
    token = keyring::key_get("particle")
) {
    # safety checks
    coreid |>
        check_arg(
            !missing(coreid) && is_scalar_character(coreid),
            "must provide a particle coreid"
        )
    sprintf("devices/%s/sendSdds", coreid) |>
        send_request(token = token, arg = "")
}

#' @describeIn particle_api request the SDDS tree values from the device, returns a tibble with `id`, `name`, `connected` and `return_value` (0 = success)
#' @export
particle_request_sdds_values <- function(
    coreid,
    token = keyring::key_get("particle")
) {
    # safety checks
    coreid |>
        check_arg(
            !missing(coreid) && is_scalar_character(coreid),
            "must provide a particle coreid"
        )
    sprintf("devices/%s/sendSddsValues", coreid) |>
        send_request(token = token, arg = "")
}

#' @describeIn particle_api get the sdds system values
#' @export
particle_get_sdds_system <- function(
    coreid,
    token = keyring::key_get("particle")
) {
    # safety checks
    coreid |>
        check_arg(
            !missing(coreid) && is_scalar_character(coreid),
            "must provide a particle coreid"
        )
    result <- sprintf("devices/%s/getSddsSystem", coreid) |>
        send_request(token = token)
    return(result$result)
}

#' @describeIn particle_api get the sdds command log from a device, returns a string that's in JSON format, use [sdds_parse_command_log] to process
#' @export
particle_get_sdds_command_log <- function(
    coreid,
    token = keyring::key_get("particle")
) {
    # safety checks
    coreid |>
        check_arg(
            !missing(coreid) && is_scalar_character(coreid),
            "must provide a particle coreid"
        )
    result <- sprintf("devices/%s/getSddsCommandLog", coreid) |>
        send_request(token = token)
    return(result$result)
}

#' @param cmds SDDS command to send
#' @describeIn particle_api sends the \code{cmds} to the particle device, returns a tibble with `id`, `name`, `connected` and `return_value` (0 = success, > 0 bit-wise indicator of commands that failed, < 0 the exact error code if it was a single command)
#' @export
particle_send_sdds_commands <- function(
    coreid,
    cmds = character(),
    token = keyring::key_get("particle")
) {
    # safety checks
    coreid |>
        check_arg(
            !missing(coreid) && is_scalar_character(coreid),
            "must provide a particle coreid"
        )
    cmds |> check_arg(is_character(cmds))
    out <- sprintf("devices/%s/sdds", coreid) |>
        send_request(token = token, arg = paste(cmds, collapse = " "))
    out$success <- check_commands_success(out$return_value, length(cmds))
    return(out)
}

# return value
check_commands_success <- function(retval, n_cmds) {
    stopifnot(is_integerish(n_cmds))
    if (n_cmds == 0) {
        return(logical())
    }
    if (n_cmds == 1) {
        return(retval == 0)
    }
    purrr::map_lgl(
        0:(n_cmds - 1L),
        ~ {
            bitwAnd(retval, bitwShiftL(1L, .x)) == 0
        }
    )
}


# convenience functions for multiple core ids =============

#' @describeIn particle_api get the sdds system values for multiple cores with [particle_get_sdds_system] and parse them right away using [sdds_parse_system]
#' @export
particle_get_and_parse_sdds_systems <- function(
    core_ids,
    timezone = Sys.timezone(),
    wide = TRUE,
    token = keyring::key_get("particle")
) {
    # fetch data
    out <- core_ids |>
        purrr::map(
            ~ {
                particle_get_sdds_system(.x, token = token) |>
                    sdds_parse_system(timezone = timezone, wide = wide) |>
                    try_catch_cnds(error_value = tibble())
            }
        )
    # return both result and combined conditions
    list(
        result = out |>
            purrr::map2(
                core_ids,
                ~ .x$result |> mutate(coreid = .y, .before = 1L)
            ) |>
            bind_rows(),
        conditions = out |> purrr::map(~ .x$conditions) |> bind_rows()
    )
}

#' @describeIn particle_api get the sdds command logs with [particle_get_sdds_command_log] for multiple cores and parse them right away using [sdds_parse_command_log]
#' @export
particle_get_and_parse_sdds_command_logs <- function(
    core_ids,
    timezone = Sys.timezone(),
    token = keyring::key_get("particle")
) {
    # fetch data
    out <- core_ids |>
        purrr::map(
            ~ {
                particle_get_sdds_command_log(.x, token = token) |>
                    sdds_parse_command_log(timezone = timezone) |>
                    try_catch_cnds(error_value = tibble())
            }
        )
    # return both result and combined conditions
    list(
        result = out |>
            purrr::map2(
                core_ids,
                ~ .x$result |> mutate(coreid = .y, .before = 1L)
            ) |>
            bind_rows(),
        conditions = out |> purrr::map(~ .x$conditions) |> bind_rows()
    )
}
