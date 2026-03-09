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

#' Set a particle access token
#'
#' This function securely stores the access token using the system keyring.
#' @export
particle_store_token <- function() {
    keyring::key_set(service = "particle")
}

# particle functions =============

#' Get devices
#' @param token particle access token
#' @param sdds_only whether to only return SDDS devices (default TRUE)
#' @return tibble of particle SDDS devices registered to the account
#' @export
particle_get_device_info <- function(
    token = keyring::key_get("particle"),
    sdds_only = TRUE
) {
    # safety checks
    check_arg(sdds_only, is_scalar_logical(sdds_only), "must be TRUE or FALSE")
    check_particle_token(token)

    endpoint <- "devices"
    request <- sprintf(
        "https://api.particle.io/v1/%s?access_token=%s",
        endpoint,
        token
    )

    # request
    handle <- curl::new_handle(timeout = 3)
    out <- try_catch_cnds({
        fetch <- request |>
            curl::curl_fetch_memory(handle = handle)
        data <- fetch$content |>
            rawToChar() |>
            jsonlite::fromJSON()
        if (!is.null(data$error)) {
            cli_abort("{cli::col_red(data$error)} - {data$error_description}")
        }
        data |>
            tibble::as_tibble() |>
            dplyr::filter(
                purrr::map_lgl(
                    .data$functions,
                    ~ if (sdds_only) "sdds" %in% .x else TRUE
                )
            )
    })

    # stop if errors
    abort_cnds(out$conditions)

    # result
    return(out$result)
}
