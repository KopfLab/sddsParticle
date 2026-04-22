# process events ====

# split event json into the components (since transmissions are usually bundled)
# @param json sdds data in JSON format
split_event_data <- function(json) {
  # get json
  parsed <- json |> parse_json()
  if (any(have_name(parsed))) {
    parsed_names <- names(parsed)[nzchar(names(parsed))]
    cli_abort(c(
      "event data cannot be named at the top level",
      "i" = "encountered named entr{?y/ies} {.field {parsed_names}}"
    ))
  }

  # individual event
  tibble(
    kind = parsed |>
      purrr::map_chr(
        ~ {
          if (is_sdds_tree(.x)) {
            "tree"
          } else if (is_sdds_treevalues(.x)) {
            "values"
          } else if (is_sdds_burstdata(.x)) {
            "burst"
          } else {
            NA_character_
          }
        }
      ),
    device = parsed |> purrr::map_chr(~ .x[["n"]] %||% NA_character_),
    type = parsed |> purrr::map_chr(~ .x[["t"]] %||% NA_character_),
    version = parsed |> purrr::map_int(~ .x[["v"]] %||% NA_integer_),
    json = parsed |>
      purrr::map_chr(jsonlite::toJSON, auto_unbox = TRUE, null = "null")
  )
}

#' Parse events data
#' @param events tibble from particle_stream_get_events()
#' @describeIn sdds_parser splits and categorizes events tibble
#' @export
sdds_split_events_data <- function(events) {
  # safety checks
  check_tibble(events, req_cols = "data")
  events |>
    dplyr::mutate(
      data = purrr::map(
        .data$data,
        ~ {
          if (is.na(.x)) {
            tibble(type = character(0), json = character(0))
          } else {
            split_event_data(.x)
          }
        }
      )
    ) |>
    tidyr::unnest(.data$data)
}


# checks if it's a tree object
is_sdds_tree <- function(obj) {
  req <- c("s", "e", "t", "v")
  all(req %in% names(obj))
}

# check if it's a tree data object
is_sdds_treevalues <- function(obj) {
  req <- c("d", "t", "v")
  all(req %in% names(obj))
}

# check if it's a burst
is_sdds_burstdata <- function(obj) {
  req <- c("b", "tb")
  all(req %in% names(obj))
}

# cache events ========

#' @describeIn sdds_parser caches trees and tree values from events tibble
#' @export
sdds_cache_events <- function(events) {
  events <- events |> sdds_split_events_data()
  events |>
    filter(.data$kind == "tree") |>
    rename("tree_json" = "json") |>
    cache_trees()
  events |>
    filter(.data$kind == "values") |>
    rename("values_json" = "json") |>
    cache_treevalues()
  invisible(NULL)
}

#' @describeIn sdds_parser reads the cached trees and tree values
sdds_read_cached_trees_and_values <- function(
  tree_cache_path = "cache/sdds_trees.csv",
  values_cache_path = "cache/sdds_values.csv"
) {
  read_cached_treevalues(values_cache_path) |>
    dplyr::left_join(
      read_cached_trees(tree_cache_path) |>
        # safety measure should there ever be duplicate structure records (shouldn't happen)
        dplyr::slice_head(n = 1, by = c("type", "version")),
      by = c("type", "version")
    )
}

# internal function for caching trees
cache_trees <- function(trees, cache_path = "cache/sdds_trees.csv") {
  # safety checks
  cols <- c("type", "version", "tree_json")
  trees |> check_tibble(req_cols = cols)
  trees <- trees |>
    select(all_of(cols)) |>
    filter(
      !is.na(.data$type),
      !is.na(.data$version),
      !is.na(.data$tree_json)
    )
  if (nrow(trees) == 0) {
    return()
  }

  # does the cache file already exist?
  if (!file.exists(cache_path)) {
    # write it for the first time
    if (!dir.exists(dirname(cache_path))) {
      dir.create(dirname(cache_path), recursive = TRUE)
    }
    trees |> readr::write_csv(file = cache_path)
  } else {
    # append to existing trees
    existing_trees <- cache_path |> read_cached_trees()
    new_trees <- trees |>
      dplyr::anti_join(existing_trees, by = c("type", "version"))
    if (nrow(new_trees) > 0L) {
      new_trees |> readr::write_csv(file = cache_path, append = TRUE)
    }
  }
}

# internal function to read cached trees
read_cached_trees <- function(cache_path = "cache/sdds_trees.csv") {
  if (!file.exists(cache_path)) {
    tibble(
      type = character(),
      version = integer(),
      tree_json = character()
    )
  } else {
    cache_path |>
      readr::read_csv(
        col_types = readr::cols(
          type = readr::col_character(),
          version = readr::col_integer(),
          tree_json = readr::col_character()
        )
      )
  }
}

# internal function for caching tree values
cache_treevalues <- function(values, cache_path = "cache/sdds_values.csv") {
  # safety checks
  cols <- c("coreid", "published_at", "type", "version", "values_json")
  values |> check_tibble(req_cols = cols)
  values <- values |>
    select(all_of(cols)) |>
    filter(
      !is.na(.data$coreid),
      !is.na(.data$published_at),
      !is.na(.data$type),
      !is.na(.data$version),
      !is.na(.data$values_json)
    )

  if (lubridate::is.POSIXct(values$published_at)) {
    # store in common datetime format
    values$published_at <- values$published_at |>
      lubridate::with_tz("UTC") |>
      lubridate::format_ISO8601() |>
      paste0("Z")
  }

  if (nrow(values) == 0) {
    return()
  }

  # does the file already exist?
  if (!file.exists(cache_path)) {
    # write for the first time
    if (!dir.exists(dirname(cache_path))) {
      dir.create(dirname(cache_path), recursive = TRUE)
    }
    values |> readr::write_csv(file = cache_path)
  } else {
    # overwrite existing values
    existing_values <- cache_path |> read_cached_treevalues()
    # overwrite values
    existing_values |>
      dplyr::anti_join(values, by = "coreid") |>
      dplyr::bind_rows(values) |>
      readr::write_csv(file = cache_path)
  }
}

# internal function to read cached tree values
read_cached_treevalues <- function(cache_path = "cache/sdds_values.csv") {
  if (!file.exists(cache_path)) {
    tibble(
      coreid = character(),
      published_at = character(),
      type = character(),
      version = integer(),
      values_json = character()
    )
  } else {
    cache_path |>
      readr::read_csv(
        col_types = readr::cols(
          coreid = readr::col_character(),
          published_at = readr::col_character(),
          type = readr::col_character(),
          version = readr::col_integer(),
          values_json = readr::col_character()
        )
      )
  }
}

# parse data =========

#' @param ds tibble with values_json and tree_json columns, e.g. that created by [sdds_read_cached_trees_and_values]
#' @describeIn sdds_parser parses tree structure and values from tibble
sdds_parse_trees_and_values <- function(ds) {
  # safety checks
  ds |> check_tibble(req_cols = c("coreid", "values_json", "tree_json"))
  # parse json
  ds <- ds |>
    dplyr::mutate(
      tree = purrr::map(.data$tree_json, sdds_parse_tree),
      values = purrr::map(.data$values_json, sdds_parse_values)
    ) |>
    dplyr::select(-"values_json", -"tree_json") |>
    tidyr::unnest(c(.data$tree, .data$values))

  # combine data (outside mutate for better error messages)
  this_call <- current_call()
  ds$tree_w_values <- purrr::pmap(
    list(ds$coreid, ds$tree, ds$values),
    function(coreid, tree, values, call = caller_call()) {
      if (is.null(tree) || is.null(values)) {
        return(NULL)
      }
      out <- sdds_combine_tree_and_values(tree, values) |> try_catch_cnds()
      out |> show_cnds(.call = this_call)
      out$result
    }
  )
  return(ds)
}

#' @param json sdds data in JSON format
#' @describeIn sdds_parser parses tree structure sent by the sddsPublishTree event, usually not called directly but by [sdds_parse_trees_and_values]
#' @export
sdds_parse_tree <- function(json) {
  # get json
  if (!missing(json) && !is_empty(json) && is.na(json[1])) {
    return(tibble(enums = list(NULL), tree = list(NULL)))
  }
  tree <- json |> parse_json()

  # safety checks if it's a tree
  if (!is_sdds_tree(tree)) {
    req <- c("s", "e", "t", "v")
    missing <- setdiff(req, names(tree))
    cli_abort(
      c(
        "this is not a complete tree: structure entr{?y/ies} {.var {missing}} not found",
        "i" = "{length(names(tree))} available entr{?y/ies}: {.var {names(tree)}}"
      )
    )
  }

  # parse enums
  enums <-
    dplyr::tibble(
      enum_id = tree$e |> purrr::map_int(~ .x[[1]]),
      enum_values = tree$e |> purrr::map(~ purrr::map_chr(.x[[2]], identity))
    )

  # options
  sdds_opts <- get_sdds_options()
  data_types <- get_sdds_data_types()

  # structure
  struct <-
    tree$s |>
    expand_structure(data_types = data_types) |>
    # row id
    dplyr::mutate(rowid = dplyr::row_number(), .before = 1L) |>
    # enum values
    dplyr::left_join(enums, by = "enum_id") |>
    # data types
    dplyr::left_join(
      dplyr::tibble(
        type = as.integer(data_types),
        data_type = names(data_types)
      ),
      by = "type"
    ) |>
    dplyr::mutate(
      is_struct = .data$type == data_types[["STRUCT"]],
      is_int = grepl("INT", .data$data_type),
      is_dbl = grepl("FLOAT", .data$data_type),
      is_text = .data$type %in% c(data_types[["STRING"]], data_types[["TIME"]]),
      is_enum = .data$type == data_types[["ENUM"]],
    ) |>
    # options
    dplyr::mutate(
      readonly = bitwAnd(.data$options, sdds_opts[['readonly']]) > 0,
      saveval = bitwAnd(.data$options, sdds_opts[['saveval']]) > 0
    )

  # assemble entry
  dplyr::tibble(
    enums = list(enums),
    tree = list(struct)
  )
}

#' @describeIn sdds_parser parses values structure sent by the sddsPublishValues event, usually not called directly but by [sdds_parse_trees_and_values]
#' @export
sdds_parse_values <- function(json) {
  # get json
  if (!missing(json) && !is_empty(json) && is.na(json[1])) {
    return(tibble(values = list(NULL)))
  }
  values <- json |> parse_json()

  # safety checks if it's a tree
  if (!is_sdds_treevalues(values)) {
    req <- c("d", "t", "v")
    missing <- setdiff(req, names(values))
    cli_abort(
      c(
        "this is not a complete set of tree values: structure entr{?y/ies} {.var {missing}} not found",
        "i" = "{length(names(values))} available entr{?y/ies}: {.var {names(values)}}"
      )
    )
  }

  # extract values
  values <- values[["d"]] |>
    expand_values() |>
    dplyr::mutate(
      v_int = .data$value |>
        purrr::map_int(~ if (is.integer(.x)) .x else NA_integer_),
      v_dbl = .data$value |>
        # store any numeric (both integer and double) in the double column
        # for later matching with tree (in case double is exactly xx.00 or
        # integer is larger unit32 which R cannot store as integer)
        purrr::map_dbl(~ if (is.numeric(.x)) .x else NA_real_),
      v_text = .data$value |>
        purrr::map_chr(~ if (is.character(.x)) .x else NA_character_),
      is_null = purrr::map_lgl(.data$value, is.null),
      unknown_type = !is_null &
        is.na(.data$v_int) &
        is.na(.data$v_dbl) &
        is.na(.data$v_text)
    )

  # safety checks
  if (any(values$unknown_type)) {
    cli_warn("encountered {sum(values$unknown_type)} value{?s} of unknown type")
  }

  # assemble tibble
  dplyr::tibble(
    values = list(values |> dplyr::select(-"value"))
  )
}

# expand values list function
expand_values <- function(v, parent_idx = NA_character_) {
  expand_value <- function(v, idx, parent_idx) {
    idx_path <- if (!is.na(parent_idx)) {
      sprintf("%s[[%s]]", parent_idx, idx)
    } else {
      sprintf("[[%s]]", idx)
    }
    if (is.list(v)) {
      return(expand_values(v, parent_idx = idx_path))
    }
    return(dplyr::tibble(idx_path = idx_path, value = list(v)))
  }
  v |>
    purrr::map2(seq_along(v), expand_value, parent_idx = parent_idx) |>
    dplyr::bind_rows()
}

#' @param tree from [sdds_parse_tree]
#' @param values from [sdds_parse_values]
#' @describeIn sdds_parser combines parsed tree tibble (from [sdds_parse_tree]) and values tibble (from [sdds_parse_values]), usually not called directly but by[sdds_parse_trees_and_values]
#' @export
sdds_combine_tree_and_values <- function(tree, values) {
  # safety checks
  values |>
    check_tibble(
      req_cols = c(
        "idx_path",
        "v_int",
        "v_dbl",
        "v_text",
        "is_null",
        "unknown_type"
      )
    )
  tree |>
    check_tibble(
      req_cols = c("path", "idx_path", "is_struct", "is_enum", "enum_values")
    )

  # combine
  tree_w_values <-
    dplyr::full_join(tree, values, by = "idx_path") |>
    dplyr::mutate(
      v_enum = purrr::pmap_chr(
        list(.data$is_enum, .data$v_int, .data$enum_values),
        function(is_enum, v_int, enum_values) {
          if (
            identical(is_enum, TRUE) && v_int %in% (seq_along(enum_values) - 1L)
          ) {
            enum_values[v_int + 1L]
          } else {
            NA_character_
          }
        }
      ),
      # either value or structure missing?
      struc_missing = is.na(.data$path),
      v_missing = is.na(.data$unknown_type) &
        !is.na(.data$is_struct) &
        !.data$is_struct,
      # non-missing, non-null values that are actually valid?
      v_valid = .data$struc_missing |
        .data$v_missing |
        .data$is_struct |
        .data$is_null |
        # allow double in case integer overflows (R doesn't have unsigned int64)
        (.data$is_int & (!is.na(.data$v_int) | !is.na(.data$v_dbl))) |
        (.data$is_dbl & !is.na(.data$v_dbl)) |
        (.data$is_text & !is.na(.data$v_text)) |
        (.data$is_enum & !is.na(.data$v_int) & !is.na(.data$v_enum))
    ) |>
    dplyr::relocate("v_enum", .after = "v_int")

  # warning messages
  if (
    any(
      tree_w_values$struc_missing |
        tree_w_values$v_missing |
        !tree_w_values$v_valid
    )
  ) {
    info <- c()
    if (any(tree_w_values$struc_missing)) {
      info <- info |>
        c(
          "{sum(tree_w_values$struc_missing)} value{?s} do not seem to belong into the structure"
        )
    }
    if (any(tree_w_values$v_missing)) {
      info <- info |>
        c(
          "{sum(tree_w_values$v_missing)} value{?s} are missing"
        )
    }
    if (any(!tree_w_values$v_valid)) {
      info <- info |>
        c(
          "{sum(!tree_w_values$v_valid)} value{?s} are not valid"
        )
    }
    cli_warn(
      c(
        "encountered issues matching the provided values with the tree",
        info |> set_names(rep("i", length(info)))
      )
    )
  }

  # return (a bit cleaned up)
  return(
    tree_w_values |>
      dplyr::filter(!.data$struc_missing) |>
      dplyr::select(-"unknown_type", -"struc_missing")
  )
}


#' @param trees_and_values output from [sdds_parse_trees_and_values]
#' @describeIn sdds_parser simplifies combined trees and values (from [sdds_parse_trees_and_values]), removes the structures themselves only leaving actual data fields
#' @export
sdds_simplify_trees_and_values <- function(
  trees_and_values,
  devices,
  timezone = Sys.timezone(),
  additional_types = list(),
  additional_converters = list()
) {
  # safety checks
  trees_and_values |> check_tibble(c("coreid", "published_at", "tree_w_values"))
  if (nrow(trees_and_values) == 0) {
    return(tibble())
  }

  # standard data types
  types <- list(
    "null" = expr(.data$v_missing | !.data$v_valid),
    "enum" = expr(.data$is_enum),
    "var_interval" = expr(.data$is_var_interval),
    "datetime" = expr(.data$base_units == "dt"),
    "duration" = expr(.data$base_units %in% names(.duration_converter)),
    "byte" = expr(.data$base_units == "byte"),
    "version" = expr(.data$is_int & .data$name == "version"),
    "integer" = expr(.data$is_int),
    "double" = expr(.data$is_dbl),
    "text" = expr(TRUE)
  )
  types <- c(
    additional_types,
    types[!names(types) %in% names(additional_types)]
  )

  # standard text converters
  converters <- list(
    "null" = null_value_to_text,
    "enum" = enum_value_to_text,
    "var_interval" = var_intervals_value_to_text,
    "datetime" = function(value, ...) {
      dt <- lubridate::ymd_hms(value, quiet = TRUE)
      if (is.na(dt)) {
        return(value)
      }
      dt |>
        lubridate::with_tz(timezone) |>
        format("%b %d %Y %H:%M:%S")
    },
    "duration" = duration_value_to_text,
    "byte" = byte_value_to_text,
    "version" = version_value_to_text,
    "integer" = integer_value_to_text,
    "double" = double_value_to_text,
    "text" = text_value_to_text
  )
  converters <- c(
    additional_converters,
    converters[!names(converters) %in% names(additional_converters)]
  )

  # safety check
  if (
    length(setdiff(names(types), names(converters))) > 0 ||
      length(setdiff(names(converters), names(types))) > 0
  ) {
    cli_abort(
      c(
        "type and converter entries don't match: ",
        "i" = "types: {names(types)}",
        "i" = "converters: {names(converters)}}"
      )
    )
  }

  # unnest
  out <- trees_and_values |>
    dplyr::select("coreid", "published_at", "tree_w_values") |>
    tidyr::unnest("tree_w_values")
  if (nrow(out) == 0) {
    return(tibble())
  }

  # parse
  types_expr <- types |> purrr::imap(~ expr((!!.x) ~ !!.y))
  out <- out |>
    # filter out structures and values without tree entries
    dplyr::filter(!.data$is_struct) |>
    # new columns
    dplyr::mutate(
      # as datetime
      published_at = .data$published_at |>
        lubridate::ymd_hms() |>
        lubridate::with_tz(timezone),
      # figure out the parent
      parent = dplyr::if_else(
        !.data$is_struct,
        stringr::str_remove(.data$path, "\\.[^.]+$"),
        .data$path
      ) |>
        # remove all units
        stringr::str_remove_all("_[^.]+"),
      # individual var publish intervals
      is_var_interval = stringr::str_detect(
        .data$parent,
        stringr::fixed("SYSTEM.publishing.varIntervals")
      ),
      # figure out the measurement base unit
      base_units = .data$path |>
        stringr::str_extract("(?<=_)([^.$]+)(?=(\\.|$))") |>
        stringr::str_replace_all("_", "/"),
      # figure out the label (without units)
      label = .data$name |> stringr::str_remove("_.*$"),
      # get value as list
      value = dplyr::case_when(
        .data$v_missing | !.data$v_valid | .data$is_struct ~ list(NULL),
        .data$is_enum ~ as.list(.data$v_enum),
        .data$is_int & !is.na(.data$v_int) ~ as.list(.data$v_int),
        # account for unsigned int64 from microcontroller (R cannot do that)
        .data$is_int & is.na(.data$v_int) ~ as.list(.data$v_dbl),
        .data$is_dbl ~ as.list(.data$v_dbl),
        TRUE ~ as.list(.data$v_text)
      ),
      # get the data type
      type = case_when(!!!types_expr),
      # do the value to text conversion
      text = purrr::pmap_chr(
        list(value = .data$value, units = .data$base_units, type = .data$type),
        function(value, units, type, converters = list()) {
          if (is.null(value)) {
            return(null_value_to_text())
          } else if (type %in% names(converters)) {
            as.character(converters[[type]](value, units))
          } else {
            NA_character_
          }
        },
        converters = !!converters
      )
    ) |>
    # arrangement
    mutate(parent = factor_in_order(parent)) |>
    mutate(.by = "coreid", row_order = row_number()) |>
    arrange(.data$parent, .data$row_order) |>
    select(-"row_order")

  # cleanup
  out <- out |>
    # no longer need these columns
    dplyr::select(
      -"idx_path",
      -"options",
      -"data_type",
      -"is_struct",
      -"is_null",
      -starts_with("v_")
    ) |>
    left_join(
      devices |> select("coreid", "corename" = "name"),
      by = "coreid"
    ) |>
    relocate("corename", .after = "coreid")
  return(out)
}

#' @describeIn sdds_parser parses the command log from a device (retrieved via particle_get_sdds_command_log())
#' @export
sdds_parse_command_log <- function(json, timezone = Sys.timezone()) {
  # get json
  empty_return <- tibble(
    datetime = integer() |> as.POSIXct(),
    cmd = character(),
    retval = integer(),
    error_code = character()
  )
  if (!missing(json) && !is_empty(json) && is.na(json[1])) {
    return(empty_return)
  }
  commands <- json |> parse_json()
  if (is_empty(commands)) {
    return(empty_return)
  }

  # safety checks if it's a command log
  req <- c("l", "tb")
  if (!all(req %in% names(commands))) {
    missing <- setdiff(req, names(commands))
    cli_abort(
      c(
        "this is not a complete command log: structure entr{?y/ies} {.var {missing}} not found",
        "i" = "{length(names(commands))} available entr{?y/ies}: {.var {names(commands)}}"
      )
    )
  }

  # error codes
  err_codes <- c(
    "portParseErr",
    "pathNotFound",
    "pathNoStruct",
    "pathNullPtr",
    "invPort",
    "invFunc"
  )

  # parse
  tb <- lubridate::ymd_hms(commands$tb)
  cmds_formatted <- tibble(
    datetime = (tb +
      # offset is in milliseconds --> divide by 1000
      lubridate::seconds(purrr::map_int(commands$l, ~ .x[[1]]) / 1000)) |>
      lubridate::with_tz(timezone),
    cmd = commands$l |> purrr::map_chr(~ .x[[2]]),
    retval = commands$l |> purrr::map_int(~ .x[[3]])
  ) |>
    dplyr::left_join(
      err_codes |> tibble::enframe(name = "retval", value = "error_code"),
      by = "retval"
    )

  # return
  return(cmds_formatted)
}

#' @describeIn sdds_parser parses the a state snapshot from a device
#' @export
sdds_parse_state_snapshot <- function(json) {
  # get json
  empty_return <- tibble(
    path = character(),
    v_num = numeric(),
    v_text = character()
  )
  if (!missing(json) && !is_empty(json) && is.na(json[1])) {
    return(empty_return)
  }
  snap <- json |> parse_json()
  if (is_empty(snap)) {
    return(empty_return)
  }
  snap |> flatten_named_list() |> bind_rows()
}

flatten_named_list <- function(x, parent = NULL, sep = ".") {
  out <- list()
  purrr::map2(names(x), x, function(var, val) {
    var <- if (is.null(parent)) var else paste0(parent, sep, var)
    if (is.list(val)) {
      flatten_named_list(val, var, sep)
    } else if (!is_empty(val) && !is.na(val)) {
      if (is.numeric(val)) {
        list(list(path = var, v_num = val))
      } else {
        list(list(path = var, v_text = val))
      }
    }
  }) |>
    unlist(recursive = FALSE)
}

# helper functions ========

# check if it's valid json
parse_json <- function(json, .env = caller_env()) {
  # safety checks
  json |>
    check_arg(
      !missing(json) && is_scalar_character(json),
      "must be provided as a single string",
      .env = .env
    )

  if (err <- !jsonlite::validate(json)) {
    errors <- strsplit(attr(err, "err"), split = "\n") |> unlist()
    abort(
      c(
        "encountered invalid JSON",
        purrr::map_chr(
          errors,
          ~ format_inline("{.x}", collapse = FALSE, keep_whitespace = TRUE)
        ) |>
          set_names(rep("i", length(errors)))
      ),
      call = .env
    )
  }
  # convert json
  json_parsed <- jsonlite::parse_json(json)
  return(json_parsed)
}

# see uTypedef.h
get_sdds_data_types <- function() {
  c(
    UINT8 = 0x01,
    UINT16 = 0x02,
    UINT32 = 0x04,
    TIME = 0x06,
    INT8 = 0x11,
    INT16 = 0x12,
    INT32 = 0x14,
    FLOAT32 = 0x24,
    ENUM = 0x31,
    STRUCT = 0x42,
    STRING = 0x81
  )
}

# see uTypedef.h
get_sdds_options <- function() {
  c(
    nothing = 0,
    readonly = 0x01,
    saveval = 0x80,
    # hereafter not yet interpreted
    mask_show = 0x0E,
    showHex = 0x04,
    showBin = 0x06,
    showString = 0x08,
    timeRel = 0x02,
    timeAbs = 0x00
  )
}

# expand sdds structure list
expand_structure <- function(
  s,
  data_types = get_sdds_data_types(),
  parent = NA_character_,
  parent_idx = NA_character_
) {
  s |>
    purrr::map2(
      seq_along(s),
      expand_structure_element,
      data_types = data_types,
      parent = parent,
      parent_idx = parent_idx
    ) |>
    dplyr::bind_rows()
}

# expand sdds list elemen
expand_structure_element <- function(
  e,
  idx,
  data_types = get_sdds_data_types(),
  parent = NA_character_,
  parent_idx = NA_character_
) {
  struc <-
    dplyr::tibble(
      name = e[[3]],
      path = if (!is.na(parent)) {
        sprintf("%s.%s", !!parent, .data$name)
      } else {
        .data$name
      },
      idx_path = if (!is.na(parent_idx)) {
        sprintf("%s[[%s]]", parent_idx, !!idx)
      } else {
        sprintf("[[%s]]", !!idx)
      },
      type = e[[1]],
      options = e[[2]],
      enum_id = NA_integer_
    )
  if (struc$type == data_types['ENUM']) {
    struc$enum_id <- e[[4]]
  } else if (struc$type == data_types['STRUCT'] && !is.null(e[[4]])) {
    # expand substructure
    struc <- dplyr::bind_rows(
      struc,
      expand_structure(
        e[[4]],
        data_types = data_types,
        parent = struc$path,
        parent_idx = struc$idx_path
      )
    )
  }
  return(struc)
}
