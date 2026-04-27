# value editing module =========

# make unique input id
uid <- function(gui_id, input_id) sprintf("%s-%s", gui_id, input_id)

# tag input with value change class
add_value_changed_class_to_input <- function(id, delay = NULL) {
  js <- format_inline(
    "document.querySelector('#{id}').closest('.form-group').classList.add('value-changed');"
  )
  if (!is.null(delay)) {
    shinyjs::delay(delay, shinyjs::runjs(js))
  } else {
    shinyjs::runjs(js)
  }
}

# standard input row
generate_standard_input_row <- function(
  label,
  widget = NA_character_,
  units = NA_character_,
  widths = c(4, 4, 4)
) {
  fluidRow(
    column(width = widths[1], tags$div(style = "margin-top: 5px;", label)),
    if (is(widget, "shiny.tag")) {
      column(width = widths[2], widget)
    } else if (!is.na(widget) && is.character(widget) && nzchar(widget)) {
      column(
        width = widths[2],
        tags$div(style = "margin-top: 5px;", tags$strong(widget))
      )
    },
    if (is(units, "shiny.tag")) {
      column(width = widths[3], units)
    } else if (!is.na(units) && is.character(units) && nzchar(units)) {
      column(width = widths[3], tags$div(style = "margin-top: 5px;", units))
    }
  )
}

# input module
input_module <- function(
  id,
  value_to_text,
  make_gui = function(...) {},
  fetch_input = function(ui_inputs, id) return(ui_inputs[[id]]),
  input_to_value = function(input, units) return(input),
  value_to_input = function(value, units) return(value),
  correct_input = function(input) return(input),
  compare_input = function(input1, input2) identical(input1, input2),
  update_input = function(session, id, input) {},
  flag_input_changed = function(id, ...) {
    add_value_changed_class_to_input(id, ...)
  }
) {
  # safety checks
  stopifnot(
    is_function(value_to_text),
    is_function(make_gui),
    is_function(fetch_input),
    is_function(input_to_value),
    is_function(value_to_input),
    is_function(correct_input),
    is_function(compare_input),
    is_function(update_input),
    is_function(flag_input_changed)
  )

  # module server
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # keep track of reactive values
    rv <- reactiveValues(
      gui_id = 0L,
      observer = NULL,
      active = FALSE,
      input_ids = character(),
      units = character(),
      original = list(),
      inputs = list(),
      changed = logical()
    )

    # reset the GUI created by this module
    reset <- function() {
      if (!is.null(isolate(rv$observer))) {
        isolate(rv$observer)$destroy()
      }
      rv$observer <- NULL
      rv$active <- FALSE
      rv$input_ids <- character()
      rv$original <- list()
      rv$inputs <- list()
      rv$changed <- logical()
    }

    # activate the GUI created by this module
    activate <- function() {
      # register inputs observer
      rv$observer <- observe({
        req(!is_empty(rv$input_ids))
        # get inputs
        rv$inputs <-
          uid(isolate(rv$gui_id), rv$input_ids) |>
          purrr::map(fetch_input, ui_inputs = input) |>
          set_names(rv$input_ids)
      })
    }

    # check if there are any changes t the inputs
    has_changed_inputs <- reactive({
      any(rv$changed)
    })

    # return the input value for the core id
    get_input <- function(input_id) rv$inputs[[input_id]]

    # return the value for the core id (parsing the input)
    get_value <- function(input_id, if_changed = TRUE) {
      # is the value NULL or has not changed?
      curr_input <- get_input(input_id)
      if (
        is.null(curr_input) ||
          all(purrr::map_lgl(curr_input, is.null)) ||
          (if_changed && !rv$changed[[input_id]])
      ) {
        return(NULL)
      }
      input_to_value(input = curr_input, rv$units[[input_id]])
    }

    # return the text for the core id (parsing the value)
    get_text <- function(input_id, units = NA_character_, if_changed = TRUE) {
      value <- get_value(input_id, if_changed)
      if (is.null(value)) {
        return(NA_character_)
      }
      value_to_text(value = value, units = units)
    }

    # copy input from one input_id to all others
    # unless core id is supplied, copies from the first one that has changes
    copy_input <- function(from_input_id = NULL) {
      if (!is.null(from_input_id) || has_changed_inputs()) {
        if (is.null(from_input_id)) {
          from_input_id <- names(rv$changed)[rv$changed][1]
        }
        from_input <- get_input(from_input_id)
        if (!is.null(from_input) || !any(purrr::map_lgl(from_input, is.null))) {
          # make the updates
          purrr::walk(
            rv$input_ids,
            ~ if (.x != from_input_id) {
              # update
              update_input(
                session = session,
                id = uid(rv$gui_id, .x),
                input = from_input
              )
              # flag as changed
              flag_input_first_time(.x)
            }
          )
        }
      }
    }

    # update inputs if they are incorrect
    update_incorrect_inputs <- function(input_id, input, corrected) {
      if (!identical(input, corrected)) {
        update_input(
          session = session,
          id = uid(rv$gui_id, input_id),
          input = corrected
        )
      }
    }

    # flag only first time
    flag_input_first_time <- function(input_id, delay = NULL) {
      if (!rv$changed[[input_id]]) {
        # first time change --> flag
        flag_input_changed(ns(uid(rv$gui_id, input_id)), delay = delay)
        rv$changed[[input_id]] <- TRUE
      }
    }

    # flag inputs if they have changed (based on corrected value)
    flag_changed_inputs <- function(input_id, corrected) {
      if (
        !is.null(corrected) &&
          !compare_input(rv$original[[input_id]], corrected)
      ) {
        flag_input_first_time(input_id)
      }
    }

    # monitor inputs
    observeEvent(rv$inputs, {
      req(!is_empty(rv$inputs))
      current_inputs <- rv$inputs[rv$input_ids]

      # run error checking
      corrected_inputs <-
        current_inputs |>
        purrr::map(
          ~ if (is.null(.x) || all(purrr::map_lgl(.x, is.null))) {
            NULL
          } else {
            correct_input(.x)
          }
        )

      # update inputs if corrections are necessary
      purrr::pwalk(
        list(rv$input_ids, current_inputs, corrected_inputs),
        update_incorrect_inputs
      )

      # figure out if value has changed and flag them if so
      purrr::pwalk(
        list(rv$input_ids, corrected_inputs),
        flag_changed_inputs
      )
    })

    # generate the edit user interface
    generate_ui <- function(
      gui_id,
      input_id,
      value,
      text,
      units,
      ...,
      changed = FALSE
    ) {
      # info message
      format_inline("generating edit UI #{gui_id} for {input_id}") |>
        log_debug(ns = ns)

      # store unique gui id if it changed
      if (!identical(rv$gui_id, gui_id)) {
        rv$gui_id <- gui_id
      }

      # keep track of input_ids rendered by this module
      if (!input_id %in% rv$input_ids) {
        rv$input_ids <- c(rv$input_ids, input_id)
      }

      # keep track of original input
      rv$original[[input_id]] <- value_to_input(value, units)
      rv$units[[input_id]] <- units
      rv$changed[[input_id]] <- FALSE
      if (changed) {
        flag_input_first_time(input_id, delay = 1000)
      }

      # generate UI
      make_gui(
        id = ns(uid(rv$gui_id, input_id)),
        input = rv$original[[input_id]],
        text = text, # usually not used
        units = units,
        changed = changed,
        ...
      )
    }

    # module functions
    list(
      reset = reset,
      activate = activate,
      generate_ui = generate_ui,
      has_changes = has_changed_inputs,
      value_to_text = value_to_text,
      get_value = get_value,
      get_text = get_text,
      copy_input = copy_input
    )
  })
}

# input module with select units
input_module_selectable_units <- function(
  id,
  input_to_value,
  value_to_input,
  value_to_text,
  units_options,
  make_gui = NULL,
  input_step = NA,
  fetch_input = function(ui_inputs, id) {
    list(
      v = ui_inputs[[id]],
      u = ui_inputs[[paste0(id, "units")]]
    )
  },
  correct_input = function(input) return(input),
  compare_input = function(input1, input2) {
    (identical(input1$v, input2$v) ||
      (!is.na(input1$v) &&
        !is.na(input2$v) &&
        near(input1$v, input2$v))) &&
      input1$u == input2$u
  },
  update_input = function(session, id, input) {
    updateNumericInput(session, id, value = input$v)
    updateSelectInput(session, paste0(id, "units"), selected = input$u)
  },
  flag_input_changed = function(id, ...) {
    add_value_changed_class_to_input(id, ...)
    add_value_changed_class_to_input(paste0(id, "units"), ...)
  }
) {
  if (is.null(make_gui)) {
    make_gui <- function(id, label, input, units, changed, ...) {
      widget <- numericInput(
        inputId = id,
        label = NULL,
        value = input$v,
        step = input_step
      )
      units <- selectInput(
        inputId = paste0(id, "units"),
        label = NULL,
        choices = units_options,
        selected = input$u
      )
      generate_standard_input_row(label, widget, units)
    }
  }
  input_module(
    id,
    value_to_text = value_to_text,
    make_gui = make_gui,
    fetch_input = fetch_input,
    input_to_value = input_to_value,
    value_to_input = value_to_input,
    correct_input = correct_input,
    compare_input = compare_input,
    update_input = update_input,
    flag_input_changed = flag_input_changed
  )
}

# specific inputs ================

# null input
value_null_input <- function(id) {
  input_module(
    id = id,
    value_to_text = null_value_to_text,
    make_gui = function(label, ...) {
      generate_standard_input_row(label, null_value_to_text())
    }
  )
}

# read-only input
value_read_only_input <- function(id) {
  input_module(
    id = id,
    value_to_text = text_value_to_text,
    make_gui = function(label, text, ...) {
      generate_standard_input_row(label, text, "(read-only)")
    }
  )
}

# text input
value_text_input <- function(id) {
  input_module(
    id = id,
    value_to_text = text_value_to_text,
    make_gui = function(id, label, input, units, changed, ...) {
      widget <- textInput(
        inputId = id,
        label = NULL,
        value = input
      )
      generate_standard_input_row(label, widget, units)
    },
    update_input = function(session, id, input) {
      updateTextInput(session, id, value = input)
    }
  )
}

# integer input
value_integer_input <- function(id) {
  input_module(
    id = id,
    value_to_text = integer_value_to_text,
    make_gui = function(id, label, input, units, changed, ...) {
      widget <- numericInput(
        inputId = id,
        label = NULL,
        value = input,
        step = 1
      )
      generate_standard_input_row(label, widget, units)
    },
    correct_input = function(input) {
      if (!is_integerish(input)) {
        return(as.integer(round(input)))
      }
      return(input)
    },
    update_input = function(session, id, input) {
      updateNumericInput(session, id, value = input)
    }
  )
}

# double input
value_double_input <- function(id) {
  input_module(
    id = id,
    value_to_text = double_value_to_text,
    make_gui = function(id, label, input, units, changed, ...) {
      widget <- numericInput(
        inputId = id,
        label = NULL,
        value = input
      )
      generate_standard_input_row(label, widget, units)
    },
    compare_input = function(input1, input2) near(input1, input2),
    update_input = function(session, id, input) {
      updateNumericInput(session, id, value = input)
    }
  )
}

# enumeration input
value_enum_input <- function(id) {
  input_module(
    id = id,
    value_to_text = enum_value_to_text,
    make_gui = function(id, label, input, choices, changed, ...) {
      widget <- selectInput(
        inputId = id,
        label = NULL,
        choices = choices,
        selected = input
      )
      generate_standard_input_row(label, widget)
    },
    update_input = function(session, id, input) {
      updateSelectInput(session, id, selected = input)
    }
  )
}

# duration input
value_duration_input <- function(id) {
  input_module_selectable_units(
    id = id,
    value_to_text = duration_value_to_text,
    value_to_input = duration_to_largest_whole_unit,
    input_to_value = function(input, units) {
      dur <- .duration_converter[[input$u]](input$v)
      if (units == "ms") {
        as.numeric(dur, "sec") * 1000L
      } else {
        as.numeric(dur, units)
      }
    },
    input_step = 1L,
    units_options = names(.duration_converter),
    correct_input = function(input) {
      # only alow integers
      input$v <- round(as.numeric(input$v))
      return(input)
    }
  )
}

# hhmm input
value_hhmm_input <- function(id, get_timezone) {
  input_module(
    id,
    value_to_text = function(value, ...) {
      HHMM_value_to_text(value, timezone = get_timezone(), ...)
    },
    make_gui = function(id, label, input, units, changed, ...) {
      hour <- selectInput(
        inputId = id,
        label = NULL,
        choices = sprintf("%02d", 0:23),
        selected = input$hour
      )
      minutes <- selectInput(
        inputId = paste0(id, "mins"),
        label = NULL,
        choices = sprintf("%02d", 0:59),
        selected = input$mins
      )
      generate_standard_input_row(label, hour, minutes)
    },
    fetch_input = function(ui_inputs, id) {
      list(
        hour = ui_inputs[[id]],
        mins = ui_inputs[[paste0(id, "mins")]]
      )
    },
    value_to_input = function(value, ...) {
      dt <- HHMM_value_to_datetime(value, timezone = get_timezone(), ...)
      list(
        hour = sprintf("%02d", lubridate::hour(dt)),
        mins = sprintf("%02d", lubridate::minute(dt))
      )
    },
    input_to_value = function(input, ...) {
      now <- lubridate::now(tzone = get_timezone())
      lubridate::hour(now) <- as.integer(input$hour)
      lubridate::minute(now) <- as.integer(input$mins)
      now |> lubridate::with_tz("UTC") |> format("%H%M")
    },
    compare_input = function(input1, input2) {
      identical(input1$hour, input2$hour) && identical(input1$mins, input2$mins)
    },
    update_input = function(session, id, input) {
      updateSelectInput(session, id, selected = input$hour)
      updateSelectInput(session, paste0(id, "mins"), selected = input$mins)
    },
    flag_input_changed = function(id, ...) {
      add_value_changed_class_to_input(id, ...)
      add_value_changed_class_to_input(paste0(id, "mins"), ...)
    }
  )
}

# publishing interval input
value_var_intervals_input <- function(id) {
  input_module(
    id,
    value_to_text = var_intervals_value_to_text,
    make_gui = function(id, label, input, units, changed, ...) {
      widget <- selectInput(
        inputId = id,
        label = NULL,
        choices = .var_intervals_conversion |>
          select("text", "value") |>
          tibble::deframe() |>
          c(list(
            "average over individual interval (if recording)" = "INDIVIDUAL"
          )),
        selected = input$s
      )
      individual <- numericInput(
        inputId = paste0(id, "value"),
        label = NULL,
        value = input$v,
        step = 1L
      )
      units <- selectInput(
        inputId = paste0(id, "units"),
        label = NULL,
        choices = names(.duration_converter),
        selected = input$u
      )
      tagList(
        generate_standard_input_row(label, widget, widths = c(4, 8)),
        div(
          id = paste0(id, "div"),
          generate_standard_input_row("", individual, units)
        ) |>
          shinyjs::hidden()
      )
    },
    fetch_input = function(ui_inputs, id) {
      list(
        s = ui_inputs[[id]],
        v = ui_inputs[[paste0(id, "value")]],
        u = ui_inputs[[paste0(id, "units")]]
      )
    },
    value_to_input = function(value, units) {
      if (value %in% as.character(.var_intervals_conversion$value)) {
        list(s = value, v = 30, u = "min")
      } else {
        dur <- duration_to_largest_whole_unit(value, "ms")
        list(s = 3L, v = dur$v, u = dur$u)
      }
    },
    input_to_value = function(input, units) {
      if (input$s %in% as.character(.var_intervals_conversion$value)) {
        return(as.integer(input$s))
      }
      dur <- .duration_converter[[input$u]](input$v)
      return(as.numeric(dur, "sec") * 1000)
    },
    correct_input = function(input) {
      # only allow integers for value
      input$v <- round(as.numeric(input$v))
      return(input)
    },
    compare_input = function(input1, input2) {
      input1$s == input2$s &&
        (input1$s < 3L || (near(input1$v, input2$v) && input1$u == input2$u))
    },
    update_input = function(session, id, input) {
      shinyjs::toggle(paste0(id, "div"), condition = input$s == "INDIVIDUAL")
      updateSelectInput(session, id, selected = input$s)
      updateNumericInput(session, paste(id, "value"), value = input$v)
      updateSelectInput(session, paste0(id, "units"), selected = input$u)
    },
    flag_input_changed = function(id, ...) {
      add_value_changed_class_to_input(id, ...)
      add_value_changed_class_to_input(paste0(id, "value"), ...)
      add_value_changed_class_to_input(paste0(id, "units"), ...)
    }
  )
}

# byte input
byte_value_to_input <- function(value, units) {
  if (value > 1024 * 1024) {
    list(v = value / 1024 / 1024, u = "MB")
  } else if (value > 1024) {
    list(v = value / 1024, u = "KB")
  } else {
    list(v = value, u = "byte")
  }
}

byte_input_to_value <- function(input, units) {
  if (input$u == "MB") {
    input$v * 1024 * 1024
  } else if (input$u == "KB") {
    input$v * 1024
  } else {
    input$v
  }
}

byte_value_to_text <- function(value, units) {
  input <- byte_value_to_input(value, units)
  paste(signif(input$v, 3), input$u)
}

value_byte_input <- function(id) {
  input_module_selectable_units(
    id = id,
    value_to_input = byte_value_to_input,
    input_to_value = byte_input_to_value,
    value_to_text = byte_value_to_text,
    units_options = c("byte", "KB", "MB")
  )
}


# TODO: move the resistance functionliaty into microloger only
resistance_value_to_input <- function(value, units) {
  if (value > 1e6) {
    list(v = value / 1e6, u = "MOhm")
  } else if (value > 1e3) {
    list(v = value / 1e3, u = "kOhm")
  } else {
    list(v = value, u = "Ohm")
  }
}

resistance_input_to_value <- function(input, units) {
  if (input$u == "MOhm") {
    input$v * 1e6
  } else if (input$u == "kOhm") {
    input$v * 1e3
  } else {
    input$v
  }
}

resistance_value_to_text <- function(value, units) {
  input <- resistance_value_to_input(value, units)
  paste(input$v, input$u)
}

value_resistance_input <- function(id) {
  input_module_selectable_units(
    id = id,
    value_to_input = resistance_value_to_input,
    input_to_value = resistance_input_to_value,
    value_to_text = resistance_value_to_text,
    units_options = c("Ohm", "kOhm", "MOhm")
  )
}
