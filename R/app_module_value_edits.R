# value editing =========

# make unique input id
uid <- function(gui_id, coreid) sprintf("%s-%s", gui_id, coreid)

# tag input with value change class
add_value_changed_class_to_input <- function(id) {
  # shinyjs::addClass(id = id, class = "value-changed") # does not seem to work for all input types
  format_inline(
    "document.querySelector('#{id}').closest('.form-group').classList.add('value-changed');"
  ) |>
    shinyjs::runjs()
}

# standard input row
generate_standard_input_row <- function(label, widget, units = NA_character_) {
  fluidRow(
    column(width = 4, tags$div(style = "margin-top: 5px;", label)),
    column(width = 4, widget),
    if (!is.na(units) && nzchar(units)) {
      column(width = 4, tags$div(style = "margin-top: 5px;", units))
    }
  )
}

# input module
input_module <- function(
  id,
  value_to_text,
  make_gui = function(...) {},
  fetch_input = function(ui_inputs, id) return(ui_inputs[[id]]),
  input_to_value = function(input) return(input),
  value_to_input = function(value) return(value),
  correct_input = function(input) return(input),
  compare_input = function(input1, input2) identical(input1, input2),
  update_input = function(session, id, input) {},
  flag_input_changed = function(id) add_value_changed_class_to_input(id)
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
      coreids = character(),
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
      rv$coreids <- character()
      rv$original <- list()
      rv$inputs <- list()
      rv$changed <- logical()
    }

    # activate the GUI created by this module
    activate <- function() {
      # register inputs observer
      rv$observer <- observe({
        req(!is_empty(rv$coreids))
        # get inputs
        rv$inputs <-
          uid(isolate(rv$gui_id), rv$coreids) |>
          purrr::map(fetch_input, ui_inputs = input) |>
          set_names(rv$coreids)
      })
    }

    # check if there are any changes t the inputs
    has_changed_inputs <- reactive({
      any(rv$changed)
    })

    # return the input value for the core id
    get_input <- function(coreid) rv$inputs[[coreid]]

    # return the value for the core id (parsing the input)
    get_value <- function(coreid, if_changed = TRUE) {
      # is the value NULL or has not changed?
      if (is.null(get_input(coreid)) || (if_changed && !rv$changed[[coreid]])) {
        return(NULL)
      }
      input_to_value(input = get_input(coreid))
    }

    # return the text for the core id (parsing the value)
    get_text <- function(coreid, units = NA_character_, if_changed = TRUE) {
      value <- get_value(coreid, if_changed)
      if (is.null(value)) {
        return(NA_character_)
      }
      value_to_text(value = value, units = units)
    }

    # copy input from one coreid to all others
    # unless core id is supplied, copies from the first one that has changes
    copy_input <- function(from_coreid = NULL) {
      if (!is.null(from_coreid) || has_changed_inputs()) {
        if (is.null(from_coreid)) {
          from_coreid <- names(rv$changed)[rv$changed][1]
        }
        from_input <- get_input(from_coreid)
        if (!is.null(from_input)) {
          # make the updates
          purrr::walk(
            rv$coreids,
            ~ if (.x != from_coreid) {
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
    update_incorrect_inputs <- function(coreid, input, corrected) {
      if (!identical(input, corrected)) {
        update_input(
          session = session,
          id = uid(rv$gui_id, coreid),
          input = corrected
        )
      }
    }

    # flag only first time
    flag_input_first_time <- function(coreid) {
      if (!rv$changed[[coreid]]) {
        # first time change --> flag
        flag_input_changed(ns(uid(rv$gui_id, coreid)))
        rv$changed[[coreid]] <- TRUE
      }
    }

    # flag inputs if they have changed (based on corrected value)
    flag_changed_inputs <- function(coreid, corrected) {
      if (
        !is.null(corrected) && !compare_input(rv$original[[coreid]], corrected)
      ) {
        flag_input_first_time(coreid)
      }
    }

    # monitor inputs
    observeEvent(rv$inputs, {
      req(!is_empty(rv$inputs))
      current_inputs <- rv$inputs[rv$coreids]

      # run error checking
      corrected_inputs <-
        current_inputs |>
        purrr::map(~ if (is.null(.x)) NULL else correct_input(.x))

      # update inputs if corrections are necessary
      purrr::pwalk(
        list(rv$coreids, current_inputs, corrected_inputs),
        update_incorrect_inputs
      )

      # figure out if value has changed and flag them if so
      purrr::pwalk(
        list(rv$coreids, corrected_inputs),
        flag_changed_inputs
      )
    })

    # generate the edit user interface
    generate_ui <- function(gui_id, coreid, value, ...) {
      # info message
      format_inline("generating edit UI #{gui_id} for {coreid}") |>
        log_debug(ns = ns)

      # store unique gui id if it changed
      if (!identical(rv$gui_id, gui_id)) {
        rv$gui_id <- gui_id
      }

      # keep track of coreids rendered by this module
      if (!coreid %in% rv$coreids) {
        rv$coreids <- c(rv$coreids, coreid)
      }

      # keep track of original value
      rv$original[[coreid]] <- value_to_input(value)
      rv$changed[[coreid]] <- FALSE

      # generate UI
      make_gui(id = ns(uid(rv$gui_id, coreid)), value = value, ...)
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

# text input
value_text_input <- function(id) {
  input_module(
    id = id,
    value_to_text = text_value_to_text,
    make_gui = function(id, label, value, units, ...) {
      widget <- textInput(
        inputId = id,
        label = NULL,
        value = value
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
    make_gui = function(id, label, value, units, ...) {
      widget <- numericInput(
        inputId = id,
        label = NULL,
        value = value,
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
    make_gui = function(id, label, value, units, ...) {
      widget <- numericInput(inputId = id, label = NULL, value = value)
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
    make_gui = function(id, label, value, choices, ...) {
      widget <- selectInput(
        inputId = id,
        label = NULL,
        choices = choices,
        selected = value
      )
      generate_standard_input_row(label, widget)
    },
    update_input = function(session, id, input) {
      updateSelectInput(session, id, selected = input)
    }
  )
}
