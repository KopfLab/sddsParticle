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
  make_gui,
  fetch_input = function(ui_inputs, id) return(ui_inputs[[id]]),
  input_to_value = function(input) return(input),
  value_to_input = function(value) return(value),
  correct_input = function(input) return(input),
  update_input = function(input) {},
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

    # set the input for the core id from a value
    set_value <- function(value) {
      input <- value_to_input(value)
    }

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

    # update inputs if they are incorrect
    update_incorrect_inputs <- function(coreid, input, corrected) {
      if (!identical(input, corrected)) {
        update_input(id = uid(rv$gui_id, coreid), input = corrected)
      }
    }

    # flag inputs if they have changed (based on corrected value)
    flag_changed_inputs <- function(coreid, corrected) {
      is_changed <- rv$changed[[coreid]] ||
        (!is.null(corrected) && !identical(rv$original[[coreid]], corrected))
      if (!rv$changed[[coreid]] && is_changed) {
        # first time change --> flag
        log_debug(ns = ns, coreid, " has changed")
        flag_input_changed(ns(uid(rv$gui_id, coreid)))
      }
      return(is_changed)
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

      # figure out if value has changed
      rv$changed <- purrr::pmap_lgl(
        list(rv$coreids, corrected_inputs),
        flag_changed_inputs
      ) |>
        set_names(rv$coreids)
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
      get_text = get_text
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
    update_input = function(id, input) {
      updateNumericInput(inputId = id, value = input)
    },
  )
}

# # integer input
# value_integer_input <- function(id) {
#   # standard functions
#   fetch_input <- function(ui_inputs, id) return(ui_inputs[[id]])
#   input_to_value <- function(input) return(input)
#   value_to_input <- function(value) return(value)
#   value_to_text <- integer_value_to_text

#   correct_input <- function(input) {
#     if (!is_integerish(input)) {
#       return(as.integer(round(input)))
#     }
#     return(input)
#   }

#   update_input <- function(id, input) {
#     updateNumericInput(inputId = id, value = input)
#   }

#   flag_input_changed <- function(id) {
#     add_value_changed_class_to_input(id)
#   }

#   make_gui <- function(id, label, value, units, ...) {
#     widget <- numericInput(inputId = id, label = NULL, value = value, step = 1)
#     generate_standard_input_row(label, widget, units)
#   }

#   moduleServer(id, function(input, output, session) {
#     # namespace
#     ns <- session$ns

#     # keep track of reactive values
#     rv <- reactiveValues(
#       gui_id = 0L,
#       observer = NULL,
#       active = FALSE,
#       coreids = character(),
#       original = list(),
#       inputs = list(),
#       changed = logical()
#     )

#     # reset the GUI created by this module
#     reset <- function() {
#       if (!is.null(isolate(rv$observer))) {
#         isolate(rv$observer)$destroy()
#       }
#       rv$observer <- NULL
#       rv$active <- FALSE
#       rv$coreids <- character()
#       rv$original <- list()
#       rv$inputs <- list()
#       rv$changed <- logical()
#     }

#     # activate the GUI created by this module
#     activate <- function() {
#       # register inputs observer
#       rv$observer <- observe({
#         req(!is_empty(rv$coreids))
#         # get inputs
#         rv$inputs <-
#           uid(isolate(rv$gui_id), rv$coreids) |>
#           purrr::map(fetch_input, ui_inputs = input) |>
#           set_names(rv$coreids)
#       })
#     }

#     # set the input for the core id from a value
#     set_value <- function(value) {
#       input <- value_to_input(value)
#     }

#     # return the input value for the core id
#     get_input <- function(coreid) rv$inputs[[coreid]]

#     # return the value for the core id (parsing the input)
#     get_value <- function(coreid, if_changed = TRUE) {
#       # is the value NULL or has not changed?
#       if (is.null(get_input(coreid)) || (if_changed && !rv$changed[[coreid]])) {
#         return(NULL)
#       }
#       input_to_value(input = get_input(coreid))
#     }

#     # return the text for the core id (parsing the value)
#     get_text <- function(coreid, units = NA_character_, if_changed = TRUE) {
#       value <- get_value(coreid, if_changed)
#       if (is.null(value)) {
#         return(NA_character_)
#       }
#       value_to_text(value = value, units = units)
#     }

#     # error check the inputs
#     # error_check_value <- function(core_id, value) {
#     #   if (!is.null(value) && !is.na(value) && value != as.integer(value)) {
#     #     corrected <- as.integer(round(value))
#     #     updateNumericInput(
#     #       session,
#     #       uid(isolate(rv$gui_id), core_id),
#     #       value = corrected
#     #     )
#     #     return(corrected)
#     #   }
#     #   return(value)
#     # }

#     # update inputs if they are incorrect
#     update_incorrect_inputs <- function(coreid, input, corrected) {
#       if (!identical(input, corrected)) {
#         update_input(id = uid(rv$gui_id, coreid), input = corrected)
#       }
#     }

#     # flag inputs if they have changed (based on corrected value)
#     flag_changed_inputs <- function(coreid, corrected) {
#       is_changed <- rv$changed[[coreid]] ||
#         (!is.null(corrected) && !identical(rv$original[[coreid]], corrected))
#       if (!rv$changed[[coreid]] && is_changed) {
#         # first time change --> flag
#         flag_input_changed(uid(rv$gui_id, coreid))
#       }
#       return(is_changed)
#     }

#     # monitor inputs
#     observeEvent(rv$inputs, {
#       req(!is_empty(rv$inputs))
#       current_inputs <- rv$inputs[rv$coreids]

#       # run error checking
#       corrected_inputs <-
#         current_inputs |>
#         purrr::map(~ if (is.null(.x)) NULL else correct_input(.x))

#       # update inputs if corrections are necessary
#       purrr::pwalk(
#         list(rv$coreids, current_inputs, corrected_inputs),
#         update_incorrect_inputs
#       )

#       # figure out if value has changed
#       rv$changed <- purrr::pmap_lgl(
#         list(rv$coreids, corrected_inputs),
#         flag_changed_inputs
#       ) |>
#         set_names(rv$coreids)
#     })

#     # generate the edit user interface
#     generate_ui <- function(gui_id, coreid, value, ...) {
#       # info message
#       format_inline("generating edit UI #{gui_id} for {coreid}") |>
#         log_debug(ns = ns)

#       # store unique gui id if it changed
#       if (!identical(rv$gui_id, gui_id)) {
#         rv$gui_id <- gui_id
#       }

#       # keep track of coreids rendered by this module
#       if (!coreid %in% rv$coreids) {
#         rv$coreids <- c(rv$coreids, coreid)
#       }

#       # keep track of original value
#       rv$original[[coreid]] <- value_to_input(value)
#       rv$changed[[coreid]] <- FALSE

#       # generate UI
#       make_gui(id = ns(uid(rv$gui_id, coreid)), value = value, ...)
#     }

#     # standard return value with the functions
#     list(
#       reset = reset,
#       activate = activate,
#       has_changes = reactive({
#         any(rv$changed)
#       }),
#       generate_ui = generate_ui,
#       value_to_text = value_to_text,
#       get_value = get_value,
#       get_text = get_text
#     )
#   })
# }

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
    update_input = function(id, input) {
      print("UPDATE") # DEBUG
      updateSelectInput(inputId = id, selected = input)
    },
  )
}

# # enum input
# value_enum_input <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     # required function definitions
#     value_to_text <- enum_value_to_text
#     get_value <- function(coreid) input[[coreid]]
#     get_text <- function(coreid, ...) get_value(coreid)
#     generate_ui <- function(gui_id, coreid, label, value, choices, ...) {
#       widget <- selectInput(
#         ns(coreid),
#         label = NULL,
#         choices = choices,
#         selected = value
#       )
#       generate_standard_input_row(label, widget)
#     }
#     # standard return value with the functions
#     list(
#       reset = function() {},
#       activate = function() {},
#       has_changes = reactive({
#         FALSE
#       }),
#       generate_ui = generate_ui,
#       value_to_text = value_to_text,
#       get_value = get_value,
#       get_text = get_text
#     )
#   })
# }
