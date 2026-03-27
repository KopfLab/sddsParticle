# value editing =========

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

value_null_input <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      generate_ui = function(label, ...) {
        generate_standard_input_row(label, null_value_to_text())
      },
      value_to_text = null_value_to_text,
      get_value = function(...) NULL,
      get_text = null_value_to_text
    )
  })
}

# integer input
value_integer_input <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # required function definitions
    value_to_text <- integer_value_to_text
    get_value <- function(coreid) input[[coreid]]
    get_text <- function(coreid, units = NA_character_) {
      get_value(coreid) |> value_to_text(units)
    }
    generate_ui <- function(coreid, label, value, units, ...) {
      # TODO: consider integer validation here
      widget <- numericInput(ns(coreid), label = NULL, value = value, step = 1)
      generate_standard_input_row(label, widget, units)
    }
    # standard return value with the functions
    list(
      generate_ui = generate_ui,
      value_to_text = value_to_text,
      get_value = get_value,
      get_text = get_text
    )
  })
}

# # FIXME: use this for validation - not needed for queuing values
# # --> that only happens when queueValue is pressed
# # observe value changes
# active_observers <- list()
# observe({
#   req(nrow(values$edit_structure) > 0)
#   for (coreid in values$edit_structure$coreid) {
#     local({
#       local_id <- coreid
#       # only create observer if it doesn't already exist
#       if (!local_id %in% names(active_observers)) {
#         active_observers[[local_id]] <<- observeEvent(
#           input[[local_id]],
#           {
#             log_debug(local_id, " changed to: ", input[[local_id]])
#           }
#         )
#       }
#     })
#   }

#   # destroy observers for removed devices
#   removed_ids <- names(active_observers) |>
#     setdiff(values$edit_structure$coreid)
#   for (id in removed_ids) {
#     active_observers[[id]]$destroy()
#     active_observers[[id]] <<- NULL
#   }
# })

# enum input
value_enum_input <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # required function definitions
    value_to_text <- enum_value_to_text
    get_value <- function(coreid) input[[coreid]]
    get_text <- function(coreid, ...) get_value(coreid)
    generate_ui <- function(coreid, label, value, choices, ...) {
      widget <- selectInput(
        ns(coreid),
        label = NULL,
        choices = choices,
        selected = value
      )
      generate_standard_input_row(label, widget)
    }
    # standard return value with the functions
    list(
      generate_ui = generate_ui,
      value_to_text = value_to_text,
      get_value = get_value,
      get_text = get_text
    )
  })
}
