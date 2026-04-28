#' sdds GUI module
#'
#' @description SDDS shiny Module.
#'
#' @param id module id
#' @describeIn sdds_module generates the ui for the sdds devices table
#' @export
sdds_ui_devices_table <- function(id) {
  ns <- NS(id)
  module_selector_table_ui(ns("devices"))
}

#' @describeIn sdds_module generates the ui for the sdds devices action links
#' @export
sdds_ui_devices_actions <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("refresh_devices"),
      "Refresh",
      icon = icon("arrows-rotate"),
    ) |>
      add_tooltip("Refresh device list."),
    module_selector_table_select_all_button(
      ns("devices"),
      border = FALSE
    ),
    module_selector_table_deselect_all_button(
      ns("devices"),
      border = FALSE
    )
  )
}

#' @describeIn sdds_module generates the ui for the sdds structures div (include in your UI layout to show/hide when devices are selected)
#' @export
sdds_ui_structures_div <- function(id, ...) {
  ns <- NS(id)
  div(id = ns("structures_div"), ...) |> shinyjs::hidden()
}

#' @describeIn sdds_module generates the ui for the sdds structures table
#' @export
sdds_ui_structures_table <- function(id) {
  ns <- NS(id)
  module_selector_table_ui(ns("structures"))
}

#' @describeIn sdds_module generates the request data button
#' @export
sdds_ui_structures_fetch_data <- function(id, ...) {
  ns <- NS(id)
  actionButton(
    ns("fetch_values"),
    "Request latest data",
    icon = icon("cloud-arrow-down"),
    ...
  ) |>
    add_tooltip(
      "Request latest structure and values from devices."
    ) |>
    shinyjs::disabled()
}


#' @describeIn sdds_module generates the ui for the sdds structures action links
#' @export
sdds_ui_structures_actions <- function(id, space = 1) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("send_commands"),
      "Send queue",
      icon = icon("paper-plane")
    ) |>
      add_tooltip(
        "Send commands to make the changes."
      ) |>
      shinyjs::disabled(),
    spaces(space),
    actionButton(
      ns("show_hide_publishing"),
      textOutput(ns("publishing_label"), inline = TRUE),
      icon = icon("upload")
    ) |>
      add_tooltip(
        "Show/hide the publishing settings for each variable."
      ) |>
      shinyjs::disabled(),
    spaces(space),
    actionButton(
      ns("show_hide_system"),
      textOutput(ns("system_label"), inline = TRUE),
      icon = icon("house")
    ) |>
      add_tooltip(
        "Show/hide the HARDWARE menu items."
      ) |>
      shinyjs::disabled(),
    spaces(space),
    actionButton(
      ns("show_hide_hardware"),
      textOutput(ns("hardware_label"), inline = TRUE),
      icon = icon("microchip"),
      style = "border: 0;"
    ) |>
      add_tooltip(
        "Show/hide the HARDWARE menu items."
      ) |>
      shinyjs::disabled(),
    spaces(space),
    actionButton(
      ns("command_logs"),
      "Fetch cmd logs",
      icon = icon("list-check")
    ) |>
      add_tooltip(
        "Show latest commands sent to devices."
      ) |>
      shinyjs::disabled(),
    spaces(space),
    actionButton(
      ns("events_stream"),
      "Show events",
      icon = icon("timeline")
    ) |>
      add_tooltip(
        "Show events sent by the selected devices."
      ) |>
      shinyjs::disabled()
  )
}

#' @describeIn sdds_module call in ui head to include
#' @export
sdds_header <- function() {
  tagList(
    shinyjs::useShinyjs(), # enable shinyjs
    use_app_utils(), # enable app utils
    tags$style(HTML(
      "
      .value-changed input,
      .value-changed select,
      .value-changed .selectize-input {
        background-color: #d4edda !important;
        border-color: #28a745 !important;
        box-shadow: 0 0 0 0.2rem rgba(40, 167, 69, 0.25);
      }
      "
    ))
  )
}

#' @param accessible_core_ids the particle devices that are allowed to be controlled
#' @describeIn sdds_module generates the server for the sdds module
#' @export
sdds_server <- function(
  id,
  token,
  timezone = Sys.timezone(),
  accessible_core_ids = NULL
) {
  # make timezone into a function if it's not
  if (!is.function(timezone)) {
    get_timezone <- reactive({
      timezone
    })
  } else {
    get_timezone <- timezone
  }

  # make accessible core ids into a function if it's not
  if (!is.function(accessible_core_ids)) {
    get_accessible_core_ids <- reactive({
      accessible_core_ids
    })
  } else {
    get_accessible_core_ids <- accessible_core_ids
  }

  # actual module server
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive values =======
    values <- reactiveValues(
      refresh_devices = 0,
      show_publishing = FALSE,
      show_system = FALSE,
      show_hardware = FALSE,
      edit_structure = tibble(),
      edit_changed = FALSE,
      edit_publishing_structure = tibble(),
      command_queue = tibble(),
      selected_core_ids = c()
    )

    # devices =============

    ## delete devmode cache
    if (in_devmode()) {
      observeEvent(
        values$refresh_devices,
        {
          if (file.exists("cache/sdds_devices.csv")) {
            unlink("cache/sdds_devices.csv")
          }
        },
        priority = 100
      )
    }

    ## refresh devices
    refresh_devices <- function() {
      values$refresh_devices <- values$refresh_devices + 1L
    }
    observeEvent(input$refresh_devices, refresh_devices())

    ## get all devices
    get_all_devices <- reactive({
      values$refresh_devices
      log_info(ns = ns, user_msg = "Fetching latest device statuses")
      # safely call function
      out <- get_devices_in_app(token = token) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## get the listed devices
    get_devices <- reactive({
      validate(need(get_all_devices(), "No devices available."))
      out <- get_all_devices() |>
        get_filtered_devices_in_app(core_ids = get_accessible_core_ids()) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## get devices for table
    get_devices_for_table <- reactive({
      # safety checks
      validate(need(get_devices(), "No devices."))
      # safely call function
      out <- get_devices() |>
        get_devices_for_table_in_app(timezone = get_timezone()) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## setup devices selector table
    devices <-
      module_selector_table_server(
        "devices",
        get_data = get_devices_for_table,
        id_column = "coreid",
        # make id column invisible
        columnDefs = list(
          list(visible = FALSE, targets = 0)
        ),
        # view all
        paging = FALSE,
        dom = "ft",
        no_data_message = "There are none."
      )

    # structures =======

    ## reactive structures poll
    get_structures_cache <- reactivePoll(
      # check every 1s (adjust as needed)
      intervalMillis = 1000,
      session = session,

      # return value indicating changes
      checkFunc = function() {
        # safely call function
        out <-
          get_cached_structures_in_app(
            core_ids = devices$get_selected_ids()
          ) |>
          try_catch_cnds()
        # don't show because this runs regularly
        return(digest::digest(out))
      },

      # get stream events
      valueFunc = function() {
        # safely call function
        out <-
          get_cached_structures_in_app(
            # have to isolate here to avoid double trigger
            core_ids = isolate(devices$get_selected_ids())
          ) |>
          try_catch_cnds()
        # don't show, return
        return(out)
      }
    )

    ## get structures
    get_structures <- reactive({
      # get structures
      req(devices$table_exists())
      out <- get_structures_cache()

      # log cnds here instead of in the poll
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return(NULL)
      }

      # got new structures
      log_info(ns = ns, user_msg = "(Re)loading control structures")

      # parse the structures
      out <- out$result |> sdds_parse_trees_and_values() |> try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return(NULL)
      }

      # are there any missing structures?
      structs <- out$result
      out <- structs |>
        get_missing_trees_in_app() |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result) && nrow(out$result) > 0) {
        msg <- format_inline(
          "Fetching structure{?s} for {out$result$type_version}"
        )
        log_info(ns = ns, user_msg = msg)

        # safely request trees
        out <-
          out$result |>
          request_sdds_trees_in_app(devices = get_devices(), token = token) |>
          try_catch_cnds()
        out |> log_cnds(ns = ns)

        if (!is.null(out$result) && any(!out$result$success)) {
          missing <- out$result |> filter(!success)
          msg <- format_inline(
            "Failed to request structural information about {missing$type_version} from {missing$name}"
          )
          log_error(ns = ns, user_msg = msg)
        }
      }

      # additional prep
      out <- structs |>
        get_structures_in_app(
          devices = get_devices(),
          timezone = get_timezone(),
          # TODO: these should be coming from the additional value modules/types/converters
          additional_types = list(
            "resistance" = expr(.data$base_units == "Ohm")
          ),
          additional_converters = list(
            "resistance" = edit_modules$resistance$value_to_text
          )
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result) || nrow(out$result) == 0) {
        return(NULL)
      }
      return(out$result)
    })

    ## structures for table
    get_structures_for_table <- reactive({
      # safety checks
      validate(need(devices$table_exists(), "No devices available."))
      validate(need(
        devices$has_data(),
        "No devices available."
      ))
      validate(need(
        get_structures(),
        if (is_empty(isolate(devices$get_selected_ids()))) {
          "Select a device."
        } else {
          "No structures available yet."
        }
      ))

      structures$reset_visible_columns()
      # safely call function
      out <- get_structures() |>
        get_structures_for_table_in_app(
          show_publishing = values$show_publishing,
          show_system = values$show_system,
          show_hardware = values$show_hardware
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## button availability
    observeEvent(devices$get_selected_ids(), {
      device_selected <- !is_empty(devices$get_selected_ids())
      shinyjs::toggleState("fetch_values", condition = device_selected)
      shinyjs::toggleState("command_logs", condition = device_selected)
      shinyjs::toggleState("events_stream", condition = device_selected)
      shinyjs::toggleState("show_hide_publishing", condition = device_selected)
      shinyjs::toggleState("show_hide_system", condition = device_selected)
      shinyjs::toggleState("show_hide_hardware", condition = device_selected)
    })

    ## fetch new values
    observeEvent(input$fetch_values, {
      req(get_devices())
      req(devices$get_selected_ids())
      log_info(ns = ns, user_msg = "Fetching values")

      # safely request device info
      out <- request_sdds_values_in_app(
        devices = get_devices(),
        core_ids = devices$get_selected_ids(),
        token = token
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result) && any(!out$result$success)) {
        msg <- format_inline(
          "Failed to request values for {out$result$name[!out$result$success]}"
        )
        log_error(ns = ns, user_msg = msg)
      }
    })

    ## selected devices
    observeEvent(
      devices$get_selected_ids(),
      {
        devices$get_selected_items()
        new_ids <- setdiff(devices$get_selected_ids(), values$selected_core_ids)
        disconnected_cores <- get_devices() |>
          filter(.data$coreid %in% new_ids, !.data$connected) |>
          pull(.data$name)
        if (length(disconnected_cores) > 0) {
          msg <- format_inline(
            "Device{?s} {disconnected_cores} {?is/are} OFFLINE and cannot be controlled remotely."
          )
          log_warning(ns = ns, user_msg = msg)
        }
        values$selected_core_ids <- devices$get_selected_ids()
      },
      ignoreNULL = FALSE
    )

    ## hide/show structures if there are selections
    observe(
      {
        req(devices$table_exists())
        shinyjs::toggle(
          "structures_div",
          condition = devices$has_data() &&
            !is_empty(devices$get_selected_ids())
        )
      }
    )

    ## show/hide buttons
    observeEvent(input$show_hide_publishing, {
      values$show_publishing <- !values$show_publishing
    })
    output$publishing_label <- renderText({
      if (!values$show_publishing) {
        "Show publishing"
      } else {
        "Hide publishing"
      }
    })
    observeEvent(input$show_hide_system, {
      values$show_system <- !values$show_system
    })
    output$system_label <- renderText({
      if (!values$show_system) {
        "Show SYSTEM"
      } else {
        "Hide SYSTEM"
      }
    })
    observeEvent(input$show_hide_hardware, {
      values$show_hardware <- !values$show_hardware
    })
    output$hardware_label <- renderText({
      if (!values$show_hardware) {
        "Show HARDWARE"
      } else {
        "Hide HARDWARE"
      }
    })

    ## setup structures selector table
    structures <-
      module_selector_table_server(
        "structures",
        get_data = get_structures_for_table,
        id_column = "path",
        escape_headers = FALSE, # to render HTML
        # row grouping
        extensions = "RowGroup",
        rowGroup = list(dataSrc = 1),
        # make path columns invisible
        columnDefs = list(
          list(visible = FALSE, targets = 0:1)
        ),
        # view all & scrolling
        paging = FALSE,
        # allow_view_all = TRUE,
        # initial_page_length = -1,
        ordering = FALSE,
        dom = "ft",
        # scrollX = TRUE,
        # scrollY = "max(200px, calc(100vh - 620px))", # account for size of header and devices table with the -x px
        selection = "single",
        auto_reselect = FALSE
      )

    # edit values ===========

    # editing modules
    edit_modules <- list(
      "null" = value_null_input("null"),
      "read_only" = value_read_only_input("read_only"),
      "integer" = value_integer_input("integer"),
      "double" = value_double_input("double"),
      "enum" = value_enum_input("enum"),
      "text" = value_text_input("text"),
      "duration" = value_duration_input("duration"),
      "hhmm" = value_hhmm_input("hhmm", get_timezone = get_timezone),
      "byte" = value_byte_input("byte"),
      "var_interval" = value_var_intervals_input("var_interval"),
      # TODO: move to micrologger
      "resistance" = value_resistance_input("resistance")
    )

    # editing modal dialog
    edit_modal_values_widgets <- reactiveVal()
    edit_modal_publishing_widgets <- reactiveVal()
    edit_modal <-
      modalDialog(
        title = h3(textOutput(ns("edit_path"))),
        tabsetPanel(
          id = ns("edit_tabs"),
          tabPanel(
            title = "Values",
            value = "values",
            br(),
            uiOutput(ns("edit_widgets"))
          ),
          tabPanel(
            title = "Publishing",
            value = "publishing",
            br(),
            uiOutput(ns("edit_publishing_widgets"))
          )
        ),
        footer = tagList(
          actionButton(
            ns("copy_to_all"),
            "Copy to all",
            icon = icon("copy"),
            style = "border: 0"
          ) |>
            add_tooltip(
              "Copy the topmost changed value (highlighted in green) to all inputs."
            ) |>
            shinyjs::disabled(),
          spaces(1),
          actionButton(
            ns("send_now"),
            "Send now",
            icon = icon("paper-plane"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Send the commands for the values highlighted in green right now."
            ) |>
            shinyjs::disabled(),
          spaces(1),
          actionButton(
            ns("add_to_queue"),
            "Add to queue",
            icon = icon("layer-group"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Don't send now but add the commands for the values highlighted in green to the command queue (click 'Send queue' to send to devices)."
            ) |>
            shinyjs::disabled(),
          modalButton("Cancel")
        ),
        size = "m",
        easyClose = TRUE
      )
    output$edit_path <- renderText({
      req(structures$get_selected_ids())
      structures$get_selected_ids()
    })
    output$edit_widgets <- renderUI({
      req(edit_modal_values_widgets())
      edit_modal_values_widgets()
    })
    output$edit_publishing_widgets <- renderUI({
      req(edit_modal_publishing_widgets())
      edit_modal_publishing_widgets()
    })

    # edit structure function
    edit_structure <- function(
      path,
      value = NULL,
      flag_as_changed = !is.null(value)
    ) {
      # safely get structure
      out <- get_structures() |>
        get_structures_for_path_in_app(path = path) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      structure <- out$result
      if (is.null(structure)) {
        return()
      }

      # overwrite existing value?
      if (!is.null(value)) {
        new_value <- value
        structure <- structure |>
          mutate(
            value = .data$value |>
              purrr::map(
                ~ {
                  new_value
                }
              )
          )
      }

      # safely get publishing structure
      out <- get_structures() |>
        get_structures_for_path_in_app(
          path = paste0("SYSTEM.publishing.varIntervals_ms.", path)
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      publishing_structure <- out$result
      if (is_empty(publishing_structure) || nrow(publishing_structure) == 0) {
        publishing_structure <- NULL
      }

      # keep track of edit structure
      values$edit_structure <- structure
      values$edit_changed <- flag_as_changed
      values$edit_publishing_structure <- publishing_structure

      # safely generate edit ui fields
      modal_session_id(modal_session_id() + 1)
      out <-
        prepare_edit_ui_in_app(
          structures = list(
            "values" = structure,
            "publishing" = publishing_structure
          ),
          gui_id = modal_session_id(),
          edit_modules = edit_modules,
          changed = c(flag_as_changed, FALSE)
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return()
      }

      # assign widgets
      edit_modal_values_widgets(out$result$values)
      edit_modal_publishing_widgets(out$result$publishing)
      edit_modal |> showModal()
    }

    # trigger generation of the edit widgets (requires unique ids each time it's recreate to manage observers)
    modal_session_id <- reactiveVal(0)
    observeEvent(
      structures$get_selected_ids(),
      {
        req(structures$get_selected_ids())
        edit_structure(path = structures$get_selected_ids())
      }
    )

    # check if there are any changes
    observe({
      req(edit_modal_values_widgets())
      has_changes <- purrr::map_lgl(edit_modules, ~ .x$has_changes())
      if (isolate(values$edit_changed)) {
        shinyjs::toggleState("copy_to_all", condition = any(has_changes)) |>
          shinyjs::delay(ms = 1000)
        shinyjs::toggleState("send_now", condition = any(has_changes)) |>
          shinyjs::delay(ms = 1000)
        shinyjs::toggleState("add_to_queue", condition = any(has_changes)) |>
          shinyjs::delay(ms = 1000)
      } else {
        shinyjs::toggleState("copy_to_all", condition = any(has_changes))
        shinyjs::toggleState("send_now", condition = any(has_changes))
        shinyjs::toggleState("add_to_queue", condition = any(has_changes))
      }
    })

    # whether to show the copy to all button
    observeEvent(values$edit_structure, {
      shinyjs::toggle(
        "copy_to_all",
        condition = nrow(values$edit_structure) > 1
      )
    })

    # copy to all event
    observeEvent(input$copy_to_all, {
      req(edit_modal_values_widgets())
      purrr::walk(edit_modules, ~ .x$copy_input())
    })

    # add to command queue, returns the row_ids of the new commands
    add_to_queue <- function() {
      # safely prepare new values
      out_values <-
        values$edit_structure |>
        prepare_new_values_in_app(
          prefix = "values",
          edit_modules = edit_modules
        ) |>
        try_catch_cnds()

      # savely prepare new publish intervals
      if (!is.null(values$edit_publishing_structure)) {
        out_publishing <-
          values$edit_publishing_structure |>
          prepare_new_values_in_app(
            prefix = "publishing",
            edit_modules = edit_modules
          ) |>
          try_catch_cnds()
      }

      # close model dialog
      removeModal()

      # check for issues
      out_values |> log_cnds(ns = ns)
      new_values <- out_values$result
      if (!is.null(values$edit_publishing_structure)) {
        out_publishing |> log_cnds(ns = ns)
        new_values <- new_values |> bind_rows(out_publishing$result)
      }
      if (is.null(new_values) || nrow(new_values) == 0) {
        return()
      }

      # update values
      old_cmds <- c()
      if (nrow(values$command_queue) > 0) {
        old_cmds <- values$command_queue$row_id
      }
      out <-
        update_command_queue_entries_in_app(
          existing_queue = isolate(values$command_queue),
          tree_w_new_values = new_values
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)

      # did it fail?
      if (is.null(out$result)) {
        return(c())
      }

      # unselect row
      structures$select_rows(c())

      # success
      values$command_queue <- out$result

      # return new row ids
      return(setdiff(values$command_queue$row_id, old_cmds))
    }

    # send right away
    observeEvent(input$send_now, {
      req(nrow(values$edit_structure) > 0)
      new_cmds <- add_to_queue()
      send_queue_commands(new_cmds)
    })

    # don't send but add to queue
    observeEvent(input$add_to_queue, {
      req(nrow(values$edit_structure) > 0)
      add_to_queue()
    })

    # send commands queue =========

    ## get data
    get_command_queue_for_table <- reactive({
      req(nrow(values$command_queue) > 0)
      input$send_commands # trigger every time we click
      values$command_queue |>
        mutate(
          status = if_else(is.na(.data$status), "not sent yet", .data$status)
        ) |>
        select(
          "row_id",
          "coreid",
          "Device" = "corename",
          "Attribute" = "path",
          "Value" = "new_text",
          "Command" = "command",
          "Status" = "status"
        ) |>
        arrange(desc(row_id))
    })

    ## disable/enable send button in structures
    observeEvent(
      values$command_queue,
      {
        shinyjs::toggleState(
          "send_commands",
          condition = !is_empty(values$command_queue)
        )
      },
      ignoreNULL = FALSE
    )

    ## trigger modal dialog
    observeEvent(input$send_commands, {
      req(nrow(values$command_queue) > 0)
      showModal(queue_modal)
    })

    ## triger selection after loading
    observeEvent(queue$is_table_reloaded(), {
      req(nrow(values$command_queue) > 0)
      ids <- values$command_queue |>
        filter(is.na(.data$status)) |>
        pull(row_id)
      if (!is_empty(ids)) {
        queue$select_rows(ids = ids)
      }
    })

    ## command queue modal
    queue_modal <- modalDialog(
      title = h3("Commands ready to send to devices"),
      module_selector_table_ui(ns("queue")),
      footer = tagList(
        actionButton(
          ns("send_queue_now"),
          "Send selected",
          icon = icon("paper-plane"),
          style = "border: 0;"
        ) |>
          add_tooltip(
            "Send the selected commands to the devices."
          ) |>
          shinyjs::disabled(),
        spaces(1),
        actionButton(
          ns("clear_queue"),
          "Clear all",
          icon = icon("xmark"),
          style = "border: 0;"
        ) |>
          add_tooltip(
            "Clear the commands table and close the dialog."
          ),
        modalButton("Close")
      ),
      easyClose = TRUE,
      size = "l"
    )
    observeEvent(
      queue$get_selected_ids(),
      {
        shinyjs::toggleState(
          "send_queue_now",
          condition = !is_empty(queue$get_selected_ids())
        )
        updateActionButton(
          inputId = "send_queue_now",
          label = sprintf(
            "Send %d selected",
            length(queue$get_selected_ids())
          )
        )
      },
      ignoreNULL = FALSE
    )
    observeEvent(input$clear_queue, {
      values$command_queue <- tibble()
      removeModal()
    })

    ## commands queue table
    queue <-
      module_selector_table_server(
        "queue",
        get_data = get_command_queue_for_table,
        id_column = "row_id",
        # make row_id and coreid columns invisible
        columnDefs = list(list(visible = FALSE, targets = 0:1)),
        # view all & scrolling
        allow_view_all = TRUE,
        initial_page_length = -1,
        dom = "ft",
        scrollX = TRUE,
        scrollY = "400px",
        selection = "multiple",
        auto_reselect = FALSE,
        ordering = FALSE
      )

    ## function to send the commands to one core
    send_commands <- function(coreid, corename, cmds) {
      msg <- format_inline(
        "Sending {length(cmds)} command{?s} to {corename[1]}"
      )
      log_info(ns = ns, user_msg = msg)
      # safely send command
      out <- coreid[1] |>
        particle_send_sdds_commands(cmds = cmds, token = token) |>
        try_catch_cnds()
      if (nrow(out$conditions) > 0) {
        msg <- format_inline("could not send commands to {corename[1]}")
        log_error(ns = ns, user_msg = msg)
        out |> log_cnds(ns = ns)
        return(rep(FALSE, length(cmds)))
      }
      # info about successful commands
      if (all(out$result$success)) {
        msg <- format_inline(
          "{qty(length(cmds))}The command{?s} {?was/were} successfully received by {corename[1]}"
        )
        log_success(ns = ns, user_msg = msg)
      } else {
        msg <- format_inline(
          "{sum(!out$result$success)}/{length(cmds)} commands were NOT successfully received by {corename[1]}"
        )
        log_warning(ns = ns, user_msg = msg)
      }
      return(out$result$success)
    }

    ## send commands
    send_queue_commands <- function(row_ids) {
      # fetch commands to send
      cmds_to_send <- values$command_queue |>
        filter(.data$row_id %in% !!row_ids)

      # send commands (done safely for each to catch individual errors)
      cmds_results <- cmds_to_send |>
        mutate(
          .by = "coreid",
          success = send_commands(coreid[1], corename[1], command)
        )

      # safely update command queue status
      out <- update_command_queue_status_in_app(
        existing_queue = values$command_queue,
        cmds_results = cmds_results
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      if (!is.null(out$result)) {
        values$command_queue <- out$result
      }
    }

    ## send the commands from the queue
    observeEvent(input$send_queue_now, {
      req(nrow(values$command_queue) > 0)
      req(queue$get_selected_ids())

      # send selected commands from the queue
      queue$get_selected_ids() |> send_queue_commands()

      # remove queue selection
      queue$select_rows(c())
    })

    # events stream ============

    ## reactive stream events poll
    get_stream_events_log <- reactivePoll(
      # check every 1s (adjust as needed)
      intervalMillis = 1000,
      session = session,

      # return value indicating changes
      checkFunc = function() {
        # safely call function
        out <-
          get_stream_events_log_in_app(
            core_ids = devices$get_selected_ids()
          ) |>
          try_catch_cnds()
        # don't show because this runs regularly
        return(digest::digest(out))
      },

      # get stream events
      valueFunc = function() {
        # safely call function
        out <-
          get_stream_events_log_in_app(
            # have to isolate here to avoid double trigger
            core_ids = isolate(devices$get_selected_ids())
          ) |>
          try_catch_cnds()
        # don't show, return
        return(out)
      }
    )

    ## actual stream events function (to safe query)
    get_stream_events <- reactive({
      req(input$events_stream)

      # get log (log cnds here instead of in the poll)
      out <- get_stream_events_log()
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return(NULL)
      }

      # additional prep
      out <- out$result |>
        get_stream_events_in_app(
          devices = get_devices(),
          timezone = get_timezone(),
          prepare_for_table = TRUE
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)

      return(out$result)
    })

    ## events stream modal
    events_modal <- modalDialog(
      title = h3("Latest data sent by the selected devices"),
      module_selector_table_ui(ns("events")),
      div(
        id = ns("data_json_div"),
        h3("Event Data"),
        shinyAce::aceEditor(
          ns("data_json"),
          "",
          mode = "json",
          theme = "cobalt",
          readOnly = TRUE,
          height = "300px"
        )
      ) |>
        shinyjs::hidden(),
      footer = tagList(
        actionButton(
          ns("copy_event_data"),
          "Copy",
          icon = icon("copy"),
          style = "border: 0;"
        ) |>
          add_tooltip(
            "Copy event data to clipboard."
          ) |>
          shinyjs::hidden(),
        actionButton(
          ns("clear_events"),
          "Clear all",
          icon = icon("xmark"),
          style = "border: 0;"
        ) |>
          add_tooltip(
            "Clear the events stream logs."
          ),
        modalButton("Close")
      ),
      easyClose = TRUE
    )

    # show events modal
    observeEvent(input$events_stream, {
      log_info(ns = ns, user_msg = "Fetching events stream")
      if (is_empty(get_stream_events())) {
        log_warning(ns = ns, user_msg = "No events logged yet")
      } else {
        showModal(events_modal)
      }
    })

    # clear the events stream logs
    observeEvent(input$clear_events, {
      ps_clear_logs()
      removeModal()
    })

    # show/hide json area and code button
    observeEvent(
      events$get_selected_ids(),
      {
        if (!is_empty(events$get_selected_ids())) {
          # update editor value safely
          out <-
            shinyAce::updateAceEditor(
              session,
              "data_json",
              events$get_selected_items() |>
                get_pretty_json_event_data_for_app()
            ) |>
            try_catch_cnds()
          if (nrow(out$conditions) > 0) {
            shinyAce::updateAceEditor(
              session,
              "data_json",
              ""
            )
          }
          out |> log_cnds(ns = ns)
        }
        shinyjs::toggleElement(
          "data_json_div",
          condition = !is_empty(events$get_selected_ids())
        )
        shinyjs::toggleElement(
          "copy_event_data",
          condition = !is_empty(events$get_selected_ids())
        )
      },
      ignoreNULL = FALSE
    )
    # copy button event: copy data to clipboard
    observeEvent(input$copy_event_data, {
      req(events$get_selected_items())
      shinyjs::runjs(sprintf(
        "navigator.clipboard.writeText('%s');",
        events$get_selected_items()$data
      ))
      log_info(ns = ns, user_msg = "Data copied to clipboard.")
    })

    ## events stream table
    events <-
      module_selector_table_server(
        "events",
        get_data = get_stream_events,
        id_column = "row_id",
        # make row_id and full data column invisible
        columnDefs = list(list(visible = FALSE, targets = 0:1)),
        # view all & scrolling
        page_lengths = list(
          c(50, 100, -1),
          c("50", "100", "All")
        ),
        scrollX = TRUE,
        scrollY = "200px",
        selection = "single"
      )

    # command logs ==============

    ## function to get the command logs
    get_command_logs_for_table <- reactive({
      req(input$command_logs)

      # the function call returns aggregated out object
      out <- particle_get_and_parse_sdds_command_logs(
        core_ids = isolate(devices$get_selected_ids()),
        timezone = isolate(get_timezone()),
        token = token
      )
      out |> log_cnds(ns = ns)
      if (is.null(out$result)) {
        return(NULL)
      }

      # prepare for table
      out <- out$result |>
        prepare_command_logs_for_table_in_app(
          devices = isolate(get_devices())
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## logs modal
    logs_modal <- modalDialog(
      title = h3("Latest commands received by the selected devices"),
      module_selector_table_ui(ns("logs")),
      footer = tagList(
        modalButton("Close")
      ),
      easyClose = TRUE
    )
    observeEvent(input$command_logs, {
      log_info(ns = ns, user_msg = "Fetching command logs")
      req(get_command_logs_for_table())
      showModal(logs_modal)
    })

    ## logs table
    log <-
      module_selector_table_server(
        "logs",
        get_data = get_command_logs_for_table,
        id_column = "row_id",
        columnDefs = list(list(visible = FALSE, targets = 0)),
        # view all & scrolling
        allow_view_all = TRUE,
        initial_page_length = -1,
        dom = "ft",
        scrollX = TRUE,
        scrollY = "250px",
        selection = "none"
      )

    # return functions ======
    list(
      get_token = function() token,
      devices = devices,
      structures = structures,
      refresh_devices = refresh_devices,
      edit_structure = edit_structure
    )
  })
}

#' @describeIn sdds_module generates the onstart function for connecting to the SDDS particle stream
#' @inheritParams particle_get_device_info
#' @param event the name of the event to listen to, default is "sddsData"
#' @export
sdds_onstart <- function(token, event = "sddsData") {
  # return onStart function
  function() {
    log_info("connecting to particle stream")
    particle_stream_connect(
      token = token,
      endpoint = paste0("events/", event),
      log = TRUE
    )
    onStop(function() {
      particle_stream_disconnect()
      log_info("Application closed.")
    })
  }
}
