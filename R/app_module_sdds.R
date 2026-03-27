#' sdds GUI module
#'
#' @description SDDS shiny Module.
#'
#' @param id module id
#' @describeIn sdds_module generates the ui for the sdds module
#' @export
sdds_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # devices
    shinydashboard::box(
      title = span(
        "Devices",
        div(
          style = "position: absolute; right: 10px; top: 5px;",

          actionButton(
            ns("refresh_devices"),
            "Refresh",
            icon = icon("arrows-rotate"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Refresh device list."
            ),
          module_selector_table_deselect_all_button(
            ns("devices"),
            border = FALSE
          )
        )
      ),
      width = 12,
      status = "info",
      solidHeader = TRUE,
      module_selector_table_ui(ns("devices")),
      footer = tagList("Select the devices you want to work with.")
    ),

    # commands / self-describing data structure
    shinydashboard::box(
      title = span(
        "Control Structures",
        div(
          style = "position: absolute; right: 10px; top: 5px;",
          actionButton(
            ns("show_hide_system"),
            textOutput(ns("system_label"), inline = TRUE),
            icon = icon("house"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Show/hide the HARDWARE menu items."
            ),
          actionButton(
            ns("show_hide_hardware"),
            textOutput(ns("hardware_label"), inline = TRUE),
            icon = icon("microchip"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Show/hide the HARDWARE menu items."
            ),
          actionButton(
            ns("send_commands"),
            "Send",
            icon = icon("paper-plane"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Send commands to make the changes."
            ) |>
            shinyjs::disabled(),
          actionButton(
            ns("command_logs"),
            "Logs",
            icon = icon("list-check"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Show latest commands sent to devices."
            ),
          actionButton(
            ns("events_stream"),
            "Events",
            icon = icon("timeline"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Show events sent by the selected devices."
            ),
          actionButton(
            ns("fetch_values"),
            "Fetch",
            icon = icon("cloud-arrow-down"),
            style = "border: 0;"
          ) |>
            add_tooltip(
              "Fetch latest structure from devices."
            )
        )
      ),
      width = 12,
      status = "info",
      solidHeader = TRUE,
      module_selector_table_ui(ns("structures")),
      footer = tagList("Click to change values (if allowed).")
    ) |>
      div(id = ns("structures_box")) |>
      shinyjs::hidden()
  )
}

#' @describeIn sdds_module generates the server for the sdds module
#' @export
sdds_server <- function(id, token, get_timezone, core_ids = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive values =======
    values <- reactiveValues(
      show_system = FALSE,
      show_hardware = FALSE,
      edit_structure = tibble(),
      command_queue = tibble()
    )

    # devices =============

    ## delete devmode cache
    if (in_devmode()) {
      observeEvent(
        input$refresh_devices,
        {
          if (file.exists("cache/sdds_devices.csv")) {
            unlink("cache/sdds_devices.csv")
          }
        },
        priority = 100
      )
    }

    ## get devices
    get_devices <- reactive({
      input$refresh_devices
      log_info(ns = ns, user_msg = "Fetching devices")
      # safely call function
      out <- get_devices_in_app(
        token = token,
        core_ids = core_ids
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## get devices for table
    get_devices_for_table <- reactive({
      # safety checks
      validate(need(get_devices(), "No devices available."))
      # safely call function
      out <- get_devices() |>
        get_devices_for_table_in_app(timezone = get_timezone()) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## setup devices selector table
    devices <- callModule(
      module_selector_table_server,
      "devices",
      get_data = get_devices_for_table,
      id_column = "coreid",
      # make id column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0)
      ),
      # view all & scrolling
      allow_view_all = TRUE,
      initial_page_length = -1,
      dom = "ft",
      scrollX = TRUE,
      scrollY = "150px"
    )

    # structures =======

    ## get structures
    get_structures <- reactive({
      req(get_devices())
      req(devices$get_selected_ids())
      log_info(ns = ns, user_msg = "Loading control structures")
      # safely call function
      out <- get_structures_in_app(
        devices = get_devices(),
        core_ids = devices$get_selected_ids(),
        timezone = get_timezone(),
        # TODO: these could be coming from the additional value modules/types/converters
        additional_types = list(
          "resistance" = expr(.data$base_units == "Ohm")
        ),
        additional_converters = list("resistance" = function(value, units) {
          if (value > 1e6) {
            paste0(value / 1e6, " MOhm")
          } else if (value > 1e3) {
            paste0(value / 1e3, " kOhm")
          } else {
            paste0(value / 1e3, " Ohm")
          }
        })
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    get_structures_for_table <- reactive({
      # safety checks
      validate(need(get_structures(), "No structures available."))

      structures$reset_visible_columns()
      # safely call function
      out <- get_structures() |>
        get_get_structures_for_table_in_app(
          show_system = values$show_system,
          show_hardware = values$show_hardware
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      return(out$result)
    })

    ## hide/show structures if there are selections
    observeEvent(
      devices$get_selected_ids(),
      {
        shinyjs::toggle(
          "structures_box",
          condition = !is_empty(devices$get_selected_ids())
        )
      },
      ignoreNULL = FALSE
    )

    ## show/hide buttons
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
    structures <- callModule(
      module_selector_table_server,
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
      allow_view_all = TRUE,
      initial_page_length = -1,
      ordering = FALSE,
      dom = "ft",
      scrollX = TRUE,
      scrollY = "max(200px, calc(100vh - 620px))", # account for size of header and devices table with the -x px
      selection = "single",
      auto_reselect = FALSE
    )

    # edit values ===========

    # editing modules
    edit_modules <- list(
      "integer" = value_integer_input("integer"),
      "enum" = value_enum_input("enum")
    )

    # trigger modal dialog
    observeEvent(structures$get_selected_ids(), {
      req(structures$get_selected_ids())
      path <- structures$get_selected_ids()
      structure <- get_structures() |> filter(.data$path == !!path)
      if (all(structure$readonly)) {
        values$edit_structure <- tibble()
        log_info(
          ns = ns,
          user_msg = sprintf("Control structure '%s' is read-only", path)
        )
      } else {
        values$edit_structure <- structure

        # generate edit ui fields
        edit_ui <-
          structure |>
          mutate(
            ui = purrr::pmap(
              list(
                type = .data$type,
                coreid = .data$coreid,
                label = .data$corename,
                value = .data$value,
                units = .data$base_units,
                choices = .data$enum_values
              ),
              function(type, coreid, label, value, units, choices) {
                if (!type %in% names(edit_modules)) {
                  return(generate_standard_input_row(
                    label,
                    sprintf("unsupported value type '%s'", type)
                  ))
                }
                return(edit_modules[[type]]$generate_ui(
                  coreid = coreid,
                  label = label,
                  value = value,
                  units = units,
                  choices = choices
                ))
              }
            )
          ) |>
          pull(.data$ui) |>
          tagList()

        # generate modal diaolog
        modalDialog(
          title = h3(path),
          edit_ui,
          footer = tagList(
            actionButton(
              ns("save_edit"),
              "Queue change",
              icon = icon("save"),
              style = "border: 0;"
            ) |>
              add_tooltip(
                "Add the change to the command queue (click Send to send to devices)."
              ),
            modalButton("Cancel")
          ),
          size = "m",
          easyClose = TRUE
        ) |>
          showModal()
      }
    })

    # save edits
    observeEvent(input$save_edit, {
      req(nrow(values$edit_structure) > 0)

      # any new values?
      new_values <-
        values$edit_structure |>
        mutate(
          new_value = purrr::pmap(
            list(type = .data$type, coreid = .data$coreid),
            function(type, coreid) {
              if (!type %in% names(edit_modules)) {
                return(NULL)
              }
              return(edit_modules[[type]]$get_value(coreid))
            }
          ),
          new_text = purrr::pmap_chr(
            list(
              type = .data$type,
              coreid = .data$coreid,
              units = .data$base_units
            ),
            function(type, coreid, units) {
              if (!type %in% names(edit_modules)) {
                return(NA_character_)
              }
              return(edit_modules[[type]]$get_text(coreid, units))
            }
          ),
          has_changed = purrr::map2_lgl(
            value,
            new_value,
            ~ !is.null(.y) & !identical(.x, .y)
          )
        ) |>
        filter(.data$has_changed)

      if (nrow(new_values) > 0) {
        values$command_queue <- bind_rows(
          isolate(values$command_queue),
          new_values |> prepare_command_queue_entries()
        ) |>
          mutate(row_id = row_number())
      }

      # close model dialog
      removeModal()
    })

    # send commands queue =========

    ## get data
    get_command_queue_for_table <- reactive({
      req(nrow(values$command_queue) > 0)
      values$command_queue |>
        mutate(
          status = if_else(is.na(.data$status), "not sent yet", .data$status)
        ) |>
        select(
          "row_id",
          "coreid",
          "Device" = "corename",
          "Attribute" = "path",
          "Previous value" = "text",
          "New value" = "new_text",
          "Command" = "command",
          "Status" = "status"
        )
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
    observeEvent(queue$table_complete(), {
      req(nrow(values$command_queue) > 0)
      ids <- values$command_queue |> filter(is.na(.data$status)) |> pull(row_id)
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
          ns("send_now"),
          "Send selected",
          icon = icon("paper-plane"),
          style = "border: 0;"
        ) |>
          add_tooltip(
            "Send the selected commands to the devices."
          ) |>
          shinyjs::disabled(),
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
          "send_now",
          condition = !is_empty(queue$get_selected_ids())
        )
        updateActionButton(
          inputId = "send_now",
          label = sprintf("Send %d selected", length(queue$get_selected_ids()))
        )
      },
      ignoreNULL = FALSE
    )
    observeEvent(input$clear_queue, {
      values$command_queue <- tibble()
      removeModal()
    })

    ## commands queue table
    queue <- callModule(
      module_selector_table_server,
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

    ## function to send the commands
    send_commands <- function(coreid, corename, cmds) {
      log_info(
        ns = ns,
        user_msg = sprintf(
          "Sending %d commands to %s",
          length(cmds),
          corename[1]
        )
      )
      out <- coreid[1] |>
        particle_send_sdds_commands(cmds = cmds) |>
        try_catch_cnds()
      if (nrow(out$conditions) > 0) {
        # TODO: how to better provide the cnds to the error function?
        show_cnds(out$conditions)
        log_error(
          ns = ns,
          user_msg = paste0("could not send commands to ", corename[1])
        )
        return(rep(FALSE, length(cmds)))
      }
      return(out$result$success)
    }

    ## send the commands
    observeEvent(input$send_now, {
      req(nrow(values$command_queue) > 0)
      req(queue$get_selected_ids())

      cmds_to_send <- values$command_queue |>
        filter(.data$row_id %in% queue$get_selected_ids())
      print(cmds_to_send)

      cmds_results <- cmds_to_send |>
        mutate(
          .by = "coreid",
          success = send_commands(coreid[1], corename[1], command)
        )
      print(cmds_results)

      values$command_queue <- values$command_queue |>
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
      print(values$command_queue)

      # FIXME: continue here!

      #particle_send_sdds_commands("0a10aced202194944a058c18", cmds = c("sensor.action=beamOff"))
    })

    # events stream ============

    ## reactive stream events poll
    get_stream_events_poll <- reactivePoll(
      # check every 1s (adjust as needed)
      intervalMillis = 1000,
      session = session,

      # return value indicating changes
      checkFunc = function() {
        # safely call function
        out <- get_stream_events_for_app(
          devices = get_devices(),
          core_ids = devices$get_selected_ids()
        ) |>
          try_catch_cnds()
        # don't show because this runs regularly
        return(digest::digest(out))
      },

      # get stream events
      valueFunc = function() {
        # safely call function
        out <- get_stream_events_for_app(
          devices = get_devices(),
          core_ids = devices$get_selected_ids(),
          timezone = get_timezone(),
          prepare_for_table = TRUE
        ) |>
          try_catch_cnds()
        # don't show, return
        return(out)
      }
    )

    ## actual stream events function (to safely query)
    get_stream_events <- reactive({
      req(input$events_stream)
      out <- get_stream_events_poll()
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
        modalButton("Close")
      ),
      easyClose = TRUE
    )
    # show events modal
    observeEvent(input$events_stream, {
      log_info(ns = ns, user_msg = "Fetching events stream")
      req(get_stream_events())
      showModal(events_modal)
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
            try_catch_cnds(augment_message = "error processing event data")
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
    events <- callModule(
      module_selector_table_server,
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

      # safely call function
      out <- get_command_logs_in_app(
        devices = isolate(get_devices()),
        core_ids = isolate(devices$get_selected_ids()),
        token = token
      ) |>
        prepare_command_logs_for_table_in_app(
          timezone = isolate(get_timezone())
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
    log <- callModule(
      module_selector_table_server,
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
  })
}

#' @describeIn sdds_module generates the onstart function for connecting to the SDDS particle stream
#' @inheritParams particle_get_device_info
#' @export
sdds_onstart <- function(token) {
  # return onStart function
  function() {
    # ps_connect(
    #   endpoint,
    #   # this is a temporary access token (valid for 10 days) but still, don't commit it!
    #   token = "c8f8da7475e2e3e01a7c16faa37519657bae9f0f",
    #   log = FALSE
    # )
    # onStop(function() {
    #   ps_disconnect()
    #   cat("\nApplication closed.\n")
    # })
  }
}
