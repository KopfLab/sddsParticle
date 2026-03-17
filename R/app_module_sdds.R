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
            ),
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
sdds_server <- function(id, token, timezone, core_ids = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive values =======
    values <- reactiveValues(
      show_system = FALSE,
      show_hardware = FALSE
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

    ## get devices tibble
    get_devices <- reactive({
      input$refresh_devices
      if (in_devmode() && file.exists("cache/sdds_devices.csv")) {
        # take from file in dev mode
        devices <- readr::read_csv(
          "cache/sdds_devices.csv",
          show_col_types = FALSE
        )
      } else {
        # always request from cloud
        devices <- particle_get_device_info(token = token) |>
          simplify_device_info()
        if (in_devmode()) {
          # store in file in devmode
          if (!dir.exists("cache")) {
            dir.create("cache")
          }
          devices |> readr::write_csv("cache/sdds_devices.csv")
        }
      }
      # if only specific coreids are allowed --> filter for them
      if (!is.null(core_ids)) {
        devices <- devices |> filter(.data$id %in% core_ids)
      }
      # subselect info
      devices |>
        mutate(
          last_heard = lubridate::ymd_hms(.data$last_heard, tz = "UTC") |>
            lubridate::with_tz(timezone) |>
            format("%b %d %Y %H:%M:%S")
        )
    })

    ## get devices for table
    get_devices_for_table <- reactive({
      req(get_devices())
      get_devices() |>
        select(
          "coreid",
          Name = "name",
          `Last heard from` = "last_heard",
          Connected = "connected",
          Status = "status",
          Firmware = "system_firmware_version",
          `MAC address` = "mac_wifi"
        )
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

    ## get structures
    get_structures <- reactive({
      req(devices$get_selected_ids())
      sdds_read_cached_trees_and_values() |>
        filter(.data$coreid %in% devices$get_selected_ids()) |>
        sdds_parse_trees_and_values() |>
        sdds_simplify_trees_and_values(
          devices = get_devices(),
          timezone = timezone
        )
    })

    get_structures_for_table <- reactive({
      req(get_structures())
      structures$reset_visible_columns()
      structs <- get_structures()
      if (!values$show_system) {
        structs <- structs |>
          filter(!stringr::str_detect(.data$path, "^SYSTEM"))
      }
      if (!values$show_hardware) {
        structs <- structs |>
          filter(!stringr::str_detect(.data$path, "^HARDWARE"))
      }
      structs |>
        prepare_simplified_tree_w_values_for_table() |>
        rename(" " = "label")
    })

    ## setup structures selector table
    structures <- callModule(
      module_selector_table_server,
      "structures",
      get_data = get_structures_for_table,
      id_column = "path",
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
      scrollY = "calc(100vh - 620px)", # account for size of header and devices table with the -x px
      selection = "single",
      auto_reselect = FALSE
    )

    # edit values ===========

    # trigger modal dialog
    observeEvent(structures$get_selected_ids(), {
      print(structures$get_selected_ids())
      req(structures$get_selected_ids())
      path <- structures$get_selected_ids()
      structure <- get_structures() |> filter(.data$path == !!path)
      if (all(structure$readonly)) {
        log_info(user_msg = paste(path, "is read-only"))
      } else {
        modalDialog(
          title = h3(path),
          structure |> generate_value_input_rows(ns),
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

    # events stream ============

    ## reactive stream
    get_stream_events <- reactivePoll(
      # check every 1s (adjust as needed)
      intervalMillis = 1000,
      session = session,

      # return value indicating changes
      checkFunc = function() {
        if (is_empty(devices$get_selected_ids())) {
          return(NULL)
        }
        get_devices() |>
          filter(.data$coreid %in% devices$get_selected_ids()) |>
          get_stream_events_for_devices() |>
          digest::digest()
      },

      # get stream events
      valueFunc = function() {
        #particle_stream_get_events() |>
        get_devices() |>
          filter(.data$coreid %in% devices$get_selected_ids()) |>
          get_stream_events_for_devices() |>
          prepare_stream_events_for_table()
      }
    )

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
      showModal(events_modal)
    })
    # show/hide json area and code button
    observeEvent(
      events$get_selected_ids(),
      {
        if (!is_empty(events$get_selected_ids())) {
          out <- try_catch_cnds(
            shinyAce::updateAceEditor(
              session,
              "data_json",
              events$get_selected_items()$data |>
                jsonlite::fromJSON() |>
                jsonlite::toJSON(
                  auto_unbox = TRUE,
                  null = "null",
                  pretty = TRUE
                )
            )
          )
          if (nrow(out$conditions) > 0) {
            shinyAce::updateAceEditor(
              session,
              "data_json",
              ""
            )
          }
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
      log_info(user_msg = "Data copied to clipboard.")
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
      input$command_logs
      isolate({
        req(get_devices())
        req(devices$get_selected_ids())
        logs <- get_devices() |>
          filter(.data$coreid %in% devices$get_selected_ids()) |>
          get_command_logs_for_devices(token = token)
        logs |>
          prepare_command_logs_for_table(timezone = timezone)
      })
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
