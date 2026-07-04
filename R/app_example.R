#' Run the user interface
#'
#' This function runs a simple example GUI that uses the sdds particle device controller modulre.
#'
#' @param timezone the timezone to user for datetime calculations
#' @inheritParams particle_get_device_info
#' @inheritParams sdds_onstart
#' @inheritParams shiny::shinyApp
#' @export
sdds_run_gui <- function(
  token = keyring::key_get("particle"),
  event = "sddsData",
  timezone = Sys.timezone(),
  accessible_core_ids = NULL,
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url"
) {
  # startup
  log_info("\n\n========================================================")
  log_info(
    "starting SDDS particle GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )

  # example quick actions (shown in the data structures card)
  quick_actions <- list(
    sdds_ui_quick_action(
      "restart",
      "Restart",
      icon = icon("gears"),
      path = "SYSTEM.action",
      value = "restart"
    ),
    sdds_ui_quick_action(
      "save",
      "Save state",
      icon = icon("floppy-disk"),
      path = "SYSTEM.action",
      value = "saveState"
    ),
    sdds_ui_quick_action(
      "change_publish_interval",
      "Change publish interval",
      icon = icon("upload"),
      path = "SYSTEM.publishing.globalInterval_ms"
    ),
    sdds_ui_quick_action(
      "start_recording",
      "Start recording",
      icon = icon("play"),
      path = "SYSTEM.publishing.record",
      value = "ON"
    ),
    sdds_ui_quick_action(
      "stop_recording",
      "Stop recording",
      icon = icon("stop"),
      path = "SYSTEM.publishing.record",
      value = "OFF"
    )
  )

  # minimalist example ui and server
  ui <- example_ui(
    timezone = timezone,
    quick_actions = quick_actions
  )
  server <- example_server(
    token = token,
    accessible_core_ids = accessible_core_ids,
    quick_actions = quick_actions
  )

  # generate app
  shinyApp(
    ui = ui,
    server = server,
    # onstart required to read the particle stream!
    onStart = sdds_onstart(token = token, event = event),
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}

example_ui <- function(
  timezone,
  quick_actions = list(),
  default_theme = "flatly"
) {
  # ui function
  function(request) {
    bslib::page_navbar(
      fillable = TRUE, # important for flex content
      title = paste0("SDDS Particle GUI v", packageVersion("sddsParticle")),
      theme = bslib::bs_theme(preset = default_theme, version = 5),
      header = sdds_header(),

      bslib::nav_spacer(), # pushes items to the right
      # timezone and theme selectors (labels replaced with hover tooltips)
      bslib::nav_item(
        selectInput(
          "timezone",
          label = NULL,
          choices = OlsonNames(),
          selected = timezone,
          width = "200px"
        ) |>
          tagAppendAttributes(class = "mb-0") |>
          add_tooltip("Timezone used for all date and time displays")
      ),
      bslib::nav_item(
        selectInput(
          "theme",
          label = NULL,
          choices = c(
            "flatly",
            "cosmo",
            "lumen",
            "minty",
            "sandstone",
            "darkly",
            "cyborg",
            "slate",
            "superhero",
            "solar"
          ),
          selected = default_theme,
          width = "150px"
        ) |>
          tagAppendAttributes(class = "mb-0") |>
          add_tooltip("Visual theme for the app")
      ),
      bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = NULL)),
      # var bar panel
      bslib::nav_panel(
        title = NULL, # single nav panel

        # DEVICES in left sidebar, DATA STRUCTURES as main content panel
        bslib::page_sidebar(
          sidebar = bslib::sidebar(
            width = "550",
            open = "open",
            # fillable so the devices card (and its table) fill the sidebar height
            fillable = TRUE,
            sdds_ui_devices_card("sdds")
          ),

          # STRUCTURES ========
          sdds_ui_structures_card(
            "sdds",
            quick_actions = quick_actions
          )
        )
      )
    )
  }
}

example_server <- function(
  token,
  quick_actions = list(),
  default_theme = "cosmo",
  accessible_core_ids = NULL
) {
  # server function
  function(input, output, session) {
    # theme
    current_theme <- reactiveVal(default_theme) # should match UI default
    observe({
      req(input$theme)
      if (input$theme != isolate(current_theme())) {
        current_theme(input$theme)
      }
      session$setCurrentTheme(bslib::bs_theme(
        preset = input$theme,
        version = 5
      ))
    })

    # sdds module (the quick actions and their gating are wired inside the module)
    sdds_server(
      "sdds",
      token,
      timezone = reactive(input$timezone),
      accessible_core_ids = accessible_core_ids,
      quick_actions = quick_actions
    )
  }
}
