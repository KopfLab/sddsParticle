#' Run the user interface
#'
#' This function runs a simple example GUI that uses the sdds particle device controller modulre.
#'
#' @param timezone the timezone to user for datetime calculations
#' @inheritParams particle_get_device_info
#' @inheritParams shiny::shinyApp
#' @export
sdds_run_gui <- function(
  token = keyring::key_get("particle"),
  timezone = Sys.timezone(),
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

  # minimalist example ui and server
  ui <- example_ui(timezone = timezone)
  server <- example_server(token = token)

  # generate app
  shinyApp(
    ui = ui,
    server = server,
    # onstart required to read the particle stream!
    onStart = sdds_onstart(token = token),
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}

example_ui <- function(timezone, default_theme = "cosmo") {
  # ui function
  function(request) {
    bslib::page_navbar(
      fillable = TRUE, # important for flex content
      title = paste0("SDDS Particle GUI v", packageVersion("sddsParticle")),
      theme = bslib::bs_theme(preset = default_theme, version = 5),
      header = sdds_header(),

      bslib::nav_spacer(), # pushes items to the right
      bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = NULL)),
      # var bar panel
      bslib::nav_panel(
        title = NULL, # single nav panel

        # SIDEBAR ========
        bslib::page_sidebar(
          sidebar = bslib::sidebar(
            # collapse the left side bar y default
            open = FALSE,
            selectInput(
              "timezone",
              label = "Timezone",
              choices = OlsonNames(),
              selected = timezone
            ),
            selectInput(
              "theme",
              label = "Theme",
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
              selected = default_theme
            )
          ),

          # DEVICES ========
          bslib::accordion(
            id = "accordion",
            multiple = TRUE,
            bslib::accordion_panel(
              "SDDS Devices",
              icon = icon("microchip"),
              bslib::card(
                full_screen = TRUE,
                max_height = 300,
                id = "devices_card",
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    position = "left",
                    width = "160",
                    sdds_ui_devices_actions("sdds")
                  ),
                  sdds_ui_devices_table("sdds")
                ),
                bslib::card_footer(
                  "Select the devices you want to work with."
                )
              )
            )
          ),

          # STRUCTURES ========
          bslib::card(
            bslib::card_header(icon("folder-tree"), "Data structures"),
            min_height = 400,
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                position = "left",
                width = "230",
                bslib::accordion(
                  open = TRUE,
                  bslib::accordion_panel(
                    "Quick actions",
                    icon = icon("bolt-lightning"),
                    actionButton("restart", "Restart", icon = icon("gears")) |>
                      shinyjs::disabled(),
                    spaces(),
                    actionButton(
                      "save",
                      "Save state",
                      icon = icon("floppy-disk")
                    ) |>
                      shinyjs::disabled()
                  ),
                  bslib::accordion_panel(
                    "Controls",
                    icon = icon("gears"),
                    sdds_ui_structures_actions("sdds")
                  )
                )
              ),
              # no full screen for structures: no point, won't work with the modal dialogs
              sdds_ui_structures_table("sdds"),
            ),
            bslib::card_footer("Select structure entry to change values.")
          )
        )
      )
    )
  }
}

example_server <- function(token, default_theme = "cosmo") {
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

    # show data structure and common actions and disable/enable common actions
    observeEvent(sdds$devices$get_selected_ids(), {
      device_selected <- !is_empty(sdds$devices$get_selected_ids())
      shinyjs::toggleState("restart", condition = device_selected)
      shinyjs::toggleState("save", condition = device_selected)
      if (device_selected) {
        bslib::accordion_panel_open(
          id = "accordion",
          values = "Common actions"
        )
        bslib::accordion_panel_open(
          id = "accordion",
          values = "Data structures"
        )
      }
    })

    # sdds
    sdds <- sdds_server("sdds", token, timezone = reactive(input$timezone))

    # common actions
    observeEvent(input$restart, {
      sdds$edit_structure("SYSTEM.action", value = "restart")
    })
    observeEvent(input$save, {
      sdds$edit_structure("SYSTEM.action", value = "saveState")
    })
  }
}
