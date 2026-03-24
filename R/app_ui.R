# user interface
app_ui <- function(default_timezone = NULL) {
  # constants
  app_title <- "SDDS GUI"
  app_title_width <- 200
  app_color <- "green"
  spinner_color <- "#2c3b41"
  app_box_default <- "#2c3b41"

  # return ui function (request param required by shiny for bookmarking)
  function(request) {
    # set spinner color
    options(spinner.color = app_color)

    # header
    header <- shinydashboard::dashboardHeader(
      title = app_title,
      titleWidth = app_title_width
    )

    # sidebar
    sidebar <-
      shinydashboard::dashboardSidebar(
        collapsed = FALSE,
        disable = FALSE,
        width = app_title_width,
        shinyjs::useShinyjs(), # enable shinyjs
        shinytoastr::useToastr(), # enable toaster
        prompter::use_prompt(), # enable prompter
        tags$head(
          # css headers
          tags$style(
            type = "text/css",
            HTML(paste(
              # body top padding
              ".box-body {padding-top: 5px; padding-bottom: 0px}",
              # custom background box
              sprintf(
                ".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}",
                app_box_default,
                app_box_default
              ),
              sprintf(
                ".box.box-solid.box-info{border:1px solid %s;}",
                app_box_default
              ),
              sep = "\n"
            ))
          )
        ),
        h5(
          a(
            "sddsParticle",
            href = "https://github.com/KopfLab/sddsParticle",
            target = "_blank"
          ),
          as.character(packageVersion("sddsParticle")),
          align = "center"
        ),
        h4("Timezone", align = "center", style = "margin: 0px;"),
        selectInput(
          "timezone",
          label = NULL,
          choices = OlsonNames(),
          selected = default_timezone
        ),
        tags$li(a(uiOutput("help", inline = TRUE))),
        if (shiny::in_devmode()) {
          actionButton("dev_mode_toggle", "Toggle Dev Mode")
        }
      )

    # body
    body <- shinydashboard::dashboardBody(sdds_ui("sdds"))

    # dashboard page
    shinydashboard::dashboardPage(
      title = app_title, # tab title
      skin = app_color, # styling
      header = header,
      sidebar = sidebar,
      body = body
    )
  }
}
