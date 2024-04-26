#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

library(bslib)

# Highcharts --------------------------------------------------------------

suppressWarnings(
  suppressMessages(
    source("../Tucannon/R/data_processing/powers/explore-powers.R")
    )
  )


# Navbar links ------------------------------------------------------------


link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/rstudio/shiny",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "My App",
      bslib::nav_panel(
        title = "Water Quality", 
        bslib::navset_tab( 
          bslib::nav_panel(title = "Dissolved Oxygen",#-------Start DO Tab----
                           card(
                             fill = TRUE,
                             full_screen = TRUE,
                             style = "resize:both;",
                             card_header("Plots that grow but don't shrink"),
                             card_body(highchartOutput("do_plot"))
                              ),
          value = "do_tab"), #-------------- End DO Tab----------------------
          bslib::nav_panel(title = "Temperature", p("Second tab content.")),
          bslib::nav_panel(title = "Date Ranges",DT::datatable(param_ranges)),
        )
      ),
      bslib::nav_panel(title = "Two", p("Second page content.")),
      bslib::nav_panel("Three", p("Third page content.")),
      bslib::nav_spacer(),
      bslib::nav_menu(
        title = "Links",
        align = "right",
        bslib::nav_item(link_shiny),
        bslib::nav_item(link_posit)
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Tucannon"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
