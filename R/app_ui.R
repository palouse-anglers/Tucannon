#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
library(janitor)
library(tidylog)
library(leaflet)
library(htmltools)
library(httr)
library(leaflet.providers)
library(leaflet.extras)
library(tidyverse)
library(geojsonsf)
library(sf)
library(leafem)
library(leafpop)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(bslib)

# Highcharts --------------------------------------------------------------

suppressWarnings(
  suppressMessages(
    source("../Tucannon/R/data_processing/powers/explore-powers.R")
    )
  )


# shapefiles --------------------------------------------------------------




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
      bslib::nav_panel(title = "Water Quality", #---------------Nav Bar------
    bslib::navset_tab(id = "navset_tabs_id",
          bslib::nav_panel(title = "Dissolved Oxygen",#-------Start DO Tab----
                           card(
                             fill = TRUE,
                             full_screen = TRUE,
                             style = "resize:both;",
                             card_header("Dissolved Oxygen"),
                             card_body(highchartOutput("do_plot"))
                              ),
          value = "do_tab"), #-------------- End DO Tab----------------------
         bslib::nav_panel(title = "Temperature", #---Start Temp Tab-----
                          layout_column_wrap(
                            #width = 1/2,
                            #height = 300,
                            card(full_screen = TRUE, card_header(""), card_body(by_year)),
                            card(full_screen = TRUE, card_header(""), card_body(by_year_box)),
                            card(full_screen = TRUE, card_header(""), card_body(by_summer)),
                            card(full_screen = TRUE, card_header(""), card_body(by_year_scatter)),
                            card(full_screen = TRUE, card_header(""), card_body(by_month))
                          ) #-----end temperature card row 
                          
                          
                          
         ), #----- End Temp Tab-------
          bslib::nav_panel(title = "Date Ranges",DT::datatable(param_ranges)),
        )
      ),
      bslib::nav_panel(title = "Watersheds Map",
                       
                       shinyWidgets::pickerInput(width = '500px',
                         options = pickerOptions(
                           `count-selected-text` = "{0} Sites Selected",
                           container = "body",
                           actionsBox = TRUE,
                           liveSearch=TRUE,selectedTextFormat= 'count > 1'),   # build buttons for collective selection
                         multiple = T,
                         inputId = "watersheds",
                         label = "HUC 12 Watersheds",
                         choices = huc12$Name,
                         #selected= filtered_huc()$Name,   
                         choicesOpt = list(
                           subtext = huc12$HUC12)
                       ),
                layout_column_wrap(
                  layout_column_wrap(uiOutput("acres_box"),
                                     uiOutput("wildlife_box")
                                     ),
                       card(id = "leaflet_map",
                         full_screen = TRUE,
                         style = "resize:both;",
                         card_header(textOutput("selectedHUC_name")),
                         card_body(leafletOutput("leafmap"))), #----- End leaflet map card-----
                       uiOutput("additional_card"),
                      value = "map_tab" ) #---- End layout_column_wrap map------
                       ),#---Nav Bar----
      bslib::nav_panel("Three", p("try")),#------Nav Bar--------
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
