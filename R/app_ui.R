#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
suppressMessages(library(janitor,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(tidylog,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(leaflet,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(htmltools,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(httr,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(leaflet.providers,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(leaflet.extras,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(tidyverse,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(geojsonsf,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(sf,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(leafem,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(leafpop,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(shiny,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(shinyWidgets,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(shinydashboard,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(bslib,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(shinyjs,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(shinydashboardPlus,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(mapview,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(htmlwidgets,quietly = TRUE,warn.conflicts = FALSE))
suppressMessages(library(stringr,quietly = TRUE,warn.conflicts = FALSE))
# Highcharts --------------------------------------------------------------

# suppressWarnings(
#   suppressMessages(
#     source("../Tucannon/R/data_processing/powers/explore-powers.R")
#     )
#   )



# Theme -------------------------------------------------------------------

theme <- bs_theme(
  # Controls the default grayscale palette
  bg = "#202123", fg = "#B8BCC2",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#EA80FC", secondary = "#48DAC6",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#EA80FC"
)

custom_css <- "
.bg-dark {
    background-color: #2c3e50 !important;
}
"

# shapefiles --------------------------------------------------------------




# Navbar links ------------------------------------------------------------


link_shiny <- tags$a(
  shiny::icon("github"), "Repository",
  href = "https://github.com/palouse-anglers/Tucannon",
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
    useShinyjs(), 
    tags$head(
      tags$script(src = "https://code.highcharts.com/highcharts.js"),
      tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
      tags$script(src ="R/www/downloadLeafletMap.js"),
      #tags$style(".leaflet-control.legend { display: none; }")
      
    ),
    # Your application UI logic
    bslib::page_navbar(
      id = "navbar_items_id",
      theme = bs_theme(fg = "rgb(101, 78, 24)", primary = "#5E9300",
                       success = "#2c3e50", font_scale = NULL, bg = "#fff"),
      title = "Tucannon", 


               
                                     
# Water Quality -----------------------------------------------------------

bslib::nav_panel(title = "Water Quality", #---------------Nav Bar------
            fluidRow(
                sliderTextInput(
                   inputId = "dateRange",
                   label = "Filter by year", 
                   choices = 1973:2024,
                   selected = c(2011,2024),
                   grid = FALSE
                 ),
                shinyWidgets::pickerInput(width = '400px',
                                          options = pickerOptions(
                                          `count-selected-text` = "{0} Months Selected",
                                          container = "body",
                                          actionsBox = TRUE,
                                          liveSearch=TRUE,selectedTextFormat= 'count > 3'),   # build buttons for collective selection
                                          multiple = TRUE,
                                          inputId = "monthRange",
                                          label = "Filter by month", 
                                          choices = month.abb[c(1:12)],
                                          selected = month.abb[c(1:12)])
                ),
    bslib::navset_tab(id = "navset_tabs_id",
                      bslib::nav_panel(title = "Temperature", #---Start Temp Tab-----
                                       layout_column_wrap(
                                         #width = 1/2,
                                         #height = 300,
                                         card(full_screen = TRUE, card_header(""), card_body(by_year)),
                                         card(full_screen = TRUE, card_header(""), card_body(highchartOutput("by_year_box"))),
                                         card(full_screen = TRUE, card_header(""), card_body(by_summer)),
                                         card(full_screen = TRUE, card_header(""), 
                                              card_body(highchartOutput("by_year_scatter"))),
                                         card(full_screen = TRUE, card_header(""), card_body(by_month))
                                       ) #-----end temperature card row 
                                       
                      ),
                                   
                       

                         
                         
                         
                         #-------Start DO Tab----              
        bslib::nav_panel(title = "Dissolved Oxygen",#-------Start DO Tab----
                         layout_column_wrap(
                           card(
                             full_screen = TRUE,
                             style = "resize:both;",
                             card_header("Dissolved Oxygen"),
                             card_body(highchartOutput("do_plot"))
                              )
                           ),
          value = "do_tab"), #-------------- End DO Tab----------------------
#----- End Temp Tab-------
          bslib::nav_panel(
            title = "Phosphorus",
                           
            layout_column_wrap(
              card(
                full_screen = TRUE,
                card_body(highchartOutput("phos_plot"))
                ),
                  card(
                    full_screen = TRUE,
                     card_body(highchartOutput("orthophos_plot")),
                    ))
          ), 
bslib::nav_panel(title = "Table",

                  DT::dataTableOutput("params_table")
                 ),
                
          bslib::nav_panel(title = "Realtime Flows",
            card(
                             full_screen = TRUE,
                             title = "Tucannon-Starbuck 13344500",
                             card_header("Tucannon-Starbuck 13344500"),
                             status = "info",
                             width = "100%",
                             card_body(htmlOutput("iframe_starbuck")),
                             card_body(htmlOutput("iframe_ecymaps")),
                             card_header("Tucannon-Marengo 35B150"),
                             card_body(htmlOutput("iframe_marengo"))
                             )
                              ), # End flow nav panel
          bslib::nav_panel(title = "TSS",
                           
                      card(
                        full_screen = TRUE,
                        fill = TRUE,
                        style = "resize:both;",
                        card_body(height = '65vh',highchartOutput("TSS_plot"))
                        )
                           
                           
                           ),
          bslib::nav_panel(title = "Turbidity",
                   card(
                full_screen = TRUE,
                fill = TRUE,
                style = "resize:both;",
                card_body(height = '65vh',highchartOutput("Turbidity_plot"))
                           )
                           
                           ),
bslib::nav_panel(title = "Bacteria",
                 card(
                   full_screen = TRUE,
                   fill = TRUE,
                   style = "resize:both;",
                   card_body(height = '65vh',highchartOutput("bacteria_plot"))
                 ) ),
          bslib::nav_panel(title = "Ammonia",
                    card(
                    full_screen = TRUE,
                    fill = TRUE,
                    style = "resize:both;",
                    card_body(height = '65vh',highchartOutput("Ammonia_plot"))
                  )
               ),
          bslib::nav_panel(title = "Date Ranges",DT::datatable(param_ranges)),
          bslib::nav_panel(title = "BMPs",
      layout_column_wrap(
            card(
            full_screen = TRUE,
            fill = TRUE,
            style = "resize:both;",
            card_header("BMPs Table"),
            card_body(height = '65vh',DT::dataTableOutput("bmps_full_table",width = "90%"))
            ),
            card(
              full_screen = TRUE,
              fill = TRUE,
              style = "resize:both;",
              card_header("BMPs Plot"),
              card_body(height = '65vh',highchartOutput("bmps_stacked"))
            ))
            ), 
         
            
             bslib::nav_panel(title = "Marengo",#-------Start DO Tab----
                         layout_column_wrap(
                           card(
                             full_screen = TRUE,
                             style = "resize:both;",
                             card_header("Temperature"),
                             card_body(highchartOutput("mrngo_water_plot") %>%
                              shinycssloaders::withSpinner(
                                image = "https://raw.githubusercontent.com/daattali/shinycssloaders/master/inst/img/custom.gif"))
                           ),
                           card(
                             full_screen = TRUE,
                             style = "resize:both;",
                             card_header("Stage Height"),
                             card_body(highchartOutput("mrngo_stage_plot")%>%
            shinycssloaders::withSpinner(
            image = "https://raw.githubusercontent.com/daattali/shinycssloaders/master/inst/img/custom.gif"))
                                       
                                       )
                           )
                         )
                         
        )
      ),

# Watersheds --------------------------------------------------------------

bslib::nav_panel(title = "Watersheds Map",
      bslib::navset_tab(id = "watersheds_map_id",
        bslib::nav_panel(
                    title = "Watersheds Map",
                       # fluidRow(
                       # actionButton("toggle_card1", "Toggle Card 1"),
                       # actionButton("toggle_card2", "Toggle Card 2"),
                       # actionButton("toggle_card3", "Toggle Card 3")
                       # ),
 
                       uiOutput("card_layout")
                  ),
        bslib::nav_panel(title = "Watersheds Table",
                    DT::dataTableOutput("selectedHUC")                 
        )
            
            )
        #),
         
      
                  

# start side panel --------------------------------------------------------
# 
# page_fillable(
#   h1("Left and right sidebar", class = "px-3 my-3"),
#   layout_sidebar(
#     sidebar = sidebar(  card(full_screen = TRUE,
#                         card_header(shinyWidgets::pickerInput(width = '380px',
#    options = pickerOptions(
#      `count-selected-text` = "{0} Sites Selected",
#      container = "body",
#      actionsBox = TRUE,
#      liveSearch=TRUE,selectedTextFormat= 'count > 1'),   # build buttons for collective selection
#    multiple = T,
#    inputId = "watersheds",
#    label = "HUC 12 Watersheds",
#    choices = huc12$Name,
#    #selected= filtered_huc()$Name,   
#    choicesOpt = list(subtext = huc12$HUC12))),
#        card_body(
#          uiOutput("acres_box")),
#        card_header(
#          checkboxGroupButtons(
#            inputId = "bmps_active",
#            selected = "Yes",
#            label = "Active BMPs",
#            choices = c("Yes", "No"),
#            checkIcon = list(
#              yes = tags$i(class = "fa fa-check-square",
#                           style = "color: #84563C"),
#              no = tags$i(class = "fa fa-square-o",
#             style = "color: #84563C")
#                                 )
#                                          )
#                                          
#                                          
#                                        ),
#   card_body(min_height = '100px',uiOutput("bmps_box",fill = "container"))
#                                        
#     )),
#     layout_sidebar(
#       sidebar = sidebar(            card(full_screen = TRUE,
#                                          style = "resize:both;",
#                                          
#                                          
#                    
#                    
#                    card_header(
#                      shinyWidgets::pickerInput(
#                        width = '380px',
#                        options = pickerOptions(
#                          `count-selected-text` = "{0} Categories Selected",
#                          container = "body",
#                          actionsBox = TRUE,
#                          liveSearch =
#                            TRUE,
#                          selectedTextFormat = 'count > 1'
#                                              ),
#                                              # build buttons for collective selection
#                                              multiple = T,
#                                              inputId = "selectInput",
#                                              label = "Category",
#                                              choices = c(
#                                                "Wildlife",
#                                                "Erosion",
#                                                "BMPs",
#                                                "Frequently Flooded Areas",
#                                                "SRP",
#                                                "Wetlands",
#                                                "Geologically Hazardous Areas",
#                                                "Landuse 2011",
#                                                "Landuse 2019"
#                                              )
#                                            )
#                           
#                                          ),
#                                          
#                                          
#                                          
#                                          
#                                          bslib::accordion(
#                                            id = "acc",
#                                            list(
#                                              uiOutput("bmps_table"), 
#                                              uiOutput("wildlife_box"),
#                                              uiOutput("erosion_box"),
#                                              uiOutput("wetlands_box"),
#                                              uiOutput("flood_box"),
#                                              uiOutput("land19_box"),
#                                              uiOutput("srp_box"),
#                                              uiOutput("land11_box"),
#                                              uiOutput("geo_box")
#                                            ),
#                                          ),
#       ), position = "right", open = FALSE),
#       card(
#         fill = TRUE,
#         id = "leaflet_map",
#         full_screen = TRUE,
#         style = "resize:both;",
#         card_body(leafletOutput("leafmap"))
#       ),
#       border = FALSE
#     ),
#     border_radius = FALSE,
#     fillable = TRUE,
#     class = "p-0"
#   #)
# 
# ),  #----- End leaflet map card-----
# 
# value = "map_tab" 
#       ) #---- End layout_column_wrap map------
#     ),
# 
# # end side panel                       


), #---end watersheds Nav Bar----
# CTUIR -------------------------------------------------------------------
bslib::nav_panel(
  title = "River Restoration",
  #---------------Nav Bar------
  bslib::navset_tab(
    id = "navset_tabs_ctuir_id",
    bslib::nav_panel(title = "Restoration",
                     
                     card(
                       full_screen = TRUE,
                       title = "Tucannon-Restoration",
                       card_header("Tucannon Restoration Projects"),
                       status = "info",
                       width = "100%",
                       card_body(htmlOutput("iframe_ctuir_rest")),
                       card_footer("Web application produced and hosted by the Confederated Tribes of the Umatilla Indian Reservation (CTUIR) GIS Program. 
Data in this web application is sourced from multiple agencies and publicly available data sources. CTUIR makes no warranty, expressed or implied, including the warranties of merchantability and fitness for a particular purpose, nor assumes any legal liability or responsibility for the accuracy, reliability, completeness or utility of these geospatial data, or for the improper or incorrect use of these geospatial data. The data depicted in this map in no way limit the extent of interests of
the Confederated Tribes of Umatilla Indian Reservation (CTUIR).Visit tucannonriver.org for more information")
                     )),
    bslib::nav_panel(title = "Geomorphic Assessment",
                     card(
                       full_screen = TRUE,
                       title = "Tucannon-Assessment",
                       card_header("Tucannon Geomorphic Assessment"),
                       status = "info",
                       width = "100%",
                       card_body(htmlOutput("iframe_ctuir_geo")),
                       card_footer("Web application produced and hosted by the Confederated Tribes of the Umatilla Indian Reservation (CTUIR) GIS Program. 
Data in this web application is sourced from multiple agencies and publicly available data sources. CTUIR makes no warranty, expressed or implied, including the warranties of merchantability and fitness for a particular purpose, nor assumes any legal liability or responsibility for the accuracy, reliability, completeness or utility of these geospatial data, or for the improper or incorrect use of these geospatial data. The data depicted in this map in no way limit 
the extent of interests of the Confederated Tribes of Umatilla Indian Reservation (CTUIR). Visit tucannonriver.org for more information")
                     ))
  )),  
# Landcover ---------------------------------------------------------------

      bslib::nav_panel("Landcover", 
                       shinyWidgets::pickerInput(width = '400px',
                       # build buttons for collective selection
                                                 multiple = FALSE,
                                                 inputId = "critpick",
                                                 label = "Critical Area", 
                                                 choices = c("Wetlands","Wildlife","Geologic Hazard"),
                                                 selected = "Wetlands"),
                       
            
                        #uiOutput("critical_ag_output"),
                        uiOutput("critical_ag_output"),
                        uiOutput("wetlands_ag_output"),
                        uiOutput("geo_ag_output")
                        
                       
                       ),#------Nav Bar--------
      bslib::nav_panel("Guidance", p("")),#------Nav Bar--------
      bslib::nav_spacer(),
      #bslib::nav_spacer(),
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
