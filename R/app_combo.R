link_repo <- tags$a(
  shiny::icon("github"), "Repository",
  href = app_inputs$repo,
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = app_inputs$posit,
  target = "_blank"
)

theme = bslib::bs_theme(
  fg = "rgb(101, 78, 24)", primary = "#5E9300",
  success = "#2c3e50", font_scale = NULL, bg = "#fff"
  )

app_ui <- function(request) {
  bslib::page_navbar(title = app_inputs$region,
                     theme = theme,
  # Water Quality ----                   
  bslib::nav_panel(title = "Water Quality",
                   # TODO modularize this
                   shiny::fluidRow(
                     sliderTextInput(
                       inputId = "dateRange",
                       label = "Filter by year", 
                       choices = 1973:2024, # range of data
                       selected = c(2011,2024), # 2011 to current year
                       grid = FALSE
                     ),
                     shinyWidgets::pickerInput(width = '400px',
                                               options = shinyWidgets::pickerOptions(
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
      bslib::navset_tab(id = "WQ_navset_tabs_id",
         bslib::nav_panel(title = "Realtime Flows",
                          bslib::card(
                            full_screen = TRUE,
                            title = app_inputs$WQ$usgs_flow_ttl,
                            bslib::card_header(app_inputs$WQ$usgs_flow_ttl),
                            status = "info",
                            width = "100%",
                            iframeUI("iframe_usgs_flow"),
                            iframeUI("iframe_wa_eco_flow"),
                            # TODO I think this should move out to its own card and be centered 
                            bslib::card_header(app_inputs$WQ$wa_eco_discharge_ttl),
                            iframeUI("iframe_wa_eco_discharge"),
                          )
                          ),
         bslib::nav_panel(title = paste0(app_inputs$gauge_location," DOE Gauge"),
                          bslib::layout_column_wrap(
                            bslib::card(
                              full_screen = TRUE,
                              style = "resize:both;",
                              bslib::card_header("Temperature")
                              # TODO Add content
                            ),
                            bslib::card( 
                              full_screen = TRUE,
                              style = "resize:both;",
                              bslib::card_header("Stage Height")
                              # TODO Add content
                            )
                          )
                          ),
         bslib::nav_panel(title = "Temperature",
                          bslib::layout_column_wrap(
                            bslib::card( 
                              full_screen = TRUE,
                              # bslib::card_header("")
                              # TODO Add content
                            ),
                            bslib::card( 
                              full_screen = TRUE,
                              # bslib::card_header("")
                              # TODO Add content
                            ),
                            bslib::card( 
                              full_screen = TRUE,
                              # bslib::card_header("")
                              # TODO Add content
                            ),
                            bslib::card( 
                              full_screen = TRUE,
                              # bslib::card_header("")
                              # TODO Add content
                            ),
                            bslib::card( 
                              full_screen = TRUE,
                              # bslib::card_header("")
                              # TODO Add content
                            )
                          )),
         bslib::nav_panel(title = "Dissolved Oxygen",
                          bslib::layout_column_wrap(
                            bslib::card( 
                              full_screen = TRUE,
                              style = "resize:both;",
                              bslib::card_header("Dissolved Oxygen")
                              # TODO Add content
                            ))),
         bslib::nav_panel(title = "Phosphorus",
                          bslib::layout_column_wrap(
                            bslib::card(
                              full_screen = TRUE
                              # TODO Add content
                            ),
                            bslib::card(
                              full_screen = TRUE
                              # TODO Add content
                            ))),
         bslib::nav_panel(title = "TSS",
                          bslib::layout_column_wrap(
                            bslib::card(
                              full_screen = TRUE,
                              fill = TRUE,
                              style = "resize:both;"
                              # TODO Add content
                            ))),
         bslib::nav_panel(title = "Turbidity",
                          bslib::layout_column_wrap(
                            bslib::card(
                              full_screen = TRUE,
                              fill = TRUE,
                              style = "resize:both;"
                              # TODO Add content
                            ))),
         bslib::nav_panel(title = "Bacteria",
                          bslib::layout_column_wrap(
                            bslib::card(
                              full_screen = TRUE,
                              fill = TRUE,
                              style = "resize:both;"
                              # TODO Add content
                            ))),
         bslib::nav_panel(title = "Ammonia",
                          bslib::layout_column_wrap(
                            bslib::card(
                              full_screen = TRUE,
                              fill = TRUE,
                              style = "resize:both;"
                              # TODO Add content
                            ))),
         bslib::nav_panel(title = "Date Ranges",
                          # TODO Add content
                          ),
         bslib::nav_panel(title = "BMPs",
                          bslib::layout_column_wrap(
                            bslib::card(
                               full_screen = TRUE,
                               fill = TRUE,
                               style = "resize:both;",
                               bslib::card_header("BMPs Table"),
                               # TODO Add content
                            ),
                          bslib::card(
                               full_screen = TRUE,
                               fill = TRUE,
                               style = "resize:both;",
                               bslib::card_header("BMPs Plot"),
                               # TODO Add content
                            ))),
         bslib::nav_panel(title = "Table"))
         
                   ),
  # Watersheds ----
  bslib::nav_panel(title = "Watersheds Map",
         bslib::navset_tab(id = "watersheds_map_id",
          bslib::nav_panel(title = "Watersheds Map"),
          bslib::nav_panel(title = "Watersheds Table")
                   
                   )),
  # River Restoration ----
  bslib::nav_panel(title = "River Restoration",
         bslib::navset_tab(
           id = "navset_tabs_river_rest",
         bslib::nav_panel(title = "Restoration",
                          bslib::card(
                            full_screen = TRUE,
                            title = app_inputs$RR$river_rest_ttl,
                            bslib::card_header(app_inputs$RR$river_rest_head),
                            status = "info",
                            width = "100%",
                            iframeUI("iframe_rest"),
                            bslib::card_footer(text_boxes$river_rest_ftn)
                          )),
         bslib::nav_panel(title = "Geomorphic Assessment",
                          bslib::card(
                            full_screen = TRUE,
                            title = app_inputs$RR$river_rest_geo_ttl,
                            bslib::card_header(app_inputs$RR$river_rest_geo_head),
                            status = "info",
                            width = "100%",
                            iframeUI("iframe_rest_geo"),
                            bslib::card_footer(text_boxes$river_rest_ftn)
                          ))
  )),
  # Landcover ----
  bslib::nav_panel(title = "Landcover",
                   shiny::fluidRow(
                     shiny::column(width = 4,
                                   style = "padding-right: 50px; padding-bottom: 10px;",
                                   # TODO Build from data
                                   shinyWidgets::pickerInput(width = '200px',
                                                             # build buttons for collective selection
                                                             multiple = FALSE,
                                                             inputId = "critpick",
                                                             label = "Critical Area", 
                                                             choices = c("Wetlands","Wildlife","Geologic Hazard","Aquifers"),
                                                             selected = "Wetlands"
                                   ),
                                   checkboxInput("corrected_checkbox", "Adjusted Acreage")
                     ),
                     column(width = 4,
                            # TODO add content
                            # valueBoxOutput("ag_acres")
                     ),
                     #column(1),
                     column(width = 4
                            # TODO add content
                            # valueBoxOutput("aquifer_ag_box"),
                            # valueBoxOutput("wetlands_box2"),
                            # valueBoxOutput("geo_haz_box"),
                            # valueBoxOutput("wildlife_box2")
                            
                     ))
                   
  ),
  # Guidance ----
  bslib::nav_panel(title = "Guidance",
                   
  ),
  # Nav additions ----
  bslib::nav_spacer(),
  #bslib::nav_spacer(),
  bslib::nav_menu(
    title = "Links",
    align = "right",
    bslib::nav_item(link_repo),
    bslib::nav_item(link_posit)
  )
  )
    
}


app_server <- function(input, output, session) {
  
  # Water Quality ----
  # Make reactive to save time when loading. Only loads when tab is clicked
  shiny::observeEvent(input$WQ_navset_tabs_id,
                      
                      if(input$WQ_navset_tabs_id == "Realtime Flows"){
                        
                        iframeServer("iframe_usgs_flow",
                                     url = app_inputs$WQ$usgs_flow_path,
                                     style = 'width:100vw;height:100vh;'
                        )
                        
                        iframeServer("iframe_wa_eco_flow",
                                     url = app_inputs$WQ$wa_eco_flow_path,
                                     style = 'width:90vw;height:90vh;')
                        
                        iframeServer("iframe_wa_eco_discharge",
                                     url = app_inputs$WQ$wa_eco_discharge_path,
                                     style = 'width:100vw;height:100vh;')
                        
                      }
                      
                      )
  
  
  # River Restoration ----
  # Make reactive to save time when loading. Only loads when tab is clicked
  shiny::observeEvent(input$navset_tabs_river_rest,
                      
                      if(input$navset_tabs_river_rest == "Restoration"){
                        iframeServer("iframe_rest",
                                     url = app_inputs$RR$river_rest_arcgis,
                                     style = 'width:90vw;height:90vh;')
                      } else if(input$navset_tabs_river_rest == "Geomorphic Assessment"){
                        iframeServer("iframe_rest_geo",
                                     url = app_inputs$RR$river_rest_geo_arcgis,
                                     style = 'width:90vw;height:90vh;')
                      }
  )
  
  
}


shiny::shinyApp(app_ui, app_server)
