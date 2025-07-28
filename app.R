load("R/sysdata.rda")
library(dplyr)
  
  # Set NULL to fix check
  Year <- NULL
  Date <- NULL
  No_BMPS <- NULL
  Name <- NULL
  
  link_repo <- shiny::tags$a(
    shiny::icon("github"), "Repository",
    href = app_inputs$repo,
    target = "_blank"
  )
  
  link_posit <- shiny::tags$a(
    shiny::icon("r-project"), "Posit",
    href = app_inputs$posit,
    target = "_blank"
  )
  
  theme = bslib::bs_theme(
    fg = "#654e18", primary = "#5E9300", info = "#2c3e50",
    success = "#2c3e50", font_scale = NULL, bg = "#fff"
  )
  
  options(highcharter.lang = list(thousandsSep = ","))
  
  app_ui <- function(request) {
    bslib::page_navbar(title = app_inputs$county_term,
                       theme = theme,
                       id = "navbar_id",
                       # Tucannon WQ ----                   
                       bslib::nav_panel(title = "Tucannon Water Quality",
                                        shiny::fluidRow(
                                          shinyWidgets::sliderTextInput(
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
                                                                           
                                                                           shiny::tags$div(
                                                                             class = "text-danger",
                                                                                 style = "margin-top: 1em; margin-bottom: 1em; display: flex; align-items: center;",
                                                                                 shiny::icon("info-circle", class = "me-2"),
                                                                                 shiny::tags$span(" The following content is independent of the data filters above.")
                                                                           ),
                                                                           bslib::page_fluid(
                                                                             bslib::layout_columns(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               bslib::card_header(app_inputs$WQ$usgs_flow_ttl),
                                                                               status = "info",
                                                                               # width = "100%",
                                                                               iframeUI("iframe_usgs_flow")
                                                                             ),
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               bslib::card_header(app_inputs$WQ$wa_eco_discharge_ttl),
                                                                               status = "info",
                                                                               height = "700px",
                                                                               iframeUI("iframe_wa_eco_discharge"),
                                                                             ),
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               bslib::card_header("State-wise Stations"),
                                                                               status = "info",
                                                                               # width = "100%",
                                                                               iframeUI("iframe_wa_eco_flow"),
                                                                             ),
                                                                             col_widths = c(12, 12, 12)
                                                                             )
                                                                           )
                                                          ),
                                                          bslib::nav_panel(title = paste0(app_inputs$gauge_location," DOE Gauge"),
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("Temperature"),
                                                                               hc_lineUI("hc_line_temp") %>%
                                                                                 shinycssloaders::withSpinner(
                                                                                   image = "https://raw.githubusercontent.com/daattali/shinycssloaders/master/inst/img/custom.gif")
                                                                             ),
                                                                             bslib::card( 
                                                                               full_screen = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("Stage Height"),
                                                                               hc_lineUI("hc_line_stage") %>%
                                                                                 shinycssloaders::withSpinner(
                                                                                   image = "https://raw.githubusercontent.com/daattali/shinycssloaders/master/inst/img/custom.gif")
                                                                             )
                                                                           )
                                                          ),
                                                          bslib::nav_panel(title = "Temperature",
                                                                             # First row: 3 cards
                                                                             bslib::layout_column_wrap(
                                                                               width = 1/3,
                                                                               bslib::card(full_screen = TRUE, 
                                                                                           hc_lineUI("hc_line_temp_an")),
                                                                               bslib::card(full_screen = TRUE, 
                                                                                           hc_boxUI("hc_box_temp")),
                                                                               bslib::card(full_screen = TRUE, 
                                                                                           hc_ts_wBMPsUI("hc_ts_temp"))
                                                                             ),
                                                                             # Info icon + soft red text
                                                                             shiny::tags$hr(style = "margin-bottom: 0.25em;"),
                                                                             shiny::tags$div(
                                                                               # class = "text-danger",
                                                                               class = "d-flex justify-content-center align-items-center text-danger",
                                                                               style = "margin-top: 0; margin-bottom: 0.5em;",
                                                                               # style = "margin-top: 0.5em; margin-bottom: 1em; display: flex; align-items: center;",
                                                                               shiny::icon("info-circle", class = "me-2"),
                                                                               shiny::tags$span("The following content is independent of the data filters above.")
                                                                             ),
                                                                             # Second row: 2 cards
                                                                             bslib::layout_column_wrap(
                                                                               width = 1/2,
                                                                               bslib::card(full_screen = TRUE,
                                                                                           by_summer),  # TODO Add content
                                                                               bslib::card(full_screen = TRUE,
                                                                                           by_month)   # TODO Add content
                                                                             )
                                                                           ),
                                                          bslib::nav_panel(title = "Dissolved Oxygen",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card( 
                                                                               full_screen = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("Dissolved Oxygen"),
                                                                               hc_ts_wBMPsUI("hc_ts_do")
                                                                             ))),
                                                          bslib::nav_panel(title = "Phosphorus",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               hc_ts_wBMPsUI("hc_ts_tphos")
                                                                             ),
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               hc_ts_wBMPsUI("hc_ts_ophos")
                                                                             ))),
                                                          bslib::nav_panel(title = "TSS",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               fill = TRUE,
                                                                               style = "resize:both;",
                                                                               hc_ts_wBMPsUI("hc_ts_tss")
                                                                             ))),
                                                          bslib::nav_panel(title = "Turbidity",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               fill = TRUE,
                                                                               style = "resize:both;",
                                                                               hc_ts_wBMPsUI("hc_ts_turb")
                                                                             ))),
                                                          bslib::nav_panel(title = "Bacteria",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               fill = TRUE,
                                                                               style = "resize:both;",
                                                                               hc_ts_wBMPsUI("hc_ts_bact")
                                                                             ))),
                                                          bslib::nav_panel(title = "Ammonia",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               fill = TRUE,
                                                                               style = "resize:both;",
                                                                               hc_ts_wBMPsUI("hc_ts_amm")
                                                                             ))),
                                                          bslib::nav_panel(title = "Date Ranges",
                                                                           DT::datatable(param_ranges)
                                                          ),
                                                          bslib::nav_panel(title = "BMPs",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               fill = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("BMPs Table"),
                                                                               bslib::card_body(height = '65vh',
                                                                                                DT::dataTableOutput("bmps_full_table", width = "90%"))
                                                                             ),
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               fill = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("BMPs Plot"),
                                                                               hc_bar_stackUI("hc_bar_bmps")
                                                                             ))),
                                                          bslib::nav_panel(title = "Table",
                                                                           DT::dataTableOutput("params_table")))
                                        
                       ),
                       # Touchet WQ ----
                       bslib::nav_panel(title = "Touchet Water Quality",
                                        shiny::fluidRow(
                                          shinyWidgets::sliderTextInput(
                                            inputId = "dateRange_2",
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
                                                                    inputId = "monthRange_2",
                                                                    label = "Filter by month", 
                                                                    choices = month.abb[c(1:12)],
                                                                    selected = month.abb[c(1:12)])
                                        ),
                                        bslib::navset_tab(id = "WQ_navset_tabs_id_2",
                                                          bslib::nav_panel(title = "Realtime Flows",
                                                                           
                                                                           shiny::tags$div(
                                                                             class = "text-danger",
                                                                             style = "margin-top: 1em; margin-bottom: 1em; display: flex; align-items: center;",
                                                                             shiny::icon("info-circle", class = "me-2"),
                                                                             shiny::tags$span(" The following content is independent of the data filters above.")
                                                                           ),
                                                                           bslib::page_fluid(
                                                                             bslib::layout_columns(
                                                                               bslib::card(
                                                                                 full_screen = TRUE,
                                                                                 bslib::card_header(app_inputs$WQ2$usgs_flow_ttl),
                                                                                 status = "info",
                                                                                 iframeUI("iframe_usgs_flow_2")
                                                                               ),
                                                                               bslib::card(
                                                                                 full_screen = TRUE,
                                                                                 title = app_inputs$WQ2$wa_eco_discharge_ttl1,
                                                                                 bslib::card_header(app_inputs$WQ2$wa_eco_discharge_ttl1),
                                                                                 status = "info",
                                                                                 height = "700px",
                                                                                 iframeUI("iframe_wa_eco_discharge_2_1"),
                                                                               ),
                                                                               bslib::card(
                                                                                 full_screen = TRUE,
                                                                                 title = app_inputs$WQ2$wa_eco_discharge_ttl2,
                                                                                 bslib::card_header(app_inputs$WQ2$wa_eco_discharge_ttl2),
                                                                                 status = "info",
                                                                                 height = "700px",
                                                                                 iframeUI("iframe_wa_eco_discharge_2_2"),
                                                                               ),
                                                                               bslib::card(
                                                                                 full_screen = TRUE,
                                                                                 title = app_inputs$WQ2$wa_eco_discharge_ttl3,
                                                                                 bslib::card_header(app_inputs$WQ2$wa_eco_discharge_ttl3),
                                                                                 status = "info",
                                                                                 height = "700px",
                                                                                 iframeUI("iframe_wa_eco_discharge_2_3"),
                                                                               ),
                                                                               bslib::card(
                                                                                 full_screen = TRUE,
                                                                                 bslib::card_header("State-wide Stations"),
                                                                                 status = "info",
                                                                                 iframeUI("iframe_wa_eco_flow_2")#
                                                                               ),
                                                                               col_widths = c(12, 12, 12, 12, 12)
                                                                             )
                                                                           )  
                                                          ),
                                                          bslib::nav_panel(title = paste0(app_inputs$gauge_location2," DOE Gauge"),
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card(
                                                                               full_screen = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("Temperature"),
                                                                               hc_lineUI("hc_line_temp_2") %>%
                                                                                 shinycssloaders::withSpinner(
                                                                                   image = "https://raw.githubusercontent.com/daattali/shinycssloaders/master/inst/img/custom.gif")
                                                                             ),
                                                                             bslib::card( 
                                                                               full_screen = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("Stage Height"),
                                                                               hc_lineUI("hc_line_stage_2") %>%
                                                                                 shinycssloaders::withSpinner(
                                                                                   image = "https://raw.githubusercontent.com/daattali/shinycssloaders/master/inst/img/custom.gif")
                                                                             )
                                                                           )
                                                          ),
                                                          bslib::nav_panel(title = "Temperature",
                                                                           # First row: 3 cards
                                                                           bslib::layout_column_wrap(
                                                                             width = 1/3,
                                                                             bslib::card(full_screen = TRUE, 
                                                                                         hc_lineUI("hc_line_temp_an_2")),
                                                                             bslib::card(full_screen = TRUE, 
                                                                                         hc_boxUI("hc_box_temp_2")),
                                                                             bslib::card(full_screen = TRUE, 
                                                                                         hc_ts_wBMPsUI("hc_ts_temp_2"))
                                                                           ),
                                                                           # Info icon + soft red text
                                                                           shiny::tags$hr(style = "margin-bottom: 0.25em;"),
                                                                           shiny::tags$div(
                                                                             # class = "text-danger",
                                                                             class = "d-flex justify-content-center align-items-center text-danger",
                                                                             style = "margin-top: 0; margin-bottom: 0.5em;",
                                                                             # style = "margin-top: 0.5em; margin-bottom: 1em; display: flex; align-items: center;",
                                                                             shiny::icon("info-circle", class = "me-2"),
                                                                             shiny::tags$span("The following content is independent of the data filters above.")
                                                                           ),
                                                                           # Second row: 2 cards
                                                                           bslib::layout_column_wrap(
                                                                             width = 1/2,
                                                                             bslib::card(full_screen = TRUE,
                                                                                         by_summer_2),  # TODO Add content
                                                                             bslib::card(full_screen = TRUE,
                                                                                         by_month_2)   # TODO Add content
                                                                           )
                                                          ),
                                                          bslib::nav_panel(title = "Dissolved Oxygen",
                                                                           bslib::layout_column_wrap(
                                                                             bslib::card( 
                                                                               full_screen = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("Dissolved Oxygen"),
                                                                               hc_ts_wBMPsUI("hc_ts_do_2")
                                                                             ))),
                                                          bslib::nav_panel(title = "Date Ranges",
                                                                           DT::datatable(param_ranges_2)
                                                          ))),
                       
                       # Watersheds ----
                       bslib::nav_panel(title = "Watersheds Map",
                                        bslib::navset_tab(id = "watersheds_map_id",
                                                          bslib::nav_panel(title = "Watersheds Map",
                                                                           bslib::layout_columns(
                                                                             bslib::card(fill = TRUE,
                                                                                  full_screen = TRUE,
                                                                                  height = 600,
                                                                                  style = "resize:both;height: 100%;",
                                                                                  id = "far_left",
                                                                                  bslib::card_header(
                                                                                    shinyWidgets::pickerInput(
                                                                                      width = '300px',
                                                                                      options = shinyWidgets::pickerOptions(
                                                                                        `count-selected-text` = "{0} Sites Selected",
                                                                                        container = "body",
                                                                                        actionsBox = TRUE,
                                                                                        liveSearch = TRUE,
                                                                                        selectedTextFormat = 'count > 1'
                                                                                      ),
                                                                                      # build buttons for collective selection
                                                                                      multiple = TRUE,
                                                                                      inputId = "watersheds",
                                                                                      label = app_inputs$WS$HUC_name,
                                                                                      choices = huc$Name,
                                                                                      choicesOpt = list(subtext = huc$HUC12)
                                                                                    )
                                                                                  ), 
                                                                                  bslib::card_body(
                                                                                    leaflet::leafletOutput("leafmap",
                                                                                                           height = 400)
                                                                                  )
                                                                             ),
                                                                             bslib::card(
                                                                               shiny::uiOutput("acres_box"),
                                                                               bslib::card_header(
                                                                                 # shinyWidgets::checkboxGroupButtons(size = "xs",
                                                                                 #                      inputId = "bmps_active",
                                                                                 #                      selected = "Yes",
                                                                                 #                      label = "Active BMPs",
                                                                                 #                      choices = c("Yes", "No"),
                                                                                 #                      checkIcon = list(
                                                                                 #                        yes = shiny::tags$i(class = "fa fa-check-square",
                                                                                 #                                     style = "color: #84563C"),
                                                                                 #                        no = shiny::tags$i(class = "fa fa-square-o",
                                                                                 #                                    style = "color: #84563C")
                                                                                 #                      )
                                                                                 # )
                                                                               ),
                                                                               bslib::card_body(min_height = '100px',
                                                                                         shiny::uiOutput("bmps_box",
                                                                                                         fill = "container"))
                                                                             ),
                                                                             bslib::card(fill = TRUE,
                                                                                         height = 600,
                                                                                         full_screen = TRUE,
                                                                                         style = "resize:both;",
                                                                                         bslib::card_header(
                                                                                           shinyWidgets::pickerInput(
                                                                                             width = '250px',
                                                                                             options = shinyWidgets::pickerOptions(
                                                                                               `count-selected-text` = "{0} Categories Selected",
                                                                                               container = "body",
                                                                                               actionsBox = TRUE,
                                                                                               liveSearch = TRUE,
                                                                                               selectedTextFormat = 'count > 1'
                                                                                             ),
                                                                                             # build buttons for collective selection
                                                                                             multiple = TRUE,
                                                                                             inputId = "selectInput",
                                                                                             label = "Category",
                                                                                             choices = c(
                                                                                               "BMPs",
                                                                                               unique(huc_combo$source)
                                                                                             )
                                                                                           )
                                                                                           
                                                                                         ),
                                                                                         mod_watershedUI("watershed_tables")
                                                                                         )
                                                                             
                                                                           )
                                                                           
                                                                           
                                                                           ),
                                                          bslib::nav_panel(title = "Watersheds Table",
                                                                           DT::dataTableOutput("selectedHUC"))
                                                          
                                        )),
                       # River Restoration ----
                       bslib::nav_panel(title = "River Restoration",
                                        bslib::navset_tab(
                                          id = "navset_tabs_river_rest",
                                          bslib::nav_panel(title = "Tucannon Restoration",
                                                           bslib::card(
                                                             full_screen = TRUE,
                                                             # bslib::card_header(app_inputs$RR$river_rest_head),
                                                             status = "info",
                                                             width = "100%",
                                                             iframeUI("iframe_rest"),
                                                             bslib::card_footer(text_boxes$river_rest_ftn)
                                                           )),
                                          bslib::nav_panel(title = "Tucannon Geomorphic Assessment",
                                                           bslib::card(
                                                             full_screen = TRUE,
                                                             # bslib::card_header(app_inputs$RR$river_rest_geo_head),
                                                             status = "info",
                                                             width = "100%",
                                                             iframeUI("iframe_rest_geo"),
                                                             bslib::card_footer(text_boxes$river_rest_ftn)
                                                           )),
                                          bslib::nav_panel(title = "Touchet Geomorphic Assessment",
                                                           bslib::card(
                                                             full_screen = TRUE,
                                                             # bslib::card_header(app_inputs$RR$river_rest_geo_head2),
                                                             status = "info",
                                                             width = "100%",
                                                             iframeUI("iframe_rest_geo_2"),
                                                             bslib::card_footer(text_boxes$river_rest_ftn)
                                                           ))
                                        )),
                       # Private Ag Land Use ----
                       bslib::nav_panel(title = "Private Ag Land Use",
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
                                                        shiny::checkboxInput("corrected_checkbox", "Adjusted Acreage")
                                          ),
                                          shiny::column(width = 4,
                                                 shinydashboard::valueBoxOutput("ag_acres")
                                          ),
                                          #column(1),
                                          shiny::column(width = 4,
                                                        shinydashboard::valueBoxOutput("lc_vbox")
                                          )),
                                        shiny::uiOutput("lc_tfl"),
                                        shiny::hr(),
                                        # shinybusy::add_busy_spinner(spin = "fading-circle", color = "#5E9300"),
                                        shiny::actionButton("landcover_report", "Generate Report", class = "btn btn-primary"),
                                        shiny::uiOutput("landcover_download"),
                                        shiny::br()
                                        # )
                                        
                                        
                       ),
                       # USDA Land Use ----
                       bslib::nav_panel(title = "USDA Land Use",
                                        shiny::fluidRow(
                                          shiny::column(width = 4,
                                                        style = "padding-right: 50px; padding-bottom: 10px;",
                                                        # TODO Build from data
                                                        shinyWidgets::pickerInput(width = '200px',
                                                                                  # build buttons for collective selection
                                                                                  multiple = FALSE,
                                                                                  inputId = "critpick_usda",
                                                                                  label = "Critical Area",
                                                                                  choices = c("Wetlands","Wildlife","Geologic Hazard","Aquifers"),
                                                                                  selected = "Wetlands"
                                                        )
                                          ),
                                          shiny::column(width = 4,
                                                        shinydashboard::valueBoxOutput("huc_acres")
                                          ),
                                          #column(1),
                                          shiny::column(width = 4,
                                                        shinydashboard::valueBoxOutput("lc_vbox_usda")
                                          )),
                                        shiny::uiOutput("lc_tfl_usda"),
                                        shiny::hr(),
                                        shiny::tags$div(
                                          class = "text-danger",
                                          style = "margin-top: 1em; margin-bottom: 1em; display: flex; align-items: center;",
                                          shiny::icon("info-circle", class = "me-2"),
                                          shiny::tags$span(" The following content is independent of the data filters above.")
                                        ),
                                        shiny::uiOutput("landuse_yr_comp")



                       ), 
                       # NRCS ----
                       bslib::nav_panel(title = "NRCS Practices",
                                        shiny::fluidRow(
                                          shiny::column(12,
                                                        shinyWidgets::pickerInput(
                                                          inputId = "land_use",
                                                          label = "Select Land Use(s):",
                                                          choices = sort(unique(nrcs_data$land_use)),
                                                          selected = "Crop",
                                                          multiple = TRUE,
                                                          options = shinyWidgets::pickerOptions(
                                                            actionsBox = TRUE,
                                                            liveSearch = TRUE,
                                                            selectedTextFormat = "count > 3",
                                                            noneSelectedText = "No land use selected"
                                                          ),
                                                          width = "100%"
                                                        )
                                          )
                                        ),
                                        
                                        shiny::br(),
                                        
                                        shiny::uiOutput("value_cards"),
                                        
                                        shiny::br(),
                                        
                                        shiny::fluidRow(
                                          shiny::column(12,
                                                        highcharter::highchartOutput("time_series", height = "400px")
                                          )
                                        ),
                                        
                                        shiny::br(),
                                        
                                        shiny::fluidRow(
                                          shiny::column(12,
                                                        DT::DTOutput("practice_table")
                                          )
                                        )
             
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
    
    # Fix for check
    X <- NULL
    fid_1 <- NULL
    TNMID <- NULL
    VPUID <- NULL
    OID_ <- NULL
    HUC12 <- NULL
    Type <- NULL
    Active <- NULL
    V1 <- NULL
    `Primary Land Use` <- NULL
    `Private Acres` <- NULL
    . <- NULL
    
    # Filtered params ----
    
    rve_params <- shiny::reactive({
      filter_data_bytime(params, 
                         year_range = input$dateRange, 
                         month_vals = input$monthRange) %>% 
        dplyr::left_join(bmps_byyear %>%
                           dplyr::filter(
                             Year >= input$dateRange[1] &  
                               Year <= input$dateRange[2]
                           ), by = "Year")
    })
    
    # Filtered touchet params ----
    
    rve_params_2 <- shiny::reactive({
      filter_data_bytime(params_2, 
                         year_range = input$dateRange_2, 
                         month_vals = input$monthRange_2) %>% 
        # dplyr::left_join(bmps_byyear, by = "Year")
        dplyr::full_join(bmps_byyear %>%
                           dplyr::filter(
                            Year >= input$dateRange_2[1] &  
                              Year <= input$dateRange_2[2]
                           ), 
                         by = "Year")
    })
    
    # Filtered BMPs ----
    # TODO see if this should be corrected, it seem odd to use Date on the left and year on the right
    rve_year_bmps <- shiny::reactive({
      
      bmps %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(Date=paste0(Year,"-05-04")
        ) %>%
        dplyr::filter(
          lubridate::year(lubridate::date(Date)) >= input$dateRange[1] &  
            lubridate::year(lubridate::date(Date)) <= input$dateRange[2]
        ) 
      
    })  
    
    rve_year_bmps_2 <- shiny::reactive({
      
      bmps %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(Date=paste0(Year,"-05-04")
        ) %>%
        dplyr::filter(
          lubridate::year(lubridate::date(Date)) >= input$dateRange_2[1] &  
            lubridate::year(lubridate::date(Date)) <= input$dateRange_2[2]
        ) 
      
    })  
    
    bmps_year <- shiny::reactive({
      rve_params() %>%
        dplyr::distinct(Year, No_BMPS) %>%
        dplyr::mutate(No_BMPS = ifelse(is.na(No_BMPS), 0, No_BMPS),
                      Year = as.double(Year))
      
    })
    
    bmps_year_2 <- shiny::reactive({
      rve_params_2() %>%
        dplyr::distinct(Year, No_BMPS) %>%
        dplyr::mutate(No_BMPS = ifelse(is.na(No_BMPS), 0, No_BMPS),
                      Year = as.double(Year))
      
    })
    
    # Stations ----
    # Marengo
    rve_station_water <- shiny::reactive({
      
      filter_data_bytime(station_water, 
                         year_range = input$dateRange, 
                         month_vals = input$monthRange)
      
    })
    
    rve_station_stage <- shiny::reactive({
      
      filter_data_bytime(station_stage, 
                         year_range = input$dateRange, 
                         month_vals = input$monthRange)
      
    })
    
    # Bolles
    rve_station_water_2 <- shiny::reactive({
      
      filter_data_bytime(station_water_2, 
                         year_range = input$dateRange_2, 
                         month_vals = input$monthRange_2)
      
    })
    
    rve_station_stage_2 <- shiny::reactive({
      
      filter_data_bytime(station_stage_2, 
                         year_range = input$dateRange_2, 
                         month_vals = input$monthRange_2)
      
    })
    
    # Tucannon Water Quality ----
    # Make reactive to save time when loading. Only loads when tab is clicked
    shiny::observeEvent(input$WQ_navset_tabs_id,
                        
                        if(input$WQ_navset_tabs_id == "Realtime Flows"){
                          
                          iframeServer("iframe_usgs_flow",
                                       url = app_inputs$WQ$usgs_flow_path,
                                       style = 'width:90vw;height:100vh;border: none;margin-left: auto;margin-right: auto;'
                          )
                          
                          iframeServer("iframe_wa_eco_flow",
                                       url = app_inputs$WQ$wa_eco_flow_path,
                                       style = 'width:92vw;height:90vh;')
                          
                          iframeServer("iframe_wa_eco_discharge",
                                       url = app_inputs$WQ$wa_eco_discharge_path,
                                       style = 'width:50vw;height:100vh;border: none;margin-left: auto;margin-right: auto;')
                          
                        } else if(input$WQ_navset_tabs_id == "Marengo DOE Gauge"){
                          
                          hc_lineServer("hc_line_temp",
                                            data = rve_station_water, # only want to send the values, not the reactive version
                                            obs_name = "deg C",
                                            y_lbl = "Temperature deg C",
                                            title = "35B150-Marengo Temperature")
                          
                          hc_lineServer("hc_line_stage",
                                        data = rve_station_stage, # only want to send the values, not the reactive version
                                        obs_name = "(ft)",
                                        y_lbl = "Stage Ht. (ft)",
                                        title = "35B150-Marengo Stage Ht. (ft)")
                          
                        } else if(input$WQ_navset_tabs_id == "Temperature"){
                          
                          temp_by_year <- shiny::reactive({
                            filter_params(data = rve_params(),
                                            param_vals = "Temperature, water",
                                            group_vars = c("Param", "Date", "Year", "Month", "Units"),
                                            arr_vars = c("Year", "Month"),
                                            res_gt0 = TRUE) %>%
                              dplyr::select(-Date) %>% 
                              dplyr::mutate(Date = lubridate::ymd(as.character(Year), truncated = 4))
                          })
                          
                          temp_by_year2 <- shiny::reactive({
                            filter_params(data = rve_params(),
                                          param_vals = "Temperature, water",
                                          group_vars = c("Param", "Date", "Year", "Month", "Units"),
                                          arr_vars = c("Year", "Month"),
                                          res_gt0 = TRUE) %>%
                              dplyr::select(-Date) %>% 
                              dplyr::mutate(Date = Year)
                          })
                          
                          temp_av_year <- shiny::reactive({
                            filter_params(data = temp_by_year(),
                                          param_vals = "Temperature, water",
                                          group_vars = c("Date", "Year"),
                                          arr_vars = c("Year"),
                                          round_dig = 2)
                          })
                          
                          hc_lineServer("hc_line_temp_an",
                                        data = temp_av_year, # only want to send the values, not the reactive version
                                        obs_name = "deg C",
                                        y_lbl = "Temperature deg C",
                                        x_lbl = "Year",
                                        title = "Annual Average")
                          
                          hc_boxServer("hc_box_temp",
                                       data = temp_by_year2, # only want to send the values, not the reactive version
                                       x_var = "Date",
                                       y_var = "Result",
                                       x_lbl = "Year",
                                       y_lbl = "Temperature deg C",
                                       obs_name = "deg C",
                                       bmp_lbl = "BMPs/Year",
                                       title = "Quartiles",
                                       bmp_dat = bmps_year)

                          hc_ts_wBMPsServer("hc_ts_temp",
                                            data = temp_by_year, # only want to send the values, not the reactive version
                                            param = "Temperature, water",
                                            obs_name = "deg C",
                                            y_lbl = "Temperature deg C",
                                            x_lbl = "Year",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Annual Average (Scatter)",
                                            bmp_dat = bmps_year)
                        } else if(input$WQ_navset_tabs_id == "Dissolved Oxygen"){
                          hc_ts_wBMPsServer("hc_ts_do",
                                            data = rve_params, # only want to send the values, not the reactive version
                                            param = "Dissolved Oxygen",
                                            obs_name = "Dissolved Oxygen mg/L",
                                            y_lbl = "Dissolved Oxygen mg/L",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Powers Road",
                                            bmp_dat = bmps_year,
                                            href = c("TMDL" = 8))
                        } else if(input$WQ_navset_tabs_id == "Phosphorus"){
                          hc_ts_wBMPsServer("hc_ts_tphos",
                                            data = rve_params, # only want to send the values, not the reactive version
                                            param = "Total Phosphorus",
                                            obs_name = "mg/L",
                                            y_lbl = "Total Phos. mg/L",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Total Phosphorus",
                                            bmp_dat = bmps_year)
                          hc_ts_wBMPsServer("hc_ts_ophos",
                                            data = rve_params, # only want to send the values, not the reactive version
                                            param = "Ortho-Phosphate",
                                            obs_name = "mg/L",
                                            y_lbl = "Orth-Phosphate mg/L",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Ortho-Phosphate",
                                            bmp_dat = bmps_year)
                        } else if(input$WQ_navset_tabs_id == "TSS"){
                          hc_ts_wBMPsServer("hc_ts_tss",
                                            data = rve_params, # only want to send the values, not the reactive version
                                            param = "Total Suspended Solids",
                                            obs_name = "mg/L",
                                            y_lbl = "TSS mg/L",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Total Suspended Solids",
                                            bmp_dat = bmps_year)
                        } else if(input$WQ_navset_tabs_id == "Turbidity"){
                          hc_ts_wBMPsServer("hc_ts_turb",
                                            data = rve_params, # only want to send the values, not the reactive version
                                            param = "Turbidity",
                                            obs_name = "NTU",
                                            y_lbl = "Turbidity NTU",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Turbidity",
                                            bmp_dat = bmps_year)
                        } else if(input$WQ_navset_tabs_id == "Bacteria"){
                          hc_ts_wBMPsServer("hc_ts_bact",
                                            data = rve_params, # only want to send the values, not the reactive version
                                            param = c("E. coli","Fecal Coliform"),
                                            obs_name = "CFU/100mL",
                                            y_lbl = "CFU/100mL",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Bacteria",
                                            bmp_dat = bmps_year)
                        } else if(input$WQ_navset_tabs_id == "Ammonia"){
                          hc_ts_wBMPsServer("hc_ts_amm",
                                            data = rve_params, # only want to send the values, not the reactive version
                                            param = "Ammonia",
                                            obs_name = "mg/L",
                                            y_lbl = "NH3 mg/L",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Ammonia",
                                            bmp_dat = bmps_year)
                        } else if(input$WQ_navset_tabs_id == "BMPs"){
                          hc_bar_stackServer("hc_bar_bmps",
                                            data = rve_year_bmps, # only want to send the values, not the reactive version
                                            x_var = "Year",
                                            y_var = "n",
                                            group_var = "Project",
                                            x_lbl = "Year",
                                            y_lbl = "Count",
                                            use_n = TRUE)
                        }
                        
                        
    )
    
    # Touchet Water Quality ----
    shiny::observeEvent(input$WQ_navset_tabs_id_2,
                        
                        if(input$WQ_navset_tabs_id_2 == "Realtime Flows"){
                          
                          iframeServer("iframe_usgs_flow_2",
                                       url = app_inputs$WQ2$usgs_flow_path,
                                       style = 'width:90vw;height:100vh;border: none;margin-left: auto;margin-right: auto;'
                          )
                          
                          iframeServer("iframe_wa_eco_flow_2",
                                       url = app_inputs$WQ$wa_eco_flow_path, # state wide so can duplicate here
                                       style = 'width:92vw;height:90vh;')

                          iframeServer("iframe_wa_eco_discharge_2_1",
                                       url = app_inputs$WQ2$wa_eco_discharge_path1,
                                       style = 'width:50vw;height:100vh;border: none;margin-left: auto;margin-right: auto;')
                          
                          iframeServer("iframe_wa_eco_discharge_2_2",
                                       url = app_inputs$WQ2$wa_eco_discharge_path2,
                                       style = 'width:50vw;height:100vh;border: none;margin-left: auto;margin-right: auto;')
                          
                          iframeServer("iframe_wa_eco_discharge_2_3",
                                       url = app_inputs$WQ2$wa_eco_discharge_path3,
                                       style = 'width:50vw;height:100vh;border: none;margin-left: auto;margin-right: auto;')
                          
                        }  else if(input$WQ_navset_tabs_id_2 == "Bolles DOE Gauge"){
                          
                          hc_lineServer("hc_line_temp_2",
                                        data = rve_station_water_2, # only want to send the values, not the reactive version
                                        obs_name = "deg C",
                                        y_lbl = "Temperature deg C",
                                        title = "32B100-Bolles Temperature")
                          
                          hc_lineServer("hc_line_stage_2",
                                        data = rve_station_stage_2, # only want to send the values, not the reactive version
                                        obs_name = "(ft)",
                                        y_lbl = "Stage Ht. (ft)",
                                        title = "32B100-Bolles Stage Ht. (ft)")
                          
                        } else if(input$WQ_navset_tabs_id_2 == "Temperature"){
                          
                          temp_by_year <- shiny::reactive({
                            filter_params(data = rve_params_2(),
                                          param_vals = "Temperature, water",
                                          group_vars = c("Param", "Date", "Year", "Month", "Units"),
                                          arr_vars = c("Year", "Month"),
                                          res_gt0 = TRUE) %>%
                              dplyr::select(-Date) %>% 
                              dplyr::mutate(Date = lubridate::ymd(as.character(Year), truncated = 4))
                          })
                          
                          temp_by_year2 <- shiny::reactive({
                            filter_params(data = rve_params_2(),
                                          param_vals = "Temperature, water",
                                          group_vars = c("Param", "Date", "Year", "Month", "Units"),
                                          arr_vars = c("Year", "Month"),
                                          res_gt0 = TRUE) %>%
                              dplyr::select(-Date) %>% 
                              dplyr::mutate(Date = Year,
                                            Date = factor(Date, levels = c(min(.data$Year)):max(.data$Year))) %>% 
                              tidyr::complete(Date, fill = list(Result = NA_real_)) %>% 
                              dplyr::mutate(Date = as.numeric(as.character(Date)))
                          })
                          
                          temp_av_year <- shiny::reactive({
                            filter_params(data = temp_by_year(),
                                          param_vals = "Temperature, water",
                                          group_vars = c("Date", "Year"),
                                          arr_vars = c("Year"),
                                          round_dig = 2)
                          })
                          
                          hc_lineServer("hc_line_temp_an_2",
                                        data = temp_av_year, # only want to send the values, not the reactive version
                                        obs_name = "deg C",
                                        y_lbl = "Temperature deg C",
                                        x_lbl = "Year",
                                        title = "Annual Average")
                          
                          hc_boxServer("hc_box_temp_2",
                                       data = temp_by_year2, # only want to send the values, not the reactive version
                                       x_var = "Date",
                                       y_var = "Result",
                                       x_lbl = "Year",
                                       y_lbl = "Temperature deg C",
                                       obs_name = "deg C",
                                       bmp_lbl = "BMPs/Year",
                                       title = "Quartiles",
                                       bmp_dat = bmps_year_2)
                          
                          hc_ts_wBMPsServer("hc_ts_temp_2",
                                            data = temp_by_year, # only want to send the values, not the reactive version
                                            param = "Temperature, water",
                                            obs_name = "deg C",
                                            y_lbl = "Temperature deg C",
                                            x_lbl = "Year",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Annual Average (Scatter)",
                                            bmp_dat = bmps_year_2)
                        } else if(input$WQ_navset_tabs_id_2 == "Dissolved Oxygen"){
                          hc_ts_wBMPsServer("hc_ts_do_2",
                                            data = rve_params_2, # only want to send the values, not the reactive version
                                            param = "Dissolved Oxygen",
                                            obs_name = "Dissolved Oxygen mg/L",
                                            y_lbl = "Dissolved Oxygen mg/L",
                                            bmp_lbl = "BMPs/Year",
                                            title = "Touchet River", # TODO update if only selecting one location
                                            bmp_dat = bmps_year_2,
                                            href = c("TMDL" = 9.5))
                        }
    )
    
    # Watershed Map ----
    # only load this page when selected to quicken startup
    shiny::observeEvent(input$navbar_id,
                        
    if(input$navbar_id == "Watersheds Map"){
      
      filtered_huc <- shiny::reactive({
        
        huc %>%
          dplyr::filter(Name %in% input$watersheds)
        
      })
      
      filtered_huc_combo <- shiny::reactive({
        
        huc_combo %>%
          dplyr::filter(Name %in% input$watersheds)
        
      })
      
      clicked_HUC <- shiny::reactiveVal(character(0))
      
      shiny::observe({
        
        if(length(input$watersheds)==0) {
          clicked_HUC(character(0))
        }
        
      })
      
      
      
      output$selectedHUC <- DT::renderDataTable({

        shiny::req(nrow(filtered_huc())>=1)
        
        DT::datatable(height = 900,
                      rownames = FALSE,
                      data= filtered_huc() %>% 
                        sf::st_drop_geometry() %>%
                        dplyr::select(Name, HUC12,HUC_Acres) %>% 
                        dplyr::mutate(HUC_Acres = round(HUC_Acres, 0)),
                      extensions = 'Buttons',
                      filter = 'top',
                      options = list(
                        lengthMenu=list(c(25,50,100,-1),c("25","50","100","All")),
                        pageLength = -1,
                        dom = 'Blfrtip',
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        ) 
        
        
      })
      
      foundational_map <- shiny::reactive({
        # Hydrography layer options
        
        leaflet::leaflet(options = leaflet::leafletOptions(attributionControl = FALSE)) %>%
          leaflet::addTiles() %>%
          leaflet::setView(lat = 46.29979,
                           lng = -118.02230,
                           zoom = 9) %>%
          leaflet::addWMSTiles(
            baseUrl = "https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WMSServer?",
            layers = "0",
            options = leaflet::WMSTileOptions(
              format = "image/png32",
              version = "1.3.0",
              minZoom = 3,
              maxZoom = 16,
              transparent = TRUE
            ),
            
            group = "Waterways"
          ) %>%
          leaflet::addPolygons(
            data = huc,
            group = "watersheds",
            layerId = huc$Name,
            color = "black",
            weight = 1,
            highlight = leaflet::highlightOptions(
              weight = 3,
              fillOpacity = 0.2,
              color = "#545c45",
              fillColor = "#2c3e50",
              opacity = 1.0,
              bringToFront = TRUE,
              sendToBack = TRUE
            ),
            # # Add label info when mouseover
            label = ~ Name,
            labelOptions = leaflet::labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "14px",
              direction = "auto"
            )
          ) %>%
          leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
          leaflet::addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
          leaflet::addProviderTiles("Esri.NatGeoWorldMap", group = "Topo") %>%
          leaflet::addProviderTiles("OpenStreetMap", group = "Street") %>%
          leaflet.extras::addSearchOSM() %>%
          leaflet.extras::addResetMapButton()
        
        
      })
      
      output$leafmap <- leaflet::renderLeaflet({
        
        pal <- leaflet::colorFactor(palette = c("goldenrod", "#84563C"), 
                                    domain = bmps$Active)
        
        foundational_map()
        
      })
      
      shiny::observeEvent(input$leafmap_shape_click,{
        
        shiny::req(input$leafmap_shape_click$group  %in% c("watersheds2","watersheds"))
        
        # capture the info of the clicked polygon
        click <- input$leafmap_shape_click
        
        # subset to clicked
        clicked_HUC_data <- unique(as.character(huc$Name[huc$Name == click$id]))
        
        selected_update <- c(input$watersheds,clicked_HUC())
        
        # Check if the name already exists in clicked_HUC
        if (clicked_HUC_data  %in% selected_update) {
          # If it exists, remove it from clicked_HUC
          clicked_HUC(selected_update[selected_update !=  clicked_HUC_data])
        } else {
          # If it doesn't exist, append it to clicked_HUC
          # Store in reactive val
          clicked_HUC(c(clicked_HUC_data, clicked_HUC()))
        }
        
        shinyWidgets::updatePickerInput(session, 
                                        "watersheds", 
                                        selected = clicked_HUC())
        
        
      })
      
      shiny::observe({
        
        pal <- leaflet::colorFactor(palette = c("#84563C", "goldenrod"),
                                    domain = bmps$Active)
        
        pal2 <- leaflet::colorFactor(palette = c( "#00FF00",
                                                  "#0000FF",
                                                  "#FFA500",
                                                  "#FFFF00",
                                                  "#808080",
                                                  "#000000"),
                                     domain = wetlands$WETLAND_TY)
        
        # pal3 <- leaflet::colorFactor(palette = c( "#00FF00",
        #                                           "#0000FF",
        #                                           "#FFA500",
        #                                           "#FFFF00",
        #                                           "#808080"),
        #                              domain = geo_hazard$frphrtd)
        
        map <- leaflet::leafletProxy("leafmap") %>%
          leaflet::clearMarkers() %>%
          leaflet::clearControls() %>%
          leaflet::clearGroup("bmp_layer") %>%
          leaflet::clearGroup("watersheds2") %>%
          leaflet::removeControl(layerId = "bmp_layer") %>%
          leaflet::addMapPane("ames_points", zIndex = 490) %>% # shown below ames_circles
          leaflet::addMapPane("ames_watersheds_selected", zIndex = 410) %>% # shown above ames_lines
          leaflet::addMapPane("ames_watersheds", zIndex = 400) %>% # shown below ames_circles
          leaflet::addPolygons(
            data = wetlands,
            group = "wetlands",
            label = ~ ACRES,
            fillColor =  ~ pal2(WETLAND_TY),
            color =  ~ pal2(WETLAND_TY),
            highlight = leaflet::highlightOptions(weight = 3, color = ~
                                                    pal2(WETLAND_TY)),
            popup = ~ leafpop::popupTable(wetlands)
          ) %>%
          # leaflet::addPolygons(
          #   data = geo_hazard,
          #   group = "geo_hazard",
          #   label = ~ round(Acrs_n_, 0),
          #   fillColor =  ~ pal3(frphrtd),
          #   color =  ~ pal3(frphrtd),
          #   highlight = leaflet::highlightOptions(weight = 3, color = ~
          #                                           pal3(frphrtd)),
          #   popup = ~ leafpop::popupTable(geo_hazard)
          # ) %>%
          leaflet::addPolygons(
            data = huc,
            group = "watersheds",
            layerId = huc$Name,
            color = "black",
            weight = 1,
            highlight = leaflet::highlightOptions(
              weight = 3,
              fillOpacity = 0.2,
              color = "#545c45",
              fillColor = "#2c3e50",
              opacity = 1.0,
              bringToFront = TRUE,
              sendToBack = TRUE
            ),
            # # Add label info when mouseover
            label = ~ Name,
            labelOptions = leaflet::labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "14px",
              direction = "auto"
            )
          )   %>%
          leaflet::addPolygons(
            data = filtered_huc(),
            group = "watersheds2",
            options = leaflet::pathOptions(pane = "ames_watersheds"),
            layerId = filtered_huc()$Name,
            color = "red",
            weight = 1,
            highlight = leaflet::highlightOptions(
              weight = 3,
              fillOpacity = 0.2,
              color = "purple",
              fillColor = "#6a0d83",
              opacity = 1.0,
              bringToFront = TRUE,
              sendToBack = TRUE
            ),
            # # Add label info when mouseover
            label = paste0("", filtered_huc()$Name),
            labelOptions = leaflet::labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "14px",
              direction = "auto"
            )
          )  %>%
          leaflet::addMarkers(
            data = stations,
            group = "WQStation",
            label = stations$Name,
            labelOptions = leaflet::labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "14px",
              direction = "auto"
            ),
            options = leaflet::pathOptions(pane = "ames_points")
          ) %>%
          leaflet::addPolygons(
            data = bmps %>% dplyr::filter(Type == "SHAPE") ,
            group = "BMP",
            options = leaflet::pathOptions(pane = "ames_points"),
            #label = ~CntrctN,
            popup = ~ leafpop::popupTable(feature.id = FALSE, bmps, row.numbers = FALSE),
            fillColor = ~ pal(Active),
            fillOpacity = 1,
            stroke = TRUE,
            color = "black",
            label = ~ Project,
            weight = 1,
            highlightOptions = leaflet::highlightOptions(
              color = "red",
              weight = 2,
              bringToFront = TRUE
            )
          ) %>%
          leaflet::addPolylines(
            data = sf::st_zm(bmps %>% dplyr::filter(Type == "LINE")),
            group = "BMP",
            options = leaflet::pathOptions(pane = "ames_points"),
            fillColor = ~ pal(Active),
            popup = ~ leafpop::popupTable(feature.id = FALSE, bmps, row.numbers = FALSE),
            fillOpacity = 1,
            stroke = TRUE,
            color = "goldenrod",
            opacity = 1,
            weight = 3,
            label = ~ Project,
            highlightOptions = leaflet::highlightOptions(
              color = "red",
              weight = 2,
              bringToFront = TRUE
            )
          ) %>%
          leaflet::addCircleMarkers(
            data = bmps %>% dplyr::filter(Type == "POINT"),
            group = "BMP",
            options = leaflet::pathOptions(pane = "ames_points"),
            fillColor = ~ pal(Active),
            popup = ~ leafpop::popupTable(feature.id = FALSE, bmps, row.numbers = FALSE),
            fillOpacity = 1,
            stroke = TRUE,
            color = "black",
            weight = 1,
            label = ~ Project
          ) %>%
          leaflet.extras::addSearchGoogle() %>%
          leaflet::addLayersControl(
            overlayGroups = c("Waterways", "BMP", "watersheds", "WQStation", "wetlands"#, "geo_hazard"
                              ),
            baseGroups = c("Topo", "Imagery", "Dark", "Street")
          ) %>%
          leaflet.extras::addFullscreenControl() %>%
          leafem::addMouseCoordinates() %>%
          leaflet::addLegend(
            layerId = "bmp_layer",
            group = "BMP",
            labels = c("Yes", "No"),
            "bottomright",
            pal = pal,
            values = c("Yes", "No"),
            title = "BMP is Active",
            opacity = 1
          ) %>%
          leaflet::addLegend(
            layerId = "wetlands_layer",
            pal = pal2,
            group = "wetlands",
            values = unique(wetlands$WETLAND_TY),
            labels = unique(wetlands$WETLAND_TY),
            "bottomright",
            title = "Wetlands",
            opacity = 1
          ) %>%
          # leaflet::addLegend(
          #   layerId = "geo_hazard_layer",
          #   pal = pal3,
          #   group = "geo_hazard",
          #   values = unique(geo_hazard$frphrtd),
          #   labels = unique(geo_hazard$frphrtd),
          #   "bottomright",
          #   title = "geo_hazard",
          #   opacity = 1
          # ) %>%
          leaflet::hideGroup(c("Waterways", "WQStation", "BMP", "wetlands"#, "geo_hazard"
                               )) %>% 
          suppressWarnings()
        
        
        map <- map %>%
          leaflet::clearControls()
        
      })
      
      
      output$acres_box <- shiny::renderUI({
        
        bslib::value_box(
          title = "HUC Acres",
          value = scales::comma(round(sum(filtered_huc()$HUC_Acres), digits = 0)),
          shiny::p(glue::glue("{length(unique(filtered_huc()$Name))} HUCs selected")),
          shiny::p(app_inputs$WS$county_huc),
          shiny::p(paste0(app_inputs$region, " & ", app_inputs$region2)),
          full_screen = TRUE,
          theme = "success"
        )
        
        
      })
      
      # reactive BMPs
      rve_bmps <- shiny::reactive({
        
        bmps2 %>%
          dplyr::filter(HUC12 %in% filtered_huc()$HUC12)# %>%
          # dplyr::filter(Active %in% input$bmps_active)
        
      })
      
      output$bmps_box <- shiny::renderUI({
        
        
        shiny::req(nrow(filtered_huc())>=1)
        
        
        total_bmps <- rve_bmps() %>%
          # sf::st_drop_geometry() %>%
          dplyr::group_by(HUC12) %>%
          dplyr::summarise(n = sum(count)) %>%
          dplyr::ungroup()
        
        
        bslib::value_box(fill = TRUE,
                  title = paste0(app_inputs$region, " & ", app_inputs$region2, " BMPs"),
                  value = sum(total_bmps$n[!is.na(total_bmps$HUC12)]),
                  showcase = shiny::icon("hammer"),
                  theme = "primary"
        )
        
      })
      
      selected_choicesrve = shiny::reactive(input$selectInput)
      
      mod_watershedServer("watershed_tables",
                          selected_choices = selected_choicesrve,
                          filtered_huc = filtered_huc_combo,
                          rve_bmps = rve_bmps)
      
      
      shiny::observe({
        
        if(nrow(rve_bmps())>1)
          shinyjs::show(id="show_bmp_plot")
        
      })
      
      
    })
    
    
    
    
    # River Restoration ----
    # Make reactive to save time when loading. Only loads when tab is clicked
    shiny::observeEvent(input$navset_tabs_river_rest,
                        
                        if(input$navset_tabs_river_rest == "Tucannon Restoration"){
                          iframeServer("iframe_rest",
                                       url = app_inputs$RR$river_rest_arcgis,
                                       style = 'width:90vw;height:90vh;')
                        } else if(input$navset_tabs_river_rest == "Tucannon Geomorphic Assessment"){
                          iframeServer("iframe_rest_geo",
                                       url = app_inputs$RR$river_rest_geo_arcgis,
                                       style = 'width:90vw;height:90vh;')
                        } else if(input$navset_tabs_river_rest == "Touchet Geomorphic Assessment"){
                          iframeServer("iframe_rest_geo_2",
                                       url = app_inputs$RR$river_rest_geo_arcgis2,
                                       style = 'width:90vw;height:90vh;')
                        }
    )
    
    # Param Table --------------------------------------------------------------
    
    output$params_table <- DT::renderDT({
      
      shiny::req(nrow(rve_params())>=1)
      
      DT::datatable(
        height = 900,
        rownames = FALSE,
        data = rve_params(),
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          lengthMenu=list(c(25,50,100,-1),c("25","50","100","All")),
          pageLength = -1,
          dom = 'Blfrtip',
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      ) 
      
      
    })
    
    # bmps table ---------------------------------------------------------
    
    
    output$bmps_full_table <- DT::renderDT({
      
      shiny::req(nrow(rve_year_bmps())>=1)
      
      DT::datatable(
        height = 900,
        rownames = FALSE,
        data = rve_year_bmps() %>%
          dplyr::select(Name, dplyr::matches("HUC\\d+"), dplyr::everything(), -Date),
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          lengthMenu=list(c(25,50,100,-1),c("25","50","100","All")),
          pageLength = -1,
          dom = 'Blfrtip',
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      ) 
      
      
    })
    

    # Private Ag Land Use ----
    private_ag_2019rve <- shiny::reactiveValues(value = private_ag_2019)

    shiny::observeEvent(input$corrected_checkbox, {
      if (input$corrected_checkbox) {
        private_ag_2019rve$value <- private_ag_2019_adj
      } else {
        private_ag_2019rve$value <- private_ag_2019
      }
    })
    
    output$ag_acres <- shinydashboard::renderValueBox({
      
      shinydashboard::valueBox(
        "Private Ag Acres",
        scales::comma(private_ag_2019rve$value),
        icon = shiny::icon("tractor")
      )
      
    })
    
    output$lc_vbox <- shinydashboard::renderValueBox({
      
      df <- switch(input$critpick,
                   "Aquifers" = ag_crit_aquifer,
                   "Wetlands" = ag_wetlands,
                   "Geologic Hazard" = ag_geo_haz,
                   "Wildlife" = ag_conservation_areas
      )
      
      
      total_ac <- sum(df$`Private Acres`)
      co_percent <- round((total_ac/private_ag_2019rve$value) * 100, digits = 2)
      whole_county <- round((total_ac/county) * 100, digits = 2)
      
      
      bslib::value_box(
        title = input$critpick,
        value = scales::comma(round(total_ac, digits = 3)),
        shiny::p(glue::glue("{co_percent}% in Ag and {whole_county}% of County")),
        full_screen = TRUE,
        theme = "success"
      )
      
      
    })
    
    output$lc_tfl <-  shiny::renderUI({
      
      
      df <- switch(input$critpick,
                   "Aquifers" = ag_crit_aquifer,
                   "Wetlands" = ag_wetlands,
                   "Geologic Hazard" = ag_geo_haz,
                   "Wildlife" = ag_conservation_areas
      )
      
      df <- df %>% dplyr::select(-V1)
      
      if(input$critpick == "Geologic Hazard"){
        df_sum <- df %>% 
          dplyr::group_by(`Primary Land Use`)%>%
          dplyr::summarise(`Private Acres`= sum(`Private Acres`)) %>% 
          dplyr::ungroup()
      } else {
        df_sum <- df
      }
      
      total_ac <- sum(df_sum$`Private Acres`)
      co_percent <- round((total_ac/private_ag_2019rve$value) * 100, digits = 2)
      whole_county <- round((total_ac/county) * 100, digits = 2)
      
      group_var <- switch(input$critpick,
                          "Aquifers" = "Aquifer",
                          "Wetlands" = "Wetland Type",
                          "Geologic Hazard" = NULL,
                          "Wildlife" = "Species/Habitat"
      )
      
      plot <- df_sum %>% 
        {if(input$critpick == "Geologic Hazard")
        {highcharter::hchart(., 
                             "column", 
                             highcharter::hcaes(x = `Primary Land Use`, y = `Private Acres`), 
                             stacking = "normal",
                             tooltip = list(
                               headerFormat = "<span style='font-size: 10px'>{point.key}</span><br/>",
                               pointFormat = glue::glue("<b>{{point.y}}</b>")
                             ))} else {
                               {highcharter::hchart(., 
                                                    "column", 
                                                    highcharter::hcaes(x = `Primary Land Use`, y = `Private Acres`, group = !!rlang::sym(group_var)), 
                                                    stacking = "normal")}
                               
                               
                             }} %>%
        highcharter::hc_xAxis(categories = sort(unique(df$`Primary Land Use`))) %>% 
        highcharter::hc_exporting(
          enabled = TRUE, 
          allowHTML = TRUE  
        )
      
      table <- DT::datatable(rownames = FALSE,
                             data= df,
                             extensions = 'Buttons',
                             filter = 'top',
                             options = list(
                               lengthMenu=list(c(25,50,100,-1),c("25","50","100","All")),
                               pageLength = -1,
                               dom = 'Blfrtip',
                               buttons = c("copy", "csv", "excel", "pdf", "print")
                             )) 
      
      bslib::layout_column_wrap(
        bslib::card(
          bslib::card_header(paste0("Percent of Ag: ", co_percent, "%")), 
          plot, 
          full_screen = TRUE),
        bslib::card(
          bslib::card_header(paste0("Acres in Ag: ", scales::comma(total_ac))), 
          table, 
          full_screen = TRUE)
      )
      
    })
    
    ## Report ----
    report_path <- shiny::reactiveVal(NULL)
    
    # Reset the report path if user changes inputs
    observeEvent(input$critpick, {
      report_path(NULL)
    })
    
    observeEvent(input$corrected_checkbox, {
      report_path(NULL)
    })
   
    shiny::observeEvent(input$landcover_report, {
      shinybusy::show_modal_spinner(text = "Generating your report. Please wait...", color = "#5E9300")

      message(paste(list.files(".", recursive = TRUE), collapse = ", "))
      
      # Temporary file for the report
      tmp_dir <- tempdir()
      old_wd <- getwd()  # Save current working dir
      
      on.exit(setwd(old_wd))  
      # setwd(tmp_dir)
      
      
      # move lc-typst-template.typ and lc-typst-show.typ to temp dir
      # file.copy(system.file("reports", "lc-typst-show.typ", package = "Tucannon"), 
      #           file.path(tmp_dir, "typst-show.typ"),
      #           overwrite = TRUE)
      # file.copy(system.file("reports", "lc-typst-template.typ", package = "Tucannon"), 
      #           file.path(tmp_dir, "typst-template.typ"),
      #           overwrite = TRUE)
      # # Copy the QMD file into temp dir and use it as the input
      # file.copy(system.file("reports", "landcover.qmd", package = "Tucannon"), 
      #           "landcover.qmd",
      #           overwrite = TRUE)
      
      
      file.copy("inst/reports/lc-typst-show.typ", 
                file.path(tmp_dir, "typst-show.typ"),
                overwrite = TRUE)
      file.copy("inst/reports/lc-typst-template.typ", 
                file.path(tmp_dir, "typst-template.typ"),
                overwrite = TRUE)
      # Copy the QMD file into temp dir and use it as the input
      file.copy("inst/reports/landcover.qmd", 
                file.path(tmp_dir, "landcover.qmd"),
                overwrite = TRUE)
      
      setwd(tmp_dir)
      
      out_path <- "landcover.pdf"
      
      
      df <- switch(input$critpick,
                   "Aquifers" = ag_crit_aquifer,
                   "Wetlands" = ag_wetlands,
                   "Geologic Hazard" = ag_geo_haz,
                   "Wildlife" = ag_conservation_areas
      )

      quarto::quarto_render(
        input = "landcover.qmd",
        output_file = out_path,
        execute_params = list(
          watershed = app_inputs$county_term,
          county_acres = county,
          crit = input$critpick,
          corrected = input$corrected_checkbox,
          crit_data = df,
          private_ag_acres = if (input$corrected_checkbox)
            private_ag_2019_adj
          else
            private_ag_2019
        ), 
        execute = TRUE
      ) 
      
      file.path(tmp_dir, out_path) |> report_path()
      
      shinybusy::remove_modal_spinner()
      
    })
    
    output$landcover_download <- shiny::renderUI({
      shiny::req(report_path())
      shiny::downloadButton("download_report", "Download Report", class = "btn btn-primary")
    })
    
    output$download_report <- shiny::downloadHandler(
      filename = function() paste0("landcover_report_",
                                   stringr::str_replace_all(stringr::str_to_lower(input$critpick), " ", "_"),"_",
                                   if(input$corrected_checkbox) "corrected_" else NULL, 
                                   Sys.Date(), ".pdf"),
      content = function(file) {
        file.copy(report_path(), file)
      }
    )
    
    # USDA Land Use ----
   huc_acresrve <- shiny::reactiveValues(value = huc_sum)
    
    
    output$huc_acres <- shinydashboard::renderValueBox({
      
      shinydashboard::valueBox(
        "USDA HUC12 Acres",
        scales::comma(huc_acresrve$value),
        icon = shiny::icon("map")
      )
      
    })
    
    output$lc_vbox_usda <- shinydashboard::renderValueBox({
      
      df <- switch(input$critpick_usda,
                   "Aquifers" = ag_crit_aquifer_usda,
                   "Wetlands" = ag_wetlands_usda,
                   "Geologic Hazard" = ag_geo_haz_usda,
                   "Wildlife" = ag_conservation_areas_usda
      )
      
      
      total_ac <- sum(df$Acres)
      acre_percent <- round((total_ac/huc_acresrve$value) * 100, digits = 2)

      
      bslib::value_box(
        title = input$critpick_usda,
        value = scales::comma(round(total_ac, digits = 3)),
        shiny::p(glue::glue("{acre_percent}% across HUC12")),
        full_screen = TRUE,
        theme = "success"
      )
      
      
    })
    
    output$lc_tfl_usda <-  shiny::renderUI({
      
      
      df <- switch(input$critpick_usda,
                   "Aquifers" = ag_crit_aquifer_usda,
                   "Wetlands" = ag_wetlands_usda,
                   "Geologic Hazard" = ag_geo_haz_usda,
                   "Wildlife" = ag_conservation_areas_usda
      )
      
      # df <- df %>% dplyr::select(-V1)
      
      if(input$critpick_usda == "Geologic Hazard"){
        df_sum <- df %>% 
          dplyr::group_by(`Primary Land Use`)%>%
          dplyr::summarise(Acres= sum(Acres)) %>% 
          dplyr::ungroup()
      } else {
        df_sum <- df
      }
      
      total_ac <- sum(df_sum$Acres)
      acre_percent <- round((total_ac/huc_acresrve$value) * 100, digits = 2)

      group_var <- switch(input$critpick_usda,
                          "Aquifers" = "Aquifer",
                          "Wetlands" = "Wetland Type",
                          "Geologic Hazard" = NULL,
                          "Wildlife" = "Species/Habitat"
      )
      
      plot <- df_sum %>% 
        {if(input$critpick_usda == "Geologic Hazard")
        {highcharter::hchart(., 
                             "column", 
                             highcharter::hcaes(x = `Primary Land Use`, y = Acres), 
                             stacking = "normal",
                             tooltip = list(
                               headerFormat = "<span style='font-size: 10px'>{point.key}</span><br/>",
                               pointFormat = glue::glue("<b>{{point.y}}</b>")
                             ))} else {
                               {highcharter::hchart(., 
                                                    "column", 
                                                    highcharter::hcaes(x = `Primary Land Use`, y = Acres, group = !!rlang::sym(group_var)), 
                                                    stacking = "normal")}
                               
                               
                             }} %>%
        highcharter::hc_xAxis(categories = unique(LU_2024$crop_name_grp)) %>% 
        highcharter::hc_exporting(
          enabled = TRUE, 
          allowHTML = TRUE  
        )
      
      table <- DT::datatable(rownames = FALSE,
                             data= df,
                             extensions = 'Buttons',
                             filter = 'top',
                             options = list(
                               lengthMenu=list(c(25,50,100,-1),c("25","50","100","All")),
                               pageLength = -1,
                               dom = 'Blfrtip',
                               buttons = c("copy", "csv", "excel", "pdf", "print")
                             )) 
      
      bslib::layout_column_wrap(
        bslib::card(
          bslib::card_header(paste0("Percent of HUC12: ", acre_percent, "%")), 
          plot, 
          full_screen = TRUE),
        bslib::card(
          bslib::card_header(paste0("Acres: ", scales::comma(total_ac))), 
          table, 
          full_screen = TRUE)
      )
      
    })
    
    output$landuse_yr_comp <- renderUI({
      bslib::layout_columns(
        col_widths = c(12, 12),
        bslib::card(
          highcharter::highchartOutput("LU_change2011_2024"),
          
          full_screen = TRUE
        ),
        bslib::card(
          leaflet::leafletOutput("usda_map", height = "1000px"),
          full_screen = TRUE
        )
      )
    })
    
    output$LU_change2011_2024 <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_chart(type = "line", inverted = FALSE) |>
        highcharter::hc_title(text = "Land Use Change (2011 vs 2024)") |>
        highcharter::hc_xAxis(
          categories = levels(LU_change$year),
          title = list(text = "Year")
        ) |>
        highcharter::hc_yAxis(
          title = list(text = "Acres")
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            marker = list(enabled = TRUE),
            dataLabels = list(enabled = TRUE, format = '{point.y:,.0f}')
          )
        ) |>
        highcharter::hc_add_series(
          data = LU_change,
          type = "line",
          highcharter::hcaes(x = year, y = acres, group = crop_name_grp),
          tooltip = list(
            pointFormat = "<b>{series.name}</b>: {point.y:,.0f} acres"
          ),
          showInLegend = TRUE
        ) %>% 
        highcharter::hc_exporting(
          enabled = TRUE, 
          allowHTML = TRUE  
        )
      
    })
    
    output$usda_map <- leaflet::renderLeaflet({
          
          leaflet::leaflet()  %>%
            # Define left and right map panes
            leaflet::addMapPane("right", zIndex = 410) %>%
            leaflet::addMapPane("left",  zIndex = 420) %>%
            
            # Add basemaps to left and right for swipe alignment
            leaflet::addTiles(group = "base", layerId = "baseid1", options = leaflet::pathOptions(pane = "right")) %>%
            leaflet:: addTiles(group = "base", layerId = "baseid2", options = leaflet::pathOptions(pane = "left")) %>%
            
            # Add WMS layer for USDA 2024 to RIGHT pane
            leaflet::addWMSTiles(
              baseUrl = "https://geoserver.megaloptera-data.com/geoserver/Columbia/wms",
              layers = "Columbia:usda_2024_colored2",
              options = leaflet::WMSTileOptions(
                format = "image/png",
                transparent = TRUE,
                version = "1.1.0",
                pane = "right"
              ),
              layerId = "wms_2024"
            ) %>%
            
            # Add WMS layer for USDA 2011 to LEFT pane
            leaflet::addWMSTiles(
              baseUrl = "https://geoserver.megaloptera-data.com/geoserver/Columbia/wms",
              layers = "Columbia:usda_2011_colored2",
              options = leaflet::WMSTileOptions(
                format = "image/png",
                transparent = TRUE,
                version = "1.1.0",
                pane = "left"
              ),
              layerId = "wms_2011"
            ) %>%
            
            # Zoom to Columbia County
            leaflet::fitBounds(
              lng1 = -118.400, lat1 = 46.126,
              lng2 = -117.750, lat2 = 46.475
            )  %>%
            
            # Add side-by-side swipe control
            leaflet.extras2::addSidebyside(
              leftId = "wms_2011",
              rightId = "wms_2024",
              layerId = "swipe_control"
            )%>%
            leaflet::addControl(html = custom_legend, position = "topright")
          
        })
    
    
    # NRCS ----
    # Unit mapping
    unit_map <- list(
      acres = c("Ac"),
      feet = c("Ft"),
      number = c("No")
    )
    
    unit_labels <- c(acres = "Area-Based Practices", feet = "Length-Based Practices", number = "Count-Based Practices")
    unit_suffixes <- c(acres = " Acres", feet = " Feet", number = "")
    unit_suffixes_plot <- c(acres = " Acres", feet = " Feet", number = " Count")
    
    # Track selected unit type
    selected_unit <- shiny::reactiveVal("acres")
    
    # Compute totals per unit type
    summary_by_unit <- shiny::reactive({
      shiny::req(input$land_use)
      
      nrcs_data %>%
        dplyr::filter(land_use %in% input$land_use) %>%
        dplyr::mutate(unit_type = dplyr::case_when(
          measurement_unit %in% unit_map$acres ~ "acres",
          measurement_unit %in% unit_map$feet ~ "feet",
          measurement_unit %in% unit_map$number ~ "number",
          TRUE ~ NA_character_
        )) %>%
        dplyr::filter(!is.na(unit_type)) %>%
        dplyr::group_by(unit_type) %>%
        dplyr::summarise(total = round(sum(applied_amount, na.rm = TRUE)), .groups = "drop")
    })
    
    # Value cards
    output$value_cards <- shiny::renderUI({
      su <- summary_by_unit()
      
      # Create individual value boxes that can be clicked
      create_value_card <- function(unit_type, icon_name) {
        val <- su %>% dplyr::filter(unit_type == !!unit_type) %>% dplyr::pull(total)
        is_available <- length(val) > 0 && val > 0
        
        # # Determine if this card should be highlighted
        is_selected <- selected_unit() == unit_type
        
        shiny::column(4,
                      shiny::div(
                        style = if (!is_available) "opacity: 0.5; cursor: not-allowed;" else "cursor: pointer;",
                        onclick = if (is_available) paste0("Shiny.setInputValue('card_", unit_type, "', Math.random())") else "",
                        bslib::value_box(
                          title = unit_labels[[unit_type]],
                          value = if (is_available) {
                            paste0(format(val, big.mark = ","), unit_suffixes[[unit_type]])
                          } else {
                            "No data"
                          },
                          showcase = shiny::icon(icon_name),
                          theme = if (is_selected) "primary" else if (is_available) "info" else "secondary",
                          class = if (is_selected) "border-primary" else ""
                        )
                      )
        )
      }
      
      shiny::fluidRow(
        create_value_card("acres", "square"),
        create_value_card("feet", "ruler-horizontal"), 
        create_value_card("number", "hashtag")
      )
    })
    
    # Observe card selection
    shiny::observeEvent(input$card_acres, {
      selected_unit("acres")
    })
    shiny::observeEvent(input$card_feet, {
      selected_unit("feet")
    })
    shiny::observeEvent(input$card_number, {
      selected_unit("number")
    })
    
    # Time series chart
    output$time_series <- highcharter::renderHighchart({
      shiny::req(input$land_use, selected_unit())
      unit <- selected_unit()
      
      df <- nrcs_data %>%
        dplyr::filter(land_use %in% input$land_use) %>%
        dplyr::mutate(unit_type = dplyr::case_when(
          measurement_unit %in% unit_map$acres ~ "acres",
          measurement_unit %in% unit_map$feet ~ "feet",
          measurement_unit %in% unit_map$number ~ "number",
          TRUE ~ NA_character_
        )) %>%
        dplyr::filter(unit_type == unit, !is.na(applied_year)) %>%
        dplyr::group_by(applied_year, land_use) %>%
        dplyr::summarise(total = round(sum(applied_amount, na.rm = TRUE)), .groups = "drop")
      
      if (nrow(df) == 0) {
        return(
          highcharter::highchart() %>%
            highcharter::hc_title(text = paste("No data available for", unit_labels[[unit]])) %>%
            highcharter::hc_subtitle(text = "Try selecting a different unit type or land use")
        )
      }
      
      highcharter::hchart(df, "line", highcharter::hcaes(x = applied_year, y = total, group = land_use)) %>%
        highcharter::hc_title(text = paste("Practice Totals Over Time:", unit_labels[[unit]])) %>%
        highcharter::hc_xAxis(title = list(text = "Year")) %>%
        highcharter::hc_yAxis(title = list(text = paste("Total", unit_suffixes_plot[[unit]]))) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          valueDecimals = 0,
          valueSuffix = unit_suffixes[[unit]]
        ) %>%
        highcharter::hc_plotOptions(
          line = list(
            marker = list(enabled = TRUE, radius = 4),
            lineWidth = 2
          )
        )
    })
    
    # Table
    output$practice_table <- DT::renderDT({
      shiny::req(input$land_use, selected_unit())
      unit <- selected_unit()
      
      df <- nrcs_data %>%
        dplyr::mutate(unit_type = dplyr::case_when(
          measurement_unit %in% unit_map$acres ~ "acres",
          measurement_unit %in% unit_map$feet ~ "feet",
          measurement_unit %in% unit_map$number ~ "number",
          TRUE ~ NA_character_
        )) %>%
        dplyr::filter(unit_type == unit, land_use %in% input$land_use) %>%
        dplyr::group_by(practice_name, land_use, measurement_unit) %>%
        dplyr::summarise(total_amount = round(sum(applied_amount, na.rm = TRUE)), .groups = "drop") %>%
        dplyr::arrange(desc(total_amount)) %>% 
        dplyr::select(practice_name, land_use, total_amount, measurement_unit)
      
      DT::datatable(
        df,
        rownames = FALSE,
        colnames = c("Practice Name", "Land Use", "Total Amount", "Unit"),
        options = list(
          lengthMenu=list(c(25,50,100,-1),c("25","50","100","All")),
          pageLength = -1,
          dom = 'Blfrtip',
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      )
    })


      
    
  }
  
  
  shiny::shinyApp(app_ui, app_server)
  



