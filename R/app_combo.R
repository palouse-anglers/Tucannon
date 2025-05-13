run_app <- function(){
  
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
                                                                               bslib::card_header("Temperature"),
                                                                               hc_lineUI("hc_line_temp")
                                                                             ),
                                                                             bslib::card( 
                                                                               full_screen = TRUE,
                                                                               style = "resize:both;",
                                                                               bslib::card_header("Stage Height"),
                                                                               hc_lineUI("hc_line_stage")
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
                                                                               # TODO Add content
                                                                             ))),
                                                          bslib::nav_panel(title = "Table",
                                                                           DT::dataTableOutput("params_table")))
                                        
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
                                                        shiny::checkboxInput("corrected_checkbox", "Adjusted Acreage")
                                          ),
                                          shiny::column(width = 4,
                                                 # TODO add content
                                                 # valueBoxOutput("ag_acres")
                                          ),
                                          #column(1),
                                          shiny::column(width = 4
                                                 # TODO add content
                                                 # valueBoxOutput("aquifer_ag_box"),
                                                 # valueBoxOutput("wetlands_box2"),
                                                 # valueBoxOutput("geo_haz_box"),
                                                 # valueBoxOutput("wildlife_box2")
                                                 
                                          ))
                                        
                       ),
                       # Guidance ----
                       bslib::nav_panel(title = "Guidance",
                                        DT::dataTableOutput("huclabels")
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
    
    # Filtered params ----
    
    rve_params <- shiny::reactive({
      filter_data_bytime(params, 
                         year_range = input$dateRange, 
                         month_vals = input$monthRange) %>% 
        dplyr::left_join(bmps_byyear, by = "Year")
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
    
    bmps_year <- shiny::reactive({
      rve_params() %>%
        dplyr::distinct(Year, No_BMPS) %>%
        dplyr::mutate(No_BMPS = ifelse(is.na(No_BMPS), 0, No_BMPS),
                      Year = as.double(Year))
      
    })
    
    # Stations ----
    
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
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          dom = 'lfrtipB',
          buttons = c('copy', 'csv', 'excel')
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
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          dom = 'lfrtipB',
          buttons = c('copy', 'csv', 'excel')
        )
      ) 
      
      
    })
    
    # HUC Labels -----------------------------------------------------------
    
    output$huclabels <- DT::renderDT({
      
      DT::datatable(huc_labels)
      
    })
    
  }
  
  
  shiny::shinyApp(app_ui, app_server)
  
}


