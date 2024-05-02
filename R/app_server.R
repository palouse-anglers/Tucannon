#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 


# load files --------------------------------------------------------------
huc12 <- sf::st_read("inst/huc_merge/HUC12.shp",quiet = TRUE)
powers <- sf::st_read("inst/huc_merge/powers.shp",quiet = TRUE)

bmp_points <-  sf::st_read("inst/huc_merge/BMP_points.shp",quiet = TRUE) 
bmp_lines <-  sf::st_read("inst/huc_merge/BMP_line.shp",quiet = TRUE)
bmp_shape <-  sf::st_read("inst/huc_merge/BMP_shape.shp",quiet = TRUE)
# Start server ------------------------------------------------------------


app_server <- function(input, output, session) {
  

# Do plot -----------------------------------------------------------------
output$do_plot <- renderHighchart({
    
  initial_model <- lm(Result ~ Date, data = params %>%
                        filter(Param == "Dissolved Oxygen"))
  
 regression_line <- data.frame(
    Date = params %>% filter(Param == "Dissolved Oxygen") %>% pull(Date),
    Result = predict(initial_model)
  )
    
  highcharter::hchart(
     params %>%
      filter(Param == "Dissolved Oxygen"),
      type="point",
      hcaes(x = Date, y = Result),
      name = "Dissolved Oxygen mg/L",
      showInLegend = TRUE
    ) %>%
      hc_add_series(tooltip = list(enabled = FALSE),
                    data = params %>%
                      filter(Param == "Dissolved Oxygen") %>%
                      select(-Result),
                    hcaes(x = Date, y = 8),
                    type = "line",
                    showInLegend = TRUE,
                    
                    dashStyle = "Dash",
                    name = "TMDL",
                    color = "red") %>%
      hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'TMDL') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' mg/L'
                            );
  } else {
                        return false;
                      }
                            }"))  %>%
      hc_rangeSelector(enabled = TRUE) %>%
      hc_yAxis(title = list(text = "Dissolved Oxygen mg/L")) %>%
      hc_title(text = "Powers Road") %>%
     hc_add_series(
       data = regression_line,
       hcaes(x = Date, y = Result),
       showInLegend = TRUE,
       name = "Regression",
       type = "line",
       color = "black"
     ) %>%
    hc_responsive(enabled = TRUE)
    
    
    
    
  })
  




# filtered huc12 ----------------------------------------------------------

filtered_huc <- reactive({
  
huc12 %>%
    dplyr::filter(Name %in% input$watersheds)
  
})



# hide input --------------------------------------------------------------

observe({
  
  
  # When the "watersheds" tab is selected, show the pickerInput
  if (input$navbar_items_id == "Watersheds Map") {
    shinyjs::show("watersheds")
  } else {
    shinyjs::hide("watersheds")
  }

  })



# watersheds picker -------------------------------------------------------


# acre box ----------------------------------------------------------------



output$acres_box <- renderUI({

  
  if (nrow(filtered_huc()) > 1) {
    chart <- highcharter::hchart(filtered_huc(),
   "column",
   hcaes(x = Name, y = HUC_Acres, group = Name))
  } else{
    chart <- NULL
  }
  
  
  value_box(
    title = "HUC Acres",
    value = scales::comma(round(sum(filtered_huc()$HUC_Acres),0)),
    p(glue::glue("{length(unique(filtered_huc()$Name))} watersheds selected")),
    p("Columbia County HUC12"),
    p("Tucannon Watershed"),
    chart,
    full_screen = TRUE,
    theme = "info"
  )
  
  
  
})


# wildlife box ------------------------------------------------------------

output$erosion_box <- renderUI({ 
  
  erosion_vars <- huc12 %>%
    select(Severe_Ac_,Mod_Ac_Wtr,No_WtrEr_A) %>%
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  erosion_names <- c("Severe","Moderate","None")
  
  total_huc_acre <-  sum(filtered_huc()$HUC_Acres)
  
  erosion_panels <- purrr::map2_dfr(
    as.character(erosion_vars),as.character(erosion_names),~
      data.frame(Erosion=.y,
                 Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]]), 0))}"),
                 Percent=glue::glue("% {round(sum(filtered_huc()[[.x]])/total_huc_acre*100,2)}"))) 
  
  card(id = "erosion_card",
       height = 300,
       style = "resize:vertical;",
       card_header("Water Erosion"),
       card_body(
         #min_height = 250,
         #DT::datatable(panels2),
         DT::datatable(erosion_panels,options = list(dom = 't'))
       )
  )
  
  
  })



output$wildlife_box <- renderUI({
  
  
accord_names <- huc12 %>%
  select(ends_with("Ac")) %>% 
    names() %>% 
    str_subset(pattern = "geometry|Weg|Cliff",negate = TRUE)
  
abv_names <- c("Elk","Mule Deer","WT Deer","Pheasant","Water Fowl","Chuckar","Sheep")

total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)

 

wildlife_panels <- purrr::map2_dfr(
  as.character(accord_names),as.character(abv_names),~
    data.frame(Animal=.y,
               Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]]), 0))}"),
               Percent=glue::glue("% {round(sum(filtered_huc()[[.x]])/total_huc_acrew*100,2)}"))) 


  card(id = "wildlife_card",
  height = 450,
  style = "resize:vertical;",
  card_header("Wildlife"),
  card_body(
    #min_height = 250,
    #DT::datatable(panels2),
    DT::datatable(wildlife_panels,options = list(dom = 't'))
  )
)


})



# Leaflet -----------------------------------------------------------------

output$leafmap <- renderLeaflet({
  
  # Hydrography layer options 
  opt <-
    leaflet::WMSTileOptions(
      format = "image/png32",
      version = "1.3.0",
      minZoom = 3,
      maxZoom = 16,
      transparent = TRUE
    )
  
  leaflet(options = leafletOptions(
    attributionControl=FALSE)) %>%
    addTiles() %>%
    setView(lat = 46.29929,lng = -118.02250,zoom = 9) %>%
    addWMSTiles(baseUrl = "https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WMSServer?",layers="0",options = opt,group="Waterways") %>%
    addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
    addProviderTiles("CartoDB.DarkMatter", group="Dark") %>%
    addProviderTiles("Esri.NatGeoWorldMap", group="Topo") %>%
    addProviderTiles("OpenStreetMap", group="Street") %>%
    addSearchOSM() %>%
    addResetMapButton() 
  

})
  


# map click ---------------------------------------------------------------


clicked_HUC <- reactiveVal(character(0))


observeEvent(input$leafmap_shape_click,{
  
 
  #print(watersheds_selected)
  print(clicked_HUC())
  
  # capture the info of the clicked polygon
  click <- input$leafmap_shape_click
  # subset to clicked
  clicked_HUC_data <- unique(as.character(huc12$Name[huc12$Name == click$id]))

  # Check if the name already exists in clicked_HUC
  if (clicked_HUC_data  %in% clicked_HUC()) {
    # If it exists, remove it from clicked_HUC
    clicked_HUC(clicked_HUC()[clicked_HUC() !=  clicked_HUC_data])
  } else {
    # If it doesn't exist, append it to clicked_HUC
    # Store in reactive val
    clicked_HUC(c(clicked_HUC(), clicked_HUC_data))
  }

  updatePickerInput(session, "watersheds", selected = clicked_HUC())
 
  })
  
  


# huc 12 datatable --------------------------------------------------------


output$selectedHUC <- DT::renderDataTable({
  # Check if clicked_HUC is NULL (no shape clicked yet)
  if(length(clicked_HUC())==0) {
    return(NULL)  # Return NULL if no shape clicked
  } else {
    # Render the DataTable with the filtered data
    DT::datatable(data = filtered_huc())
  }

  })
  


output$selectedHUC_name <- renderText({
  # Check if clicked_HUC is NULL (no shape clicked yet)
    shiny::req(input$leafmap_shape_click)
    # Render the DataTable with the filtered data
    clicked_HUC() 
  
  
})
  

# other card --------------------------------------------------------------

show_additional_card <- reactive({
  !is.null(input$leafmap_shape_click)
})

output$additional_card <- renderUI({
  
  req(input$leafmap_shape_click)

    card(
      id = "additional_cards",
      full_screen = TRUE,
      style = "resize:both;",
      card_header(verbatimTextOutput("selectedHUC_name")),
      card_body(DT::dataTableOutput("selectedHUC"))
    )

})


# Leaflet Proxy -----------------------------------------------------------
#outputOptions(output, "leafmap", suspendWhenHidden = FALSE)

observe({
  
 # clicked_HUC()
 #  
 # print(filtered_huc())
  
  map <- leafletProxy("leafmap") %>%
    setView(lat = 46.29929,lng = -118.02250,zoom = 9) %>%
    clearShapes() %>%
    clearMarkers() %>%
    addPolygons(data=huc12,
                group="watersheds",
                layerId = huc12$Name,
                color = "black",
                weight = 1,
                highlight = highlightOptions(
                  weight = 3,
                  fillOpacity = 0.2,
                  color = "purple",
                  fillColor = "#6a0d83",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                # # Add label info when mouseover
                label = ~Name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "14px",
                  direction = "auto"))   %>%
    #addPolygons(data=private_ag23,popup = ~popupTable(private_ag23)) %>%
    addMarkers(data=powers,group="WQStation") %>%
    addPolygons(data=bmp_shape,
                group="BMP",
                label = ~CntrctN,
                popup = ~ popupTable(bmp_shape),
                fillColor = "goldenrod",
                fillOpacity = 1,
                stroke = TRUE,
                color="black",
                weight= 1,
                highlightOptions = highlightOptions(color = "red", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addPolylines(data=st_zm(bmp_lines),
                 group="BMP",
                 fillColor = "goldenrod",
                 popup = ~ popupTable(bmp_lines),
                 fillOpacity = 1,
                 stroke = TRUE,
                 color="goldenrod",
                 opacity = 1,
                 weight= 3,
                 label = ~CntrctN,
                 highlightOptions = highlightOptions(color = "red", weight = 2,
                                                     bringToFront = TRUE)) %>%
    addCircleMarkers(data=bmp_points,
                     group="BMP",
                     fillColor = "goldenrod",
                     popup = ~ popupTable(bmp_points),
                     fillOpacity = 1,
                     stroke = TRUE,
                     color="black",
                     weight= 1,
                     label = ~CntrctN)%>%
    #addSearchGoogle() %>%
    addLayersControl(
      overlayGroups = c("Waterways","BMP","watersheds","WQStation"),
      baseGroups = c("Topo","Imagery", "Dark", "Street")
    ) %>%
    leaflet.extras::addFullscreenControl() %>%
    leafem::addMouseCoordinates() %>%
    hideGroup(c("Waterways","WQStation","BMP")) %>%
    addPolygons(data=filtered_huc(),
                group="watersheds",
                layerId = filtered_huc()$Name,
                color = "red",
                weight = 1,
                highlight = highlightOptions(
                  weight = 3,
                  fillOpacity = 0.2,
                  color = "purple",
                  fillColor = "#6a0d83",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                # # Add label info when mouseover
                label = ~htmltools::HTML(paste(filtered_huc()$Name)),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "14px",
                  direction = "auto")) 
    
  
  
  bounds <- filtered_huc() %>% 
    st_bbox() %>% 
    as.character()
  

  # set_zoom <- st_bbox(filtered_huc())
  # 
  # xmin <- min(set_zoom[1],na.rm = TRUE)
  # ymin <- min(set_zoom[2],na.rm = TRUE)
  # xmax <- max(set_zoom[3],na.rm = TRUE)
  # ymax <- max(set_zoom[4],na.rm = TRUE)
  # 
   map <- map %>%
     fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
     #flyToBounds(xmin,ymin,xmax,ymax)

  
})

} #================ End Server===========================================-


