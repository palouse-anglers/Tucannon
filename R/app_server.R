#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 


# load files --------------------------------------------------------------
huc12 <- sf::st_read("inst/huc_merge/HUC12.shp")
powers <- sf::st_read("inst/huc_merge/powers.shp")


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
  
  
  leaflet() %>%
    addTiles() %>%
    #addPolygons(data=columbia_co) %>%
    #addPolygons(data=private_ag23) %>%
    addPolygons(data=huc12,
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
                  direction = "auto")) %>%
    #addPolygons(data=private_ag23,popup = ~popupTable(private_ag23)) %>%
    addMarkers(data=powers,group="WQStation")


})
  

# map click ---------------------------------------------------------------

#clicked_HUC <- reactive({selected_huc=list()})
clicked_HUC <- reactiveVal(NULL)

observeEvent(input$leafmap_shape_click, {
  
  # capture the info of the clicked polygon
  click <- input$leafmap_shape_click
  # subset to clicked
  clicked_HUC_data <- huc12[huc12$Name == click$id,]
  # Store in reactive val
  clicked_HUC(clicked_HUC_data)
   
  print(clicked_HUC())
  
  })
  
  
output$selectedHUC <- DT::renderDataTable({
  # Check if clicked_HUC is NULL (no shape clicked yet)
  if (is.null(clicked_HUC())) {
    return(NULL)  # Return NULL if no shape clicked
  } else {
    # Render the DataTable with the filtered data
    DT::datatable(data = clicked_HUC())
  }

  })
  


output$selectedHUC_name <- renderText({
  # Check if clicked_HUC is NULL (no shape clicked yet)
    shiny::req(input$leafmap_shape_click)
    # Render the DataTable with the filtered data
    clicked_HUC() %>% dplyr::pull(Name)
  
  
})
  
} #================ End Server===========================================-


