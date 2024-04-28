#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
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
    addResetMapButton() %>%
    #addSearchGoogle() %>%
    addLayersControl(
      overlayGroups = c("Waterways"),
      baseGroups = c("Imagery", "Dark", "Topo", "Street")
    ) %>%
    leaflet.extras::addFullscreenControl() %>%
    leafem::addMouseCoordinates() %>%
    hideGroup(c("Waterways"))
  
  
  
  
})
  
  
  
  
  
  
  
} #--------------------------- End Server---------------------------------


