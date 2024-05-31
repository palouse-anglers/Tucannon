#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 
library(sf)
library(stringr)

# load files --------------------------------------------------------------
# Starbuck
# https://waterdata.usgs.gov/nwis/inventory?site_no=13344500&agency_cd=USGS
# Starbuck Dashboard
# https://dashboard.waterdata.usgs.gov/api/gwis/2.1/service/site?agencyCode=USGS&siteNumber=13344500&open=151971

# Marengo
# https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationDetails?sta=35B150

suppressWarnings(
  suppressMessages(
    source("../Tucannon/R/data_processing/powers/explore-powers.R")
  )
)



huc12 <- sf::st_read("inst/huc_merge/HUC12_mod.shp",quiet = TRUE) %>%
  select(HUC12) %>%
  left_join(read.csv("inst/huc_merge/HUC12_reworked3.csv") %>% 
              mutate(HUC12=as.character(HUC12)),by="HUC12")

names_huc12 <- names(huc12)

# huc12 <- sf::st_read("../../../humme/Downloads/Layers/HUC12_Metrics_All.shp") %>%
#   st_transform(.,crs=4326)

stations <- sf::st_read("inst/huc_merge/stations.shp",quiet = TRUE)

# Wetlands

wetlands <- 
  sf::st_read("inst/shapefiles/columbia-wetlands.shp",quiet=TRUE) %>%
  sf::st_transform(.,crs=4326)

# wetlands <- sf::st_read("inst/huc_merge/wetlands_huc12_merge.shp",quiet = TRUE) %>%
#   select(WETLAND,Acrs_n_)

# Geologically Hazardous Areas
geo_hazard <- sf::st_read("inst/huc_merge/geo_hazard_huc_merge.shp",quiet = TRUE) %>%
  select(frphrtd,weg,muname,Acrs_n_)

# Frequently Flooded Areas
freq_flood <- sf::st_read("inst/huc_merge/freq_flood_huc_merge.shp",quiet = TRUE) %>%
  select(SYMBOL,Acrs_n_)




# pr_ag23 <- sf::st_read("../../Downloads/Layers/Private_Ag_23.shp",quiet = TRUE) %>%
#   st_transform(.,crs=4326)


bmp_points <-  sf::st_read("inst/huc_merge/BMP_points.shp",quiet = TRUE) 
bmp_lines <-  sf::st_read("inst/huc_merge/BMP_line.shp",quiet = TRUE)
bmp_shape <-  sf::st_read("inst/huc_merge/BMP_shape.shp",quiet = TRUE)

# TODO outside of app
bmps <- bmp_points %>%
  bind_rows(bmp_lines) %>%
  bind_rows(bmp_shape) %>%
  mutate(active=tidyr::replace_na(ifelse(activty=="ACTIVE","Yes","No"),"No"),
           project=case_when(
           stringr::str_detect(project,"METER") ~ "FLOW METER",
           stringr::str_detect(project,"DEVELOPMENT") ~ "WATER DEVELOPMENT",
         TRUE ~ project)) %>%
  select(HUC12,
         Program=program,
         Project=project,
         Year=instll_,
         Active=active,
         Type=type,
         ID=Cntr_ID)%>%
  left_join(huc12 %>%
  select(HUC12,Name) %>%
  sf::st_drop_geometry(),by="HUC12")



# Marengo
marengo_water <- data.table::fread("R/data_processing/marengo_water.csv") %>%
  mutate(Date=lubridate::ymd(Date))

marengo_stage <- data.table::fread("R/data_processing/marengo_stage.csv")%>%
  mutate(Date=lubridate::ymd(Date))

# Start server ------------------------------------------------------------


app_server <- function(input, output, session) {
  

  
# full_bmps <- reactive{(
#   bmps
#   
# )}  

  
  # reactive BMPs
  rve_year_bmps <- reactive({
    
    bmps %>%
      sf::st_drop_geometry() %>%
      mutate(Date=paste0(Year,"-05-04")
             ) %>%
      dplyr::filter(
        lubridate::year(lubridate::date(Date)) >= input$dateRange[1] &  lubridate::year(lubridate::date(Date)) <= input$dateRange[2]) 
 
      
  })  
  
  
# Year and Month filter for plots 
rve_params <- reactive({
  
  params %>%
  dplyr::filter(year(Date) >= input$dateRange[1] & year(Date) <= input$dateRange[2]) %>% 
  dplyr::filter(Month %in% input$monthRange)

  })
  
# reactive BMPs
 rve_bmps <- reactive({
   
   bmps %>%
     dplyr::filter(HUC12 %in% filtered_huc()$HUC12) %>%
     dplyr::filter(Active %in% input$bmps_active)
   
 })
 
    
rve_mngo_water <- reactive({
  
  marengo_water %>%
    dplyr::filter(year(Date) >= input$dateRange[1] & year(Date) <= input$dateRange[2]) %>% 
    dplyr::filter(Month %in% input$monthRange)
  
})

rve_mngo_stage <- reactive({
  
  marengo_stage %>%
    dplyr::filter(year(Date) >= input$dateRange[1] & year(Date) <= input$dateRange[2]) %>% 
    dplyr::filter(Month %in% input$monthRange)
  
})
 

# Marengo Water Temp ------------------------------------------------------

output$mrngo_water_plot <- highcharter::renderHighchart({
  
  req(length(input$monthRange)>=1)
  req(nrow(rve_mngo_water()>1))
  
  
  mngo_model <- broom::augment(
    lm(Result ~ Date, data = rve_mngo_water()
    ))
  
  
  hchart(
    rve_mngo_water(), "line", 
    hcaes(x = Date, y = Result)
  )%>%
    hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'TMDL') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +'deg C'
                            );
  } else {
                        return false;
                      }
                            }"))  %>%
    hc_yAxis(title = list(text = "Temperature")) %>%
    hc_title(text = "35B150-Marengo Temperature")%>%
    hc_add_series(
      tooltip = list(enabled = FALSE),
      dashStyle = "Dash",
      data = mngo_model,
      hcaes(x = Date, y = .fitted),
      showInLegend = TRUE,
      name = "Regression",
      type = "line",
      color = "black"
    )%>%
    hc_exporting(enabled = TRUE, 
                 buttons = list(contextButton = list(menuItems = list(
                   list(
                     textKey = "downloadPNG",
                     onclick = JS("function() { this.exportChart(); }")
                   )
                 )))) 
  
})

# marengo stage plot ------------------------------------------------------


output$mrngo_stage_plot <- renderHighchart({
  
  req(length(input$monthRange)>=1)
  req(nrow(rve_mngo_water()>1))
  req(input$navset_tabs_id == "Marengo")
  
  mngo_smodel <- broom::augment(
    lm(Result ~ Date, data = rve_mngo_stage()
    ))
  
  
  hchart(
    rve_mngo_stage(), "line", 
    hcaes(x = Date, y = Result)
  )%>%
    hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'TMDL') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +'deg C'
                            );
  } else {
                        return false;
                      }
                            }"))  %>%
    hc_yAxis(title = list(text = "Stage Ht. (ft)")) %>%
    hc_title(text = "35B150-Marengo Stage Ht. (ft)")%>%
    hc_add_series(
      tooltip = list(enabled = FALSE),
      dashStyle = "Dash",
      data = mngo_smodel,
      hcaes(x = Date, y = .fitted),
      showInLegend = TRUE,
      name = "Regression",
      type = "line",
      color = "black"
    )%>%
    hc_exporting(enabled = TRUE, 
                 buttons = list(contextButton = list(menuItems = list(
                   list(
                     textKey = "downloadPNG",
                     onclick = JS("function() { this.exportChart(); }")
                   )
                 )))) 
  
})
# Do plot -----------------------------------------------------------------
output$do_plot <- renderHighchart({
    
 #  initial_model <- lm(Result ~ Date, data = params %>%
 #                        filter(Param == "Dissolved Oxygen"))
 #  
 # regression_line <- data.frame(
 #    Date = params %>% filter(Param == "Dissolved Oxygen") %>% pull(Date),
 #    Result = predict(initial_model)
 #  )
 # 
 
  #req(input$navset_tabs_id == "Dissolved Oxygen")
  
 req(nrow(rve_params() %>% 
            filter(Param == "Dissolved Oxygen"))>=1)
  
 do_model <- broom::augment(
   lm(Result ~ Date, data = rve_params() %>% 
                                 filter(Param == "Dissolved Oxygen")
                               ))
 
 
 
  highcharter::hchart(
    rve_params() %>%
      filter(Param == "Dissolved Oxygen"),
      type="point",
      hcaes(x = Date, y = Result),
      name = "Dissolved Oxygen mg/L",
      showInLegend = TRUE
    ) %>%
      hc_add_series(tooltip = list(enabled = FALSE),
                    data = rve_params() %>%
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
      hc_yAxis(title = list(text = "Dissolved Oxygen mg/L")) %>%
      hc_title(text = "Powers Road") %>%
    hc_add_series(
      tooltip = list(enabled = FALSE),
      dashStyle = "Dash",
      data = do_model,
      hcaes(x = Date, y = .fitted),
      showInLegend = TRUE,
      name = "Regression",
      type = "line",
      color = "black"
    ) %>%
    hc_responsive(enabled = TRUE)%>%
    hc_exporting(enabled = TRUE, 
                 buttons = list(contextButton = list(menuItems = list(
                   list(
                     textKey = "downloadPNG",
                     onclick = JS("function() { this.exportChart(); }")
                   )
                 )))) %>%
    hc_chart(
      backgroundColor = "#FFFFFF" 
    )
    
    
    
    
  })

 

# Reactive Temperature ----------------------------------------------------

 temp_params <- reactive({ 
   
   
   rve_params() %>%
   filter(Param=="Temperature, water") %>%
   group_by(Date) %>%
   summarise(Result=mean(Result,na.rm = TRUE)) %>%
   mutate(Month=lubridate::month(Date,label=TRUE,abbr = TRUE),
          Year=lubridate::year(Date)
   ) %>%
   arrange(Year,Month) %>%
   filter(Result >0) %>%
   ungroup()
   
 
})
 

# Temp Table --------------------------------------------------------------

 
 
 output$params_table <- DT::renderDT({
 

   req(nrow(rve_params()>=1))
   
   DT::datatable(height = 900,
     data=rve_params(),
     extensions = 'Buttons',
     filter = 'top',
     options = list(
       lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100","All")),
       dom = 'lfrtipB',
       buttons = c('copy', 'csv', 'excel')
     )
   ) 
   
 
 })




## bmps table ---------------------------------------------------------


output$bmps_full_table <- DT::renderDT({
  
  
  req(nrow(rve_year_bmps()>=1))
  
  
  DT::datatable(height = 900,rownames = FALSE,
                data=rve_year_bmps() %>% select(Name,HUC12,everything(),-Date),
                extensions = 'Buttons',
                filter = 'top',
                options = list(
                  lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100","All")),
                  dom = 'lfrtipB',
                  buttons = c('copy', 'csv', 'excel')
                )
  ) 
  
  
})

output$bmps_stacked <- renderHighchart({
  

  req(nrow(rve_year_bmps()>=1))
  
  bmps2 <- rve_year_bmps() %>%
    st_drop_geometry() %>%
    group_by(Year,Project) %>%
    tally()

bmps2 %>%
hchart("column", hcaes(x = Year, y = n, group = Project), 
       stacking = "normal")%>%
  hc_exporting(enabled = TRUE, 
               buttons = list(contextButton = list(menuItems = list(
                 list(
                   textKey = "downloadPNG",
                   onclick = JS("function() { this.exportChart(); }")
                 )
               )))) 
  
  

})




# Temp boxplot ------------------------------------------------------------
 output$by_year_box <- renderHighchart({
   
   req(input$navset_tabs_id == "Temperature")
   req(nrow(temp_params()>=1))


 
   hcboxplot(x=temp_params()$Result,
                          var = temp_params()$Year,
                          outliers = FALSE,name="Degrees C")%>% 
   hc_chart(type = "column")%>%
   hc_title(text = "Quartiles")%>%
   hc_rangeSelector(enabled = FALSE)%>%
     hc_exporting(enabled = TRUE, 
                  buttons = list(contextButton = list(menuItems = list(
                    list(
                      textKey = "downloadPNG",
                      onclick = JS("function() { this.exportChart(); }")
                    )
                  )))) %>%
     hc_chart(
       backgroundColor = "#FFFFFF" 
     ) %>%
     hc_exporting(enabled = TRUE, 
                  buttons = list(contextButton = list(menuItems = list(
                    list(
                      textKey = "downloadPNG",
                      onclick = JS("function() { this.exportChart(); }")
                    )
                  ))))
 
 })
 
 output$by_year_scatter <- renderHighchart({
 
   
   req(input$navset_tabs_id == "Temperature")
   
   req(nrow(temp_params()>=2))
  
  temp_model <- broom::augment(lm(Result ~ Year, data = temp_params()))
   
   
  highcharter::hchart(
   temp_params() %>%
     group_by(Year)%>%
     summarise(Result=round(mean(Result,na.rm = TRUE),2)) %>%
     ungroup(),
   type="scatter",
   hcaes(x = Year, y = Result),
   name = "Degrees C",
   showInLegend = TRUE
 )%>%
   hc_add_series(
     tooltip = list(enabled = FALSE),
     dashStyle = "Dash",
     data = temp_model,
     hcaes(x = Year, y = .fitted),
     showInLegend = TRUE,
     name = "Regression",
     type = "line",
     color = "black"
   ) %>%
   hc_plotOptions(line = list(
     marker = list(
       enabled = FALSE
     )
   ))%>%
   hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'Regression') {
                            return (
                            ' <br>Date: ' + this.point.Year +
                            ' <br>Result: ' + this.point.Result +' Deg C'
                            );
  } else {
                        return false;
                      }
                            }"))%>%
   hc_yAxis(title = list(text = "Degrees C")) %>%
   hc_title(text = "Annual Average (scatter)") %>%
    hc_exporting(enabled = TRUE, 
                 buttons = list(contextButton = list(menuItems = list(
                   list(
                     textKey = "downloadPNG",
                     onclick = JS("function() { this.exportChart(); }")
                   )
                 )))) %>%
    hc_chart(
      backgroundColor = "#FFFFFF" 
    )
 
  })
 

# TSS ---------------------------------------------------------------------

 output$TSS_plot <- renderHighchart({
   
   req(input$navset_tabs_id == "TSS")
   
   tss <- rve_params() %>%
     filter(Param=="Total Suspended Solids") %>%
     group_by(Date) %>%
     mutate(Result=round(mean(Result),2))%>%
     distinct(Date,Result,Units) %>%
     arrange(Date)%>%
     ungroup()
   
   req(nrow(tss)>1)
   
  tss_model <- broom::augment(lm(Result ~ Date, data = tss))
   
   
   tss %>%
     highcharter::hchart(
       type="scatter",
       hcaes(x = Date, y = Result),
       name = "mg/L",
       showInLegend = TRUE
     )%>%
     hc_add_series(
       tooltip = list(enabled = FALSE),
       dashStyle = "Dash",
       data = tss_model,
       hcaes(x = Date, y = .fitted),
       showInLegend = TRUE,
       name = "Regression",
       type = "line",
       color = "black"
     )%>%
     hc_plotOptions(line = list(
       marker = list(
         enabled = FALSE
       )
     ))%>%
     hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'Regression') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' mg/L'
                            );
  } else {
                        return false;
                      }
                            }"))%>%
     hc_yAxis(title = list(text = "mg/L")) %>%
     hc_title(text = "Total Suspended Solids")%>%
     hc_exporting(enabled = TRUE, 
                  buttons = list(contextButton = list(menuItems = list(
                    list(
                      textKey = "downloadPNG",
                      onclick = JS("function() { this.exportChart(); }")
                    )
                  )))) %>%
     hc_chart(
       backgroundColor = "#FFFFFF" 
     )
   
 })
 
 # TSS ---------------------------------------------------------------------
 
 output$Turbidity_plot <- renderHighchart({
   
   req(input$navset_tabs_id == "Turbidity")
   
   turbidity <- rve_params() %>%
     filter(Param=="Turbidity") %>%
     group_by(Date) %>%
     mutate(Result=round(mean(Result),2))%>%
     distinct(Date,Result,Units) %>%
     arrange(Date)%>%
     ungroup()
   
   req(nrow(turbidity)>1)
   
   turbidity_model <- broom::augment(lm(Result ~ Date, data = turbidity))
   
   
   turbidity %>%
     highcharter::hchart(
       type="scatter",
       hcaes(x = Date, y = Result),
       name = "mg/L",
       showInLegend = TRUE
     )%>%
     hc_add_series(
       tooltip = list(enabled = FALSE),
       dashStyle = "Dash",
       data = turbidity_model,
       hcaes(x = Date, y = .fitted),
       showInLegend = TRUE,
       name = "Regression",
       type = "line",
       color = "black"
     )%>%
     hc_plotOptions(line = list(
       marker = list(
         enabled = FALSE
       )
     ))%>%
     hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'Regression') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' NTU'
                            );
  } else {
                        return false;
                      }
                            }"))%>%
     hc_yAxis(title = list(text = "NTU")) %>%
     hc_title(text = "Turbidity")%>%
     hc_exporting(enabled = TRUE, 
                  buttons = list(contextButton = list(menuItems = list(
                    list(
                      textKey = "downloadPNG",
                      onclick = JS("function() { this.exportChart(); }")
                    )
                  )))) %>%
     hc_chart(
       backgroundColor = "#FFFFFF" 
     )
   
 })
 
# Phosphorus --------------------------------------------------------------
output$phos_plot <- renderHighchart({

  req(input$navset_tabs_id == "Phosphorus")
  
  phos <- rve_params() %>%
  filter(Param=="Total Phosphorus") %>%
  group_by(Date) %>%
  mutate(Result=round(mean(Result),2))%>%
  distinct(Date,Result,Units) %>%
  arrange(Date)%>%
  ungroup()

  req(nrow(phos)>1)

  
  phos_model <- broom::augment(lm(Result ~ Date, data = phos))



phos %>%
  highcharter::hchart(
    type="scatter",
    hcaes(x = Date, y = Result),
    name = "mg/L",
    showInLegend = TRUE
  )%>%
  hc_add_series(
    tooltip = list(enabled = FALSE),
    dashStyle = "Dash",
    data = phos_model,
    hcaes(x = Date, y = .fitted),
    showInLegend = TRUE,
    name = "Regression",
    type = "line",
    color = "black"
  )%>%
  hc_plotOptions(line = list(
    marker = list(
      enabled = FALSE
    )
  ))%>%
  hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'Regression') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' mg/L'
                            );
  } else {
                        return false;
                      }
                            }"))%>%
  hc_yAxis(title = list(text = "mg/L")) %>%
  hc_title(text = "Total Phosphorus")%>%
  hc_exporting(enabled = TRUE, 
               buttons = list(contextButton = list(menuItems = list(
                 list(
                   textKey = "downloadPNG",
                   onclick = JS("function() { this.exportChart(); }")
                 )
               )))) %>%
  hc_chart(
    backgroundColor = "#FFFFFF" 
  )

})

 # Ammonia ---------------------------------------------------------------------
 
 output$Ammonia_plot <- renderHighchart({
   
   req(input$navset_tabs_id == "Ammonia")
   
   Ammonia <- rve_params() %>%
     filter(Param=="Ammonia") %>%
     group_by(Date) %>%
     mutate(Result=round(mean(Result),2))%>%
     distinct(Date,Result,Units) %>%
     arrange(Date)%>%
     ungroup()
   
   req(nrow(Ammonia)>1)
   
   Ammonia_model <- broom::augment(lm(Result ~ Date, data = Ammonia))
   
   
   Ammonia %>%
     highcharter::hchart(
       type="scatter",
       hcaes(x = Date, y = Result),
       name = "mg/L",
       showInLegend = TRUE
     )%>%
     hc_add_series(
       tooltip = list(enabled = FALSE),
       dashStyle = "Dash",
       data = Ammonia_model,
       hcaes(x = Date, y = .fitted),
       showInLegend = TRUE,
       name = "Regression",
       type = "line",
       color = "black"
     )%>%
     hc_plotOptions(line = list(
       marker = list(
         enabled = FALSE
       )
     ))%>%
     hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'Regression') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' mg/L'
                            );
  } else {
                        return false;
                      }
                            }"))%>%
     hc_yAxis(title = list(text = "mg/L")) %>%
     hc_title(text = "Turbidity")%>%
     hc_exporting(enabled = TRUE, 
                  buttons = list(contextButton = list(menuItems = list(
                    list(
                      textKey = "downloadPNG",
                      onclick = JS("function() { this.exportChart(); }")
                    )
                  )))) %>%
     hc_chart(
       backgroundColor = "#FFFFFF" 
     )
   
 })
 
 # Bacteria ---------------------------------------------------------------------
 
 output$bacteria_plot <- renderHighchart({
   
   req(input$navset_tabs_id == "Bacteria")
   
   Bacteria <- rve_params() %>%
     filter(Param %in% c("E. coli","Fecal Coliform")) %>%
     group_by(Date,Param) %>%
     mutate(Result=round(mean(Result),2))%>%
     distinct(Date,Result,Units) %>%
     arrange(Date)%>%
     ungroup()
   
   req(nrow(Bacteria)>1)
   
   Bacteria_model <- broom::augment(lm(Result ~ Date, data = Bacteria))
   
   
   Bacteria %>%
     highcharter::hchart(
       type="scatter",
       hcaes(x = Date, y = Result,group=Param, name=Param),
       showInLegend = TRUE
     )%>%
     hc_add_series(
       tooltip = list(enabled = FALSE),
       dashStyle = "Dash",
       data = Bacteria_model,
       hcaes(x = Date, y = .fitted),
       showInLegend = TRUE,
       name = "Regression",
       type = "line",
       color = "black"
     )%>%
     hc_plotOptions(line = list(
       marker = list(
         enabled = FALSE
       )
     ))%>%
     hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'Regression') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' CFU/100mL'
                            );
  } else {
                        return false;
                      }
                            }"))%>%
     hc_yAxis(title = list(text = "CFU/100mL")) %>%
     hc_title(text = "Bacteria")%>%
     hc_exporting(enabled = TRUE, 
                  buttons = list(contextButton = list(menuItems = list(
                    list(
                      textKey = "downloadPNG",
                      onclick = JS("function() { this.exportChart(); }")
                    )
                  )))) %>%
     hc_chart(
       backgroundColor = "#FFFFFF" 
     )
   
 })
 
 
 
# Phosphorus --------------------------------------------------------------
output$orthophos_plot <- renderHighchart({
  
  req(input$navset_tabs_id == "Phosphorus")
  
  ophos <- rve_params() %>%
    filter(Param=="Ortho-Phosphate") %>%
    group_by(Date) %>%
    mutate(Result=round(mean(Result),2))%>%
    distinct(Date,Result,Units) %>%
    arrange(Date)%>%
    ungroup()
  
  req(nrow(ophos)>1)
  
  ophos_model <- broom::augment(lm(Result ~ Date, data = ophos))
  
  ophos %>%
    highcharter::hchart(
      type="scatter",
      hcaes(x = Date, y = Result),
      name = "mg/L",
      showInLegend = TRUE
    )%>%
    hc_add_series(
      tooltip = list(enabled = FALSE),
      dashStyle = "Dash",
      data = ophos_model,
      hcaes(x = Date, y = .fitted),
      showInLegend = TRUE,
      name = "Regression",
      type = "line",
      color = "black"
    )%>%
    hc_plotOptions(line = list(
      marker = list(
        enabled = FALSE
      )
    ))%>%
    hc_tooltip(formatter = JS("function(){
  
  if (this.series.name !== 'Regression') {
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' mg/L'
                            );
  } else {
                        return false;
                      }
                            }"))%>%
    hc_yAxis(title = list(text = "mg/L")) %>%
    hc_title(text = "Ortho-Phosphate")%>%
    hc_exporting(enabled = TRUE, 
                 buttons = list(contextButton = list(menuItems = list(
                   list(
                     textKey = "downloadPNG",
                     onclick = JS("function() { this.exportChart(); }")
                   )
                 )))) %>%
    hc_chart(
      backgroundColor = "#FFFFFF" 
    )
  
})


# Iframes -----------------------------------------------------------------

## Starbuck Flow ----------------------------------------------------------

output$iframe_starbuck <- renderUI({
  
  req(input$navset_tabs_id == "Realtime Flows")
  
  iframe_tag <- tags$iframe(
    src = "https://dashboard.waterdata.usgs.gov/api/gwis/2.1/service/site?agencyCode=USGS&siteNumber=13344500&open=151971",
    style='width:100vw;height:100vh;'
    )

})

## Marengo Flow ----------------------------------------------------------

output$iframe_marengo <- renderUI({
  
  req(input$navset_tabs_id == "Realtime Flows")
  
  iframe_tag <- tags$iframe(
    src = "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/35B150/35B150_DSG_SD.PNG",  
    style='width:90vw;height:100vh;'
  )
  
})

## Ecy Map ----------------------------------------------------------

output$iframe_ecymaps <- renderUI({
  
  req(input$navset_tabs_id == "Realtime Flows")
  
  iframe_tag <- tags$iframe(
    src = "https://apps.ecology.wa.gov/continuousflowandwq/",  
    style='width:100vw;height:100vh;'
  )
  
})



## CTUIR  ----------------------------------------------------------

output$iframe_ctuir_geo <- renderUI({
  iframe_tag <- tags$iframe(
    src = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=799651538e3f4cacb540a7ec8fba1ce7",  
    style='width:95vw;height:90vh;'
  )
  
})

## CTUIR2  ----------------------------------------------------------

output$iframe_ctuir_rest <- renderUI({
  
  # TODO req tab ctuir
  
  iframe_tag <- tags$iframe(
    src = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=a9cb09c5dfb04adbb4110871dce534d5",  
    style='width:95vw;height:90vh;'
  )
  
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

  
  if (nrow(filtered_huc()) >= 1) {
    chart <- highcharter::hchart(filtered_huc(),"column",
   hcaes(x = Name, y = HUC_Acres, group = Name)) %>%
      hc_xAxis(
        labels = list(
          style = list(
            color = "#FFFFFF"  # Color of x-axis labels
          )
        )) %>%
      hc_yAxis(
        labels = list(
          style = list(
            color = "#FFFFFF"  # Color of x-axis labels
          )
        ))%>%
      hc_legend(
        labels = list(
          style = list(
            color = "#FFFFFF"  # Color of x-axis labels
          )
        ))
 
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
    theme = "success"
  )
  
  
  
})



# erosion box ------------------------------------------------------------

erosion_panels <- reactive({
  
  erosion_vars <- huc12 %>%
    select(Severe_Ac_,Mod_Ac_Wtr,No_WtrEr_A) %>%
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  erosion_names <- c("Severe","Moderate","None")
  
  total_huc_acre <-  sum(filtered_huc()$HUC_Acres)
  
 purrr::map2_dfr(
    as.character(erosion_vars),as.character(erosion_names),~
      data.frame(Erosion=.y,
        Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]],na.rm=TRUE), 0))}"),
        Percent=glue::glue("% {round(sum(filtered_huc()[[.x]],na.rm=TRUE)/total_huc_acre*100,2)}"))) 
  
  
})

output$erosion_box <- renderUI({ 
  
  req("Erosion" %in% input$selectInput)
  

  card(id="erosion_id",
  height = "250px",
  card_header("Erosion"),
  DT::datatable(erosion_panels(),options = list(dom = 't'))
  )

  
  })





# wildlife box ------------------------------------------------------------

wildlife_panels <- reactive({
  
  accord_names <- huc12 %>%
    select(ends_with("Ac")) %>% 
    names() %>% 
    str_subset(pattern = "geometry|Weg|Cliff",negate = TRUE)
  
  abv_names <- c("Elk","Mule Deer","WT Deer","Pheasant","Water Fowl","Chuckar","Sheep")
  
  total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)
  
  
purrr::map2_dfr(
    as.character(accord_names),as.character(abv_names),~
      data.frame(Animal=.y,
                 Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]],na.rm=TRUE), 0))}"),
                 Percent=glue::glue("% {round(sum(filtered_huc()[[.x]],na.rm=TRUE)/total_huc_acrew*100,2)}"))) 
  
})

output$wildlife_box <- renderUI({
  
  req("Wildlife" %in% input$selectInput)
  
  card(id="wild_id",
       height = "410px",
       card_header("Wildlife"),
       DT::datatable(wildlife_panels(),options = list(dom = 't'))
  )
  



})

# geo box ------------------------------------------------------------

geo_panels <- reactive({
  
  geo_names <- huc12 %>%
    select(starts_with("GEO")) %>% 
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  abv_geo_names <- c('Moderate','Not rated','Severe' ,'Slight','NA')
  
  total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)
  
  purrr::map2_dfr(
    as.character(geo_names),as.character(abv_geo_names),~
    data.frame(Severity=.y,
                 Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]],na.rm=TRUE), 0))}"),
                 Percent=glue::glue("% {round(sum(filtered_huc()[[.x]],na.rm=TRUE)/total_huc_acrew*100,2)}"))) 
  
  
})

output$geo_box <- renderUI({
  
  req("Geologically Hazardous Areas" %in% input$selectInput)

 
 card(id="geo_id",
      height = "350px",
      card_header("Geologically Hazardous Areas"),
      DT::datatable(geo_panels(),options = list(dom = 't'))
      )
  

})

# wetlands box ------------------------------------------------------------

wet_panels <- reactive({
  
  wet_names <- huc12 %>%
    select(Freshwater.Emergent.Wetland, Freshwater.Forested.Shrub.Wetland,
           Freshwater.Pond,Lake,Other,Riverine) %>% 
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  abv_wet_names <- c("Emergent", "Forested-Shrub",
                     "Pond","Lake","Other","Riverine")
  
  total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)
  
  purrr::map2_dfr(
    as.character(wet_names),as.character(abv_wet_names),~
    data.frame(Type=.y,
    Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]],na.rm=TRUE), 0))}"),
    Percent=glue::glue("% {round(sum(filtered_huc()[[.x]],na.rm=TRUE)/total_huc_acrew*100,2)}"))) 
  
   
})

output$wetlands_box <- renderUI({
  
  req("Wetlands" %in% input$selectInput)

  card(id="wet_id",
       height = "380px",
       card_header("Wetlands"),
       DT::datatable(wet_panels(),options = list(dom = 't'))
  )
  
  # accordion_panel(title = "Wetlands",
  #                 open=TRUE,
  #                 value="Wetlands",
  #                 DT::datatable(geo_panels,options = list(dom = 't'))
  # )
  
})


#land 19 box ------------------------------------------------------------

land19_panels <- reactive({
  
  land19_names <- huc12 %>%
    select(ends_with("_19")) %>% 
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  abv_land19_names <- c("Dryland Crops", "Cultivated Crops",
                        "Rangeland","Irrigated Crops")
  
  total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)

  purrr::map2_dfr(
    as.character(land19_names),as.character(abv_land19_names),~
      data.frame(Type=.y,
                 Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]],na.rm=TRUE), 0))}"),
                 Percent=glue::glue("% {round(sum(filtered_huc()[[.x]],na.rm=TRUE)/total_huc_acrew*100,2)}"))) 
  
})


output$land19_box <- renderUI({
  
  req("Landuse 2019" %in% input$selectInput)
  
  card(id = "ag19_id",
       height = "350px",
       card_header("Agriculture Land Use 2019"),
       card_body(
         DT::datatable(land19_panels(),options = list(dom = 't'),rownames=FALSE)
       )
  )

  
})

#SRP box ------------------------------------------------------------

srp_panels <-  reactive({
  
  srp_names <- huc12 %>%
    select(starts_with("SRP")) %>% 
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  srp_abv_names <- c("Managed for Livestock","Managed for Agriculture","Managed for Livestock")
  
  total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)
  

    data.frame(
            Acres=glue::glue("{scales::comma(round(sum(as.numeric(filtered_huc()$SRP_ACRES),na.rm=TRUE), 0))}"),
            Percent=glue::glue("% {round(sum(as.numeric(filtered_huc()$SRP_ACRES),na.rm=TRUE)/total_huc_acrew*100,2)}")
            )
  
  
  
})


output$srp_box <- renderUI({
  
  req("SRP" %in% input$selectInput)
  
  
  card(id = "srp_id",
       height = "200px",
       card_header("SRP"),
       card_body(
         DT::datatable(srp_panels(),options = list(dom = 't'),rownames=FALSE)
       ))
  
  
  
})

#land 11 box ------------------------------------------------------------

land11_panels <-  reactive({
  
  land11_names <- huc12 %>%
    select("Crops...Dryland","Rangeland","Crops...Irrigated") %>% 
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  abv_land11_names <- c("Dryland Crops","Rangeland","Irrigated Crops")
  
  total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)
  
  purrr::map2_dfr(
    as.character(land11_names),as.character(abv_land11_names),~
      data.frame(Type=.y,
                 Acres=glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]],na.rm=TRUE), 0))}"),
                 Percent=glue::glue("% {round(sum(filtered_huc()[[.x]],na.rm=TRUE)/total_huc_acrew*100,2)}"))) 
  
  
})

output$land11_box <- renderUI({
  
  req("Landuse 2011" %in% input$selectInput)
  

  card(id = "ag11_id",
       height = "300px",
       card_header("Agriculture Land Use 2011"),
       card_body(
         DT::datatable(land11_panels(),options = list(dom = 't'),rownames=FALSE)
         ))
  
  
  
})

output$bmps_table <- renderUI({
  
  req("BMPs" %in% input$selectInput)
  
  
  card(id = "bmps",
       height = "300px",
       card_header("BMPs"),
       card_body(
         DT::datatable(rve_bmps() %>%
                         st_drop_geometry() %>%
                         group_by(Project) %>%
                         tally(),options = list(dom = 't'),rownames=FALSE))
       )
  
  
  
})


# bmps box ----------------------------------------------------------------

 

output$bmps_box <- renderUI({
  
 
req(nrow(filtered_huc())>=1)


  total_bmps <- rve_bmps() %>%
    st_drop_geometry() %>%
    group_by(HUC12) %>%
    tally() %>%
    ungroup()

  
  
  # total_bmps <- max(
  #   nrow(rve_bmps(),0))
  
  # active_bmps <- rve_bmps() %>%
  #   filter(Active=="Yes") %>%
  #   st_drop_geometry() %>%
  #   group_by(HUC12) %>%
  #   tally() %>%
  #   ungroup()
  # 
  # active_bmps <- max(
  #   nrow(rve_bmps() %>%
  #          filter(input$bmps_active=="No")),0)
  # 
  # inactive_bmps <- rve_bmps() %>%
  #   filter(input$bmps_active=="No") %>%
  #   st_drop_geometry() %>%
  #   group_by(HUC12) %>%
  #   tally() %>%
  #   ungroup()
  
  # inactive_bmps <- max(
  #   nrow(rve_bmps() %>%
  #        filter(input$bmps_active=="No")),0)
  # 
  
  #inactive_bmps <- ifelse(length(inactive_bmps==0),"",inactive_bmps)
  
  #total_count <- sum(total_bmps$n[!is.na(total_bmps$HUC12)])
  
 # bmp_count=ifelse(nrow(total_bmps)>1,0,sum(total_bmps)))
 

  value_box(
    title = "Tucannon Watershed BMPs",
    value = sum(total_bmps$n[!is.na(total_bmps$HUC12)]),
    # p(active_bmps$n,"Active"),
    #p(inactive_bmps,"Not active"),
    showcase = bsicons::bs_icon("hammer"),
    theme = "primary"
  )
  
})


# bmps table --------------------------------------------------------------


# output$bmps_plot <- renderUI({
# 
# 
#   req(nrow(rve_bmps()>1))
# 
#     # DT::datatable(bmps %>%
#     #               dplyr::filter(HUC12 %in% filtered_huc()$HUC12),
#     #               options = list(dom = 't')
#     #
# 
#   highcharter::hchart(
#     rve_bmps()  %>%
#       st_drop_geometry() %>%
#       group_by(Project) %>%
#       tally(),
#     "column",
#     hcaes(x = Project, y = n))%>%
#     hc_exporting(enabled = TRUE,
#                  buttons = list(contextButton = list(menuItems = list(
#                    list(
#                      textKey = "downloadPNG",
#                      onclick = JS("function() { this.exportChart(); }")
#                    )
#                  ))))%>%
#     hc_tooltip(formatter = JS("function(){
# 
#   if (this.series.name !== 'TMDL') {
#                             return (
#                             ' <br>Project: ' + this.point.Project +
#                             ' <br>Count: ' + this.point.Count +''
#                             );
#   } else {
#                         return false;
#                       }
#                             }"))
# 
# 
# 
# 
# })
# flood box ------------------------------------------------------------

flood_panels <- reactive({
  
  accord_names <- huc12 %>%
    select(ends_with("FLDACRE")) %>% 
    names() %>% 
    str_subset(pattern = "geometry",negate = TRUE)
  
  
  total_huc_acrew <-  sum(filtered_huc()$HUC_Acres)
  
  data.frame(
    Acres = glue::glue("{scales::comma(round(sum(filtered_huc()[['FLDACRE']],na.rm=TRUE), 0))}"),
    Percent = glue::glue("% {round(sum(filtered_huc()[['FLDACRE']],na.rm=TRUE)/total_huc_acrew*100,2)}")
    )
  
  
})

output$flood_box <- renderUI({
  
  req("Frequently Flooded Areas" %in% input$selectInput)

  
  card(id = "flood_id",
       height = "150px",
       card_header("Frequently Flooded Areas"),
       card_body(DT::datatable(flood_panels(),options = list(dom = 't')))
       )

})


# accordion ---------------------------------------------------------------

# observeEvent({
#   
#   if("Frequently Flooded" %in% input$selectInput) {
#     shinyjs::show("flood_id")
#   } else {
#     shinyjs::hide("flood_id")
#   }
# 
# })

accordion_state <- reactiveVal(NULL)


# observeEvent(input$type_checkbox, {
#   
# # bslib::accordion_panel_update(
# #                     id="acc", 
# #                     target = "Frequently Flooded",
# #                     open=TRUE,
# #                     session = get_current_session())
#   
# accordion_panel_remove("acc", 
#                        target = "Frequently Flooded",
#                        session = session)  
# 
# })


# Leaflet -----------------------------------------------------------------



foundational_map <- reactive({
  
  
  # Hydrography layer options 

  leaflet(options = leafletOptions(
    attributionControl=FALSE)) %>%
    addTiles() %>%
    setView(lat = 46.29929,lng = -118.02250,zoom = 9) %>%
    addWMSTiles(baseUrl = "https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WMSServer?",layers="0",
                options = leaflet::WMSTileOptions(
                  format = "image/png32",
                  version = "1.3.0",
                  minZoom = 3,
                  maxZoom = 16,
                  transparent = TRUE
                ),
                
                group="Waterways") %>%
    addPolygons(data=huc12,
                group="watersheds",
                layerId = huc12$Name,
                color = "black",
                weight = 1,
                highlight = highlightOptions(
                  weight = 3,
                  fillOpacity = 0.2,
                  color = "#545c45",
                  fillColor = "#2c3e50",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                # # Add label info when mouseover
                label = ~Name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "14px",
                  direction = "auto"))%>%
    addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
    addProviderTiles("CartoDB.DarkMatter", group="Dark") %>%
    addProviderTiles("Esri.NatGeoWorldMap", group="Topo") %>%
    addProviderTiles("OpenStreetMap", group="Street") %>%
    addSearchOSM() %>%
    addResetMapButton() 
  

})
  

output$leafmap <- renderLeaflet({
  
pal <- colorFactor(palette = c("goldenrod", "#84563C"), 
                   domain = bmps$Active)
  
  foundational_map()
  
})



user_created_map <- reactive({
  
  # call the foundational Leaflet map
  foundational_map() %>%
    
    # store the view based on UI
    setView( lng = input$map_center$lng
             ,lat = input$map_center$lat
             ,zoom = input$map_zoom
    )
  
})


# map click ---------------------------------------------------------------


clicked_HUC <- reactiveVal(character(0))


observe({
  
  if(length(input$watersheds)==0) {
     clicked_HUC(character(0))
  }

  })



observeEvent(input$leafmap_shape_click,{
  
  req(input$leafmap_shape_click$group == "watersheds")
  
  
  #print(watersheds_selected)
  print(clicked_HUC())
  print(input$watersheds)
  
  
  
  # capture the info of the clicked polygon
  click <- input$leafmap_shape_click
  print(click)
  
  # subset to clicked
  clicked_HUC_data <- unique(as.character(huc12$Name[huc12$Name == click$id]))

  selected_update <- c(input$watersheds,clicked_HUC())
  
  # # Check if the name already exists in clicked_HUC
  # if (clicked_HUC_data  %in% clicked_HUC()) {
  #   # If it exists, remove it from clicked_HUC
  #   clicked_HUC(clicked_HUC()[clicked_HUC() !=  clicked_HUC_data])
  # } else {
  #   # If it doesn't exist, append it to clicked_HUC
  #   # Store in reactive val
  #   clicked_HUC(c(clicked_HUC(), clicked_HUC_data))
  # }
  # 
  # 
  
  # Check if the name already exists in clicked_HUC
  if (clicked_HUC_data  %in% selected_update) {
    # If it exists, remove it from clicked_HUC
    clicked_HUC(selected_update[selected_update !=  clicked_HUC_data])
  } else {
    # If it doesn't exist, append it to clicked_HUC
    # Store in reactive val
    clicked_HUC(c(clicked_HUC_data, clicked_HUC()))
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

# show_additional_card <- reactive({
#   !is.null(input$leafmap_shape_click)
# })
# 
# output$additional_card <- renderUI({
#   
#   req(input$leafmap_shape_click)
# 
#     card(
#       id = "additional_cards",
#       full_screen = TRUE,
#       style = "resize:both;",
#       card_header(verbatimTextOutput("selectedHUC_name")),
#       card_body(DT::dataTableOutput("selectedHUC"))
#     )
# 
# })


# Leaflet Proxy -----------------------------------------------------------
#outputOptions(output, "leafmap", suspendWhenHidden = FALSE)

observe({
  

  
  pal <- colorFactor(palette = c("#84563C", "goldenrod"), 
                     domain = bmps$Active)
  
  pal2 <- colorFactor(palette = c( "#00FF00",  
                                   "#0000FF",              
                                   "#FFA500",                     
                                   "#FFFF00",  
                                   "#808080",                       
                                   "#000000"),
                     domain = wetlands$WETLAND_TY)
  
  
 
  
 # clicked_HUC()
 #  
 # print(filtered_huc())
  
  map <- leafletProxy("leafmap") %>%
    setView(lat = 46.29929,lng = -118.02250,zoom = 10) %>%
    clearShapes() %>%
    clearMarkers() %>%
    clearGroup("bmp_layer") %>%
    removeControl(layerId = "bmp_layer") %>%
    addMapPane("ames_points", zIndex = 490) %>% # shown below ames_circles
    addMapPane("ames_watersheds_selected", zIndex = 410) %>% # shown above ames_lines
    addMapPane("ames_watersheds", zIndex = 400) %>% # shown below ames_circles
    addPolygons(data=wetlands,
                group="wetlands",
                label = ~ACRES,
                fillColor =  ~pal2(WETLAND_TY),
                color =  ~pal2(WETLAND_TY),
                highlight = highlightOptions(
                  weight = 3,
                color = ~pal2(WETLAND_TY)),
                popup = ~ popupTable(wetlands)
                ) %>%
    addPolygons(data=huc12,
                group="watersheds",
                layerId = huc12$Name,
                color = "black",
                weight = 1,
                highlight = highlightOptions(
                  weight = 3,
                  fillOpacity = 0.2,
                  color = "#545c45",
                  fillColor = "#2c3e50",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                # # Add label info when mouseover
                label = ~Name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "14px",
                  direction = "auto"))   %>%
       addPolygons(data=filtered_huc(),
                group="watersheds",
                options = pathOptions(pane = "ames_watersheds"),
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
                label=paste0("",filtered_huc()$Name),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "14px",
                  direction = "auto")
    )  %>%
    #addPolygons(data=private_ag23,popup = ~popupTable(private_ag23)) %>%
    addMarkers(data=stations,group="WQStation",options = pathOptions(pane = "ames_points")) %>%
    addPolygons(data=bmps %>% filter(Type=="SHAPE") ,
                group="BMP",
                options = pathOptions(pane = "ames_points"),
                #label = ~CntrctN,
                popup = ~ popupTable(feature.id = FALSE,bmps,row.numbers = FALSE),
                fillColor = ~pal(Active),
                fillOpacity = 1,
                stroke = TRUE,
                color="black",
                label = ~Project,
                weight= 1,
                highlightOptions = highlightOptions(color = "red", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addPolylines(data=st_zm(bmps %>% filter(Type=="LINE")),
                 group="BMP",
                 options = pathOptions(pane = "ames_points"),
                 fillColor = ~pal(Active),
                 popup = ~ popupTable(feature.id = FALSE,bmps,row.numbers = FALSE),
                 fillOpacity = 1,
                 stroke = TRUE,
                 color="goldenrod",
                 opacity = 1,
                 weight= 3,
                 label = ~Project,
                 highlightOptions = highlightOptions(color = "red", weight = 2,
                                                     bringToFront = TRUE)) %>%
    addCircleMarkers(data=bmps %>% filter(Type=="POINT"),
                     group="BMP",
                     options = pathOptions(pane = "ames_points"),
                     fillColor = ~pal(Active),
                     popup = ~ popupTable(
                       feature.id = FALSE,
                       bmps,
                       row.numbers = FALSE),
                     fillOpacity = 1,
                     stroke = TRUE,
                     color="black",
                     weight= 1,
                     label = ~Project
                     ) %>%
    #addSearchGoogle() %>%
    addLayersControl(
      overlayGroups = c("Waterways","BMP","watersheds","WQStation","wetlands"),
      baseGroups = c("Topo","Imagery", "Dark", "Street")
    ) %>%
    leaflet.extras::addFullscreenControl() %>%
    leafem::addMouseCoordinates() %>%
    addLegend(layerId = "bmp_layer",
              group = "BMP",
              labels = c("Yes","No"),
      "bottomright",
      pal = pal,
      values = c("Yes","No"),
      title = "BMP is Active",
      opacity = 1
    ) %>%
    addLegend(layerId = "wetlands_layer",
              pal=pal2,
              group = "wetlands",
              values = unique(wetlands$WETLAND_TY),
              labels = unique(wetlands$WETLAND_TY),
              "bottomright",
              title = "Wetlands",
              opacity = 1
    ) %>%
    hideGroup(c("Waterways","WQStation","BMP","wetlands"))

  
  
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

   #print(filtered_huc())
   
})



# download map ------------------------------------------------------------


# output$dl <- downloadHandler(
#   
#   filename = paste0( Sys.Date()
#                      , "_customLeafletmap"
#                      , ".html"
#   ),
#   content = function(file){
#     
# saveWidget(
#       widget = user_created_map(),
#       file = file
#     )
# 
#   } 
# ) 


# observeEvent(input$dl, {
#   map_html <- user_created_map() %>% 
#   htmlwidgets::saveWidget(file = "", selfcontained = FALSE)
#   session$sendCustomMessage(
#     "downloadLeafletMap",
#     list(
#       mapData = map_html
#     )
#   )
# })


} #================ End Server===========================================-


