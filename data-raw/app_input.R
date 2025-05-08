
# Text inputs ----

app_inputs <- list(
  
  region = "Tucannon",
  gauge_location = "Marengo",
  
  repo = "https://github.com/palouse-anglers/Tucannon",
  posit = "https://posit.co",
  
  # water quality ----
  WQ = list(
    usgs_flow_ttl = "Tucannon-Starbuck 13344500",
    wa_eco_discharge_ttl = "Tucannon-Marengo 35B150",
  
    usgs_flow_path = "https://dashboard.waterdata.usgs.gov/api/gwis/2.1/service/site?agencyCode=USGS&siteNumber=13344500&open=151971",
    wa_eco_flow_path = "https://apps.ecology.wa.gov/continuousflowandwq/",
    wa_eco_discharge_path = "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/35B150/35B150_DSG_SD.PNG"),
  
  # river restoration ----
  RR = list(
    river_rest_ttl = "Tucannon-Restoration",
    river_rest_head = "Tucannon Restoration Projects",
    river_rest_geo_ttl = "Tucannon-Assessment",
    river_rest_geo_head = "Tucannon Geomorphic Assessment",
  
    river_rest_arcgis = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=799651538e3f4cacb540a7ec8fba1ce7",
    river_rest_geo_arcgis = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=a9cb09c5dfb04adbb4110871dce534d5")
  
)

text_boxes <- list(
  
  # river restoration ----
  river_rest_ftn = paste0("Web application produced and hosted by the Confederated Tribes of the Umatilla Indian Reservation (CTUIR) GIS Program. ",
                         "Data in this web application is sourced from multiple agencies and publicly available data sources. CTUIR makes no warranty, expressed or implied, including the warranties of merchantability and fitness for a particular purpose, nor assumes any legal liability or responsibility for the accuracy, reliability, completeness or utility of these geospatial data, or for the improper or incorrect use of these geospatial data. The data depicted in this map in no way limit the extent of interests of ",
                         "the Confederated Tribes of Umatilla Indian Reservation (CTUIR).Visit tucannonriver.org for more information")
  
)

# Data ----

# Following some from droplet > Tucannon-VSP > calc_acre_in_hu6
acres_in_huc <- function(huc_layer,shape){
  
  sf::st_intersection(huc_layer, shape) %>%
    dplyr::mutate(Acres_in_huc = as.numeric(units::set_units(sf::st_area(.), "acre"))) 
  
}

# Land use ----

ag_conservation_areas <- data.table::fread("inst/huc_merge/ag_conservation_areas.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(
    `Primary Land Use` = AQ1,
    `Private Acres` = Ag_Acres,
    `Species/Habitat` = comname
  )

ag_geo_haz <- data.table::fread("inst/huc_merge/ag_geo_haz.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = AQ1, 
                `Private Acres` = Ag_Acres)

ag_crit_aquifer <- data.table::fread("inst/huc_merge/ag_crit_aquifer.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = AQ1, 
                `Private Acres` = Ag_Acres)

ag_frqflood <- data.table::fread("inst/huc_merge/ag_frqflood.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = AQ1, 
                `Private Acres` = Ag_Acres)

ag_wetlands <- data.table::fread("inst/huc_merge/ag_wetlands.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(
    `Primary Land Use` = AQ1,
    `Private Acres` = Ag_Acres,
    `Wetland Type` = WETLAND_TY
  )


huc12 <- sf::st_read("inst/huc_merge/HUC12_mod.shp", quiet = TRUE) %>%
  dplyr::select(HUC12) %>%
  tidyr::left_join(
    read.csv("inst/huc_merge/HUC12_reworked3.csv") %>%
      mutate(HUC12 = as.character(HUC12)),
    by = "HUC12"
  )

names_huc12 <- names(huc12)

stations <- sf::st_read("inst/huc_merge/stations.shp", quiet = TRUE)

## Wetlands ----

wetlands <-
  sf::st_read("inst/shapefiles/columbia-wetlands.shp", quiet = TRUE) %>%
  sf::st_transform(., crs = 4326)

# Geologically Hazardous Areas
geo_hazard <- sf::st_read("inst/huc_merge/geo_hazard_huc_merge.shp", quiet = TRUE) %>%
  dplyr::select(frphrtd, weg, muname, Acrs_n_)

# Frequently Flooded Areas
freq_flood <- sf::st_read("inst/huc_merge/freq_flood_huc_merge.shp", quiet = TRUE) %>%
  dplyr::select(SYMBOL, Acrs_n_)

huc_12_labels <- data.table::fread("R/data_processing/data_labels_HUC12.csv")

# BMPs ----

bmp_points <-  sf::st_read("inst/huc_merge/BMP_points.shp", quiet = TRUE) 
bmp_lines <-  sf::st_read("inst/huc_merge/BMP_line.shp", quiet = TRUE)
bmp_shape <-  sf::st_read("inst/huc_merge/BMP_shape.shp", quiet = TRUE)

bmps <- bmp_points %>%
  dplyr::bind_rows(bmp_lines) %>%
  dplyr::bind_rows(bmp_shape) %>%
  dplyr::mutate(
    active = tidyr::replace_na(ifelse(activty=="ACTIVE", "Yes", "No"), "No"),
    project = dplyr::case_when(
      stringr::str_detect(project, "METER") ~ "FLOW METER",
      stringr::str_detect(project, "DEVELOPMENT") ~ "WATER DEVELOPMENT",
      TRUE ~ project
    )) %>% 
  dplyr::select(
    HUC12,
    Program = program,
    Project = project,
    Year = instll_,
    Active = active,
    Type = type,
    ID = Cntr_ID
  ) %>%
tidyr::left_join(huc12 %>%
            select(HUC12, Name) %>%
            sf::st_drop_geometry(), by="HUC12")

## Stations data ----
### Marengo ----
# station_water previously called marengo_water
# TODO update path
station_water <- data.table::fread("R/data_processing/marengo_water.csv") %>%
  splyr::mutate(Date = lubridate::ymd(Date))

# station_stage previously called marengo_stage
# TODO update path
station_stage <- data.table::fread("R/data_processing/marengo_stage.csv") %>%
  dplyr::mutate(Date = lubridate::ymd(Date))

### Powers ----
params <-  data.table::fread("inst/powers_raw/EIMDiscreteResults_2024Apr23_8034.csv") %>%
  dplyr::transmute(
    Param = Result_Parameter_Name,
    Date = lubridate::mdy(Field_Collection_Start_Date),
    Year = lubridate::year(Date),
    Units = Result_Value_Units,
    Month = as.character(lubridate::month(Date, label = TRUE, abbr = TRUE)),
    Result = as.numeric(Result_Value)
  ) 

param_ranges <- params %>%
  dplyr::group_by(Param, Units) %>%
  dplyr::summarise(Min_Date = range(Date)[1], 
                   Max_Date = range(Date)[2]) %>%
  dplyr::distinct()

temp_params <- params %>%
  dplyr::filter(Param == "Temperature, water") %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(Result = mean(Result, na.rm = TRUE)) %>%
  dplyr::mutate(
    Month = lubridate::month(Date, label = TRUE, abbr = TRUE),
    Year = lubridate::year(Date),
    Year2 = as.factor(Year)
  ) %>%
  dplyr::arrange(Year, Month) %>%
  dplyr::filter(Result > 0) %>%
  dplyr::ungroup()

by_year <- temp_params %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Result = round(mean(Result, na.rm = TRUE), 2)) %>%
  dplyr::ungroup() %>%
  highcharter::hchart("line", 
                      highcharter::hcaes(x = Year, y = Result), 
                      name = "Degrees C") %>%
  #highcharter::hc_rangeSelector(enabled = TRUE) %>%
  highcharter::hc_yAxis(title = list(text = "Degrees C")) %>%
  highcharter::hc_title(text = "Annual Average") %>%
  highcharter::hc_exporting(enabled = TRUE, buttons = 
                              list(contextButton =
                                     list(menuItems = 
                                            list(list(
                                              textKey = "downloadPNG",
                                              onclick = JS("function() { this.exportChart(); }")
                                            )))))


by_month <- temp_params %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(Result = round(mean(Result, na.rm = TRUE), 2)) %>%
  dplyr::ungroup() %>%
  highcharter::hchart("areaspline", 
                      highcharter::hcaes(x = Month, y = Result), 
                      name = "Degrees C") %>%
  #hc_rangeSelector(enabled = TRUE) %>%
  highcharter::hc_yAxis(title = list(text = "Degrees C")) %>%
  highcharter::hc_title(text = "Monthly Average") %>%
  highcharter::hc_exporting(enabled = TRUE, 
                            buttons = list(contextButton = 
                                             list(menuItems = 
                                                    list(list(
                                                      textKey = "downloadPNG",
                                                      onclick = JS("function() { this.exportChart(); }")
                                                    )))))

# Temperature Panel
# by_year > explore_powers.R > temp_params > params > R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv
# params > R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv

# DO, Phosphorus, TSS, Turbidity, Bacteria, Ammonia, Table
# params (see above)

# Data Ranges Panel
# param_ranges > params > R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv

# Watershed Map 
# huc12 > server > inst/huc_merge/HUC12_mod.shp

# Landcover
# ag_crit_aquifer > server > inst/huc_merge/ag_crit_aquifer.csv
# private_ag_2019 # hardcoded in server
# county # hardcoded in server
# ag_wetlands > server > inst/huc_merge/ag_wetlands.csv
# ag_geo_haz > server > inst/huc_merge/ag_geo_haz.csv
# ag_conservation_areas > inst/huc_merge/ag_conservation_areas.csv

# Guidance
# huc_12_labels > R/data_processing/data_labels_HUC12.csv


usethis::use_data(app_inputs, text_boxes, overwrite = TRUE, internal = TRUE)
