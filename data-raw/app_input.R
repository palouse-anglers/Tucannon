
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
  
  WS = list(
    HUC_name = "HUC 12 Watersheds",
    county_huc = "Columbia County HUC12"
  ),
  
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


## Land use ----

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

huc <- sf::st_read("inst/huc_merge/HUC12_mod.shp", quiet = TRUE) %>%
  dplyr::select(HUC12) %>%
  dplyr::left_join(
    read.csv("inst/huc_merge/HUC12_reworked3.csv") %>%
      dplyr::mutate(HUC12 = as.character(HUC12)),
    by = "HUC12"
  )

names_huc <- names(huc)

huc_labels <- data.table::fread("inst/huc_merge/data_labels_HUC12.csv")

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


# BMPs ----

bmp_points <-  sf::st_read("inst/huc_merge/BMP_points.shp", quiet = TRUE) 
bmp_lines <-  sf::st_read("inst/huc_merge/BMP_line.shp", quiet = TRUE)
bmp_shape <-  sf::st_read("inst/huc_merge/BMP_shape.shp", quiet = TRUE)

bmps <- bmp_points %>%
  dplyr::bind_rows(bmp_lines) %>%
  dplyr::bind_rows(bmp_shape) %>%
  dplyr::mutate(
    active = tidyr::replace_na(ifelse(activty == "ACTIVE", "Yes", "No"), "No"),
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
  dplyr::left_join(huc %>%
            select(HUC12, Name) %>%
            sf::st_drop_geometry(), by="HUC12")


bmps_byyear <- bmps %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(No_BMPS = n()) %>% 
  dplyr::ungroup()%>%
  tidyr::complete(Year = seq(min(.data$Year, na.rm = TRUE),
                             max(.data$Year, na.rm = TRUE), 
                             by = 1), 
                  fill = list("No_BMPS" = 0)) %>% 
  dplyr::arrange(Year) %>%
  dplyr::mutate(Cume_BMPs=cumsum(No_BMPS))



## Stations data ----
### Marengo ----
# station_water previously called marengo_water
# TODO update path
station_water <- data.table::fread("inst/marengo_processed/marengo_water.csv") %>%
  dplyr::mutate(Date = lubridate::ymd(Date))

# station_stage previously called marengo_stage
# TODO update path
station_stage <- data.table::fread("inst/marengo_processed/marengo_stage.csv") %>%
  dplyr::mutate(Date = lubridate::ymd(Date))

### Powers ----
params <-  data.table::fread("inst/powers_raw/EIMDiscreteResults_2024Apr23_8034.csv") %>%
  dplyr::transmute(
    Param = Result_Parameter_Name,
    Date = lubridate::mdy(Field_Collection_Start_Date),
    Year = lubridate::year(Date),
    Units = Result_Value_Units,
    Month = lubridate::month(Date, label = TRUE, abbr = TRUE),
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
    Year2 = factor(Year)
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
                      name = "deg C",
                      tooltip = list(
                        headerFormat = "Month: {point.key}<br/>",
                        pointFormat = "Result: {point.y} deg C"
                      )) %>%
  #hc_rangeSelector(enabled = TRUE) %>%
  highcharter::hc_yAxis(title = list(text = "Temperature deg C")) %>%
  highcharter::hc_title(text = "Monthly Average") %>%
  highcharter::hc_exporting(enabled = TRUE, 
                            buttons = list(contextButton = 
                                             list(menuItems = 
                                                    list(list(
                                                      textKey = "downloadPNG",
                                                      onclick = JS("function() { this.exportChart(); }")
                                                    )))))


by_summer <- temp_params %>%
  dplyr::filter(Year %in% c(2000, 2005, 2011, 2017, 2022, 2023),
                Month %in% c("May", "Jun", "Jul", "Aug")) %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::summarise(Result = round(mean(Result, na.rm = TRUE), 2)) %>%
  dplyr::ungroup() %>%
  highcharter::hchart("bubble", 
                      highcharter::hcaes(x = Month, 
                                         y = Result, 
                                         group = Year),
                      tooltip = list(
                        headerFormat = "Year: {series.name}<br/>Month: {point.key}<br/>",
                        pointFormat = "Result: {point.y} deg C"
                      )) %>%
  highcharter::hc_yAxis(title = list(text = "Temperature deg C")) %>%
  highcharter::hc_title(text = "Summer Months") %>%
  highcharter::hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = list(
    list(
      textKey = "downloadPNG",
      onclick = JS("function() { this.exportChart(); }")
    )
  ))))


# Watersheds ----

watershed_tbl <- tibble::tribble(
  ~choice, ~type, ~column, ~label,
  "Change Detection", "Change", "Imperv", 'Impervious Surface Increase', 
  "Change Detection", "Change", "SImperv", 'Semi-Impervious Surface Increase', 
  "Change Detection", "Change", "TreeDec",'Tree Loss', 
  "Change Detection", "Change", "TotalChg", 'Total Change',
  "Erosion", "Erosion", "No_WtrEr_A", "None",
  "Erosion", "Erosion", "Mod_Ac_Wtr", "Moderate",
  "Erosion", "Erosion", "Severe_Ac_", "Severe",
  "Wildlife", "Animal", "RM_Elk_Ac", "Elk",
  "Wildlife", "Animal", "Mule_Dr_Ac", "Mule Deer",
  "Wildlife", "Animal", "NWWT_Dr_Ac", "WT Deer",
  "Wildlife", "Animal", "RN_Phea_Ac", "Pheasant",
  "Wildlife", "Animal", "WtrFowl_Ac", "Water Fowl",
  "Wildlife", "Animal", "Chuckar_Ac", "Chuckar",
  "Wildlife", "Animal", "BHSheep_Ac", "Sheep",
  "Geologically Hazardous Areas", "Severity", "GEONR", "Not rated",
  "Geologically Hazardous Areas", "Severity", "GEOSLI", "Slight",
  "Geologically Hazardous Areas", "Severity", "GEOMOD", "Moderate",
  "Geologically Hazardous Areas", "Severity", "GEOSEV", "Severe",
  "Geologically Hazardous Areas", "Severity", "GEONA", "NA",
  "Wetlands", "Type", "Freshwater.Emergent.Wetland", "Emergent",
  "Wetlands", "Type", "Freshwater.Forested.Shrub.Wetland", "Forested-Shrub",
  "Wetlands", "Type", "Freshwater.Pond", "Pond",
  "Wetlands", "Type", "Lake", "Lake",
  "Wetlands", "Type", "Riverine", "Riverine",
  "Wetlands", "Type", "Other", "Other",
  "Landuse 2019", "Type", "Crops...Dryland_19", "Dryland Crops",
  "Landuse 2019", "Type", "Cultivated.Crops_19", "Cultivated Crops",
  "Landuse 2019", "Type", "Rangeland_19", "Rangeland",
  "Landuse 2019", "Type", "Crops...Irrigated_19", "Irrigated Crops",
  "SRP", "SRP", "SRP_ACRES", "SRP",
  "Aquifers", "Aquifers", "Ground.Water...Well_Aquifer", "Aquifers",
  "Landuse 2011", "Type", "Crops...Dryland", "Dryland Crops",
  "Landuse 2011", "Type", "Rangeland", "Rangeland",
  "Landuse 2011", "Type", "Crops...Irrigated", "Irrigated Crops",
  "Frequently Flooded Areas", "Frequently Flooded Areas", "FLDACRE", "Frequently Flooded Areas",
  "BMPs", NA, NA, NA,
)



private_ag_2019 <- 354543
private_ag_2019_adj <- 334546
county <- 558037

# TODO update this
usethis::use_data(app_inputs, text_boxes, ag_conservation_areas, ag_geo_haz, ag_crit_aquifer, ag_frqflood, 
                  ag_wetlands, huc, huc_labels, stations, 
                  wetlands, geo_hazard, freq_flood, bmps, bmps_byyear, 
                  station_water, station_stage, params, param_ranges, 
                  by_year, by_month, by_summer, watershed_tbl, private_ag_2019, private_ag_2019_adj, county,
                  overwrite = TRUE, internal = TRUE)
