
# Text inputs ----

app_inputs <- list(
  
  county_name  = "Columbia",
  county_term = "Columbia County",
  
  region = "Tucannon",
  region2 = "Touchet",
  gauge_location = "Marengo",
  gauge_location2 = "Bolles",
  
  repo = "https://github.com/palouse-anglers/Tucannon",
  posit = "https://posit.co",
  
  # water quality ----
  WQ = list(
    usgs_flow_ttl = "Tucannon-Starbuck 13344500",
    wa_eco_discharge_ttl = "Tucannon-Marengo 35B150",
  
    usgs_flow_path = "https://dashboard.waterdata.usgs.gov/api/gwis/2.1/service/site?agencyCode=USGS&siteNumber=13344500&open=151971",
    # wa_eco_flow_path = "https://apps.ecology.wa.gov/continuousflowandwq/",
    wa_eco_flow_path = "https://gis.ecology.wa.gov/portal/apps/instant/basic/index.html?appid=fb8ab17802754f689a0025414c4b8d66&level=8",
    wa_eco_discharge_path = "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/35B150/35B150_DSG_SD.PNG"),
  
  WQ2 = list(
    usgs_flow_ttl = "Touchet-Gallaher 14016820",
    usgs_flow_path = "https://dashboard.waterdata.usgs.gov/api/gwis/2.1.1/service/site?agencyCode=USGS&siteNumber=14016820&open=338720",
    
    wa_eco_discharge_ttl1 = "Touchet-Bolles 32B100",
    wa_eco_discharge_path1 = "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/32B100/32B100_STG_SD.PNG",
    wa_eco_discharge_ttl2 = "Touchet-Dayton 32E050",
    wa_eco_discharge_path2 = "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/32E050/32E050_STG_SD.PNG",
    wa_eco_discharge_ttl3 = "Touchet-Mountain Home Pk. 32K070",
    wa_eco_discharge_path3 = "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/32K070/32K070_STG_SD.PNG"
    
  ),
  
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
    river_rest_geo_ttl2 = "Touchet-Assessment",
    river_rest_geo_head2 = "Touchet Geomorphic Assessment",
  
    river_rest_arcgis = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=799651538e3f4cacb540a7ec8fba1ce7",
    river_rest_geo_arcgis = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=a9cb09c5dfb04adbb4110871dce534d5",
    
    river_rest_geo_arcgis2 = "https://www.arcgis.com/apps/webappviewer/index.html?id=14681ec2671b4b9eab41bb0b786a8caa"
    
    )
  
  
)

text_boxes <- list(
  
  # river restoration ----
  river_rest_ftn = paste0("Web application produced and hosted by the Confederated Tribes of the Umatilla Indian Reservation (CTUIR) GIS Program. ",
                         "Data in this web application is sourced from multiple agencies and publicly available data sources. CTUIR makes no warranty, expressed or implied, including the warranties of merchantability and fitness for a particular purpose, nor assumes any legal liability or responsibility for the accuracy, reliability, completeness or utility of these geospatial data, or for the improper or incorrect use of these geospatial data. The data depicted in this map in no way limit the extent of interests of ",
                         "the Confederated Tribes of Umatilla Indian Reservation (CTUIR).Visit tucannonriver.org for more information")
  
)

custom_legend <- paste(readLines("inst/usda/2011-2024_land_use_comp_legend.html"), collapse = "\n")

# Data ----


## Land use ----


# testing
LU_2011_raw <- data.table::fread("inst/usda/USDA_cdl_huc12_2011.csv") %>% select(-V1)
LU_2011_raw <- LU_2011_raw %>% 
  dplyr::mutate(crop_name_grp = dplyr::case_when(crop_name %in% c("Herbaceous Wetlands", "Open Water", "Woody Wetlands") ~ "Wetlands/Water",
                                                 crop_name %in% c("Alfalfa", "Apples", "Barley", 
                                                                  "Blueberries", "Camelina",  "Canola", 
                                                                  "Cherries", "Chick Peas", "Corn", 
                                                                  "Fallow/Idle Cropland", "Grapes", "Grassland/Pasture",
                                                                  "Lentils", "Mustard", "Oats", "Other Hay/Non Alfalfa", 
                                                                  "Peaches", "Pears", "Peas", "Potatoes",
                                                                  "Sod/Grass Seed", "Soybeans", "Spring Wheat", "Strawberries", 
                                                                  "Triticale", "Walnuts", "Winter Wheat", "Other Tree Crops", "Christmas Trees",
                                                                  "Dry Beans", "Flaxseed", "Herbs", "Nectarines", "Other Crops", "Sweet Corn"
                                                 ) ~ "Cropland/Pasture",
                                                 crop_name %in% c("Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrubland") ~ "Forest/Shrubland",
                                                 crop_name %in% c("Developed/Open Space", "Developed/Low Intensity", 
                                                                  "Developed/Med Intensity", "Developed/High Intensity", "Barren") ~ "Developed/Barren",
                                                 TRUE ~ "Unassigned"
  ))
assertthat::assert_that(!"Unassigned" %in% unique(LU_2011_raw$crop_name_grp))

# sum up acres within HUC and crop_name_grp
LU_2011 <- LU_2011_raw %>% 
  dplyr::group_by(huc12, crop_name_grp) %>% 
  dplyr::summarise(acres = sum(acres)) %>% 
  dplyr::ungroup() 


# LU_2011_primary <- LU_2011 %>% 
#   dplyr::arrange(huc12, desc(acres)) %>% 
#   dplyr::group_by(huc12) %>% 
#   dplyr::slice(1) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::select(-acres)

LU_2024_raw <- data.table::fread("inst/usda/USDA_cdl_huc12_2024.csv") %>% select(-V1)
LU_2024_raw <- LU_2024_raw %>% 
  dplyr::mutate(crop_name_grp = dplyr::case_when(crop_name %in% c("Herbaceous Wetlands", "Open Water", "Woody Wetlands") ~ "Wetlands/Water",
                                                 crop_name %in% c("Alfalfa", "Apples", "Barley", 
                                                                  "Blueberries", "Camelina",  "Canola", 
                                                                  "Cherries", "Chick Peas", "Corn", 
                                                                  "Fallow/Idle Cropland", "Grapes", "Grassland/Pasture",
                                                                  "Lentils", "Mustard", "Oats", "Other Hay/Non Alfalfa", 
                                                                  "Peaches", "Pears", "Peas", "Potatoes",
                                                                  "Sod/Grass Seed", "Soybeans", "Spring Wheat", "Strawberries", 
                                                                  "Triticale", "Walnuts", "Winter Wheat", "Other Tree Crops", "Christmas Trees",
                                                                  "Dry Beans", "Flaxseed", "Herbs", "Nectarines", "Other Crops", "Sweet Corn"
                                                 ) ~ "Cropland/Pasture",
                                                 crop_name %in% c("Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrubland") ~ "Forest/Shrubland",
                                                 crop_name %in% c("Developed/Open Space", "Developed/Low Intensity", 
                                                                  "Developed/Med Intensity", "Developed/High Intensity", "Barren") ~ "Developed/Barren",
                                                 TRUE ~ "Unassigned"
  ))
assertthat::assert_that(!"Unassigned" %in% unique(LU_2024_raw$crop_name_grp))


# sum up acres within HUC and crop_name_grp
LU_2024 <- LU_2024_raw %>% 
  dplyr::group_by(huc12, crop_name_grp) %>% 
  dplyr::summarise(acres = sum(acres)) %>% 
  dplyr::ungroup() 

LU_change <- dplyr::full_join(
  LU_2011 %>% dplyr::rename(acres.2011 = acres),
  LU_2024 %>% dplyr::rename(acres.2024 = acres),
  by = c("huc12", "crop_name_grp")
) %>% dplyr::group_by(crop_name_grp) %>% 
  dplyr::summarise(acres.2011 = sum(acres.2011, na.rm = TRUE),
            acres.2024 = sum(acres.2024, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_longer(
    cols = starts_with("acres."),
    names_to = "year",
    names_prefix = "acres\\.",
    values_to = "acres"
  ) %>% 
  mutate(year = factor(year, levels = c("2011", "2024")))



LU_2024_primary <- LU_2024 %>% 
  dplyr::arrange(huc12, desc(acres)) %>% 
  dplyr::group_by(huc12) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-acres)

ag_conservation_areas <- data.table::fread("inst/huc_merge/ag_conservation_areas.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(
    `Primary Land Use` = AQ1,
    `Private Acres` = Ag_Acres,
    `Species/Habitat` = comname
  )

ag_conservation_areas_cc_raw <-  data.table::fread("inst/cc_huc12/columbia_critical_conservation_huc12.csv") %>% select(-V1)
ag_conservation_areas_usda <- ag_conservation_areas_cc_raw %>% 
  dplyr::left_join(LU_2024_primary, by = "huc12") %>% 
  dplyr::group_by(crop_name_grp, comname) %>% 
  dplyr::summarise(acres = sum(acres)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(crop_name_grp = factor(crop_name_grp, levels = c("Wetlands/Water", "Cropland/Pasture", "Forest/Shrubland", "Developed/Barren")),
                comname = factor(comname)) %>% 
  tidyr::complete(crop_name_grp, comname, fill = list(acres = 0)) %>% 
  dplyr::mutate(crop_name_grp = as.character(crop_name_grp),
                comname = as.character(comname)) %>% 
  dplyr::mutate(acres = round(acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = crop_name_grp,
         `Species/Habitat` = comname,
         `Acres` = acres)

ag_geo_haz <- data.table::fread("inst/huc_merge/ag_geo_haz.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = AQ1,
                `Private Acres` = Ag_Acres)

ag_geo_haz_cc_raw <- data.table::fread("inst/cc_huc12/columbia_geologic_hazard_huc12.csv") %>% dplyr::select(-V1) %>% 
  dplyr::mutate(forpehrtdc = dplyr::if_else(forpehrtdc == "", "Unknown", forpehrtdc))
ag_geo_haz_usda <- ag_geo_haz_cc_raw %>% 
  dplyr::left_join(LU_2024_primary, by = "huc12") %>% 
  dplyr::group_by(crop_name_grp, forpehrtdc) %>% 
  dplyr::summarise(acres = sum(acres)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(crop_name_grp = factor(crop_name_grp, levels = c("Wetlands/Water", "Cropland/Pasture", "Forest/Shrubland", "Developed/Barren")),
                # forpehrtdc = dplyr::if_else(forpehrtdc == "", "Unknown", forpehrtdc),
                forpehrtdc = factor(forpehrtdc)) %>% 
  tidyr::complete(crop_name_grp, forpehrtdc, fill = list(acres = 0)) %>% 
  dplyr::mutate(crop_name_grp = as.character(crop_name_grp),
                forpehrtdc = as.character(forpehrtdc)) %>% 
  dplyr::mutate(acres = round(acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = crop_name_grp,
                `Geo Hazard Severity` = forpehrtdc,
                `Acres` = acres)

ag_crit_aquifer <- data.table::fread("inst/huc_merge/ag_crit_aquifer.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = AQ1, 
                `Private Acres` = Ag_Acres)

ag_crit_aquifer_cc_raw <- data.table::fread("inst/cc_huc12/columbia_aquifer_huc12.csv")
ag_crit_aquifer_usda <- ag_crit_aquifer_cc_raw %>% 
  dplyr::left_join(LU_2024_primary, by = "huc12") %>% 
  dplyr::group_by(crop_name_grp, Aquifer) %>% 
  dplyr::summarise(acres = sum(acres)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(crop_name_grp = factor(crop_name_grp, levels = c("Wetlands/Water", "Cropland/Pasture", "Forest/Shrubland", "Developed/Barren")),
                Aquifer = factor(Aquifer)) %>% 
  tidyr::complete(crop_name_grp, Aquifer, fill = list(acres = 0)) %>% 
  dplyr::mutate(crop_name_grp = as.character(crop_name_grp),
                Aquifer = as.character(Aquifer)) %>% 
  dplyr::mutate(acres = round(acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = crop_name_grp,
                `Acres` = acres)

ag_frqflood <- data.table::fread("inst/huc_merge/ag_frqflood.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = AQ1, 
                `Private Acres` = Ag_Acres)

ag_frqflood_cc_raw <- data.table::fread("inst/cc_huc12/columbia_flooded_huc12.csv") %>% select(-V1)
ag_frqflood_usda <- ag_frqflood_cc_raw %>% 
  dplyr::left_join(LU_2024_primary, by = "huc12") %>% 
  dplyr::group_by(crop_name_grp) %>% 
  dplyr::summarise(acres = sum(acres)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(crop_name_grp = factor(crop_name_grp, levels = c("Wetlands/Water", "Cropland/Pasture", "Forest/Shrubland", "Developed/Barren"))) %>% 
  tidyr::complete(crop_name_grp, fill = list(acres = 0)) %>% 
  dplyr::mutate(crop_name_grp = as.character(crop_name_grp)) %>% 
  dplyr::mutate(acres = round(acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = crop_name_grp,
                `Acres` = acres)

ag_wetlands <- data.table::fread("inst/huc_merge/ag_wetlands.csv") %>%
  dplyr::mutate(Ag_Acres = round(Ag_Acres, 0)) %>%
  dplyr::rename(
    `Primary Land Use` = AQ1,
    `Private Acres` = Ag_Acres,
    `Wetland Type` = WETLAND_TY
  )

ag_wetlands_cc_raw <- data.table::fread("inst/cc_huc12/columbia_wetlands_huc12.csv") %>% select(-V1)
ag_wetlands_usda <- ag_wetlands_cc_raw %>% 
  dplyr::left_join(LU_2024_primary, by = "huc12") %>% 
  dplyr::group_by(crop_name_grp, WETLAND_TY) %>% 
  dplyr::summarise(acres = sum(acres)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(crop_name_grp = factor(crop_name_grp, levels = c("Wetlands/Water", "Cropland/Pasture", "Forest/Shrubland", "Developed/Barren")),
                WETLAND_TY = factor(WETLAND_TY)) %>% 
  tidyr::complete(crop_name_grp, WETLAND_TY, fill = list(acres = 0)) %>% 
  dplyr::mutate(crop_name_grp = as.character(crop_name_grp),
                WETLAND_TY = as.character(WETLAND_TY)) %>% 
  dplyr::mutate(acres = round(acres, 0)) %>%
  dplyr::rename(`Primary Land Use` = crop_name_grp,
                `Wetland Type` = WETLAND_TY,
                `Acres` = acres)

srp_cc_raw <- data.table::fread("inst/cc_huc12/columbia_srp_huc12.csv") %>% select(-V1)
forest_priority_cc_raw <- data.table::fread("inst/cc_huc12/columbia_forest_priority_huc12.csv") %>% select(-V1) %>% 
  dplyr::mutate(Priority = factor(Priority, levels = c("Low", "Moderate","High"))) %>% 
  dplyr::arrange(Priority) %>% 
  dplyr::mutate(Priority = as.character(Priority))


change_cc_raw <- data.table::fread("inst/cc_huc12/columbia_change_detect_2011_2017.csv") %>% 
  tidyr::pivot_longer(-huc12) %>% 
  dplyr::mutate(name = dplyr::case_when(name == "TreeDecAc" ~ "Tree Loss",
                          name == "SemiIncAc" ~ "Semi-Impervious Surface Increase",
                          name == "TotCngAc" ~ "Total Change"),
         name = factor(name, levels = c("Semi-Impervious Surface Increase", "Tree Loss", "Total Change"))) %>% 
  dplyr::arrange(huc12, name) %>% 
  dplyr::mutate(name = as.character(name)) %>% 
  dplyr::rename(acres = value)


# TODO Update data
# huc <- sf::st_read("inst/huc_merge/HUC12_mod.shp", quiet = TRUE) %>%
#   dplyr::select(HUC12) %>%
#   dplyr::left_join(
#     read.csv("inst/huc_merge/HUC12_reworked3.csv") %>%
#       dplyr::mutate(HUC12 = as.character(HUC12)),
#     by = "HUC12"
#   )
# 
# names_huc <- names(huc)

huc <-  sf::st_read("inst/cc_huc12/columbia_county_huc12.shp", quiet = TRUE) %>% 
  rename(Name = name,
         HUC12 = huc12,
         HUC_Acres = acres)

huc_reduce <- huc %>% 
  dplyr::select(Name, huc12 = HUC12, HUC_Acres) %>% 
  sf::st_drop_geometry()

huc_combo <- bind_rows(
    ag_conservation_areas_cc_raw %>% 
      dplyr::mutate(source = "Wildlife",
             huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = comname) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"), 
    ag_geo_haz_cc_raw %>% 
      dplyr::mutate(source = "Geologically Hazardous Areas",
             huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = forpehrtdc) %>%
      dplyr::left_join(huc_reduce, by = "huc12"), 
    ag_crit_aquifer_cc_raw %>% 
      dplyr::mutate(source = "Aquifer",
             huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = Aquifer) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"), 
    ag_frqflood_cc_raw %>% 
      dplyr::mutate(source = "Frequently Flooded Areas",
             huc12 = as.character(huc12)) %>% 
      dplyr::mutate(group = NA) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"), 
    srp_cc_raw %>% 
      dplyr::mutate(source = "SRP",
                    huc12 = as.character(huc12)) %>% 
      dplyr::mutate(group = NA) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"), 
    ag_wetlands_cc_raw %>%
      dplyr::mutate(source = "Wetlands",
             huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = WETLAND_TY) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"),
    change_cc_raw %>%
      dplyr::mutate(source = "Change Detection",
                    huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = name) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"),
    forest_priority_cc_raw %>%
      dplyr::mutate(source = "Forest Priority",
                    huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = Priority) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"),
    LU_2011 %>% 
      dplyr::mutate(source = "USDA Primary Land Use 2011",
                    huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = crop_name_grp) %>% 
      dplyr::left_join(huc_reduce, by = "huc12"),
    LU_2024 %>% 
      dplyr::mutate(source = "USDA Primary Land Use 2024",
                    huc12 = as.character(huc12)) %>% 
      dplyr::rename(group = crop_name_grp) %>% 
      dplyr::left_join(huc_reduce, by = "huc12")
)

huc_sum <- round(sum(huc$HUC_Acres, na.rm = TRUE), 0)


# TODO Update data
# stations <- sf::st_read("inst/huc_merge/stations.shp", quiet = TRUE)

stations <- tibble::tribble(
  ~Name, ~Station, ~lon, ~lat,
  "Tucannon Powers Rd","35B060",-118.1555,46.53766,
  "Tucannon at Marengo","35B150",-117.7501,46.44017,
  "Tucannon at Starbuck","13344500",-118.0663,46.50542,
  "Touchet at Gallaher","14016820",-118.1115722,46.27704167,
  "Touchet at Bolles","32B100",	-118.22115, 	46.274276,
  "Touchet at Dayton","32E050",-117.95306,46.29762,
  "Touchet at Mountain Home Pk.","32K070",-117.89319,46.23676,
)

stations <- sf::st_as_sf(stations, coords = c("lon", "lat"), crs = 4326)

## Wetlands ----

# TODO future: update to pull from geoserver
wetlands <-
  sf::st_read("inst/shapefiles/columbia-wetlands.shp", quiet = TRUE) %>%
  sf::st_transform(., crs = 4326)

# Geologically Hazardous Areas
geo_hazard <- sf::st_read("inst/huc_merge/geo_hazard_huc_merge.shp", quiet = TRUE) %>%
  dplyr::select(frphrtd, weg, muname, Acrs_n_) %>% 
  dplyr::mutate(frphrtd = dplyr::if_else(is.na(frphrtd), "Unknown", frphrtd),
                frphrtd = factor(frphrtd, levels = c("Slight", "Moderate", "Severe", "Not rated", "Unknown")))

# Frequently Flooded Areas
freq_flood <- sf::st_read("inst/huc_merge/freq_flood_huc_merge.shp", quiet = TRUE) %>%
  dplyr::select(SYMBOL, Acrs_n_)


## BMPs ----

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
station_water <- data.table::fread("inst/marengo_processed/marengo_water.csv") %>%
  dplyr::mutate(Date = lubridate::ymd(Date))

# station_stage previously called marengo_stage
station_stage <- data.table::fread("inst/marengo_processed/marengo_stage.csv") %>%
  dplyr::mutate(Date = lubridate::ymd(Date))

### Bolles ----
station_water_2 <- data.table::fread("inst/bolles_processed/bolles_water.csv") %>%
  dplyr::mutate(Date = lubridate::ymd(Date))

# station_stage previously called marengo_stage
station_stage_2 <- data.table::fread("inst/bolles_processed/bolles_stage.csv") %>%
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
                                              onclick = highcharter::JS("function() { this.exportChart(); }")
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
                                                      onclick = highcharter::JS("function() { this.exportChart(); }")
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
      onclick = highcharter::JS("function() { this.exportChart(); }")
    )
  ))))

### Touchet ----
params_2 <-  data.table::fread("inst/touchet_eim_processed/touchet_filtered.csv")

param_ranges_2 <- params_2 %>%
  dplyr::group_by(Param, Units) %>%
  dplyr::summarise(Min_Date = range(Date)[1], 
                   Max_Date = range(Date)[2]) %>%
  dplyr::distinct()

temp_params_2 <- params_2 %>%
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

by_year_2 <- temp_params_2 %>%
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
                                              onclick = highcharter::JS("function() { this.exportChart(); }")
                                            )))))


by_month_2 <- temp_params_2 %>%
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
                                                      onclick = highcharter::JS("function() { this.exportChart(); }")
                                                    )))))


by_summer_2 <- temp_params_2 %>%
  dplyr::filter(Year >= 2014,
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
      onclick = highcharter::JS("function() { this.exportChart(); }")
    )
  ))))

### NRCS ----

nrcs_data <- read.csv("inst/NRCS/clean_nrcs_practices.csv") %>% 
  dplyr::mutate(applied_amount = dplyr::case_when(measurement_unit == "SqFt" ~ applied_amount * 0.0000229568,
                                                  TRUE ~ applied_amount),
                measurement_unit = dplyr::case_when(measurement_unit == "SqFt" ~ "Ac",
                                                    TRUE ~ measurement_unit),
                applied_year = lubridate::year(applied_date)) %>%
  dplyr::filter(!is.na(applied_amount), !is.na(measurement_unit)) %>% 
  dplyr::mutate(land_use = factor(land_use))

# Watersheds ----

private_ag_2019 <- 354543
private_ag_2019_adj <- 334546
county <- 558037

# TODO update this
usethis::use_data(app_inputs, text_boxes, custom_legend, ag_conservation_areas, ag_geo_haz, ag_crit_aquifer, ag_frqflood, 
                  ag_wetlands, 
                  LU_change, LU_2024,
                  ag_conservation_areas_usda, ag_geo_haz_usda, ag_crit_aquifer_usda, ag_frqflood_usda, ag_wetlands_usda,
                  huc_sum,
                  huc, stations, huc_combo, 
                  wetlands, geo_hazard, freq_flood, bmps, bmps_byyear, 
                  station_water, station_stage, params, param_ranges, 
                  station_water_2, station_stage_2,
                  by_year, by_month, by_summer, 
                  nrcs_data,
                  private_ag_2019, private_ag_2019_adj, county,
                  params_2, param_ranges_2, temp_params_2, by_year_2, by_month_2, by_summer_2,
                  overwrite = TRUE, internal = TRUE)
