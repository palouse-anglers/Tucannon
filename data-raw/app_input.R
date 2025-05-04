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

usethis::use_data(app_inputs, text_boxes, overwrite = TRUE, internal = TRUE)
