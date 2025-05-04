app_inputs <- list(
  
  region = "Tucannon",
  
  # water quality
  usgs_flow_path = "https://dashboard.waterdata.usgs.gov/api/gwis/2.1/service/site?agencyCode=USGS&siteNumber=13344500&open=151971",
  wa_eco_flow_path = "https://apps.ecology.wa.gov/continuousflowandwq/",
  wa_eco_discharge_path = "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/35B150/35B150_DSG_SD.PNG",
  
  # river restoration
  river_rest_arcgis = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=799651538e3f4cacb540a7ec8fba1ce7",
  river_rest_geo_arcgis = "https://ctuirgis.maps.arcgis.com/apps/webappviewer/index.html?id=a9cb09c5dfb04adbb4110871dce534d5"
  
)

usethis::use_data(app_inputs, overall = TRUE, internal = TRUE)