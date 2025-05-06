## code to prepare `datasets` dataset goes here

# Following some frop droplet > Tucannon-VSP > calc_acre_in_hu6
acres_in_huc <- function(huc_layer,shape){
  
  sf::st_intersection(huc_layer, shape) %>%
    dplyr::mutate(Acres_in_huc = as.numeric(units::set_units(sf::st_area(.), "acre"))) 
  
}


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
  ) #%>%
  # tidyr::left_join(huc12 %>%
  #             select(HUC12, Name) %>%
  #             sf::st_drop_geometry(), by="HUC12")

# Needs

# DOE Gauge Panel
# marengo_water > R/data_processing/marengo_water.csv
# marengo_stage > R/data_processing/marengo_stage.csv

# Temperature Panel
# by_year > explore_powers.R > temp_params > params > R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv
# params > R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv

# DO, Phosphorus, TSS, Turbidity, Bacteria, Ammonia, Table
# params (see above)

# Data Ranges Panel
# param_ranges > params > R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv

# BMPs Panel
# bmps (see above)

# Watershed Map 
# huc12 > server > inst/huc_merge/HUC12_mod.shp
# bmps (see above)

# Landcover
# ag_crit_aquifer > server > inst/huc_merge/ag_crit_aquifer.csv
# private_ag_2019 # hardcoded in server
# county # hardcoded in server
# ag_wetlands > server > inst/huc_merge/ag_wetlands.csv
# ag_geo_haz > server > inst/huc_merge/ag_geo_haz.csv
# ag_conservation_areas > inst/huc_merge/ag_conservation_areas.csv

# Guidance
# huc_12_labels > R/data_processing/data_labels_HUC12.csv

usethis::use_data(datasets, overwrite = TRUE)
