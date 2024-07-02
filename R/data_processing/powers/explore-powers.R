

# import data -------------------------------------------------------------
library(dplyr)
library(data.table)
library(lubridate)
library(highcharter)
library(readxl)
library(stringr)

# # land cover
# Landcover_Crosswalk_Working2 <- read_excel("inst/Landcover_Crosswalk_Working2.xlsx", 
#                                            sheet = "Landcover_Crosswalk")

# Discrete results
params <-  data.table::fread("R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv")%>%
  transmute(Param=Result_Parameter_Name,
         Date=mdy(Field_Collection_Start_Date),
         Year=lubridate::year(Date),
         Units=Result_Value_Units,
         Month=as.character(lubridate::month(Date,label = TRUE, abbr = TRUE)),
         Result=as.numeric(Result_Value)) 



# daily_water_temp <- 
#   data.table::fread("R/data_processing/powers/EIMSummarizedTimeSeriesResults_2024Apr23_18288.csv")%>%
#   mutate(Date=as.Date(lubridate::mdy_hms(Field_Collection_Start_Date_Time))) %>%
#   filter(Statistical_Basis=="Mean")
# 
# all_dates <- as.Date(seq(min(daily_water_temp$Date), max(daily_water_temp$Date), by = "day"))
# missing_dates <- as.Date(setdiff(all_dates, daily_water_temp$Date))
# missing_data <- data.frame(Date = missing_dates, Result_Value = NA)
# 
# # Combine original data and missing data
# combined_temp_data <- daily_water_temp %>%
#   select(Base_Parameter_Name,Date,Result_Value) %>%
#   bind_rows(missing_data) %>%
#   tidyr::fill(Base_Parameter_Name) %>%
#   group_by(Base_Parameter_Name,Date) %>%
#   summarise(Result=mean(Result_Value,na.rm = TRUE)) %>%
#   mutate(Month=lubridate::month(Date,label=TRUE,abbr = TRUE),
#          Year=lubridate::year(Date),
#           Year2=as.factor(Year)
#          ) %>%
#   arrange(Year,Month)
# 
# combined_temp_data %>%
#   group_by(Month) %>%
#   count(is.na(Result))
# 
# 



# water temp --------------------------------------------------------------

temp_params <- params %>%
  filter(Param=="Temperature, water") %>%
  group_by(Date) %>%
  summarise(Result=mean(Result,na.rm = TRUE)) %>%
  mutate(Month=lubridate::month(Date,label=TRUE,abbr = TRUE),
         Year=lubridate::year(Date),
         Year2=as.factor(Year)
  ) %>%
  arrange(Year,Month) %>%
  filter(Result >0) %>%
  ungroup()
  

# regression --------------------------------------------------------------



initial_model_temp <- lm(Result ~ Date, data =temp_params)

regression_line_temp <- data.frame(
  Date = temp_params %>% pull(Date),
  Result = predict(initial_model_temp)
) %>%
  mutate(Year=lubridate::year(Date)) %>%
          group_by(Year) %>%
         mutate(Result=mean(Result)) %>% 
         ungroup() %>%
  distinct(Result,Year)


temp_model <- broom::augment(lm(Result ~ Year, data = temp_params))


# Yearly average ----------------------------------------------------------

by_year <- temp_params %>%
  group_by(Year)%>%
  summarise(Result=round(mean(Result,na.rm = TRUE),2)) %>%
  ungroup() %>%
  hchart("line",hcaes(x = Year, y = Result), name="Degrees C") %>%
  #hc_rangeSelector(enabled = TRUE) %>%
  hc_yAxis(title = list(text = "Degrees C")) %>%
  hc_title(text = "Annual Average")%>%
  hc_exporting(enabled = TRUE, 
               buttons = list(contextButton = list(menuItems = list(
                 list(
                   textKey = "downloadPNG",
                   onclick = JS("function() { this.exportChart(); }")
                 )
               ))))
  

by_month <- temp_params %>%
  group_by(Month)%>%
  summarise(Result=round(mean(Result,na.rm = TRUE),2)) %>%
  ungroup() %>%
  hchart("areaspline",hcaes(x = Month, y = Result), name="Degrees C") %>%
  #hc_rangeSelector(enabled = TRUE) %>%
  hc_yAxis(title = list(text = "Degrees C")) %>%
  hc_title(text = "Monthly Average")%>%
  hc_exporting(enabled = TRUE, 
               buttons = list(contextButton = list(menuItems = list(
                 list(
                   textKey = "downloadPNG",
                   onclick = JS("function() { this.exportChart(); }")
                 )
               ))))
  

by_summer <- temp_params %>%
  filter(Year %in% c(2000,2005,2011,2017,2022,2023),
         Month %in% c("May","Jun","Jul","Aug")) %>%
  group_by(Year,Month)%>%
  summarise(Result=round(mean(Result,na.rm = TRUE),2)) %>%
  ungroup() %>%
  hchart("bubble",hcaes(x= Month,y = Result, group=Year)) %>%
  hc_yAxis(title = list(text = "Degrees C")) %>%
  hc_title(text = "Summer Months")%>%
  hc_exporting(enabled = TRUE, 
               buttons = list(contextButton = list(menuItems = list(
                 list(
                   textKey = "downloadPNG",
                   onclick = JS("function() { this.exportChart(); }")
                 )
               ))))






# air temp ----------------------------------------------------------------

# 
# air_dat <- data_to_boxplot(combined_temp_data %>% 
#                          filter(Result>0) %>% 
#                          filter(Base_Parameter_Name=="Temperature, air") %>%
#                          ungroup(), 
#                        Result, 
#                        group_var = Year,
#                        group_var2 = Month)
# 
# 
# air_temp_box_plot <- highchart() %>%
#   hc_xAxis(type = "category") %>%
#   hc_add_series_list(air_dat) %>%
#   hc_rangeSelector(enabled = TRUE) %>%
#   hc_title(text = "Air Tempereature")
# 
# 




# temp_plot <- hchart(combined_temp_data %>% 
#                       filter(Result>0),
#        "area",
#        hcaes(x = Date, y = Result, group = Base_Parameter_Name)) %>%
#        hc_yAxis(title = list(text = "Celcius"))%>%
#        hc_title(text = "Water")
# 
# temp_plot$set(hc$set(jsCode = "chart.series[0].setVisible(true); chart.series[0].legendItem.element.onclick();")
# )
# 
# hcboxplot(data=
#     combined_temp_data %>% 
#     filter(Result>0),
#   group = Base_Parameter_Name,
#   x = Date,
#   var = Result,
#   name = "Temperature",
#   color = "#2980b9",
#   outliers = TRUE
# )


# 
# daily_water_temp_ranges <- daily_water_temp %>%
#   group_by(Result_Parameter_Name,Result_Value_Units)%>%
#   summarise(Min_Date=range(Date)[1],
#             Max_Date=range(Date)[2]) 
# 
# 
# water_temp <- 
#   data.table::fread("R/data_processing/powers/EIMTimeSeriesResults_2024Apr23_184405.csv")%>%
#   mutate(Date=lubridate::mdy_hms(Field_Collection_Date_Time))
# 
# 
# 
# 
# water_ranges <- water_temp %>%
#   group_by(Result_Parameter_Name,Result_Value_Units)%>%
#   summarise(Min_Date=range(Date)[1],
#             Max_Date=range(Date)[2]) 
# 
# 
# 
# 
param_ranges <- params %>%
  group_by(Param,Units)%>%
summarise(Min_Date=range(Date)[1],
          Max_Date=range(Date)[2]) %>%
  distinct()


# DO ----------------------------------------------------------------------


# hc_do <- highcharter::hchart(
#   params %>%
#     filter(Param == "Dissolved Oxygen"),
#   type="point",
#   hcaes(x = Date, y = Result),
#   name = "Dissolved Oxygen mg/L",
#   showInLegend = TRUE
# ) %>%
#   hc_add_series(tooltip = list(enabled = FALSE),
#     data = params %>%
#     filter(Param == "Dissolved Oxygen") %>%
#     select(-Result),
#     hcaes(x = Date, y = 8),
#     type = "line",
#     showInLegend = TRUE,
#     
#     dashStyle = "Dash",
#     name = "TMDL",
#     color = "red") %>%
#   hc_tooltip(formatter = JS("function(){
#   
#   if (this.series.name !== 'TMDL') {
#                             return (
#                             ' <br>Date: ' + this.point.Date +
#                             ' <br>Result: ' + this.point.Result +' mg/L'
#                             );
#   } else {
#                         return false;
#                       }
#                             }"))  %>%
#   hc_rangeSelector(enabled = TRUE) %>%
#   hc_yAxis(title = list(text = "Dissolved Oxygen mg/L")) %>%
#   hc_title(text = "Powers Road")


# Temp --------------------------------------------------------------------



