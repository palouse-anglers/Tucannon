

# import data -------------------------------------------------------------
library(dplyr)
library(data.table)
library(lubridate)
library(highcharter)


daily_water_temp <- 
  data.table::fread("R/data_processing/powers/EIMSummarizedTimeSeriesResults_2024Apr23_18288.csv")%>%
  mutate(Date=lubridate::mdy_hms(Field_Collection_Start_Date_Time))



daily_water_temp_ranges <- daily_water_temp %>%
  group_by(Result_Parameter_Name,Result_Value_Units)%>%
  summarise(Min_Date=range(Date)[1],
            Max_Date=range(Date)[2]) 


water_temp <- 
  data.table::fread("R/data_processing/powers/EIMTimeSeriesResults_2024Apr23_184405.csv")%>%
  mutate(Date=lubridate::mdy_hms(Field_Collection_Date_Time))


water_ranges <- water_temp %>%
  group_by(Result_Parameter_Name,Result_Value_Units)%>%
  summarise(Min_Date=range(Date)[1],
            Max_Date=range(Date)[2]) 


# Discrete results
params <-  data.table::fread("R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv")%>%
  mutate(Param=Result_Parameter_Name,
         Date=mdy(Field_Collection_Start_Date),
         Result=as.numeric(Result_Value)) 


param_ranges <- params %>%
  group_by(Param,Result_Value_Units)%>%
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



