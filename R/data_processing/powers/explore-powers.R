

# import data -------------------------------------------------------------
library(dplyr)
library(data.table)
library(lubridate)
library(highcharter)


daily_water_temp <- 
  data.table::fread("R/data_processing/powers/EIMSummarizedTimeSeriesResults_2024Apr23_18288.csv")%>%
  mutate(Date=lubridate::mdy(Field_Collection_Start_Date))


water_temp <- 
  data.table::fread("R/data_processing/powers/EIMTimeSeriesResults_2024Apr23_184405.csv")%>%
  mutate(Date=lubridate::mdy(Field_Collection_Date))


water_ranges <- water_temp %>%
  group_by(Result_Parameter_Name)%>%
  summarise(Min_Date=range(Date)[1],
            Max_Date=range(Date)[2]) %>%
  print(n=Inf)


# Discrete results
params <-  data.table::fread("R/data_processing/powers/EIMDiscreteResults_2024Apr23_8034.csv")%>%
  mutate(Param=Result_Parameter_Name,
         Date=mdy(Field_Collection_Start_Date),
         Result=as.numeric(Result_Value)) %>%
  as_tibble()


param_ranges <- params %>%
  group_by(Param,Result_Value_Units)%>%
  summarise(
            Min_Date=range(Date)[1],
            Max_Date=range(Date)[2]) %>%
  print(n=Inf)


hc_do <- highcharter::hchart(
  params %>%
    filter(Param == "Dissolved Oxygen"),
  type="point",
  hcaes(x = Date, y = Result),
  name = "Dissolved Oxygen mg/L",
  showInLegend = TRUE
) %>%
  hc_add_series(
    data = params %>%
      filter(Param == "Dissolved Oxygen"),
    hcaes(x = Date, y = 8),
    type = "line",
    showInLegend = TRUE,
    dashStyle = "Dash",
    name = "TMDL",
    color = "red") %>%
  hc_tooltip(formatter = JS("function(){
                            return (
                            ' <br>Date: ' + this.point.Date +
                            ' <br>Result: ' + this.point.Result +' mg/L'
                            )
                            }"))  %>%
  hc_rangeSelector(enabled = TRUE)



range(params$Date)


dplyr::glimpse(water_temp)

unique(water_temp$Result_Parameter_Name)



unique(params$Result_Parameter_Name)


