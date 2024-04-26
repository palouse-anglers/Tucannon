#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  

# Do plot -----------------------------------------------------------------
output$do_plot <- renderHighchart({
    
  initial_model <- lm(Result ~ Date, data = params %>%
                        filter(Param == "Dissolved Oxygen"))
  
 regression_line <- data.frame(
    Date = params %>% filter(Param == "Dissolved Oxygen") %>% pull(Date),
    Result = predict(initial_model)
  )
    
  highcharter::hchart(
     params %>%
      filter(Param == "Dissolved Oxygen"),
      type="point",
      hcaes(x = Date, y = Result),
      name = "Dissolved Oxygen mg/L",
      showInLegend = TRUE
    ) %>%
      hc_add_series(tooltip = list(enabled = FALSE),
                    data = params %>%
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
      hc_rangeSelector(enabled = TRUE) %>%
      hc_yAxis(title = list(text = "Dissolved Oxygen mg/L")) %>%
      hc_title(text = "Powers Road") %>%
     hc_add_series(
       data = regression_line,
       hcaes(x = Date, y = Result),
       showInLegend = TRUE,
       name = "Regression",
       type = "line",
       color = "black"
     ) %>%
    hc_responsive(enabled = TRUE)
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
} #--------------------------- End Server---------------------------------


