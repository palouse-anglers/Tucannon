hc_ts_wBMPsUI <- function(id) {
  ns <- shiny::NS(id)
  highcharter::highchartOutput(ns("hc_ts"))
}


hc_ts_wBMPsServer <- function(id, data, param, obs_name, y_lbl, x_lbl = "Date", bmp_lbl, title, bmp_dat, href = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$hc_ts <- highcharter::renderHighchart({
      
      # Set NULL to fix check
      Date <- NULL
      Result <- NULL
      .fitted <- NULL
      Year <- NULL
      No_BMPS <- NULL
      . <- NULL
      Param <- NULL
      
      if(length(param)>1){
        grp_var <- c("Param", "Date", "Units")
        arr_var <- c("Param", "Date")
      } else {
        grp_var <- c("Date", "Units")
        arr_var <- c("Date")
      }
      
      if(x_lbl == "Year"){
        obs_tool_tip <- "' <br>Year: ' + Highcharts.dateFormat('%Y', this.x) +"
      } else {
        obs_tool_tip <- "' <br>Date: ' + this.point.Date +"
      }
      
      df <- filter_params(data = data(), 
                          param_vals = param, 
                          group_vars = grp_var, 
                          round_dig = 2, 
                          arr_vars = arr_var)
      
      shiny::req(nrow(df)>1)
      
      mod <- broom::augment(stats::lm(Result ~ Date, data = df))
      
      href_name <- if (!is.null(href)) names(href) else ""
      
      df %>%
        {if(length(param) > 1){
          highcharter::hchart(.,
            type = "scatter",
            mapping = highcharter::hcaes(x = Date, y = Result, group = Param, name = Param),
            showInLegend = TRUE,
            tooltip = list(
              headerFormat = "<span style='font-size: 10px'>{point.key}</span><br/>",
              pointFormat = glue::glue("<b>{{point.y}} {obs_name} </b>")
            )
          )} else { 
            highcharter::hchart(.,
            type = "scatter",
            mapping = highcharter::hcaes(x = Date, y = Result),
            name = obs_name,
            showInLegend = TRUE,
            tooltip = list(
              headerFormat = "<span style='font-size: 10px'>{point.key}</span><br/>",
              pointFormat = glue::glue("<b>{{point.y}} {obs_name} </b>")
            )
          )}} %>% 
        {if(!is.null(href)){
          highcharter::hc_add_series(., tooltip = list(enabled = FALSE),
                        data = df %>%
                          dplyr::select(-Result) %>% 
                          dplyr::mutate(href = href),
                        highcharter::hcaes(x = Date, y = href),
                        type = "line",
                        showInLegend = TRUE,
                        dashStyle = "Dash",
                        name = names(href),
                        color = "red")

        } else .} %>%
        highcharter::hc_yAxis_multiples(
          list(title = list(text = y_lbl), opposite = FALSE),
          list(showLastLabel = TRUE, 
               opposite = TRUE, 
               title = list(text = bmp_lbl))
        ) %>%
        highcharter::hc_add_series(
          data = mod,
          highcharter::hcaes(x = Date, y = .fitted),
          name = "Regression",
          type = "line",
          color = "black",
          tooltip = list(enabled = FALSE),
          dashStyle = "Dash",
          showInLegend = TRUE
        ) %>%
        highcharter::hc_add_series(
          name = "BMPs",
          data = bmp_dat() %>% 
            dplyr::mutate(Year = lubridate::ymd(Year, truncated = 4)), # lubridate::ymd( , truncated = 4) 
          highcharter::hcaes(x = Year, y = No_BMPS),
          type = "areaspline",
          fillOpacity = 0.3,
          stacking = "normal",
          zIndex = 0,
          connectNulls = TRUE, 
          color = "darkgreen",
          yAxis = 1,
          tooltip = list(
            headerFormat = "<span style='font-size: 10px'>{point.key:%Y}</span><br/>",
            pointFormat = "<b>{point.y} BMPs</b>"
          ),
          showInLegend = TRUE,
          visible = FALSE
        ) %>%
        highcharter::hc_plotOptions(
          line = list(
            marker = list(
              enabled = FALSE
            )
          )
        ) %>%
        # turn off regression tooltip
        highcharter::hc_tooltip(formatter = highcharter::JS(glue::glue("function(){{
  
  if (this.series.name == 'BMPs') {{
                            return (
                            ' <br>Year: ' + Highcharts.dateFormat('%Y', this.x) +
                            ' <br>Count: ' + this.y +' BMPs'
                            );
  }} else if (this.series.name === 'Regression' || this.series.name === '{href_name}') {{
        return false;
      }} else  {{
                            return (
                            {obs_tool_tip}
                            ' <br>Result: ' + this.point.Result +' {obs_name}'
                            );
  }}
                            }}")))%>%
        highcharter::hc_title(
          text = title
        ) %>%
        highcharter::hc_xAxis(title = list(text = x_lbl)) %>%
        highcharter::hc_exporting(
          enabled = TRUE, 
          buttons = list(
            contextButton = list(
              menuItems = list(
                list(
                  textKey = "downloadPNG",
                  onclick = highcharter::JS("function() { this.exportChart(); }")
                )
              )
            )
          )
        ) %>%
        highcharter::hc_chart(
          backgroundColor = "#FFFFFF"
        ) #%>%
        # highcharter::hc_xAxis(
        #   type = "datetime"
        # )
    })
  })
}


# usage
# ui <- shiny::fluidPage(
#   hc_ts_wBMPsUI("myhc_ts")
# )
# 
# server <- function(input, output, session) {
#   hc_ts_wBMPsServer("myhc_ts",
#                param = "Total Suspended Solids",
#                obs_name = "mg/L",
#                y_lbl = "TSS mg/L",
#                bmp_lbl = "BMPs/Year",
#                title = "Total Suspended Solids")
# }
# 
# shiny::shinyApp(ui, server)


hc_lineUI <- function(id) {
  ns <- shiny::NS(id)
  highcharter::highchartOutput(ns("hc_line"))
}


hc_lineServer <- function(id, data, obs_name, y_lbl, x_lbl = "Date", title) {
  shiny::moduleServer(id, function(input, output, session) {
    output$hc_line <- highcharter::renderHighchart({
      
      # Set NULL to fix check
      Date <- NULL
      Result <- NULL
      .fitted <- NULL
      
      df <- data()
      
      shiny::req(nrow(df)>1)
      
      mod <- broom::augment(stats::lm(Result ~ Date, data = df))
      
      if(x_lbl == "Year"){
        obs_tool_tip <- "' <br>Year: ' + Highcharts.dateFormat('%Y', this.x) +"
      } else {
        obs_tool_tip <- "' <br>Date: ' + this.point.Date +"
      }
      
      df %>%
        highcharter::hchart(
          type = "line",
          mapping = highcharter::hcaes(x = Date, y = Result),
          tooltip = list(
            headerFormat = "<span style='font-size: 10px'>{point.key}</span><br/>"#,
            # pointFormat = glue::glue("<b>{{point.y}} {obs_name} </b>")
          )
        ) %>%
        highcharter::hc_add_series(
          data = mod,
          highcharter::hcaes(x = Date, y = .fitted),
          name = "Regression",
          type = "line",
          color = "black",
          tooltip = list(enabled = FALSE),
          dashStyle = "Dash",
          showInLegend = TRUE
        ) %>%
        highcharter::hc_plotOptions(
          line = list(
            marker = list(
              enabled = FALSE
            )
          )
        ) %>%
        # turn off regression tooltip
        highcharter::hc_tooltip(formatter = highcharter::JS(glue::glue("function(){{
  
  if (this.series.name === 'Regression') {{
        return false;
      }} else  {{
                            return (
                            {obs_tool_tip}
                            ' <br>Result: ' + this.point.Result +' {obs_name}'
                            );
  }}
                            }}")))%>%
        highcharter::hc_title(
          text = title
        ) %>%
        highcharter::hc_xAxis(title = list(text = x_lbl)) %>%
        highcharter::hc_yAxis(title = list(text = y_lbl)) %>%
        highcharter::hc_exporting(
          enabled = TRUE, 
          buttons = list(
            contextButton = list(
              menuItems = list(
                list(
                  textKey = "downloadPNG",
                  onclick = highcharter::JS("function() { this.exportChart(); }")
                )
              )
            )
          )
        ) %>%
        highcharter::hc_chart(
          backgroundColor = "#FFFFFF"
        ) #%>%
      # highcharter::hc_xAxis(
      #   type = "datetime"
      # )
    })
  })
}