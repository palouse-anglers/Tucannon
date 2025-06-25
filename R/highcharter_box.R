hc_boxUI <- function(id) {
  ns <- shiny::NS(id)
  highcharter::highchartOutput(ns("hc_box"))
}


hc_boxServer <- function(id, data, x_var, y_var, round_dig = 2, x_lbl, y_lbl, obs_name, title = NULL, bmp_dat, bmp_lbl) {
  shiny::moduleServer(id, function(input, output, session) {
    output$hc_box <- highcharter::renderHighchart({
      
      # To fix check
      Year <- NULL
      No_BMPS <- NULL
      
      df <- data()
      
      shiny::req(nrow(df)>1)
      
      
      df <- df %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(x_var))) %>%
        dplyr::summarise(
          low = round(min(!!rlang::sym(y_var), na.rm = TRUE), digits = round_dig),
          q1 = round(stats::quantile(!!rlang::sym(y_var), 0.25, na.rm = TRUE), digits = round_dig),
          median = round(stats::median(!!rlang::sym(y_var), na.rm = TRUE), digits = round_dig),
          q3 = round(stats::quantile(!!rlang::sym(y_var), 0.75, na.rm = TRUE), digits = round_dig),
          high = round(max(!!rlang::sym(y_var), na.rm = TRUE), digits = round_dig)
        ) %>% 
        dplyr::ungroup() %>% 
        suppressWarnings()
      
      box_data <- apply(df[, -1], 1, as.list)
      
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "boxplot") %>%
        highcharter::hc_xAxis(categories = df[[x_var]]) %>%
        highcharter::hc_add_series(data = box_data, name = obs_name) %>% 
        highcharter::hc_title(text = title)%>%
        highcharter::hc_xAxis(title = list(text = x_lbl)) %>%
        highcharter::hc_yAxis_multiples(
          list(title = list(text = y_lbl), opposite = FALSE),
          list(showLastLabel = TRUE, 
               opposite = TRUE, 
               title = list(text = bmp_lbl))
        ) %>%
        highcharter::hc_add_series(
          name = "BMPs",
          data = bmp_dat() %>% 
            dplyr::mutate(Year = factor(as.numeric(Year))) %>% # lubridate::ymd( , truncated = 4) 
            dplyr::arrange(Year), 
          highcharter::hcaes(x = Year, y = No_BMPS),
          type = "areaspline",
          fillOpacity = 0.3,
          stacking = "normal",
          zIndex = 0,
          connectNulls = TRUE,
          color = "darkgreen",
          yAxis = 1,
          tooltip = list(
            headerFormat = "Year: {point.key}<br/>",
            pointFormat = "Count: {point.y} BMPs"
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
        )
    })
  })
}