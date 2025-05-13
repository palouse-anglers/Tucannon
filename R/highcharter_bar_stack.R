hc_bar_stackUI <- function(id) {
  ns <- shiny::NS(id)
  highcharter::highchartOutput(ns("hc_bar_stack"))
}


hc_bar_stackServer <- function(id, data, x_var, y_var, group_var, x_lbl, y_lbl, title = NULL, use_n = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    output$hc_bar_stack <- highcharter::renderHighchart({
      
      df <- data()
      
      shiny::req(nrow(df)>1)
      
      if(use_n){
        df <- df %>% 
          dplyr::group_by(across(all_of(c(x_var, group_var)))) %>% 
          dplyr::tally()
        y_var <- "n"
      }
      
      
      df %>%
        highcharter::hchart("column", 
               highcharter::hcaes(x = !!rlang::sym(x_var), 
                                  y = !!rlang::sym(y_var), 
                                  group = !!rlang::sym(group_var)), 
               stacking = "normal")%>%
        {if(!is.null(title)){highcharter::hc_title(.,
          text = title
        )} else .} %>%
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
        )
    })
  })
}