mod_watershedUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("cards_ui"))
}

# mod_watershed_cards_server.R
mod_watershedServer <- function(id, selected_choices, filtered_huc, rve_bmps) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Fix check
    choice <- NULL
    Project <- NULL
    Name <- NULL
    n <- NULL
    
    
    ns <- session$ns

    output$cards_ui <- shiny::renderUI({
      shiny::req(filtered_huc(), length(selected_choices() > 0))
      
      total_huc_acrew <- sum(filtered_huc()$HUC_Acres, na.rm = TRUE)
      
      selected_tbl <- watershed_tbl %>%
        dplyr::filter(choice %in% selected_choices())
      
      choice_list <- unique(selected_tbl$choice)
      
      cards <- list()
      
      ## 1. Handle BMPs first if selected
      if ("BMPs" %in% choice_list) {
        bmp_data <- rve_bmps()
        bmp_table <- bmp_data %>%
          sf::st_drop_geometry() %>%
          dplyr::group_by(Project) %>%
          dplyr::tally()
        
        footer_btn <- if (nrow(bmp_data) > 1) {
          shiny::actionButton(ns("show_bmp_plot"), "Show BMP Plot")
        } else NULL
        
        max_rows <- nrow(bmp_table)
        body_height <- if (max_rows <= 5) "400px" else "auto"
        
        bmp_card <- bslib::card(
          id = ns("bmps"),
          full_screen = TRUE,
          style = glue::glue("max-height: {body_height}; overflow-y: auto;"),
          bslib::card_header("BMPs"),
          bslib::card_body(
            DT::datatable(bmp_table, options = list(dom = 't'), rownames = FALSE)
          ),
          if (!is.null(footer_btn)) bslib::card_footer(footer_btn)
        )
        
        cards[[1]] <- bmp_card
        # Remove BMPs from regular choices
        choice_list <- setdiff(choice_list, "BMPs")
      }
      
      ## 2. Add regular cards
      other_cards <- lapply(choice_list, function(choice) {
        df <- selected_tbl %>% dplyr::filter(choice == !!choice)
        
        table_data <- purrr::map2_dfr(
          as.character(df$column), as.character(df$label),
          ~ data.frame(
            Placeholder = .y,
            Acres = glue::glue("{scales::comma(round(sum(filtered_huc()[[.x]], na.rm = TRUE), 0))}"),
            Percent = glue::glue("% {round(sum(filtered_huc()[[.x]], na.rm = TRUE) / total_huc_acrew * 100, 2)}")
          )
        ) 
        
        # Conditionally name or omit the first column
        if (nrow(df) > 1) {
          table_data <- stats::setNames(table_data, c(unique(df$type), "Acres", "Percent"))
        } else {
          table_data <- stats::setNames(table_data[, -1, drop = FALSE], c("Acres", "Percent"))
        }
        
        max_rows <- nrow(table_data)
        body_height <- if (max_rows <= 3) "300px" else "auto"
        
        bslib::card(
          style = glue::glue("max-height: {body_height}; overflow-y: auto;"),
          bslib::card_header(choice),
          bslib::card_body(
            DT::datatable(table_data, options = list(dom = 't'), rownames = FALSE)
          )
        )
      })
      
      cards <- c(cards, other_cards)
      
      bslib::accordion(
        !!!cards,
        always_open = FALSE
      )
      
    })
    
    ## 3. Show modal if BMP button is clicked
    shiny::observeEvent(input$show_bmp_plot, {
      shiny::showModal(
        shiny::modalDialog(
        size = "xl",
        title = "BMP Plot",
        highcharter::highchartOutput(ns("bmps_plot_watersheds"))
      ))
    })
    
    # 4. Render BMP Plot
    output$bmps_plot_watersheds <- highcharter::renderHighchart({
      shiny::req(nrow(rve_bmps()) > 1)
      
      highcharter::hchart(
        rve_bmps() %>%
          sf::st_drop_geometry() %>%
          dplyr::group_by(Name, Project) %>%
          dplyr::tally(),
        "column",
        highcharter::hcaes(x = Name, y = n, group = Project),
        stacking = "normal"
      ) %>%
        highcharter::hc_exporting(enabled = TRUE)
    })
    
  })
}
