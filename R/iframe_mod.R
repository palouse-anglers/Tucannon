iframeUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("iframe"))
}
 

iframeServer <- function(id, url, style) {
  shiny::moduleServer(id, function(input, output, session) {
    output$iframe <- shiny::renderUI({
      shiny::tags$iframe(
        src = url,
        style= style
      )
    })
  })
}


# usage
# ui <- shiny::fluidPage(
#   iframeUI("myiframe")
# )
# 
# server <- function(input, output, session) {
#   iframeServer("myiframe", 
#                url = "https://www.wikipedia.org", 
#                style = 'width:90vw;height:90vh;')
# }
# 
# shiny::shinyApp(ui, server)
