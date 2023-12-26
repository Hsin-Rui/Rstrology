#' Horoscope calculation app
#'
#' @import shiny
#' @import shinythemes
#' @export
#' 

Rstrology_app <- function(...){
  library(shiny)
  library(shinythemes)
  
  ui <- navbarPage(
    "Horoscope Calculator", theme=shinytheme("flatly"),
        tabPanel("Sinle Chart",
                single_chart_ui("single_chart")
        )
  )
  
  server <- function(input, output, session){
      single_chart_server("single_chart")
  }
  
  
  shiny::shinyApp(ui, server, ...)
}
