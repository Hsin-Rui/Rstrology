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
    "占星計算機", theme=shinytheme("flatly"),
        tabPanel("星盤1",
                sidebarPanel(
                  dateInput("date", label="日期")
                ),
                mainPanel(
                  tabsetPanel(type="tabs",
                              tabPanel("星盤2", shiny::textOutput("text"))
                              )
                )
        )
  )
  
  server <- function(input, output, session){
    output$text <- renderText(
      as.character(input$date)
    )
  }
  
  
  shiny::shinyApp(ui, server, ...)
}
