#' UI part Shiny module: single chart
#' 
#' @import shiny
#' @import shinyDatetimePickers
#' 

single_chart_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    includeCSS("www/custom.css"),
    sidebarPanel(
      HTML("<h4>Please enter data</h4>
           <p>Click to change time<br></>"),
      shinyDatetimePickers::datetimePickerInput(ns("date"))
    ),
    mainPanel(
      textOutput(ns("date"))
    )
  )
}

#' Server part shiny: single chart
#' 
#' @import shiny
#' @import shinyDatetimePickers
#' 


single_chart_server <- function(id){
    moduleServer(id, function(input, output, session){
    output[["date"]]  <- renderText({
        as.character(input[["date"]])
      })
    })
}
