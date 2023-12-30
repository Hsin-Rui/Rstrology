#' UI part Shiny module: single chart
#' 
#' @param id shiny module ID
#' @param i18n a shiny.i18n::Translator object that links different languages
#' 
#' @import shiny
#' @import shinyDatetimePickers
#' 

single_chart_ui <- function(id, i18n) {
  
  countries <- unique(cities$country)

  ns <- NS(id)
  
  fluidPage(
    includeCSS("./inst/www/custom.css"),
    sidebarPanel(
      h4(i18n$t("please_enter_data")),
      p(i18n$t("click_to_change")),
      shinyDatetimePickers::datetimePickerInput(ns("date")),
      br(),
      selectizeInput(ns("country"), label="country", choices=countries, selected=countries[1], multiple=FALSE),
      selectizeInput(ns("city"), label="city", choices=cities$city[1], selected=cities$city[1], multiple=FALSE),
      actionButton(ns("more_cities"), label="Other Cities")
    ),
    mainPanel(
      textOutput(ns("date"))
    )
  )
}

#' Server part shiny: single chart
#' 
#' @param id Shiny module ID
#' 
#' @import shiny
#' @import shinyDatetimePickers
#' 


single_chart_server <- function(id){
  
    moduleServer(id, function(input, output, session){
      
      observe({ # input city ####
        if(input$more_cities){
          updateSelectizeInput(session,
                               "city",
                               choices=cities$city [(cities$country %in% input$country)],
                               server=TRUE)
        }
        # else{
        #   updateSelectizeInput(session,
        #                        "city",
        #                        choices=cities$city [(cities$country %in% input$country) & cities$big_city %in% TRUE],
        #                        server=T, selected="Taipei")
        # }
      })
      
      
    output[["date"]]  <- renderText({
         c(as.character(input[["date"]]), 
          cat(as.character(input[["city"]])),
          as.character(input[["city"]]),
          as.character(input[["country"]]))
      })
    })
}
