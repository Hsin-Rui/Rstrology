#' UI part Shiny module: main page
#' 
#' @param id shiny module ID
#' 
#' @import shiny
#' @import shinyDatetimePickers
#' 

mainpage_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    sidebarPanel(
      radioButtons(ns("language"),
                   "",
                   selected="zh",
                   choiceNames = c("繁體中文",
                                   "English",
                                   "German"),
                   choiceValues = c("zh",
                                    "en",
                                    "de"))
    ),
    #tags$div(id="readmehere",
    #         div(id="readmediv",
    #             tags$h4(i18n$t("ui_mainpage_loading"))))
    mainPanel(
      includeHTML("./inst/html/main_zh.html")
      )
  )
}
  
#' Server part shiny: main page
#' 
#' @param id Shiny module ID
#' @htmltools
#' 


mainpage_server <- function(id){
  
  # includeHTML("./inst/html/main_zh.html")
  moduleServer(id, function(input, output, session){
  output[["choices"]] <- renderText({
    as.character(input[["language"]])
  })
  
  return(list(
    language = reactive(input$language)
  ))
  })
}
