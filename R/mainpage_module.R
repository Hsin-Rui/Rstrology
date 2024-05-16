#' UI part Shiny module: main page
#' 
#' @param id shiny module ID
#' @param i18n a shiny.i18n::Translator object that links different languages
#' 
#' @import shiny
#' @import shinyDatetimePickers
#' 

mainpage_ui <- function(id, i18n){
  
  ns <- NS(id)
  
  fluidPage(
    includeCSS("./inst/www/custom.css"),
    sidebarPanel(
      radioButtons(ns("language"),
                   "",
                   selected="zh",
                   choiceNames = c("",
                                   "English",
                                   "Deutsch"),
                   choiceValues = c("zh",
                                    "en",
                                    "de")),
      width = 12,
    ),
      tags$div(id="readmehere",
               div(id="readmediv",
                    includeHTML(i18n$get_translations()["ui_mainpage_readmefile","zh"])))
    
  )
}
  
#' Server part shiny: language pick
#' 
#' @param id Shiny module ID
#' @param r6 an R6 object to help with communication between modules (for translation etc.)
#' 
#' @return a list with selected language in reactive function
#' 
#' @import gargoyle
#' 

language_pick_server <- function(id, r6){
  
  moduleServer(id, function(input, output, session){
  
    observeEvent(input$language, {  
      r6$language <- input$language
      gargoyle::trigger("change_language")
      })
    
  return(list(
    language = reactive(input$language)
  ))
  })
}

#' Server part shiny: main page
#' 
#' @param id Shiny module ID
#' 
#' @import htmltools
#' 


mainpage_server <- function(id){
  
    moduleServer(id, function(input, output, session){
      observeEvent(input$language, {
        path <- paste("./inst/files/main_", input$language, ".html", sep="")
        
        removeUI(selector="#readmediv",immediate=T)
        insertUI(immediate=T,
                 selector="#readmehere",session=session,
                 ui=div(id="readmediv",
                        includeHTML(path)
                        )
                 )
      })
    })
}

