#' Horoscope calculation app
#'
#' @param ... other parameters
#' 
#' @import shiny
#' @import shinythemes
#' @import shiny.i18n
#' @export
#' 

Rstrology_app <- function(...){
  
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = "./inst/csv/",
                         separator_csv="|")
  active_ui_lang <- grep("ui",i18n$get_languages(), invert=T, value=T)
  #i18n$set_translation_language("zh")
  
  ui <- 
    fluidPage(shiny.i18n::usei18n(i18n),
              
      navbarPage(title = i18n$t("app_name"),
                 theme=shinytheme("flatly"), # Horoscope Calculator
                 
                 tabPanel(i18n$t("ui_nav_page_main"), # Home 
                          mainpage_ui("mainpage")),
                 
                 tabPanel(i18n$t("ui_nav_page_single_chart"),
                          single_chart_ui("single_chart")
                 )
      )
  )
    
  
  server <- function(input, output, session){
    
      # i18n <- reactive({
      #   selected <- input$language
      #   if(length(selected) > 0 && selected %in% i18n$get_languages()){
      #     i18n$set_translation_language(selected)
      #   }
      #   i18n
      # })
    observeEvent(mainpage$language(), {
      shiny.i18n::update_lang(mainpage$language(), session)
    })
    
    mainpage <-  mainpage_server("mainpage")
    
    single_chart_server("single_chart")
  }
  
  
  shiny::shinyApp(ui, server, ...)
}


## TODO. 1. shinyi18n -- successfully integrated. Now make homepage with multiple languages & other pages...
## TODO. 2. location (Taiwan, U.S., Germany, Austria, UK, Japan, Korea, Other Countries but Taiwan first.)