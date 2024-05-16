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

  ui <- 
    fluidPage(shiny.i18n::usei18n(i18n),
              
      navbarPage(title = i18n$t("Horoscope Calculator"),
                 theme=shinytheme("flatly"), # Horoscope Calculator
                 
                 tabPanel(i18n$t("Home"), # Home 
                          mainpage_ui("mainpage", i18n)),
                 
                 tabPanel(i18n$t("Single Chart"),
                          single_chart_ui("single_chart", i18n)
                 )
      )
  )
    
  
  server <- function(input, output, session, ...){
    
    observeEvent(language$language(), {
      shiny.i18n::update_lang(language$language(), session)
    })

    gargoyle::init("change_language")
    r6_object <- DataManager$new(translation_csvs_path = "./inst/csv/", separator_csv="|")

    language <-  language_pick_server("mainpage", r6=r6_object)
    update_select_input_server("single_chart", r6=r6_object)
    
    mainpage_server("mainpage")
    single_chart_server("single_chart")
    
  }
  
  shiny::shinyApp(ui, server, ...)
}


## TODO. 1. cities translation in selectInput
## TODO. 2. calculate astrological elements