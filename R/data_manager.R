#' @title R6 Class to store & process data.
#' @description
#' An R6 Class to communicate between shiny modules
#' 
#' @import R6
#' @import shiny.i18n
#' 

DataManager <- R6::R6Class(
  "DataManager",
  inherit = shiny.i18n::Translator,
  public = list(
    #' @field language (`character()`)\cr
    #' Language selected for shinyApp. Default to Chinese.
    language = "zh",
    #' @field horoscope_datetime (`POSIXct()`)\cr
    #' Date & time of the horoscope. Default to current datetime.
    horoscope_datetime = Sys.Date(),
    #' @field horoscope_timzezone (`character()`)\cr
    #' Time zone. Default to Asia/Taipei
    horoscope_timezone = "Asia/Taipei"
  )
)
