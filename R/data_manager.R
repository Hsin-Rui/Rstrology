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
    horoscope_datetime = Sys.time(),
    #' @field horoscope_timezone (`character()`)\cr
    #' Time zone. Default to Asia/Taipei
    horoscope_timezone = "Asia/Taipei",
    #' @field horoscope_city (`character()`)\cr
    #' City with longitude & latitude. Default to Taipei City
    horoscope_city = NULL,
    #' @field planet_position (`list()`)\cr
    #' A list of dataf rame containing planetary positions and house cusps
    planet_position = NULL,
    #' @field chart (`list()`)\cr
    #' #' ggplot object
    chart = NULL,
    
    #' @description
    #' Initalize the R6 class using the original shiny.i18n Translator class initialize function
    #' furthermore, initialize horoscope city, calculate planatery positions & draw charts
    #' 
    #' @param translation_csvs_path the path to the folder where the csv files for translation are stored
    #' @param separator_csv csv separator
    #' 
    initialize = function(translation_csvs_path = "./inst/csv/", separator_csv="|"){
      super$initialize(translation_csvs_path=translation_csvs_path, separator_csv=separator_csv)
      self$horoscope_city <- cities$city [1]
      self$update_chart()
    },
    
    #' @description
    #' update horoscope. Calculate planetary positions, draw chart
    #' 
    update_chart = function(){
      self$planet_position <- calculate_planet_position(self$horoscope_datetime, self$horoscope_timezone, self$horoscope_city)
      data <- self$planet_position$planetary_position
      data <- data[!(row.names(data) %in% "true_node"),]
      self$chart <- draw_whole_sign_chart(data)
    }
  )
)
