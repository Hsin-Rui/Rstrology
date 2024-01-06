#' An R6 Class to communicate between shiny modules
#' 
#' @import R6
#' 
#' @field language current language version
#' 

DataManager <- R6::R6Class(
  "DataManager",
  public = list(
    language = "zh"
  )
)