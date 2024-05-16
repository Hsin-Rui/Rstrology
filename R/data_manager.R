#' An R6 Class to communicate between shiny modules
#' 
#' @import R6
#' @import shiny.i18n
#' 
#' @field language current language version
#' 

DataManager <- R6::R6Class(
  "DataManager",
  inherit = shiny.i18n::Translator,
  public = list(
    language = "zh"
  )
)
