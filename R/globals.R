#' define global variables
#' 
#' @import utils
#' @import swephR
#' 

zodiac_sign <- c("a", #aries
                 "s", #taurus
                 "d", #gemini
                 "f", #cancer
                 "g", #leo
                 "h", #virgo
                 "j", #libra
                 "k", #scorpio
                 "l", #sagittarius
                 "z", #capricorn
                 "x", #aquarius
                 "c") #pisces
SE <- swephR::SE

globals <- utils::globalVariables("cities")
globals <- utils::globalVariables("zodiac_sign")
globals <- utils::globalVariables("SE")
