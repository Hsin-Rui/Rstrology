#' Convert a date/time into Julian Day
#' 
#' @param date A POSIXct class date time string
#' @param timezone A string of time zone. It has to be time zone that lubridate recognizes. Default is "Asia/Taipei"
#' 
#' @importFrom lubridate year month day hour minute is.POSIXct
#' @importFrom swephR swe_date_conversion
#' @importFrom rlang abort
#' 
#' @return An integer of Julian Day
#' 


date_to_jd <- function(date, timezone="Asia/Taipei"){
  
  if (isFALSE(is.POSIXct(date))) rlang::abort(message="input is not POSIXct")
  
  date <- as.character(date)
  date <- lubridate::as_datetime(as.character(date), tz="Asia/Taipei")
  
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::day(date)
  hour <- lubridate::hour(date)
  minute <- lubridate::minute(date)
  
  hour <- hour + minute/60
  
  jd <- swephR::swe_date_conversion(year,month,day,hour,"g")$jd
  
  return(jd)
}

#' A simple function to determine sign order
#' 
#' @param start_from an integer between 1 to twelve
#' @return a sequence of 12 numbers
#' 

define_sign_order <- function(start_from=1){
  
  start_from <- as.integer(start_from)
  
  if(start_from > 12 | start_from < 1) rlang::abort("Values out of range. Input has to be 1 to 12.")
  if(start_from==1) return(1:12)
  if(start_from >1){
    sign_order <- start_from:length(zodiac_sign)
    c(sign_order, 1:(12-length(sign_order)))
  }
  
}

#' Find sodiac sign given a value between 0 and 360
#'
#' @param deg a numeric value between 0 and 360
#' @return a single number for zodiac sign
#' 

find_sign <- function(deg){
  
  if(deg < 0 | deg >= 360){ rlang::abort("Values out of range. Input must be between 0 and 360") }
  if(!is.numeric(deg)) { rlang::abort("Input is not numeric") }
  
  which(dplyr::between(x=rep(deg, 12), left=seq(0, 330, by=30), right=seq(30, 360, by=30)))
  
}

#' Enter a sign number to find its opposite sign
#'
#' @param sign a numeric value between 1 and 12
#' @return an umeric value representing the opposite sign
#' 

find_opposite_sign <- function(sign){
  
  sign <- as.integer(sign)
  
  if(sign < 1 | sign > 12){ rlang::abort("Values out of range. Input must be between 1 and 12") }
  if(!is.numeric(sign)) { rlang::abort("Input is not numeric") }
  
  opposite <- sign + 6
  if (opposite > 12) opposite <- opposite - 12
  
  return(opposite)
  
}

#' Convert planet names to glyphs using AstroGadget.ttf
#'
#' @param x a character vector of planet names
#' @importFrom dplyr recode
#' @return a character vector of alphabets for glyphs
#' 

convert_planet_symbol <- function(x){
  
  dplyr::recode(x, 
                "sun"="A",
                "moon"="B",
                "mercury"="C",
                "venus"="D",
                "mars"="E",
                "jupiter"="F",
                "saturn"="G",
                "uranus"="H",
                "neptune"="I",
                "pluto"="J",
                "true_node"="L",
                "mean_node"="L",
                "asc"="P",
                "mc"="Q",
                "vertex"="Vx",
                "chiron"="U"
                )
  
}

#' Optimize distance of chart objects
#'
#' @param planet_theta a numeric vector of theta value (to determine x and y of the circle)
#' @param adjust_distance a numeric value. The threshold for the absolute distance of theta to decide for which elements a new theta value should be given
#' @param steps how far should the element be moved away each time the iteration is done. 
#' @param selected_elements all planets in the chart (character vector)
#' 
#' @importFrom dplyr between
#' @return a numeric vector of theta
#' 

adjust_planet_theta <- function(planet_theta, selected_elements=selected_elements, adjust_distance=600, steps=50) {
  
  distance_to_asc <- abs(planet_theta [selected_elements %in% "asc"] - planet_theta)
  adjusting_order <- order(distance_to_asc)
  
  for (i in adjusting_order) {
    
    distance <- planet_theta[i] - planet_theta[1:length(planet_theta)]
    distance [distance==0] <- Inf
    nees_adjustment <-  dplyr::between(abs(distance), rep(0, length(distance)), rep(adjust_distance, length(distance)))
    
    while(TRUE %in% nees_adjustment) {
      
      planet_theta  [nees_adjustment & distance > 0] <- planet_theta  [nees_adjustment & distance > 0] - steps # planets on the left move to left
      planet_theta  [nees_adjustment & distance < 0] <- planet_theta  [nees_adjustment & distance < 0] + steps # planets on the right move to right
      
      distance <- planet_theta[i] - planet_theta[1:length(planet_theta)]
      distance [distance==0] <- Inf
      nees_adjustment <-  dplyr::between(abs(distance), rep(0, length(distance)), rep(adjust_distance, length(distance)))
      
    }
    
  }
  
  return(planet_theta)
  
}