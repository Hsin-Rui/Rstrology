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