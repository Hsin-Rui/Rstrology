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


