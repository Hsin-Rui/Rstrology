#' Convert a date/time into Julian Day
#' 
#' @param date A string of date (ISO format) YYYY-MM-DD
#' @param hour An integer of hour.
#' @param minute An integer of minute
#' @param timezone A string of time zone. It has to be time zone that lubridate recognizes. Default is "Asia/Taipei"
#' 
#' @return An integer of Julian Day
#' 


date_to_jd <- function(date, hour, minute, timezone="Asia/Taipei"){
  
  #date <- "1986-02-13"
  #hour <- 20
  #minute <- 30
  
  date <- lubridate::as_datetime(date, tz=timezone)
  lubridate::hour(date) <- hour
  date <- lubridate::as_datetime(date, tz="UTC")
  
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::day(date)
  hour <- lubridate::hour(date)
  
  hour <- hour + minute/60
  
  jd <- swephR::swe_date_conversion(year,month,day,hour,"g")$jd
  
  return(jd)
}

#' Convert current time to Julian Day
#' 
#' 
#' @import lubridate
#' @import swephR
#' 
#' @return Julian day (integer).
#' 

current_time_to_jd <- function(){
  
  current_time <- lubridate::as_datetime(Sys.time(), tz="UTC")
  year <- as.numeric(lubridate::year(current_time))
  month <- as.numeric(lubridate::month(current_time))
  day <- as.numeric(lubridate::day(current_time))
  hour <- as.numeric(lubridate::hour(current_time))
  minute <- as.numeric(lubridate::minute(current_time))

  hour <- hour + minute/60
  jd <- swephR::swe_date_conversion(year,month,day,hour,"g")$jd

  return(jd)
  
}



