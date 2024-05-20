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
#' @param theta a numeric vector of theta value (to determine x and y of the circle)
#' @param adjust_distance a numeric value. The threshold for the absolute distance of theta to decide for which elements a new theta value should be given
#' @param steps how far should the element be moved away each time the iteration is done. 
#' 
#' @importFrom dplyr between
#' @return a numeric vector of theta
#' 

adjust_planet_theta <- function(theta, adjust_distance=650, steps=90) {
  
  for (i in 1:length(theta)) {
    
    distance <- determine_distance(theta, i=i)
      
    while(TRUE %in% (distance==0)) {
      
      theta [distance == 0] <- theta [distance==0] + steps
      distance <- determine_distance(theta, i=i)
      
    }
    
    needs_adjustment <-  dplyr::between(abs(distance), rep(0, length(distance)), rep(adjust_distance, length(distance)))
    should_continue <- TRUE %in% needs_adjustment
    
    while(should_continue) {
      
      if(dplyr::between(theta[i], 0, 3000)) {
        
        theta [needs_adjustment] <- theta [needs_adjustment] + steps # for planets in 7th house, always move to right
        
      } else {
        
        theta  [needs_adjustment & distance > 0] <- theta  [needs_adjustment & distance > 0] - steps # planets on the left move to left
        theta  [needs_adjustment & distance < 0] <- theta  [needs_adjustment & distance < 0] + steps # planets on the right move to right
        
      }
      
      distance <- theta[i] - theta
      distance [i] <- Inf
      needs_adjustment <-  dplyr::between(abs(distance), rep(0, length(distance)), rep(adjust_distance, length(distance)))
      should_continue <- TRUE %in% needs_adjustment
      
    }
    
  }
  
  return(theta)
  
}

#' Examine whether elements are overlapping with each other
#' 
#' @param theta a numeric vector of theta
#' @param criteria targeted distance
#' 
#' @return a signle logical value (TRUE = still needs revision)
#' 

examine_distance <- function(theta, criteria=550){
  
  res <- rep(NA, length(theta))
  
  for (i in 1:length(theta)) {
    
    distance <- theta[i]-theta
    distance [i] <- Inf
    res[i] <- TRUE %in% (abs(distance) < criteria)
      
  }
  
  return(TRUE %in% res)
  
}

#' Optimize until all planets are not overlapping
#' 
#' @param theta a numeric vector
#' @param planets all planets selected for chart
#'

optmize_planet_position <- function(theta, planets){
  
  ## two methods are used to optimize planetary positions
  
  names(theta) <- convert_planet_symbol(planets)
  original_order <- order(theta)
  
  ## 1. this is the first method: starts from the sun, looks for planets that are too close, move them away little by little in iteration
  new_theta <- adjust_planet_theta(theta)  
  should_continue <- examine_distance(new_theta)

  m <- 0
  while(isTRUE(should_continue) & m < 5) {
    
    new_theta <- adjust_planet_theta(new_theta)
    should_continue <- examine_distance(new_theta)
    m <- m + 1
    
  }
  
  ## after that, some normalization is done, so that values are within range (0-36000)
  new_theta <- new_theta [order(new_theta)]
  names(new_theta) <- planets [original_order]
  names(new_theta) <- convert_planet_symbol(names(new_theta))
  new_theta <- normalize_theta(new_theta)
  
  current_order <- names(new_theta)
  should_continue <- examine_distance(new_theta)
  
  ## 2. second method: looks for Stellium, normalize the planets so that they have 600theta distance
  m <- 0
  while(isTRUE(should_continue) & m < 20) {
     
    new_theta <- rescale_planet_theta(new_theta)
    should_continue <- examine_distance(new_theta)
    m <- m+1
    
  }
  
  ## Final check, when planets are in 6 & 7 house, there could be problems (0 degree & 359 degree are next to each other)
  has_western_planets <- (TRUE %in% (new_theta < 1000)) & (TRUE %in% (new_theta > 35000))
  
  if (isTRUE(has_western_planets)) {
    new_theta <- reorder_west_planets(theta, new_theta)
  }
  
  new_theta <- new_theta [match(current_order, names(new_theta))]
  
  return(new_theta)
  
}

#' Given start time, unit, and value, generate a new date time string
#'
#' @param unit unit (Days, Hrs, Min, Mon, Yrs)
#' @param start_time POXIXct class date time
#' @param value a numeric value (1-30)
#' @importFrom lubridate days hours minutes years
#' 


add_datetime <- function(start_time, unit, value){
  
  if (unit=="Day") {
    new_time <- start_time + lubridate::days(value)
    } else if (unit=="Hrs") {
    new_time <- start_time + lubridate::hours(value)
    } else if (unit=="Min") {
    new_time <- start_time + lubridate::minutes(value)
    } else if (unit=="Mon") {
    new_time <- start_time + months(1)
    } else {
    new_time <- start_time + lubridate::years(1)
  }
  
  return(new_time)
  
}

#' Given start time, unit, and value, generate a new date time string
#'
#' @param unit unit (Days, Hrs, Min, Mon, Yrs)
#' @param start_time POXIXct class date time
#' @param value a numeric value (1-30)
#' @importFrom lubridate days hours minutes years
#' 

minus_datetime <- function(start_time, unit, value){
  
  if (unit=="Day") {
    new_time <- start_time - lubridate::days(value)
  } else if (unit=="Hrs") {
    new_time <- start_time - lubridate::hours(value)
  } else if (unit=="Min") {
    new_time <- start_time - lubridate::minutes(value)
  } else if (unit=="Mon") {
    new_time <- start_time - months(1)
  } else {
    new_time <- start_time - lubridate::years(1)
  }
  
  return(new_time)
  
}

#' Normalize theta (ensure theta within range after optimization)
#' 
#' @param theta a numeric vector (integer)
#'

normalize_theta <- function(theta) {
  
  if(TRUE %in% (theta < 0)) theta [theta < 0] <- theta [theta < 0] + 36000
  if(TRUE %in% (theta > 36000)) theta [theta > 36000] <- theta [theta > 36000] - 36000
  
  return(theta)
  
}

#' Determine theta distance
#' 
#' @param x a vector of theta
#' @param i iteration index
#' 

determine_distance <- function(x, i) {
  
  distance <- x[i] - x
  distance [i] <- Inf
  
  return(distance)
}

#' Calculate distance difference of a theta string (prior element - subsequent element)
#' @param x a vector of theta (must be sorted in order)
#' 

calculate_difference <- function(x){
  
  distance <- x- dplyr::lead(x)
  distance[is.na(distance)] <- 0
  
  return(distance)
  
}

#' Optimize theta by re-distributing space equally (applied mainly on stellium)
#' @param x a vector of theta (must be sorted in order)
#' 

rescale_planet_theta <- function(x){
  
  x <- sort(x)
  distance <- calculate_difference(x)
  distance[length(distance)] <- Inf
  
  # get the first planet distance smaller than 600
  position <- min(which(abs(distance) < 600))
  selected <- rep(FALSE, length(x))
  
  for(i in (position):length(x)){
    
    result <- abs(x[i] - x[i+1]) < 750
    selected[i] <- result
    if(!isTRUE(result)) break 
   
  }
  
  selected[i] <- TRUE
  
  if(position > 1) {
    
    for(i in(position):2){
      
      result <- abs(x[i] - x[i-1]) < 750
      selected[i-1] <- result
      if(!isTRUE(result)) break 
      
    }
    
  }
  
  objects_of_interest <- x[selected]


  for (i in 2:(length(objects_of_interest))){
      objects_of_interest[i] <- objects_of_interest[i-1] + 600
  }
  
  x [which(names(x) %in% names(objects_of_interest))] <- objects_of_interest
  return(x)
}

#' Take original theta and manipulated theta, reorder to ensure planets are displayed in correct order
#' 
#' @param theta original theta vector
#' @param new_theta manipulated theta vector
#' 
#' @importFrom dplyr between, case_when
#' 

reorder_west_planets <- function(theta, new_theta){
  
  original <- dplyr::case_when(dplyr::between(theta, 0, 3000) ~ theta + 36000, TRUE ~ theta)
  new <- dplyr::case_when(dplyr::between(new_theta, 0, 3000) ~ new_theta + 36000, TRUE ~ new_theta)
  needs_rescale <- examine_distance(new)
  
  if(isTRUE(needs_rescale)) new <- rescale_planet_theta(new)
  
  original_order <- sort(original)
  new_order <- sort(new)
  
  start_from <- which(names(new_order) %in% names(original_order)[1])
  if(start_from != 1) {
    position <- c(start_from:length(original_order), 1:(start_from)-1)
    new_order <- new_order[position]
  }
  
  needs_reorder <- FALSE %in% (names(original_order) == names(new_order))
  
  if(isTRUE(needs_reorder)) {
    
    original <- sort(original)
    new <- sort(new)
    names(new) <- names(original)
    
  }
  
  new <- dplyr::case_when(new > 36000 ~ new - 36000, TRUE ~ new)
  return(new)
}
