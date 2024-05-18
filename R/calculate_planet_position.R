#' This function calculate planetary positions and other chart elements by calling functions in swephR
#' planetary positions are calculated by calling swe_calc_ut()
#' ASC, MC, Vertex and house cusps are calculated by calling swe_houses_ex()
#' swe_calc_ut returns 6 double parameters: 1. longitude, 2. latitude, 3. distance, 4. speed in longitude, 5. speed in latitude, 6. speed in distance
#' 
#' @param date A POSIXct class date time string
#' @param timezone A string of time zone. It has to be time zone that lubridate recognizes. Default is "Asia/Taipei"
#' @param city A character string of city along with longitude and latitude (acquired in cities dataset)
#' 
#' @importFrom swephR swe_set_ephe_path swe_calc_ut swe_houses_ex
#' @importFrom here here
#' @importFrom stringr str_extract
#' 

calculate_planet_position <- function(date, timezone, city){
  
  se_path <- (here::here("inst/files/se_data"))
  swe_set_ephe_path(se_path)
  # call date_to_jd convert date to julian day
  jd <- date_to_jd(date = date, timezone = timezone)
  
  # calculate planatory position & speed
  elements <- list(SE$SUN, SE$MOON, SE$MERCURY, SE$VENUS, SE$MARS, SE$JUPITER, SE$SATURN, SE$URANUS, SE$NEPTUNE, SE$PLUTO, SE$CHIRON, SE$MEAN_NODE, SE$TRUE_NODE)
  
  calculate_degree_and_speed <- function(x){swe_calc_ut(jd, x, SE$FLG_SPEED)$xx[c(1,4)]}
  position <- data.frame(sapply(elements, calculate_degree_and_speed))
  names(position) <- c("sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn", "uranus", "neptune", "pluto", "chiron", "mean_node", "true_node")

  # calculate ASC & MC
  longitude <- as.numeric(stringr::str_extract(city, "^.{2,}lng: (.{2,}),", group=1))
  latitude <- as.numeric(stringr::str_extract(city, "lat: (.{1,})$", group=1))
  
  ascension <- swe_houses_ex(jd, 0, geolat = latitude, geolon = longitude, hsys = "W")$ascmc
  asc <- ascension[1] # first element is ASC
  mc <- ascension[2] # second element is MC
  vertex <- ascension[4] # 4th element is Vertex
  
  position$asc <- c(asc, 0)
  position$mc <- c(mc, 0)
  position$vertex <- c(vertex,0)
  
  # calculate house cusps  ## whole sign = W, equal = "E", Placidus = "P", Koch = "K"
  systems <- list("W", "E", "P", "K")

  calculate_house_cusps <- function(x){swe_houses_ex(jd, 0, geolat = latitude, geolon = longitude, hsys = x)$cusps[2:13]} 
  
  house_cusps <- data.frame(sapply(systems, calculate_house_cusps))
  names(house_cusps) <- c("whole_sign", "equal", "placidus", "koch")
  
  return(list(planetary_position=position ,house_cusps=house_cusps))

}

