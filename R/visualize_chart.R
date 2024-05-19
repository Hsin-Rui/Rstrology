#' Draw chart template using ggplot
#' 
#' @param style chart style (whole sign, chris brennan, others)
#' 
#' @importFrom showtext showtext_auto
#' @import ggplot2
#' 
#' @return ggplot object (three possible empty chart templates for further plotting)
#' 

draw_chart_template <- function(style="whole sign"){
  
  ## 1. define x,y for for circles

  outer_circle <- get_circle_coords(length.out=1080)
  outer_circle2 <- get_circle_coords(r=0.9, length.out=1080)
  inner_circle <- get_circle_coords(r=0.5, length.out=1080)
  inner_circle2 <- get_circle_coords(r=0.4, length.out=1080)

  mytheme<- list(theme_void()+theme(panel.background = element_rect(colour= "white", fill= "white")))
  
  ## 2. call showtext_auto so that astrological symbol will be rendered
  
  load_fonts()
  showtext::showtext_auto()
  
  ## 3. define sign division & whole sign house cusps
  
  equal_division <- seq(from=1, by=1080/12, length.out=12)
  
  sign_x <- outer_circle$x [equal_division]
  sign_x_end <- outer_circle2$x [equal_division]
  sign_y <- outer_circle$y [equal_division]
  sign_y_end <- outer_circle2$y [equal_division]
  
  cusps_x <- inner_circle2$x [equal_division]
  cusps_y <- inner_circle2$y [equal_division]
  
  ## 4. define x, y of house number for whole sign chart
  
  house_position <- get_circle_coords(r=0.45, length.out=360)
  house_x <- house_position$x[seq(from=15, by=30, length.out=12)]
  house_y <- house_position$y[seq(from=15, by=30, length.out=12)]
  house_number <- as.character(1:12)
  
  ## 5. draw chris brennan style chart template
  
  p_chris_prennan <- 
    ggplot()+
      geom_path(aes(x=outer_circle$x,y=outer_circle$y), linewidth=0.3)+
      geom_path(aes(x=outer_circle2$x, y=outer_circle2$y), linewidth=0.3)+
      mytheme+
      coord_equal()+
      geom_segment(aes(x=sign_x, y=sign_y, xend=0, yend=0), color="black", linewidth=0.3)+
      # define the size of graph. the outer circle has x & y of -1 to 1.
      xlim(-1.05, 1.05)+
      ylim(-1.05, 1.05)
  
  if(style=="chris brennan") return(p_chris_prennan)

  ## 6. draw common parts of the template
  
  p_common <-  
    ggplot()+
    # draw four circles
    geom_path(aes(x=outer_circle$x,y=outer_circle$y), linewidth=0.3)+
    geom_path(aes(x=outer_circle2$x, y=outer_circle2$y), linewidth=0.3)+
    geom_path(aes(x=inner_circle$x, y=inner_circle$y), linewidth=0.3)+
    geom_path(aes(x=inner_circle2$x, y=inner_circle2$y), linewidth=0.3)+
    # define the size of graph. the outer circle has x & y of -1 to 1.
    xlim(-1.10, 1.10)+
    ylim(-1.05, 1.05)+
    # add custom theme (white background etc.)
    mytheme+
    # make coordinates x & y equally long
    coord_equal()
  
  ## 7. draw whole sign chart template
  p_whole_sign <-
    p_common +
    geom_segment(aes(x=sign_x, y=sign_y, xend=sign_x_end, yend=sign_y_end), color="black", linewidth=0.3) +
    # house division
    geom_segment(aes(x=cusps_x, y=cusps_y, xend=sign_x_end, yend=sign_y_end), color="grey50", linewidth=0.2) +
    # house number
    geom_text(aes(x=house_x, y=house_y, label=c(7:12, 1:6)), size=3.5)
  
  if(style=="whole sign") return(p_whole_sign)
  
  ## 8. draw template for whole sign house chart
  sign_x
  
  p_quadrant <-
    p_common+
      geom_segment(aes(x=cusps_x[c(1,7)], y=cusps_y[c(1,7)], xend=c(1.08, -1.08), yend=sign_y[c(1,7)]), 
                   color="black", linewidth=0.4, 
                   arrow = arrow(length = unit(0.15, "inches")))

  if(style=="others") return(p_quadrant)
  
}

#' Load fonts to show astrological symbols
#' 
#' @importFrom sysfonts font_add
#'

load_fonts <- function(){
  
    sysfonts::font_add(family="AstroGadget", regular=here("inst/fonts/AstroGadget.ttf"))
    sysfonts::font_add(family="HamburgSymbols", regular=here("inst/fonts/HamburgSymbols.ttf"))
    sysfonts::font_add(family="AstroDotBasic", regular=here("inst/fonts/AstroDotBasic.ttf"))
    
}

#' Calculate x and y of a circle
#' 
#' @param r rate of the circle
#' @param ... all other argumebts
#' 
#' @importFrom tibble tibble
#' @import magrittr
#' 

get_circle_coords <- function(r = 1, ...) {
  tibble::tibble(theta = seq(0, 2 * pi, ...),
                 x     = cos(theta) * r,
                 y     = sin(theta) * r)
}

#' Visualize chart in whole sign stype
#' 
#' @param planet_position a data frame (obtained by calculate_planet_position)
#' 

draw_whole_sign_chart <- function(planet_position){
  
  load_fonts()
  showtext_auto()
  
  p <- readRDS(here::here("./inst/ggplot_objects/p_empty_whole_sign.rds"))
  
  selected_elements <- row.names(planet_position)
  # 1. put on zodiac sign 
  ## determine sign order
  
  asc_sign <- find_sign(planet_position$deg [selected_elements %in% "asc"])
  sign_order <- define_sign_order(asc_sign)
  
  ## get coordinates of x and x
  
  circle <- get_circle_coords(r=0.95, length.out=156)
   
  sign_x <- circle$x[seq(from=7, by=13, length.out=12)] ## the seventh value is near by x = -1
  sign_y <- circle$y[seq(from=7, by=13, length.out=12)]
  
  sign_x <- sign_x [define_sign_order(7)]
  sign_y <- sign_y [define_sign_order(7)]
  
  p <- 
    p +
    geom_text(aes(x=sign_x, y=sign_y, label=zodiac_sign[sign_order]), family="HamburgSymbols", size=6, color=zodiac_sign_color[sign_order])
  
  ## 2. put on planets etc.
  
  ## let the seventh house (x=1) be the starting point
  starting_sign <- find_opposite_sign(asc_sign)
  starting_deg <- (starting_sign - 1)*30
  
  coords_planet_points <- get_circle_coords(r=0.9, length.out=36000)
  coords_planet_glyphs <- get_circle_coords(r=0.82, length.out=36000)
  coords_lines <- get_circle_coords(r=0.87, length.out=36000)
  
  ## x and y of geom_point for exact planetary position (r = 0.9)
  planet_on_circle <- as.vector(planet_position$deg - starting_deg) # re-scaling
  planet_on_circle [planet_on_circle < 0] <- planet_on_circle [planet_on_circle < 0] + 360 # position (on the circle) of planets

  planet_theta <- as.integer(planet_on_circle /360 *36000) + 1
  planet_x_on_circle <- coords_planet_points$x [planet_theta[order(planet_theta)]]
  planet_y_on_circle <- coords_planet_points$y [planet_theta[order(planet_theta)]]
  
  ## determine the position of planet glyphs
  
  new_theta <- optmize_planet_position(planet_theta, selected_elements = selected_elements)
  
  planet_x_glyphs <- coords_planet_glyphs$x [new_theta] 
  planet_y_glyphs <- coords_planet_glyphs$y [new_theta]
  
  planet_position <- planet_position[match(names(new_theta), planet_position$planet_glyphs),]
  
  replaced <- planet_theta[order(planet_theta)] != new_theta # check which elements have been moved for better plotting

  ## draw lines only for those being manipulated
  lines_end_x <- coords_lines$x [new_theta] [replaced]
  lines_end_y <- coords_lines$y [new_theta] [replaced]
  lines_x <- coords_planet_points$x [planet_theta[order(planet_theta)]] [replaced]
  lines_y <- coords_planet_points$y [planet_theta[order(planet_theta)]] [replaced]
  
  ## sign glyphs to indicate position
  planet_sign_coord <- get_circle_coords(r=0.66, length.out=36000)
  planet_sign_x <- planet_sign_coord$x [new_theta]
  planet_sign_y <- planet_sign_coord$y [new_theta]
  
  ## degree
  deg <- paste(planet_position$deg_in_sign, "\u00b0", sep="")
  deg_coord <- get_circle_coords(r=0.73, length.out=36000)
  deg_x <- deg_coord$x [new_theta]
  deg_y <- deg_coord$y [new_theta]
  
  ## minutes
  minute <- paste(planet_position$min_in_sign, "'", sep="")
  min_coord <- get_circle_coords(r=0.6, length.out=36000)
  min_x <- min_coord$x [new_theta] 
  min_y <- min_coord$y [new_theta]
  
  ## retrograde planets
  degree_color <- dplyr::case_when(planet_position$speed < 0 ~ "darkred", TRUE ~ "black")
  retrograde_coord <- get_circle_coords(r=0.56, length.out=36000)
  retrograde_x <- retrograde_coord$x [new_theta] [planet_position$speed < 0]
  retrograde_y <- retrograde_coord$y [new_theta] [planet_position$speed < 0]

  p +
    ## put on zodiac signs
    geom_point(aes(x=planet_x_on_circle, y=planet_y_on_circle), color=planet_position$planet_color)+
    ## put on planetary glyphs
    geom_text(aes(x=planet_x_glyphs, y=planet_y_glyphs, label=names(new_theta)), family=planet_position$font_gpyphs, size=planet_position$font_size)+
    ## draw lines to clearly indicate planetary position
    geom_segment(aes(x=lines_x, xend=lines_end_x, y=lines_y, yend=lines_end_y), color="grey65") +
    geom_text(aes(x=planet_sign_x, y=planet_sign_y, label=zodiac_sign[planet_position$sign]), family="HamburgSymbols", color=planet_position$planet_color)+
    geom_text(aes(x=deg_x, y=deg_y, label=deg), size=3.1, color=degree_color) +
    geom_text(aes(x=min_x, y=min_y, label=minute), size=2.9, color=degree_color) +
    geom_text(aes(x=retrograde_x, y=retrograde_y, label="R"), size=2.4, color="darkred")
  
}
