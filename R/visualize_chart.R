#' Draw chart template using ggplot
#' 
#' @importFrom tibble tibble
#' @import ggplot2
#' 

draw_chart_template <- function(){
  
  planet_position <- calculate_planet_position(Sys.time(), city = cities$city[1])
  planet_position$planetary_position
  planet_position$house_cusps

  ## 1. define the function for drawing circles
  get_circle_coords <- function(r = 1, ...) {
    tibble::tibble(theta = seq(0, 2 * pi, ...),
           x     = cos(theta) * r,
           y     = sin(theta) * r)
  }
  
  ## 2. define x,y for for circles

  outer_circle <- get_circle_coords(length.out=1080)
  outer_circle2 <- get_circle_coords(r=0.9, length.out=1080)
  inner_circle <- get_circle_coords(r=0.5, length.out=1080)
  inner_circle2 <- get_circle_coords(r=0.4, length.out=1080)

  mytheme<- list(theme_void()+theme(panel.background = element_rect(colour= "white", fill= "white")))

  ## 3. define x, y for the asepct tables
  left_end <- -0.2
  right_end <- left_end + 1.3
  bottom <- -2.5
  top<- bottom+1.3

  x_vert <- seq(from=left_end, by=0.1, length.out=13)
  x_hor <- rep(left_end, 14)
  y_vert <- rep(bottom, 13)
  y_hor <- seq(from=bottom, by=0.1, length.out=14)

  xend_vert <- x_vert
  xend_hor <- c(rep(right_end-0.1, 2), seq(from=right_end-0.1, by=-0.1, length.out=12))
  yend_vert <- c(top, seq(top, by=-0.1, length.out=12))
  yend_hor <- seq(from=bottom, by=0.1, length.out=14)
  
  ## 4. define x, y for astrological elements (aspect table)
  
  
  
  ## 5. define x, y for division of zodiac signs
  
    ggplot()+
    geom_path(data=outer_circle, aes(x=x,y=y), linewidth=0.3)+
    geom_path(data=outer_circle2, aes(x=x, y=y), linewidth=0.3)+
    geom_path(data=inner_circle, aes(x=x, y=y), linewidth=0.3)+
    geom_path(data=inner_circle2, aes(x=x, y=y), linewidth=0.3)+
    xlim(-1.05, 1.2)+
    ylim(-2.55, 1.05)+
    mytheme+
    coord_equal()+
    geom_segment(aes(x=x_vert,y=y_vert,xend=xend_vert,yend=yend_vert),linewidth=0.3)+
    geom_segment(aes(x=x_hor,y=y_hor,xend=xend_hor,yend=yend_hor),linewidth=0.3)
  
}