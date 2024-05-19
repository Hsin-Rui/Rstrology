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
      geom_path(data=outer_circle, aes(x=x,y=y), linewidth=0.3)+
      geom_path(data=outer_circle2, aes(x=x, y=y), linewidth=0.3)+
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
    geom_path(data=outer_circle, aes(x=x,y=y), linewidth=0.3)+
    geom_path(data=outer_circle2, aes(x=x, y=y), linewidth=0.3)+
    geom_path(data=inner_circle, aes(x=x, y=y), linewidth=0.3)+
    geom_path(data=inner_circle2, aes(x=x, y=y), linewidth=0.3)+
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
#' @parem ... all other argumebts
#' 
#' @importFrom tibble tibble
#' 

get_circle_coords <- function(r = 1, ...) {
  tibble::tibble(theta = seq(0, 2 * pi, ...),
                 x     = cos(theta) * r,
                 y     = sin(theta) * r)
}