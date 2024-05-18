#' Draw chart template using ggplot
#' 
#' @param style chart style (whole sign, chris brennan, others)
#' 
#' @importFrom tibble tibble
#' @importFrom showtext showtext_auto
#' @import ggplot2
#' @return ggplot object (three possible empty chart templates for further plotting)
#' 

draw_chart_template <- function(style="whole sign"){
  
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
  
  planet_table_x <- rep(-1, 14)
  planet_table_y <- seq(top, by=-0.1, length.out=14)
  planets <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "U", "L", "P", "Q")
  
  ## 5. call showtext_auto so that astrological symbol will be rendered
  
  load_fonts()
  showtext::showtext_auto()
  
  ## 6. define sign division & whole sign house cusps
  
  equal_division <- seq(from=1, by=1080/12, length.out=12)
  
  sign_x <- outer_circle$x [equal_division]
  sign_x_end <- outer_circle2$x [equal_division]
  sign_y <- outer_circle$y [equal_division]
  sign_y_end <- outer_circle2$y [equal_division]
  
  cusps_x <- inner_circle2$x [equal_division]
  cusps_y <- inner_circle2$y [equal_division]
  
  ## 7. define x, y of house number for whole sign chart
  
  house_position <- get_circle_coords(r=0.45, length.out=360)
  house_x <- house_position$x[seq(from=15, by=30, length.out=12)]
  house_y <- house_position$y[seq(from=15, by=30, length.out=12)]
  house_number <- as.character(1:12)
  
  ## 8. draw chris brennan style chart template
  
  p_chris_prennan <- 
    ggplot()+
      geom_path(data=outer_circle, aes(x=x,y=y), linewidth=0.3)+
      geom_path(data=outer_circle2, aes(x=x, y=y), linewidth=0.3)+
      mytheme+
      coord_equal()+
      geom_segment(aes(x=sign_x, y=sign_y, xend=0, yend=0), color="black", linewidth=0.3)
  
  if(style=="chris brennan") return(p_chris_prennan)

  ## 9. draw common parts of the template
  
  p_common <-  
    ggplot()+
    # draw four circles
    geom_path(data=outer_circle, aes(x=x,y=y), linewidth=0.3)+
    geom_path(data=outer_circle2, aes(x=x, y=y), linewidth=0.3)+
    geom_path(data=inner_circle, aes(x=x, y=y), linewidth=0.3)+
    geom_path(data=inner_circle2, aes(x=x, y=y), linewidth=0.3)+
    # define the size of graph. the outer circle has x & y of -1 to 1.
    xlim(-1.05, 1.2)+
    ylim(-2.55, 1.05)+
    # add custom theme (white background etc.)
    mytheme+
    # make coordinates x & y equally long
    coord_equal()+
    # aspect table lines
    geom_segment(aes(x=x_vert,y=y_vert,xend=xend_vert,yend=yend_vert),linewidth=0.3)+ 
    geom_segment(aes(x=x_hor,y=y_hor,xend=xend_hor,yend=yend_hor),linewidth=0.3) +
    # All astrological elements on planet table with AstroDotBasic fonts
    annotate("text", x= planet_table_x, y=planet_table_y, label=planets, color="black", size=4.5, family ="AstroDotBasic") +
    # sign border
    geom_segment(aes(x=sign_x, y=sign_y, xend=sign_x_end, yend=sign_y_end), color="black", linewidth=0.3)
  
  if(style=="others") return(p_common)
  
  ## 10. draw template for whole sign house chart
  
  p_common_whole_sign <-
    p_common +
    # house division
    geom_segment(aes(x=cusps_x, y=cusps_y, xend=sign_x_end, yend=sign_y_end), color="grey50", linewidth=0.2) +
    # house number
    geom_text(aes(x=house_x, y=house_y, label=c(7:12, 1:6)), size=3.5)
  
  if(style=="whole sign") return(p_common_whole_sign)
  
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
