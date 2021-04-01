#' two colors
#'
#' \code{clr2} is a combination of two colors I like.
#'
#' A vector containg blue and orange.
#'
#' @examples
#' ggplot(tibble( x = 1:2))+
#'   coord_equal()+
#'   geom_tile(aes(x = x, y = 1, fill = factor(x)))+
#'   scale_fill_manual(values = clr2)+
#'   theme_void()+
#'   theme(legend.position = 'none')
#'
#' @export
clr2 <-  c('#084082ff','#f0a830ff')

#' five colors
#'
#' \code{clr5} is a combination of five colors I like.
#'
#' This is just a shortcut for RColorBrewer::brewer.pal(5,'Set1').
#'
#' @examples
#' ggplot(tibble( x = 1:5))+
#'   coord_equal()+
#'   geom_tile(aes(x = x, y = 1, fill = factor(x)))+
#'   scale_fill_manual(values = clr5)+
#'   theme_void()+
#'   theme(legend.position = 'none')
#'
#' @export
clr5 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")


#' Programmatically darken colors
#'
#' superseded: Please use `prismatic::clr_darken()` instead
#'
#' \code{clr_darken} produces a darker version on given colors.
#'
#' Takes a vector of colors and darkens them by a given factor.
#' 1 returns the input color, 0 returns black.
#'
#' @param color string (color, mandatory)
#' @param factor numeric scalar (optional), between 0 and 1, amount of darkening
#'
#' @seealso \code{\link{clr_lighten}}
#'
#' @examples
#' ggplot(tibble( x = 1:5))+
#'   coord_equal()+
#'   geom_tile(aes(x = x, y = 1, fill = factor(x)))+
#'   scale_fill_manual(values = map_chr((1:5)/5,.f = clr_darken, color = clr2[[2]]))+
#'   theme_void()+
#'   theme(legend.position = 'none')
#'
#' @export
clr_darken <- function(color, factor = .5){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(col), maxColorValue = 255)
  col
}

#' Programmatically lighten colors
#'
#' \code{clr_lighten} produces a lighter version on given colors.
#'
#' superseded: Please use `prismatic::clr_lighten()` instead
#'
#' Takes a vector of colors and lightens them by a given factor.
#' 1 returns the input color, 0 returns white
#'
#' @param color string (color, mandatory)
#' @param factor numeric scalar (optional), between 0 and 1, amount of lightening
#'
#' @seealso \code{\link{clr_darken}}
#'
#' @examples
#' ggplot(tibble( x = 1:5))+
#'   coord_equal()+
#'   geom_tile(aes(x = x, y = 1, fill = factor(x)))+
#'   scale_fill_manual(values = map_chr((1:5)/5,.f = clr_lighten, color = clr2[[1]]))+
#'   theme_void()+
#'   theme(legend.position = 'none')
#'
#' @export
clr_lighten <- function(color, factor = .2){
  anti_col <- (255 - col2rgb(color))*factor
  col <- col2rgb(color) + anti_col
  col <- rgb(t(col), maxColorValue = 255)
  col
}

#' Add a generic hamlet to a ggplot
#'
#' \code{hypo_hamlet_generic} adds a generic hamlet with custom color.
#'
#' This function allows the placement of a generic hamlet within a ggplot.
#' It is based on \code{ggplot2::annotation_custom()} and allows to specify
#' the coloration of the generic hamlet.
#'
#' @param col string  (color, optional), defaul = darkgray
#' @param width_in numeric scalar (optional), width (in graph x axis units)
#' @param height_in numeric scalar (optional), width (in graph y axis units)
#' @param face string scalar (optional), one of l,r side the hamlet is facing
#' @param x_in horizontal position
#' @param y_in vertical position
#' @param ... catch all parameter to allow excess parameter through purrr::pmap
#'
#' @seealso \code{\link{hypo_coral_generic}}
#'
#' @examples
#' ggplot()+
#'   hypo_hamlet_generic(col = clr2[[1]])
#'
#' @export
hypo_hamlet_generic <- function(col = 'darkgray',x_in = .5, y_in = .5,
                                width_in = 1, height_in = 1,
                                face = c('l','r'), ...){
  annotation_custom(grob = purrr::reduce2(.x = c(1:3,6:10,5),
                                   .y = c(col, rep(clr_darken(col), 3),
                                          rep( clr_lighten(col),5)),
                                   .f = hypo_recolor_svg,
                                   .init = hypo_generic_img$grob[hypo_generic_img$side == face][[1]]),
                    xmin = x_in-.5*width_in,
                    xmax = x_in+.5*width_in,
                    ymin = y_in-.5*height_in,
                    ymax = y_in+.5*height_in,
                    ...)
}

#' Add a generic coral to a ggplot
#'
#' \code{hypo_coral_generic} adds a generic coral with custom color.
#'
#' This function allows the placement of a coral sketch within a ggplot.
#' It is based on \code{ggplot2::annotation_custom()} and allows to specify
#' the coloration of the coral.
#'
#' @param col string  (color, optional), defaul = darkgray
#' @param width_in numeric scalar (optional), width (in graph x axis units)
#' @param height_in numeric scalar (optional), width (in graph y axis units)
#' @param coral_side string scalar (optional), one of (l, r) - orientation of the coral
#' @param coral_type string scalar (optional), one of (branch, table, brain) - coral type
#' @param x_in horizontal position
#' @param y_in vertical position
#' @param ... catch all parameter to allow excess parameter through purrr::pmap
#'
#' @seealso \code{\link{hypo_hamlet_generic}}
#'
#' @examples
#' ggplot()+
#'   hypo_coral_generic(col = clr2[[2]])+
#'   hypo_hamlet_generic(col = clr2[[1]], x = .6, y = .6,, width_in = .3)   +
#'   hypo_hamlet_generic(col = clr2[[1]], x = .4, y = .3,  face = 'r', width_in = .3)
#'
#' @export
hypo_coral_generic <- function(col = 'darkgray',x_in = .5, y_in = .5,
                                width_in = 1, height_in = 1,
                               coral_side = c('l','r'),
                               coral_type = c('branch','table','brain'),
                               ...){
  annotation_custom(grob = hypo_coral_img %>%
                      filter(side == coral_side , type == coral_type) %>%
                      .$grob %>%
                      .[[1]] %>%
                      hypo_recolor_svg(color = col),
                    xmin = x_in-.5*width_in,
                    xmax = x_in+.5*width_in,
                    ymin = y_in-.5*height_in,
                    ymax = y_in+.5*height_in,
                    ...)
}
