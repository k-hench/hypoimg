#' Display a single flags
#'
#' \code{hypo_flag_test} displays a single flag.
#'
#' Hypogen comes with a set of illustrations of flags of the countries adjacent
#' to the Caribbean and the Golf of Mexico.
#' The function \code{hypo_flag_test} displays a single flag as well
#' as its name (to be used in other hypo_flag* functions, note that line breaks
#' in the country names need to be replaced with an undercore if used as ID
#' in other functions)
#'
#' @param geo string skalar (madatory), flag to be displayed
#'
#' @examples
#' hypo_flag_test(geo = 'panama')
hypo_flag_test <- function(geo){
  nr_geo <- which(hypo_flag$geo == geo)
  ggplot()+
    annotation_custom(hypo_flag$flag[[nr_geo]],
                      xmin = -1,xmax = 1,ymin = -1, 1)+
    geom_text(data = tibble(x=1.5,y=0,
                            label=str_replace_all(geo,'_','\n')),
              aes(x=x,y=y,label=label),size=3)+
    coord_fixed()+theme_void()+
    scale_x_continuous(limits = c(-1.2,2))+
    scale_y_continuous(limits = c(-1.2,1.2))
}

#' Display available flags
#'
#' \code{hypo_flag_palette} displays all available flags.
#'
#' Hypogen comes with a set of illustrations of flags of the countries adjacent
#' to the Caribbean and the Golf of Mexico.
#' The function \code{hypo_flag_palette} displays all available flags as well
#' as their name (to be used in other hypo_flag* functions, note that line breaks
#' in the country names need to be replaced with an undercore if used as ID
#' in other functions)
#'
#' @param species interger vector (optional, elements >= 1 & <= 41), subselect
#'   flags to be displayed
#'
#' @examples
#' hypo_flag_palette()
#'
#' hypo_flag_palette(1:4)
#'
#' @export
hypo_flag_palette <- function(x = 1:41){
  cowplot::plot_grid(plotlist = map(hypo_flag$geo[x], hypo_flag_test))
}

#' Add a flag to a ggplot
#'
#' \code{hypo_anno_flag} adds a left facing hamlet annotation to a ggplot.
#'
#' Hypogen comes with a set of illustrations of flags of the countries adjacent
#' to the Caribbean and the Golf of Mexico.
#' The function \code{hypo_anno_flag} uses the \code{ggplot2::annotation_custom()}
#' function to add a single flag to an existring ggplot.
#'
#' @param geo string skalar (manatory), one of the available flag IDs
#' @param xmin numeric skalar (optional), left boundary of the annotation
#' @param xmax numeric skalar (optional), right boundary of the annotation
#' @param ymin numeric skalar (optional), lower boundary of the annotation
#' @param ymax numeric skalar (optional), upper boundary of the annotation
#'
#' @seealso \code{\link{hypo_flag_palette}},
#'   \code{\link{hypo_anno_l}},
#'   \code{\link{hypo_anno_r}}
#'
#' @examples
#' ggplot(tibble(x = 1, y = 1), aes(x = x, y = y))+
#'   geom_point()+
#'   hypo_anno_flag('mexico', xmax = 1.2, ymax = 1.2)
#'
#' @export
hypo_anno_flag <- function(geo, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,...){
  stopifnot(length(geo) == 1)
  stopifnot(is.character(geo))
  stopifnot(geo %in% hypo_flag$geo)

  nr_geo <- which(hypo_flag$geo == geo)

  annotation_custom(hypo_flag$flag[[nr_geo]],
                    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

#' One single flag legend element
#'
#' \code{hypo_anno_flag_single} combines a single flag with a colored circle
#'
#' The function \code{hypo_anno_flag_single} provides the basic building
#' block of the single flag color legend. It combines a single flag
#' annotaion with a background circle which can be coloured.
#'
#' Aditionally, the flag label can be added to the  plot.
#'
#' @param geo string skalar (manatory), one of the available flag IDs
#' @param flag_lwd numeric skalar (optional), line width of the optional
#'   frame of the flag annotation
#' @param flag_line_color color skalar (a string, optional), line width of the optional
#'   frame of the flag annotation
#' @param flag_radius_scale numeric skalar (optional), scaling of the optional
#'   flag annotation frames diameter
#' @param circle_color color skalar (a string, optional), the color of the
#'   background circle outline
#' @param circle_fill color skalar (a string, optional), the fill of the
#'   background circle
#' @param circle_lwd numeric skalar (optional), the width of the background
#'   circle outline
#' @param plot_names logical skalar (optional), should the species label
#'   be added?
#' @param plot_name_size numeric skalar (optional), the species label size
#' @param font_family string skalar (optional), the species label font family
#'
#' @seealso \code{\link{hypo_anno_flag_pair}},
#'   \code{\link{hypo_anno_single}}
#'
#' @examples
#' hypo_anno_flag_single(geo = 'trinidad_and_tobago',
#'   flag_lwd = 1, flag_line_color = 'black', flag_radius_scale = 1,
#'   circle_color = 'black', circle_lwd = 2, plot_names = TRUE)
#'
#' @export
hypo_anno_flag_single <- function(geo, flag_lwd = 1, flag_line_color = NA, flag_radius_scale = 1,
                                  circle_color = NA, circle_fill = "white", circle_lwd = .5,
                             plot_names = FALSE, plot_name_size = 3,font_family = 'sans',...){
  stopifnot(length(geo) == 1)
  stopifnot(length(plot_names) == 1)
  stopifnot(is.logical(plot_names))
  stopifnot(is.character(geo))
  stopifnot(geo %in% hypo_flag$geo)

  nr_geo <- which(hypo_flag$geo == geo)

  p <- ggplot()+
    ggforce::geom_circle(data = tibble(x = 0, y = 0, r = .28),
                         aes(x0 = x, y0 = y,r = r),
                         fill = circle_fill, color = circle_color, lwd = circle_lwd)+
    coord_fixed(xlim = c(-1, 1))+
    theme_void()+
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(limits = c(-.35, .4))+
    annotation_custom(hypo_flag$flag[[nr_geo]],
                      xmin = .1,xmax = .5,ymin = -.2, ymax = .2)+
    ggforce::geom_circle(data = tibble(x = .3, y = 0, r = .18 * flag_radius_scale),
                       aes(x0 = x, y0 = y,r = r),
                       fill = NA, color = flag_line_color, lwd = flag_lwd)

  if(plot_names){
    names_df <- tibble(name = str_replace_all(geo,'_',' '),
                       x = .3,
                       y = -.35)

    p_names <- p +
      geom_text(data = names_df, aes(x = x, y = y, label = stringr::str_to_title(name)),
                size = plot_name_size , family = font_family)
    return(p_names)
  } else {
    return(p)
  }
}

#' One paired flag legend element
#'
#' \code{hypo_anno_flag_pair} combines a paired flag with a colored circle
#'
#' The function \code{hypo_anno_flag_pair} provides the basic building
#' block of the paired flag color legend. It combines a paired flag
#' annotaion with a background circle which can be coloured.
#'
#' Aditionally, the flag labels can be added to the  plot.
#'
#' @param left string skalar (manatory), one of the available flags
#' @param right string skalar (manatory), one of the available flags
#' @param flag_lwd numeric skalar (optional), line width of the optional
#'   frame of the flag annotation
#' @param flag_line_color color skalar (a string, optional), line width of the optional
#'   frame of the flag annotation
#' @param flag_radius_scale numeric skalar (optional), scaling of the optional
#'   flag annotation frames diameter
#' @param circle_color color skalar (a string, optional), the color of the
#'   background circle outline
#' @param circle_fill color skalar (a string, optional), the fill of the
#'   background circle
#' @param circle_lwd numeric skalar (optional), the width of the background
#'   circle outline
#' @param plot_names logical skalar (optional), should the species labels
#'   be added?
#' @param plot_name_size numeric skalar (optional), the species label size
#' @param font_family string skalar (optional), the species label font family
#'
#' @seealso \code{\link{hypo_anno_flag_single}},
#'   \code{\link{hypo_anno_pair}}
#'
#' @examples
#' hypo_anno_flag_pair(left= 'belize', right = 'panama',
#'   flag_lwd = 1, flag_line_color = 'black',
#'   circle_color = 'black', plot_names = TRUE,
#'   plot_name_size = 5)
#'
#' @export
hypo_anno_flag_pair <- function(left, right,flag_lwd = 1, flag_line_color = NA, flag_radius_scale = 1,
                                circle_color = NA, circle_fill = "white", circle_lwd = .5,
                           plot_names = FALSE, plot_name_size = 3,font_family = 'sans',...){
  stopifnot(length(left) == 1)
  stopifnot(length(right) == 1)
  stopifnot(length(plot_names) == 1)
  stopifnot(is.logical(plot_names))
  stopifnot(is.character(left) & is.character(right))
  stopifnot(left %in% hypo_flag$geo)
  stopifnot(right %in% hypo_flag$geo)

  nr_left <- which(hypo_flag$geo == left)
  nr_right <- which(hypo_flag$geo == right)

  p <- ggplot()+
    geom_circle(data= tibble(x = 0, y = 0, r = .28),
                aes(x0 = x, y0 = y,r = r),
                fill = circle_fill, color = circle_color, lwd = circle_lwd)+
    coord_fixed(xlim = c(-1, 1))+
    theme_void()+
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(limits = c(-.4, .365))+
    annotation_custom(hypo_flag$flag[[nr_left]],
                      xmin = .05,xmax = .55,ymin = -.2, ymax = .2)+
    annotation_custom(hypo_flag$flag[[nr_right]],
                      xmin = -.55,xmax = -.05,ymin = -.2, ymax = .2)+
    ggforce::geom_circle(data = tibble(x = c(-.3,.3), y = rep(0,2),
                                       r = rep(.18 * flag_radius_scale)),
                         aes(x0 = x, y0 = y,r = r),
                         fill = NA, color = flag_line_color, lwd = flag_lwd)

  if(plot_names){
    names_df <- tibble(name = c(str_replace_all(left,'_',' '),str_replace_all(right,'_',' ')),
                       position = c('left', 'right'),
                       x = c(.325, -.325),
                       y = c(-.35, -.35))

    p_names <- p +
      geom_text(data = names_df, aes(x = x, y = y, label = stringr::str_to_title(name)),
                size = plot_name_size , family = font_family)
    return(p_names)
  } else {
    return(p)
  }
}


#' Constructs a legend of single flags
#'
#' \code{hypo_legend_flag_single} combines several single flag legend elements
#'
#' The function \code{hypo_legend_flag_single} constructs a single flag legend
#' from a vector of flag IDs and a choosen color map (matching in length).
#'
#' Flag labels can optionally be included.
#'
#' @param geo string skalar (manatory), one of the available flag IDs
#' @param flag_lwd numeric skalar (optional), line width of the optional
#'   frame of the flag annotation
#' @param flag_line_color color skalar (a string, optional), line width of the
#'   optional frame of the flag annotation
#' @param flag_radius_scale numeric skalar (optional), scaling of the optional
#'   flag annotation frames diameter
#' @param color_map color vector (a string, optional), the color map (must
#'   match the species vector in length)
#' @param circle_color color skalar (a string, optional), the color of the
#'   background circle outline
#' @param circle_lwd numeric skalar (optional), the width of the background
#'   circle outlines
#' @param plot_names logical skalar (optional), should the species label
#'   be added?
#' @param plot_name_size numeric skalar (optional), the species label size
#' @param font_family string skalar (optional), the species label font family
#' @param plot logical scalar (optional), toggle the output to be either a plot
#'   or a list of plots
#'
#' @seealso \code{\link{hypo_legend_pair}},
#'   \code{\link{hypo_legend_single}}
#'
#' @examples
#' clr <- RColorBrewer::brewer.pal(3,'Oranges')
#'
#' left_flag <- c('anguilla','barbados','trinidad_and_tobago')
#'
#' hypo_legend_flag_single(geo = left_flag,
#'                         flag_lwd = 1, flag_line_color = 'black',
#'                         flag_radius_scale = 1,
#'                         color_map = clr,
#'                         circle_color = 'black', plot_names = TRUE)
#' @export
hypo_legend_flag_single <- function(geo,color_map,
                               flag_lwd = 1, flag_line_color = NA, flag_radius_scale = 1,
                               circle_color = NA, circle_lwd = .5,
                               plot_names = FALSE, plot_name_size = 3,font_family = 'sans',
                               plot = TRUE){
  n <- length(geo)
  stopifnot(n > 0)
  stopifnot(length(color_map) == n)
  stopifnot(is.character(geo))

  legend_df <- tibble(geo = geo,
                      circle_fill = color_map,
                      flag_lwd = rep(flag_lwd, n),
                      flag_line_color = rep(flag_line_color, n),
                      flag_radius_scale = rep(flag_radius_scale, n),
                      circle_color = rep(circle_color, n),
                      circle_lwd = rep(circle_lwd, n),
                      plot_names = rep(plot_names, n),
                      plot_name_size = rep(plot_name_size, n),
                      font_family = rep(font_family, n))

  legend_list <- legend_df %>%
    purrr::pmap(hypo_anno_flag_single)

  if (plot == TRUE) {
    out <- cowplot::plot_grid(plotlist = legend_list,
                              ncol = 1,align='v')
    return(out)
  } else {
    return(legend_list)
  }
}

#' Constructs a legend of paired flags
#'
#' \code{hypo_legend_flag_pair} combines several paired flag legend elements
#'
#' The function \code{hypo_legend_flag_pair} constructs a paired flag legend
#' from two vectors of flags and a choosen color map (matching in length).
#'
#' @param left string vector (manatory), one of the available flag IDs
#' @param right string vector (manatory), one of the available flag IDs
#' @param flag_lwd numeric skalar (optional), line width of the optional
#'   frame of the flag annotation
#' @param flag_line_color color skalar (a string, optional), line width of the
#'   optional frame of the flag annotation
#' @param flag_radius_scale numeric skalar (optional), scaling of the optional
#'   flag annotation frames diameter
#' @param color_map color vector (a string, optional), the color map (must
#'   match the species vector in length)
#' @param circle_color color skalar (a string, optional), the color of the
#'   background circle outline
#' @param circle_lwd numeric skalar (optional), the width of the background
#'   circle outlines
#' @param plot_names logical skalar (optional), should the species label
#'   be added?
#' @param plot_name_size numeric skalar (optional), the species label size
#' @param font_family string skalar (optional), the species label font family
#' @param plot logical scalar (optional), toggle the output to be either a plot
#'   or a list of plots
#'
#' @seealso \code{\link{hypo_legend_single}},
#'   \code{\link{hypo_legend_pair}}
#'
#' @examples
#' clr <- RColorBrewer::brewer.pal(3,'Blues')
#'
#' left_flag <- c('anguilla','barbados','trinidad_and_tobago')
#' right_flag <- c('usa','virgin_islands','cuba')
#'
#' hypo_legend_flag_pair(left = left_flag, right = right_flag,
#'                       color_map = clr,
#'                       flag_lwd = 1, flag_line_color = 'black',
#'                       flag_radius_scale = 1,
#'                       circle_color = 'black', plot_names = TRUE)
#' @export
hypo_legend_flag_pair <- function(left,right,color_map,
                                  flag_lwd = 1, flag_line_color = NA, flag_radius_scale = 1,
                                  circle_color = NA, circle_lwd = .5,
                                  plot_names = FALSE, plot_name_size = 3,
                                  font_family = 'sans', plot = TRUE){
  n <- length(left)
  stopifnot(n > 0)
  stopifnot(length(right) == n)
  stopifnot(length(color_map) == n)
  stopifnot(is.character(left) & is.character(right))

  legend_df <- tibble(left = left,
                      right = right,
                      circle_fill = color_map,
                      flag_lwd = rep(flag_lwd, n),
                      flag_line_color = rep(flag_line_color, n),
                      flag_radius_scale = rep(flag_radius_scale, n),
                      circle_color = rep(circle_color, n),
                      circle_lwd = rep(circle_lwd, n),
                      plot_names = rep(plot_names, n),
                      plot_name_size = rep(plot_name_size, n),
                      font_family = rep(font_family, n))

  legend_list <- legend_df %>%
    purrr::pmap(hypo_anno_flag_pair)

  if (plot == TRUE) {
    out <- cowplot::plot_grid(plotlist = legend_list,
                              ncol = 1,align='v')
    return(out)
  } else {
    return(legend_list)
  }
}
