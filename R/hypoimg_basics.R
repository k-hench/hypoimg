.onAttach <- function(libname, pkgname) {
  packageStartupMessage("--- Welcome to hypoimg ---")
}

.onLoad <- function(libname, pkgname) {
  theme_set(theme_grey())
}
#' Add a left facing hamlet to a ggplot
#'
#' \code{hypo_anno_l} adds a left facing hamlet annotation to a ggplot.
#'
#' Hypogen comes with a set of illustrations of differnt halmet species.
#' The function \code{hypo_anno_l} uses the \code{ggplot2::annotation_custom()}
#' function to add a single left facing hamlet to an existring ggplot.
#'
#' @param species string skalar (manatory), one of "aberrans","atlahua",
#'   "castroaguirrei","chlorurus","ecosur","floridae","gemma","gumigutta",
#'   "guttavarius","indigo","liberte","maculiferus","maya","nigricans",
#'   "providencianus","puella","randallorum","tan","unicolor"
#' @param xmin numeric skalar (optional), left boundary of the annotation
#' @param xmax numeric skalar (optional), right boundary of the annotation
#' @param ymin numeric skalar (optional), lower boundary of the annotation
#' @param ymax numeric skalar (optional), upper boundary of the annotation
#'
#' @seealso \code{\link{hypo_anno_r}},
#'   \code{\link{hypo_anno_flag}}
#'
#' @examples
#' ggplot(tibble(x = 1, y = 1),
#' aes(x = x, y = y))+
#'   geom_point()+
#'   hypo_anno_l('unicolor', xmax = 1.2, ymax = 1.2)+
#'   hypo_anno_r('aberrans', xmin = .8, ymin = .8)
#'
#' @export
hypo_anno_l <- function(species, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf){
    stopifnot(length(species) == 1)
    stopifnot(is.character(species))
    stopifnot(species %in% hypo_img$spec)

    nr_species <- which(hypo_img$spec == species)

    annotation_custom(hypo_img$l[[nr_species]], xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  }

#' Add a right facing hamlet to a ggplot
#'
#' \code{hypo_anno_r} adds a right facing hamlet annotation to a ggplot.
#'
#' Hypogen comes with a set of illustrations of differnt halmet species.
#' The function \code{hypo_anno_r} uses the \code{ggplot2::annotation_custom()}
#' function to add a single right facing hamlet to a existring ggplot.
#'
#' @param species string skalar (manatory), one of "aberrans","atlahua",
#'   "castroaguirrei","chlorurus","ecosur","floridae","gemma","gumigutta",
#'   "guttavarius","indigo","liberte","maculiferus","maya","nigricans",
#'   "providencianus","puella","randallorum","tan","unicolor"
#' @param xmin numeric skalar (optional), left boundary of the annotation
#' @param xmax numeric skalar (optional), right boundary of the annotation
#' @param ymin numeric skalar (optional), lower boundary of the annotation
#' @param ymax numeric skalar (optional), upper boundary of the annotation
#'
#' @seealso \code{\link{hypo_anno_l}},
#'   \code{\link{hypo_anno_flag}}
#'
#' @examples
#' ggplot(tibble(x = 1, y = 1),
#' aes(x = x, y = y))+
#'   geom_point()+
#'   hypo_anno_l('unicolor', xmax = 1.2, ymax = 1.2)+
#'   hypo_anno_r('aberrans', xmin = .8, ymin = .8)
#'
#' @export
hypo_anno_r <- function(species, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,...){
  stopifnot(length(species) == 1)
  stopifnot(is.character(species))
  stopifnot(species %in% hypo_img$spec)

  nr_species <- which(hypo_img$spec == species)

  annotation_custom(hypo_img$r[[nr_species]], xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

#' One single hamlet legend element
#'
#' \code{hypo_anno_single} combines a single hamlet with a colored circle
#'
#' The function \code{hypo_anno_single} provides the basic building block
#' of the single hamlet color legend. It combines a single hamlet annotaion
#' with a background circle which can be coloured.
#'
#' Aditionally, the species label can be added to the  plot.
#'
#' Available species:
#'   "aberrans", "atlahua", "castroaguirrei", "chlorurus", "ecosur",
#'   "floridae", "gemma", "gumigutta", "guttavarius", "indigo",
#'   "liberte", "maculiferus", "maya", "nigricans", "providencianus",
#'   "puella", "randallorum", "tan", "unicolor"
#'
#' @param species string skalar (manatory), one of the available species
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
#' @seealso \code{\link{hypo_anno_pair}},
#'   \code{\link{hypo_anno_flag_single}}
#'
#' @examples
#' hypo_anno_single('indigo',circle_color = 'black',
#'   plot_names = TRUE)
#'
#' @export
hypo_anno_single <- function(species, circle_color = NA, circle_fill = "white", circle_lwd = .5,
                             plot_names = FALSE, plot_name_size = 3,font_family = 'sans',...){
  stopifnot(length(species) == 1)
  stopifnot(length(plot_names) == 1)
  stopifnot(is.logical(plot_names))
  stopifnot(is.character(species))
  stopifnot(species %in% hypo_img$spec)

  nr_species <- which(hypo_img$spec == species)

  p <- ggplot()+
    ggforce::geom_circle(data= tibble(x = 0, y = 0, r = .28),
                aes(x0 = x, y0 = y,r = r),
                fill = circle_fill, color = circle_color, lwd = circle_lwd)+
    coord_fixed(xlim = c(-1, 1))+
    theme_void()+
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(limits = c(-.37, .37))+
    annotation_custom(hypo_img$l[[nr_species]],
                      xmin = -.45,xmax = .45,ymin = -Inf, ymax = Inf)

  if(plot_names){
    names_df <- tibble(name = str_c('italic(',hypo_img$geno[nr_species],'.~',species,')'),
                       x = 0,
                       y = -.35)

    p_names <- p +
      geom_text(data = names_df, aes(x = x, y = y, label = name),
                size = plot_name_size , parse = TRUE, family = font_family)
    return(p_names)
  } else {
    return(p)
  }
}

#' One paired hamlet legend element
#'
#' \code{hypo_anno_pair} combines a paired hamlet with a colored circle
#'
#' The function \code{hypo_anno_pair} provides the basic building block
#' of the paired hamlet color legend. It combines a paired hamlet annotaion
#' with a background circle which can be coloured.
#'
#' Aditionally, the species labels can be added to the  plot.
#'
#' Available species:
#'   "aberrans", "atlahua", "castroaguirrei", "chlorurus", "ecosur",
#'   "floridae", "gemma", "gumigutta", "guttavarius", "indigo",
#'   "liberte", "maculiferus", "maya", "nigricans", "providencianus",
#'   "puella", "randallorum", "tan", "unicolor"
#'
#' @param left string skalar (manatory), one of the available species
#' @param right string skalar (manatory), one of the available species
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
#' @seealso \code{\link{hypo_anno_single}},
#'   \code{\link{hypo_anno_flag_pair}}
#'
#' @examples
#' hypo_anno_pair('indigo','puella',
#'   circle_color = 'black',plot_names = TRUE)
#'
#' @export
hypo_anno_pair <- function(left, right, circle_color = NA, circle_fill = "white", circle_lwd = .5,
                           plot_names = FALSE, plot_name_size = 3,font_family = 'sans',...){
  stopifnot(length(left) == 1)
  stopifnot(length(right) == 1)
  stopifnot(length(plot_names) == 1)
  stopifnot(is.logical(plot_names))
  stopifnot(is.character(left) & is.character(right))
  stopifnot(left %in% hypo_img$spec)
  stopifnot(right %in% hypo_img$spec)

  nr_left <- which(hypo_img$spec == left)
  nr_right <- which(hypo_img$spec == right)

  p <- ggplot()+
    ggforce::geom_circle(data= tibble(x = 0, y = 0, r = .28),
                aes(x0 = x, y0 = y,r = r),
                fill = circle_fill, color = circle_color, lwd = circle_lwd)+
    coord_fixed(xlim = c(-1, 1))+
    theme_void()+
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(limits = c(-.4, .38))+
    annotation_custom(hypo_img$l[[nr_left]],
                      xmin = .05,xmax = 1,ymin = -Inf, ymax = Inf)+
    annotation_custom(hypo_img$r[[nr_right]],
                      xmin = -1,xmax = -.05,ymin = -Inf, ymax = Inf)

  if(plot_names){
    names_df <- tibble(name = str_c('italic(H.~',c(left, right),')'),
                       position = c('left', 'right'),
                       x = c(.475, -.475),
                       y = c(-.35, -.35))

    p_names <- p +
      geom_text(data = names_df, aes(x = x, y = y, label = name),
                size = plot_name_size , parse = TRUE, family = font_family)
    return(p_names)
  } else {
    return(p)
  }
}

#' One paired hamlet legend element with two fills
#'
#' \code{hypo_anno_pair_split} combines a paired hamlet with a two-colored circle
#'
#' The function \code{hypo_anno_pair_split} provides the basic building block
#' of the paired hamlet color legend. It combines a paired hamlet annotaion
#' with a background circle which can be coloured using two fills for
#' the two species.
#'
#' Aditionally, the species labels can be added to the  plot.
#'
#' Available species:
#'   "aberrans", "atlahua", "castroaguirrei", "chlorurus", "ecosur",
#'   "floridae", "gemma", "gumigutta", "guttavarius", "indigo",
#'   "liberte", "maculiferus", "maya", "nigricans", "providencianus",
#'   "puella", "randallorum", "tan", "unicolor"
#'
#' @param left string skalar (manatory), one of the available species
#' @param right string skalar (manatory), one of the available species
#' @param circle_color color skalar (a string, optional), the color of the
#'   background circle outline
#' @param circle_fill_left color skalar (a string, optional), the fill of the
#'   left background
#' @param circle_fill_right color skalar (a string, optional), the fill of the
#'   right background
#' @param circle_lwd numeric skalar (optional), the width of the background
#'   circle outline
#' @param plot_names logical skalar (optional), should the species labels
#'   be added?
#' @param plot_name_size numeric skalar (optional), the species label size
#' @param font_family string skalar (optional), the species label font family
#'
#' @seealso \code{\link{hypo_anno_single}},
#'   \code{\link{hypo_anno_flag_pair}}
#'
#' @examples
#' hypo_anno_pair_split(left = 'puella', right = 'indigo',
#'   circle_color = 'black', plot_names = TRUE)
#'
#' @export
hypo_anno_pair_split <- function(left, right, circle_color = NA, circle_fill_left = "white",circle_fill_right = "lightgray",
                                 circle_lwd = .5, plot_names = FALSE, plot_name_size = 3,font_family = 'sans',...){
  stopifnot(length(left) == 1)
  stopifnot(length(right) == 1)
  stopifnot(right != left)
  stopifnot(length(plot_names) == 1)
  stopifnot(is.logical(plot_names))
  stopifnot(is.character(left) & is.character(right))
  stopifnot(left %in% hypo_img$spec)
  stopifnot(right %in% hypo_img$spec)

  nr_left <- which(hypo_img$spec == left)
  nr_right <- which(hypo_img$spec == right)

  p <- ggplot()+
    ggforce::geom_arc_bar(data= tibble(spec = c(str_c(2,left),str_c(1,right))),
                          aes(fill = spec, x0 = 0, y0 = 0, r0 = 0, r = .28, amount  = 1 ),
                          stat = 'pie',col = NA)+
    ggforce::geom_circle(data = tibble(x = 0, y = 0, r = .28),
                         aes(x0 = x, y0 = y,r = r),
                         fill = NA, color = circle_color, lwd = circle_lwd)+
    coord_fixed(xlim = c(-1, 1))+
    theme_void()+
    scale_fill_manual(values = c(circle_fill_left,circle_fill_right),guide=FALSE)+
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(limits = c(-.4, .38))+
    # careful: RIGHT (nr_right) side of circe is facing LEFT (hypo_img$l)
    annotation_custom(hypo_img$l[[nr_right]],
                      xmin = .05,xmax = 1,ymin = -Inf, ymax = Inf)+
    annotation_custom(hypo_img$r[[nr_left]],
                      xmin = -1,xmax = -.05,ymin = -Inf, ymax = Inf)

  if(plot_names){
    names_df <- tibble(name = str_c('italic(H.~',c(left, right),')'),
                       position = c('left', 'right'),
                       x = c(.475, -.475),
                       y = c(-.35, -.35))

    p_names <- p +
      geom_text(data = names_df, aes(x = x, y = y, label = name),
                size = plot_name_size , parse = TRUE, family = font_family)
    return(p_names)
  } else {
    return(p)
  }
}

#' Constructs a legend of single hamlets
#'
#' \code{hypo_legend_single} combines several single hamlet legend elements
#'
#' The function \code{hypo_legend_single} constructs a single hamlet legend
#' from a vector of hamlet species and a choosen color map (matching in length).
#'
#' Hamlet species labels can optionally be included.
#'
#' Available species:
#'   "aberrans", "atlahua", "castroaguirrei", "chlorurus", "ecosur",
#'   "floridae", "gemma", "gumigutta", "guttavarius", "indigo",
#'   "liberte", "maculiferus", "maya", "nigricans", "providencianus",
#'   "puella", "randallorum", "tan", "unicolor"
#'
#' @param species string vector (manatory), can only contain available species
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
#'   \code{\link{hypo_legend_flag_single}}
#'
#' @examples
#' clr <- viridis::inferno(4)
#'
#' species <- c('unicolor', 'liberte', 'maya', 'castroaguirrei')
#'
#' hypo_legend_single(species = species, color_map = clr_single,
#'   circle_color = 'black', plot_names = TRUE)
#'
#' @export
hypo_legend_single <- function(species,color_map,
                               circle_color = NA, circle_lwd = .5,
                               plot_names = FALSE, plot_name_size = 3,font_family = 'sans',
                               ncol = 1, plot = TRUE){
  n <- length(species)
  stopifnot(n > 0)
  stopifnot(length(color_map) == n)
  stopifnot(is.character(species))

  legend_df <- tibble(species = species,
                      circle_fill = color_map,
                      circle_color = rep(circle_color, n),
                      circle_lwd = rep(circle_lwd, n),
                      plot_names = rep(plot_names, n),
                      plot_name_size = rep(plot_name_size, n),
                      font_family = rep(font_family, n))

  legend_list <- legend_df %>%
    purrr::pmap(hypo_anno_single)

  if (plot == TRUE) {
    out <- cowplot::plot_grid(plotlist = legend_list,
                                ncol = ncol,align='v')
    return(out)
  } else {
    return(legend_list)
  }

}

#' Constructs a legend of paired hamlets
#'
#' \code{hypo_legend_pair} combines several paired hamlet legend elements
#'
#' The function \code{hypo_legend_pair} constructs a paired hamlet legend
#' from two vectors of hamlet species and a choosen color map (matching in length).
#'
#' Hamlet species labels can optionally be included.
#'
#' Available species:
#'   "aberrans", "atlahua", "castroaguirrei", "chlorurus", "ecosur",
#'   "floridae", "gemma", "gumigutta", "guttavarius", "indigo",
#'   "liberte", "maculiferus", "maya", "nigricans", "providencianus",
#'   "puella", "randallorum", "tan", "unicolor"
#'
#' @param left string vector (manatory), can only contain available species
#' @param right string vector (manatory), can only contain available species
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
#'   \code{\link{hypo_legend_flag_pair}}
#'
#' @examples
#' clr <- viridis::viridis(4)
#'
#' left <- c('unicolor', 'liberte', 'maya', 'castroaguirrei')
#' right <- c('guttavarius', 'gumigutta', 'atlahua', 'randallorum')
#'
#' hypo_legend_pair(left= left, right = right, color_map = clr,
#'   circle_color = 'black', plot_names = TRUE)
#' @export
hypo_legend_pair <- function(left,right,color_map,
                             circle_color = NA, circle_lwd = .5,
                             plot_names = FALSE, plot_name_size = 3,
                             font_family = 'sans',ncol = 1, plot = TRUE){
  n <- length(left)
  stopifnot(n > 0)
  stopifnot(length(right) == n)
  stopifnot(length(color_map) == n)
  stopifnot(is.character(left) & is.character(right))

  legend_df <- tibble(left = left,
                      right = right,
                      circle_fill = color_map)

  legend_list <- legend_df %>%
    purrr::pmap(hypo_anno_pair,
                circle_color = circle_color,
                circle_lwd = circle_lwd,
                plot_names = plot_names,
                plot_name_size = plot_name_size,
                font_family = font_family)

  if (plot == TRUE) {
    out <- cowplot::plot_grid(plotlist = legend_list,
                              ncol = ncol,align='v')
    return(out)
  } else {
    return(legend_list)
  }
}


#' Constructs a legend of paired hamlets with two fills
#'
#' \code{hypo_legend_pair_split} combines several paired hamlet legend elements
#'
#' The function \code{hypo_legend_pair} constructs a paired hamlet legend
#' from two vectors of hamlet species and a choosen color map (matching in length).
#'
#' Hamlet species labels can optionally be included.
#'
#' Available species:
#'   "aberrans", "atlahua", "castroaguirrei", "chlorurus", "ecosur",
#'   "floridae", "gemma", "gumigutta", "guttavarius", "indigo",
#'   "liberte", "maculiferus", "maya", "nigricans", "providencianus",
#'   "puella", "randallorum", "tan", "unicolor"
#'
#' @param left string vector (manatory), can only contain available species
#' @param right string vector (manatory), can only contain available species
#' @param color_map_left color vector (a string, optional), the color map (must
#'   match the species vector in length)
#' @param color_map_right color vector (a string, optional), the color map (must
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
#'   \code{\link{hypo_legend_flag_pair}}
#'
#' @examples
#' clr <- viridis::viridis(4)
#'
#' left <- c('unicolor', 'liberte', 'maya', 'castroaguirrei')
#' right <- c('guttavarius', 'gumigutta', 'atlahua', 'randallorum')
#'
#' hypo_legend_pair(left= left, right = right, color_map = clr,
#'   circle_color = 'black', plot_names = TRUE)
#' @export
hypo_legend_pair_split <- function(left,right,
                                   color_map_left,
                                   color_map_right,
                                   circle_color = NA,
                                   circle_lwd = .5,
                                   plot_names = FALSE,
                                   plot_name_size = 3,
                                   font_family = 'sans',ncol = 1, plot = TRUE){
  n <- length(left)
  stopifnot(n > 0)
  stopifnot(length(right) == n)
  stopifnot(length(color_map_left) == n)
  stopifnot(length(color_map_right) == n)
  stopifnot(is.character(left) & is.character(right))

  legend_df <- tibble(left = left,
                      right = right,
                      circle_fill_left = color_map_left,
                      circle_fill_right = color_map_right)

  legend_list <- legend_df %>%
    purrr::pmap(hypo_anno_pair_split,
                circle_color = circle_color,
                circle_lwd = circle_lwd,
                plot_names = plot_names,
                plot_name_size = plot_name_size,
                font_family = font_family)

  if (plot == TRUE) {
    out <- cowplot::plot_grid(plotlist = legend_list,
                              ncol = ncol,align='v')
    return(out)
  } else {
    return(legend_list)
  }
}
