#' Shaded LG backgrounds
#'
#' \code{geom_hypo_LG} adds the linkage group (LG) boundaries to background of a ggplot.
#'
#' To add context to genome wide plots of a statistic (e.g. Fst) this function
#' adds a alternating shaded background to the plot.
#'
#'
#' @examples
#'
#' @seealso \code{\link{scale_fill_hypo_LG_bg}}
#'
#' ggplot() +
#'   geom_hypo_LG()+
#'   scale_color_hypo_LG()
#'
#' @export
hypo_anno_l <- function(species, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,...){
    stopifnot(length(species) == 1)
    stopifnot(is.character(species))
    stopifnot(species %in% hypo_img$spec)

    nr_species <- which(hypo_img$spec == species)

    annotation_custom(hypo_img$l[[nr_species]], xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  }

hypo_anno_r <- function(species, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,...){
  stopifnot(length(species) == 1)
  stopifnot(is.character(species))
  stopifnot(species %in% hypo_img$spec)

  nr_species <- which(hypo_img$spec == species)

  annotation_custom(hypo_img$r[[nr_species]], xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

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
    scale_y_continuous(limits = c(-.35, .35))+
    annotation_custom(hypo_img$l[[nr_species]],
                      xmin = -.45,xmax = .45,ymin = -Inf, ymax = Inf)

  if(plot_names){
    names_df <- tibble(name = str_c('italic(H.~',species,')'),
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

hypo_legend_single <- function(species,color_map,
                               circle_color = NA, circle_lwd = .5,
                               plot_names = FALSE, plot_name_size = 3,font_family = 'sans',
                               plot = TRUE){
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
                                ncol = 1,align='v')
    return(out)
  } else {
    return(legend_list)
  }

}

hypo_legend_pair <- function(left,right,color_map,
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
                      circle_color = rep(circle_color, n),
                      circle_lwd = rep(circle_lwd, n),
                      plot_names = rep(plot_names, n),
                      plot_name_size = rep(plot_name_size, n),
                      font_family = rep(font_family, n))

  legend_list <- legend_df %>%
    purrr::pmap(hypo_anno_pair)

  if (plot == TRUE) {
    out <- cowplot::plot_grid(plotlist = legend_list,
                              ncol = 1,align='v')
    return(out)
  } else {
    return(legend_list)
  }

}

