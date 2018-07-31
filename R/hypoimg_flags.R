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

hypo_flag_palette <- function(x=1:41){
  cowplot::plot_grid(plotlist = map(hypo_flag$geo[x],hypo_flag_test))
}


hypo_anno_flag <- function(geo, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,...){
  stopifnot(length(geo) == 1)
  stopifnot(is.character(geo))
  stopifnot(geo %in% hypo_flag$geo)

  nr_geo <- which(hypo_flag$geo == geo)

  annotation_custom(hypo_flag$flag[[nr_geo]],
                    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}


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
      geom_text(data = names_df, aes(x = x, y = y, label = name),
                size = plot_name_size , family = font_family)
    return(p_names)
  } else {
    return(p)
  }
}

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
                       x = c(.3, -.3),
                       y = c(-.35, -.35))

    p_names <- p +
      geom_text(data = names_df, aes(x = x, y = y, label = name),
                size = plot_name_size , family = font_family)
    return(p_names)
  } else {
    return(p)
  }
}


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
