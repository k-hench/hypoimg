#' The custom grob geom
#'
#' This geom builds heavily on the answer by baptiste on the
#' tidiverse github forum:
#' https://github.com/tidyverse/ggplot2/issues/1399
#'
#' Only the parameters height, width and angle have been added.
#'
#' @seealso \code{\link{geom_hypo_grob}}
hypo_geom_grob_custom <- ggproto(
  "hypo_geom_grob_custom",
  Geom,
  setup_data = function(self, data, params) {
    data <- ggproto_parent(Geom, self)$setup_data(data, params)
    data
  },

  draw_group = function(data, panel_scales, coord) {
    vp <- grid::viewport(x=data$x, y=data$y, h = data$height,width = data$width, angle = data$angle)
    g <- grid::editGrob(data$grob[[1]], vp = vp)
    ggplot2:::ggname("geom_hypo_grob", g)
  },

  required_aes = c("grob","x","y"),
  default_aes = list(height = 1, width = 1, angle = 0)

)

#' Provides the grob geom
#'
#' \code{geom_hypo_grob} provides a geom of grobs
#'
#' The function \code{geom_hypo_grob} provides a way to add grob annotations
#' as geom so that the can be distributed over differnt facets.
#'
#' This geom builds heavily on the answer by baptiste on the
#' tidiverse github forum:
#' https://github.com/tidyverse/ggplot2/issues/1399
#'
#' Only the parameters height, width and angle have been added.
#'
#' @param mapping Set of aesthetic mappings created by aes()
#' @param data The data to be displayed in this layer. This shoul be a tibble
#'   containing the grobs as one column, x and y values as columns and potential
#'   additional aesthetics (width, height and angle). Note, that x and y should
#'   be within 0 and 1 and that width and height represent the ration comprared
#'   to default size of the grob.
#' @param stat          The statistical transformation to use on the data for this layer, as a string.
#' @param position      Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm         logical.
#' @param show.legend   logical.
#' @param inherit.aes   logical.
#' @param ...           parameters passed to layer
#'
#' @export
geom_hypo_grob <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = FALSE,
                         ...) {
  layer(
    geom = hypo_geom_grob_custom,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
