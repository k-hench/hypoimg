#' Read a cairo svg
#'
#' \code{hypo_read_svg} imports a cairo svg image file.
#'
#' This function reads a (cairo) svg file and turns it into
#' a 'grid object' (grob). This can then be used to annotate
#' ggplot plots.
#'
#' To convert a regular svg into a cairo svg use the
#' grConvert package:
#'
#' \code{grConvert::convertPicture("image.svg","image.c.svg")}
#'
#' @param file_path string skalar (manatory), the cairo svg to import
#'
#'
#' @examples
#' svg_file <- system.file("extdata", "logo.c.svg", package = "hypoimg")
#' svg <- hypo_read_svg(svg_file)
#'
#' @export
hypo_read_svg <- function(file_path){
  grImport2::readPicture(file_path) %>%
    grImport2::pictureGrob(.) %>%
    grid::gList(.) %>%
    grid::gTree(children = . )
}

#' Recolor a svg layer
#'
#' \code{hypo_recolor_svg} redefines the color of a specific svg layer.
#'
#' This function redefines the color of a specific svg layer.
#' The svg needs to be read in by hypo_read_svg.
#'
#' @param svg svg object (manatory), the svg object
#' @param layer integer skalar (optional), the layer to recolor
#' @param color string skalar (optional), the new color
#'
#'
#' @examples
#' svg_file <- system.file("extdata", "logo.c.svg", package = "hypoimg")
#' svg <- hypo_read_svg(svg_file)
#' svg_new <- hypo_recolor_svg(svg,color = 'red')
#'
#' @export
hypo_recolor_svg <- function(svg, layer = 1, color = 'darkgray'){
  svg[[4]][[1]][[4]][[1]][[4]][[layer]]$gp$fill <- color
  svg
}

#' Creates a table svg color variants
#'
#' \code{hypo_recolor_grob_table} turns a svg obect into a table of color variants.
#'
#' This function creates a tibble of colorvariants from a 'raw' svg.
#' This table can then be fed \code{into geom_hypo_grob()} to annotate
#' factes of a ggplot with individual color variants of the svg image.
#'
#' @param svg svg object (manatory), the svg object
#' @param layer integer skalar (manatory, default = 1), the layer to recolor
#' @param levels vector (manatory), the levels of the intended facets
#' @param colormap string vector (manatory), the new colors
#' @param x numeric vector (optional), x-center of the annotation (length = length(leveles))
#' @param y numeric vector (optional), y-center of the annotation (length = length(leveles))
#' @param angle numeric scalar (optional), angle of the annotation
#' @param width numeric scalar (optional), width of the annotation
#' @param height numeric scalar (optional), height of the annotation
#'
#' @seealso \code{\link{geom_hypo_grob}},
#'
#' @examples
#' svg_tab <- recolor_grob_table(svg,LETTERS[1:3],c('red','green','blue'),layer = 1)
#'
#' @export
hypo_recolor_grob_table <- function(svg,layer = 1,
                                    levels,colormap,
                               x = rep(.5,length(levels)),
                               y = rep(.5,length(levels)),
                               angle = rep(0,length(levels)),
                               width = 1,
                               height = 1){
  stopifnot(length(levels) == length(colormap))
  stopifnot(length(x) == length(colormap))
  stopifnot(length(y) == length(colormap))
  stopifnot(length(angle) == length(colormap))
  stopifnot(length(layer) == 1)
  stopifnot(is.numeric(layer))
  stopifnot(length(width) == 1)
  stopifnot(is.numeric(width))
  stopifnot(length(height) == 1)
  stopifnot(is.numeric(height))

  n = length(levels)
  grb <- tibble::tibble(x = x, y = y,
                        angle = angle,
                        width = rep(width, n),
                        heigth = rep(height, n),
                        grob = purrr::pmap(tibble(svg = rep(list(svg), n),
                                                  layer = rep(layer, n),
                                                  color = colormap),
                                           hypo_recolor_svg),
                        grp = levels)
  grb
}
