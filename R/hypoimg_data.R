#' Several coral drawings.
#'
#' A data set holding several coral drawings.
#'
#' @format A tibble with 6 rows and 3 variables:
#' \describe{
#'   \item{side}{string, orientation of the drawing (left/ right)}
#'   \item{type}{string, type of coral}
#'   \item{grob}{grob ({grid} object), the actual drawing}
#' }
#' @source drawing by KH.
#' @examples
#' ggplot() +
#'   geom_hypo_grob(data = hypo_coral_img,
#'                  aes(x = .5, y = .5, grob = grob)) +
#'   facet_grid(side ~ type)
"hypo_coral_img"

#' Several Country Flags.
#'
#' A data set holding several flags.
#'
#' @format A tibble with 41 rows and 2 variables:
#' \describe{
#'   \item{geo}{string, name of the country}
#'   \item{grob}{grob ({grid} object), the actual flag}
#' }
#' @source modified from \url{wikipedia.org}.
#' @examples
#' ggplot() +
#'   geom_hypo_grob(data = hypo_coral_img,
#'                  aes(x = .5, y = .5, grob = grob)) +
#'   facet_grid(side ~ type)
"hypo_flag"

#' Generic hamlet drawing.
#'
#' A data set holding a generic hamlet drawing.
#'
#' @format A tibble with 2 rows and 2 variables:
#' \describe{
#'   \item{side}{string, orientation of the drawing (left/ right)}
#'   \item{grob}{grob ({grid} object), the actual drawing}
#' }
#' @source drawing by KH.
#' @examples
#' ggplot() +
#'   geom_hypo_grob(data = hypo_generic_img,
#'                  aes(x = .5, y = .5, grob = grob)) +
#'   facet_wrap(side ~ .)
"hypo_generic_img"

#' Hamlet species drawings.
#'
#' A data set holding species specific hamlet drawings.
#'
#' @format A tibble with 22 rows and 4 variables:
#' \describe{
#'   \item{spec}{string, species name}
#'   \item{geno}{string, genus (sic.), abbreviation}
#'   \item{l}{grob ({grid} object), the left facing drawings}
#'   \item{r}{grob ({grid} object), the right facing drawings}
#' }
#' @source drawing by KH.
#' @examples
#' ggplot() +
#'   geom_hypo_grob(data = hypo_img,
#'                  aes(x = .5, y = .5, grob = l)) +
#'   facet_wrap(spec ~ .)
#'
#' ggplot() +
#'   geom_hypo_grob(data = hypo_img,
#'                  aes(x = .5, y = .5, grob = r)) +
#'   facet_wrap(spec ~ .)
"hypo_img"

#' Hamlet Outline Coordinates.
#'
#' A data set holding the coordinates of the hamlet outline.
#'
#' @format A tibble with 147 rows and 2 variables:
#' \describe{
#'   \item{x}{numeric, the x coordinates}
#'   \item{y}{numeric, the y coordinates}
#' }
#'
#' @source drawing by KH.
#'
#' @examples
#' ggplot(data = hypo_outline, aes(x = x, y = y)) +
#'   coord_equal() +
#'   geom_path() +
#'   geom_point(aes(color = seq_along(x)))
"hypo_outline"

#' Hamlet trait drawings.
#'
#' A data set holding trair specific hamlet drawings.
#'
#' @format A tibble with 11 rows and 3 variables:
#' \describe{
#'   \item{trait}{string, trait name}
#'   \item{l}{grob ({grid} object), drawings of the traits, cropped to circle}
#'   \item{r}{grob ({grid} object), drawings of the traits, whole}
#' }
#' @source drawing by KH.
#' @examples
#' ggplot() +
#'   geom_hypo_grob(data = hypo_trait_img,
#'                  aes(x = .5, y = .5, grob = grob_circle)) +
#'   facet_wrap(trait ~ .)
#'
#' ggplot() +
#'   geom_hypo_grob(data = hypo_trait_img,
#'                  aes(x = .5, y = .5, grob = grob_whole)) +
#'  facet_wrap(trait ~ .)
"hypo_trait_img"
