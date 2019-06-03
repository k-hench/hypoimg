#' Add metadata to exported plots
#'
#' \code{hypo_save()} adds a comment to an exported plot.
#'
#' Sometimes it can be useful to add metadata to an exported plot.
#' A typical situation would be to add the name of the r-script
#' producing the figure.
#' This can potentially save some time when trying to find the
#' original script later in life.
#'
#' Note: The function assumes a UNIX platform and depends on exiftool
#' to be installed and located within the $PATH.
#'
#' See https://www.sno.phy.queensu.ca/~phil/exiftool/index.html
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param comment string skalar,comment to add to the figure metadata.
#' @param ... Other arguments passed on to the \code{ggsave()} function.
#' @seealso \code{\link{hypo_show_metadata}},
#' @export
#' @examples
#' tibble(x = rnorm(20),y= rnorm(20)) %>%
#'   ggplot(aes(x,y))+geom_point()
#'
#' hypo_save('~/Desktop/test_plot.png',comment = 'Created by script test.R')
#' hypo_show_metadata('~/Desktop/test_plot.png')
#'
hypo_save <- function(filename, plot = last_plot(), comment = 'exported with hypoimg::hypo_save()', ...){
  require(tidyverse)

  file_type <- filename %>% str_sub(.,start = -3,end = -1)

  ggsave(filename = filename, plot = plot, ...)

  if (file_type %in% c('png','pdf')) {
    create_metadata <- str_c('exiftool -overwrite_original -Description="',comment,'" ',filename)
    system(create_metadata)

  } else {
    cat(str_c(crayon::red('FAIL!'),' Sorry, currently only png and pdf are suported.'))
  }


}

#' Read metadata from an exported plot
#'
#' \code{hypo_show_metadata()} reads metadata from an exported plot.
#'
#' If we previously added metadata to an exported plot, we need
#' some way of reading those.
#' The function \code{hypo_show_metadata()} reports any comment added
#' with \code{hypo_save()}.
#'
#' Note: The function assumes a UNIX platform and depends on exiftool
#' to be installed and located within the $PATH.
#'
#' See https://www.sno.phy.queensu.ca/~phil/exiftool/index.html
#'
#' @param filename File name to create on disk.
#' @seealso \code{\link{hypo_save}},
#' @export
#' @examples
#' tibble(x = rnorm(20),y= rnorm(20)) %>%
#'   ggplot(aes(x,y))+geom_point()
#'
#' hypo_save('~/Desktop/test_plot.png',comment = 'Created by script test.R')
#' hypo_show_metadata('~/Desktop/test_plot.png')
hypo_show_metadata <- function(filename){
  command <- str_c('exiftool ',filename,' | grep Description | sed "s/.*: //"')
  system(command,intern = TRUE)
}
