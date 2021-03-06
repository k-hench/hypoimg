% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypoimg_flags.R
\name{hypo_flag_palette}
\alias{hypo_flag_palette}
\title{Display available flags}
\usage{
hypo_flag_palette(x = 1:41)
}
\arguments{
\item{species}{interger vector (optional, elements >= 1 & <= 41), subselect
flags to be displayed}
}
\description{
\code{hypo_flag_palette} displays all available flags.
}
\details{
Hypogen comes with a set of illustrations of flags of the countries adjacent
to the Caribbean and the Golf of Mexico.
The function \code{hypo_flag_palette} displays all available flags as well
as their name (to be used in other hypo_flag* functions, note that line breaks
in the country names need to be replaced with an undercore if used as ID
in other functions)
}
\examples{
hypo_flag_palette()

hypo_flag_palette(1:4)

}
