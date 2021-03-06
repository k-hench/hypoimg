% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypoimg_flags.R
\name{hypo_flag_test}
\alias{hypo_flag_test}
\title{Display a single flags}
\usage{
hypo_flag_test(geo)
}
\arguments{
\item{geo}{string skalar (madatory), flag to be displayed}
}
\description{
\code{hypo_flag_test} displays a single flag.
}
\details{
Hypogen comes with a set of illustrations of flags of the countries adjacent
to the Caribbean and the Golf of Mexico.
The function \code{hypo_flag_test} displays a single flag as well
as its name (to be used in other hypo_flag* functions, note that line breaks
in the country names need to be replaced with an undercore if used as ID
in other functions)
}
\examples{
hypo_flag_test(geo = 'panama')

}
