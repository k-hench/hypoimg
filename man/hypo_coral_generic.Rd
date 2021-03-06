% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypoimg_generic.R
\name{hypo_coral_generic}
\alias{hypo_coral_generic}
\title{Add a generic coral to a ggplot}
\usage{
hypo_coral_generic(col = "darkgray", x_in = 0.5, y_in = 0.5,
  width_in = 1, height_in = 1, coral_side = c("l", "r"),
  coral_type = c("branch", "table", "brain"), ...)
}
\arguments{
\item{col}{string  (color, optional), defaul = darkgray}

\item{width_in}{numeric skalar (optional), width (in graph x axis units)}

\item{height_in}{numeric skalar (optional), width (in graph y axis units)}

\item{coral_side}{string skalar (optional), one of (l, r) - orientation of the coral}

\item{coral_type}{string skalar (optional), one of (branch, table, brain) - coral type}

\item{x}{numeric skalar (optional), horizontal center for coral placement}

\item{y}{numeric skalar (optional), vertical center for coral placement}
}
\description{
\code{hypo_coral_generic} adds a generic coral with custom color.
}
\details{
This function allows the placement of a coral sketch within a ggplot.
It is based on \code{ggplot2::annotation_custom()} and allows to specify
the coloration of the coral.
}
\examples{
ggplot()+
  hypo_coral_generic(col = clr2[[2]])+
  hypo_hamlet_generic(col = clr2[[1]], x = .6, y = .6,, width_in = .3)   +
  hypo_hamlet_generic(col = clr2[[1]], x = .4, y = .3,  face = 'r', width_in = .3)

}
\seealso{
\code{\link{hypo_hamlet_generic}}
}
