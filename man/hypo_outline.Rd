% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypoimg_data.R
\docType{data}
\name{hypo_outline}
\alias{hypo_outline}
\title{Hamlet Outline Coordinates.}
\format{
A tibble with 147 rows and 2 variables:
\describe{
\item{x}{numeric, the x coordinates}
\item{y}{numeric, the y coordinates}
}
}
\source{
drawing by KH.
}
\usage{
hypo_outline
}
\description{
A data set holding the coordinates of the hamlet outline.
}
\examples{
ggplot(data = hypo_outline, aes(x = x, y = y)) +
  coord_equal() +
  geom_path() +
  geom_point(aes(color = seq_along(x)))
}
\keyword{datasets}
