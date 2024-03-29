% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypoimg_generic.R
\docType{data}
\name{clr2}
\alias{clr2}
\title{two colors}
\format{
An object of class \code{character} of length 2.
}
\usage{
clr2
}
\description{
\code{clr2} is a combination of two colors I like.
}
\details{
A vector containg blue and orange.
}
\examples{
ggplot(tibble( x = 1:2))+
  coord_equal()+
  geom_tile(aes(x = x, y = 1, fill = factor(x)))+
  scale_fill_manual(values = clr2)+
  theme_void()+
  theme(legend.position = 'none')

}
\keyword{datasets}
