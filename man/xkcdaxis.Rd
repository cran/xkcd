% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xkcdaxis.R
\name{xkcdaxis}
\alias{xkcdaxis}
\title{Plot the axis}
\usage{
xkcdaxis(xrange, yrange, ...)
}
\arguments{
\item{xrange}{The range of the X axe.}

\item{yrange}{The range of the Y axe.}

\item{...}{Other arguments.}
}
\value{
A layer with the axis.
}
\description{
This function plots the axis
}
\details{
It plots the axis of the graph.
}
\examples{
\dontrun{
xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
p <- ggplot() +
     geom_point(aes(mpg, wt), data=mtcars) +
     xkcdaxis(xrange,yrange)
p
}
}
