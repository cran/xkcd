\name{xkcdaxis}
\alias{xkcdaxis}
\title{
Plots the axis
}
\description{
  This function plots the axis. 
  }
\usage{
xkcdaxis(xrange, yrange, ...)
}

\arguments{
 \item{xrange}{The range of X axe.}
 \item{yrange}{The range of Y axe.}
  %   function to be called.}
  % \item{doitalsoforoptargs}{Logical. See Details.}
  \item{...}{Other arguments.}
}
\details{
  It plots the axis of the graph. 
 }
\value{
A layer with the axis.
}
\examples{
\dontrun{
xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
p <- ggplot() + geom_point(aes(mpg, wt), data=mtcars) +
     xkcdaxis(xrange,yrange)
p
}
}

\keyword{ manip }