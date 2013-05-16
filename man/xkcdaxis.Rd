\name{xkcdaxis}
\alias{xkcdaxis}
\title{
Plots the axis
}
\description{
  This function plots the axis. 
  }
\usage{
xkcdaxis(xrange, yrange)
}

\arguments{
 \item{xrange}{The range of X axe.}
 \item{yrange}{The range of Y axe.}
  %   function to be called.}
  % \item{doitalsoforoptargs}{Logical. See Details.}
  % \item{...}{Other arguments.}
}
\details{
  It plots the axis of the graph. 
 }
\value{
A layer with the axis.
}
% \references{
%   fibosworld 2013. Change fonts in ggplot2, and create xkcd style graphs \url{http://fibosworld.wordpress.com/2013/02/17/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/}

%   Various Authors 2012. How can we make xkcd style graphs in R?
%   \url{http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r}

%   \url{http://simonsoftware.se/other/xkcd.ttf}
%  }
% \author{
% Emilio Torres Manzanera
% }
% \note{
%  To install the fonts:
  
% 	download.file("http://simonsoftware.se/other/xkcd.ttf",
% 	dest="xkcd.ttf")
% 	system("mkdir ~/.fonts")
% 	system("cp xkcd.tff -t ~/.fonts")
% 	library(extrafont)
% 	font_import()
  
% }
% \seealso{
%   \code{\link[gpplot2]{aes_string}}
% }
\examples{
xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
p <- ggplot() + geom_point(aes(mpg, wt), data=mtcars) +
     xkcdaxis(xrange,yrange)
p
}

\keyword{ manip }