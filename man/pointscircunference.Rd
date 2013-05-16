\name{poinstcircunference}
\alias{pointscircunference}
\title{
Calculates a handwritten circunference
}
\description{
  This function estimate points to draw a handwritten circunference.
  }
  \usage{
pointscircunference(x=0, y=0, diameter=1, ratioxy=1,
                    npoints= 16, alpha= runif(1, 0, pi/2))
}

\arguments{
  \item{x}{A numeric value. Coordinate x of the center of the circunference.}
  \item{y}{A numeric value. Coordinate y of the  center of the circunference.}
  \item{diameter}{A numeric value. Diameter fo the circunference.}
  \item{ratioxy}{A numeric value. Ratio x to y.}
  \item{npoints}{Number of points (including the limits).}
  \item{alpha}{Numeric. Angle to begin to calculate the circunferece.}

}
\details{
  It calculates a circunference in a XKCD style.

  By default, the data are smoothed using a Bezier curve. It gives 60 points.
}
\value{
A data frame with the points of the circunference.
}
% \references{

%  }
% \author{
% Emilio Torres Manzanera
% }
% \note{
%  Generic function.
% }
% \seealso{
%   \code{\link[base]{jitter}}
% }
\examples{
plot(pointscircunference(x=0, y=0), ylim=c(-1,1), xlim=c(-1,1))
plot(pointscircunference(x=0, y=0), ylim=c(-1,1), xlim=c(-2,2))
plot(pointscircunference(x=0, y=0, ratioxy=2),  ylim=c(-1,1), xlim=c(-2,2))
}

%\keyword{ manip }