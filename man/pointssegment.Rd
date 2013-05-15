\name{pointssegment}
\alias{pointssegment}
\title{
Interpolates between two points
}
\description{
  This function calculates intermediate points between two points. The
  intermidate points can be jittered and smoothed with a Bezier curve.
  }
  \usage{
pointssegment(xbegin, ybegin, xend, yend,
              npoints = 10, xjitteramount= 0, yjitteramount=0, bezier = TRUE)
}

\arguments{
  \item{xbegin}{A numeric value. Coordinate x of the point from which to interpolate.}
  \item{ybegin}{A numeric value. Coordinate y of the point from which to interpolate.}
  \item{xend}{A numeric value. Coordinate x of the point to which to interpolate.}
  \item{yend}{A numeric value. Coordinate y of the point to which to interpolate.}
  \item{npoints}{Number of points (including the limits).}
  \item{xjitteramount, yjitteramount}{Numeric. Amount of jitter.}
  \item{bezier}{Logical. Use or not the Bezier curves to smooth the
  jittered data.}
}
\details{
  It calculates the intermediate points between two points. If there are
  no jitter amounts, then there are no interpolations.

  By default, the data are smoothed using a Bezier curve. It gives 30 points.
}
\value{
A data frame with the interpolated values.
}
% \references{

%  }
% \author{
% Emilio Torres Manzanera
% }
% \note{
%  Generic function.
% }
\seealso{
  \code{\link[base]{jitter}}
}
\examples{
plot(pointssegment(xbegin=0, ybegin=0, xend=10, yend=10))
plot(pointssegment(xbegin=0, ybegin=0, xend=10, yend=10,
                   xjitteramount=2,yjitteramount=2))
plot(pointssegment(xbegin=0, ybegin=0, xend=10, yend=10,
                   xjitteramount=2,yjitteramount=2,bezier=FALSE))
}

%\keyword{ manip }