\name{mappingjoin}
\alias{mappingjoin}
\title{
Joins two sets of aesthetics
}
\description{
  This function joins two sets of aesthetics.
}
\usage{
mappingjoin(x,y)
}

\arguments{
  \item{x}{Set of aesthetics.}
  \item{y}{Set of aesthetics.}

}
\details{
  It creates an aesthetic layer joining both sets of aesthetics. If
  there are common names between the sets, only those ones of the
  \code{x} set are used.
}
\value{
A mapping of aesthetics.
}
% \references{
%   D. J. Bernstein The cdbmake and cdbdump programs \url{http://cr.yp.to/cdb/cdbmake.html}

%    Yusuke Shinyama (2003) Constant Database (cdb) Internals
%    \url{http://www.unixuser.org/~euske/doc/cdbinternals/}
   
%    Michael Tokarev (2012) TinyCDB - a Constant DataBase
%    \url{http://www.corpit.ru/mjt/tinycdb.html}
%  }
% \author{
% Emilio Torres Manzanera
% }
\note{
 Internal use.
}
\seealso{
  \code{\link[ggplot2]{aes_string}}
}
\examples{
x <- aes(x=x1,z=z1)
y <- aes(x=x2,z=z2,t=t2)
mappingjoin(x,y)
}

\keyword{ manip }