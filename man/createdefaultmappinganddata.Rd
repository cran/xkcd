\name{createdefaultmappinganddata}
\alias{createdefaultmappinganddata}
\title{
Creates a canonical mapping and its associated data base
}
\description{
  This function transforms the mapping and the data base to get a
  canonical mapping where the aesthetic variables are equal to the names
  of the data base.
}
\usage{
createdefaultmappinganddata(mapping, data, mandatoryarguments=c("x","y"))
}

\arguments{
  \item{mapping}{Set of aesthetics.}
  \item{data}{Default data frame. It is expected that all layers are
	included in the data set.}
  \item{mandatoryarguments}{Arguments that must appear in the mapping}
}
\details{
  It creates an aesthetic layer with the default names. If it is
  necessary, it changes the names of the data base.
}
\value{
A list with a mapping of aesthetics and a data frame.
}
% \references{
%   D. J. Bernstein The cdbmake and cdbdump programs \url{http://cr.yp.to/cdb/cdbmake.html}

%    Yusuke Shinyama (2003) Constant Database (cdb) Internals
%    \url{http://www.unixuser.org/~euske/doc/cdbinternals/}
   
%    Michael Tokarev (2012) TinyCDB - a Constant DataBase
%    \url{http://www.corpit.ru/mjt/tinycdb.html}
%  }
\author{
Emilio Torres Manzanera
}
\note{
 Internal use.
}
\seealso{
  \code{\link[ggplot2]{aes_string}}
}
\examples{
data <- data.frame(x1=1,y1=2)
mapping <- aes(x = x1 + y1, y = y1) # default names: x and y
mapping
## List of 2
##  $ x: language x1 + y1
##  $ y: symbol y1

createdefaultmappinganddata(mapping,data)
## $mapping
## List of 2
##  $ x: symbol x
##  $ y: symbol y

## $data
##   x y x1 y1
## 1 3 2  1  2
}

\keyword{ manip }