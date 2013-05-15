\name{xkcd-package}
\alias{xkcd-package}
\alias{xkcd}
\docType{package}
\title{
Plot ggplot graphics in a XKCD style
}
\description{
  XKCD is a webcomic of romance, sarcasm, math, and language created by
  Randall Munroe. This package tries to give a satisfactory answer to
  the question How can we make xkcd style graphs in R?. The package
  provides a set of ggplot2 functions for plotting data in a XKCD style.
}
\details{
\tabular{ll}{
Package: \tab xkcd\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2013-04-27\cr
License: \tab GPL3\cr
}
This package introduces three main functions: \command{xkcdman}, \command{xkcdline} and
\command{xkcdrect}. The \command{xkcdman} function creates a XKCD man
and the other one plot pseudo handwritten objects. These functions
are derived from the \command{geom_path} of the package
ggplot2. All the funcionalities of the ggplot system can be used. See
the vignettes for detailed examples.
}
\author{
Emilio Torres Manzanera

Maintainer: Emilio Torres Manzanera <torres@uniovi.es>
}
\references{
  Hadley Wickham 2012. ggplot2 \url{http://ggplot2.org/}
  
Randall Munroe. A webcomic of romance, sarcasm, math, and language
\url{http://xkcd.com/}

Various Authors 2012. How can we make xkcd style graphs in R? \url{http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r}
}
% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
% ~~ the R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link[ggplot2]{geom_path}}
}
\examples{
p <- ggplot() + geom_point(aes(mpg, wt), data=mtcars) + theme_xkcd()
}
