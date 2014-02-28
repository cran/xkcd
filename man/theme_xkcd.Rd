\name{theme_xkcd}
\alias{theme_xkcd}
\title{
Creates a XKCD theme
}
\description{
  This function creates a XKCD theme for ggplot graphics. 
  }
\usage{
theme_xkcd()
}

\arguments{
  None
  % \item{data}{A data frame.}
  % \item{fun}{A non-empty character string naming the
  %   function to be called.}
  % \item{doitalsoforoptargs}{Logical. See Details.}
  % \item{...}{Other arguments.}
}
\details{
  It uses the XKCD fonts. You must install this type of fonts. See Note.
 }
\value{
A theme with white background and fonts of the XKCD family.
}
\references{
  fibosworld 2013. Change fonts in ggplot2, and create xkcd style graphs \url{http://fibosworld.wordpress.com/2013/02/17/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/}

  Various Authors 2012. How can we make xkcd style graphs in R?
  \url{http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r}

  \url{http://simonsoftware.se/other/xkcd.ttf}
 }
% \author{
% Emilio Torres Manzanera
% }
\note{
 To install the fonts (See the vignette):
  
	download.file("http://simonsoftware.se/other/xkcd.ttf",
	dest="xkcd.ttf")
	system("mkdir ~/.fonts")
	system("cp xkcd.tff -t ~/.fonts")
	##library(sysfonts)
	font.add("xkcd",  regular = "xkcd.ttf")
	
  
}
% \seealso{
%   \code{\link[gpplot2]{aes_string}}
% }
\examples{
\dontrun{ 
p <- ggplot() + geom_point(aes(mpg, wt), data=mtcars) + theme_xkcd()
p
}
}

\keyword{ manip }