\name{doforeachrow}
\alias{doforeachrow}
\title{
Applies a function to each row of a data frame
}
\description{
  This function applies a function to each row of a data frame. All
  variables of the data frame and the optional arguments are passed as
  arguments to the function. 
  }
\usage{
doforeachrow(data, fun, doitalsoforoptargs, ...)
}

\arguments{
  \item{data}{A data frame.}
  \item{fun}{A non-empty character string naming the
    function to be called.}
  \item{doitalsoforoptargs}{Logical. See Details.}
  \item{...}{Other arguments.}
}
\details{
  It applies a function to each row of the data frame. The variables of the
  data frame and the optional arguments are passed as arguments. If
  there are common names among the data frame and the optional
  arguments, those values of the data frame are used.

   If \code{doitalsoforoptargs} is TRUE, then try to use the row of the
   optional arguments, if it is possible (only when the length of the
   optional argument is equal to the number of rows of the data
   base). Otherwise, (\code{doitalsoforoptargs} = FALSE or the lengths
   are different), the whole optional arguments are passed to the function.
 }
\value{
A list for each row with the values returned by the function.
}
% \references{

%  }
% \author{
% Emilio Torres Manzanera
% }
\note{
 Generic function.
}
% \seealso{
%   \code{\link[gpplot2]{aes_string}}
% }
\examples{
fun <- "seq"
data <- data.frame(from=c(1,10), to=c(2,11))
doforeachrow(data, fun, TRUE)
## [[1]]
## [1] 1 2

## [[2]]
## [1] 10 11

doforeachrow(data, fun, TRUE, length.out=c(4))
## [[1]]
## [1] 1.000000 1.333333 1.666667 2.000000

## [[2]]
## [1] 10.00000 10.33333 10.66667 11.00000

doforeachrow(data, fun, TRUE, length.out=c(3,5))
## [[1]]
## [1] 1.0 1.5 2.0

## [[2]]
## [1] 10.00 10.25 10.50 10.75 11.00
}

\keyword{ manip }