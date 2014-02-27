# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2014-02-27 jue 16:10 emilio on emilio-Satellite-P100>
# =====================================================================


pointssegment <- function(xbegin, ybegin, xend, yend, npoints = 10, xjitteramount= 0, yjitteramount=0, bezier = TRUE) {
  ##require(Hmisc) # bezier
  if(npoints < 2 )
    stop("npoints must be greater than 1")
  ## If there are no jitters, do not interpolate
  if( xjitteramount == 0 & yjitteramount == 0) npoints <- 2 
  x <- seq(xbegin,xend,length.out = npoints)
  if( (xend - xbegin) != 0 ) {
    y <- (yend - ybegin) * ( x - xbegin ) / (xend - xbegin) + ybegin
  } else {
    y <-  seq(ybegin, yend, length.out = npoints)
  }
  if(xjitteramount !=0) x <- jitter(x, amount=xjitteramount)
  if(yjitteramount !=0) y <- jitter(y, amount=yjitteramount)
  x[1] <- xbegin
  y[1] <- ybegin
  x[length(x)] <- xend
  y[length(y)] <- yend
  if(bezier & length(x)>2 & (xjitteramount != 0 | yjitteramount != 0)) {
    data <- data.frame(bezier(x=x, y=y, evaluation=30))
  }
  else data <- data.frame(x=x,y=y)   
  data
}

