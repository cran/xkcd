## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-03 Fri 21:24 emilio on emilio-laptop2>
## ============================================================

## Axis
xkcdaxis <- function(xrange, yrange) {
  if( is.null(xrange) | is.null(yrange) )
    stop("Arguments are: xrange, yrange")
  xjitteramount <- diff(xrange)/50
  yjitteramount <- diff(yrange)/50
  ## This cause R CMD check to give the note
  ## “no visible binding for global variable”
  ## Notes do not forbbiden the submission
  ## I will follow this suggestion:
  ## http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  ##mappingsegment <- aes(x=x,y=y,xend=xend,yend=yend) ## Put it within a with!!!
 
  dataaxex <- data.frame(xbegin=xrange[1]-xjitteramount,
                          ybegin=yrange[1]-yjitteramount,
                          xend=xrange[2]+xjitteramount,
                          yend=yrange[1]-yjitteramount)
  mappingsegment <- with(dataaxex, aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend))
  axex <- xkcdline(mappingsegment, dataaxex, yjitteramount = yjitteramount, mask = FALSE )
  
 
  dataaxey <- data.frame(xbegin=xrange[1]-xjitteramount,
                          ybegin=yrange[1]-yjitteramount,
                          xend=xrange[1]-xjitteramount,
                          yend=yrange[2]+yjitteramount)
  mappingsegment <- with(dataaxey, aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend))
  axey <- xkcdline(mappingsegment, dataaxey, xjitteramount = xjitteramount, mask = FALSE )
  coordcarte <- coord_cartesian(xlim = xrange + 1.5*c(-xjitteramount,xjitteramount),
                                ylim = yrange + 1.5*c(-yjitteramount,yjitteramount))
  list(c(axex,axey), coordcarte,theme_xkcd())
}
