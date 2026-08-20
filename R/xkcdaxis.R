## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2018-05-23 17:37 emilio on emilio-despacho>
## ============================================================

#' Plot the axis
#'
#' This function plots the axis in an XKCD style.
#'
#' @param xrange The range of the X axe.
#' @param yrange The range of the Y axe.
#' @param ... Other arguments passed to geom_xkcdpath.
#' @return A list of layers containing the axes, coordinate system, and theme.
#' @import ggplot2
#' @import extrafont
#' @importFrom Hmisc bezier
#' @export
#' @examples
#' \dontrun{
#' xrange <- range(mtcars$mpg)
#' yrange <- range(mtcars$wt)
#' p <- ggplot() +
#'      geom_point(aes(mpg, wt), data=mtcars) +
#'      xkcdaxis(xrange,yrange)
#' p
#' }
xkcdaxis <- function(xrange, yrange, ...) {
  if( is.null(xrange) | is.null(yrange) )
    stop("Arguments are: xrange, yrange")
  
  # Calculate jitter amounts based on ranges
  xjitteramount <- diff(xrange)/50
  yjitteramount <- diff(yrange)/50
  
  # Data for the X-axis line
  dataaxex <- data.frame(x=xrange[1]-xjitteramount,
                         y=yrange[1]-yjitteramount,
                         xend=xrange[2]+xjitteramount,
                         yend=yrange[1]-yjitteramount)
  
  mappingsegment <- aes(x=x,y=y,xend=xend,yend=yend)

  # Draw X-axis
  axex <- geom_xkcdpath(mappingsegment,
                        dataaxex,
                        yjitteramount = yjitteramount,
                        mask = FALSE,
                        ... )
  
  # Data for the Y-axis line
  dataaxey <- data.frame(x=xrange[1]-xjitteramount,
                         y=yrange[1]-yjitteramount,
                         xend=xrange[1]-xjitteramount,
                         yend=yrange[2]+yjitteramount)
  
  # Draw Y-axis
  axey <- geom_xkcdpath(mappingsegment,
                        dataaxey,
                        xjitteramount = xjitteramount,
                        mask = FALSE,
                        ... )
  
  # Set up the coordinate system and theme
  coordcarte <- coord_cartesian(xlim = xrange + 1.5*c(-xjitteramount,xjitteramount),
                                ylim = yrange + 1.5*c(-yjitteramount,yjitteramount))
  
  # Return the layers
  list(c(axex,axey), coordcarte, theme_xkcd())
}