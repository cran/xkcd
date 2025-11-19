## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2018-05-23 12:15 emilio on emilio-despacho>
## ============================================================

pointscircunference <- function(x =0, y=0, diameter = 1, ratioxy=1, npoints = 16, alpha=  runif(1, 0, pi/2), ...) {
  center <- c(x,y)
  r <- rep( diameter / 2, npoints )
  tt <- seq(alpha,2*pi + alpha,length.out = npoints)
  r <- jitter(r)
  sector <-  tt > alpha & tt <= ( pi/ 2 + alpha)
  r[ sector ] <- r[sector] * 1.05
  sector <-  tt > ( 2 * pi/2 + alpha)  & tt < (3* pi/ 2 +alpha)
  r[ sector ] <- r[sector] * 0.95
  xx <- center[1] + r * cos(tt) * ratioxy
  yy <- center[2] + r * sin(tt)
  return(data.frame(Hmisc::bezier(x = xx, y =yy, evaluation=60)))
}

pointssegment <- function(x, y, xend, yend, npoints = 10, xjitteramount= 0, yjitteramount=0, bezier = TRUE, ...) {
  if(npoints < 2 )
    stop("npoints must be greater than 1")
  if( xjitteramount == 0 & yjitteramount == 0) npoints <- 2
  xbegin <- x
  ybegin <- y
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
    data <- data.frame(Hmisc::bezier(x=x, y=y, evaluation=30))
  }
  else data <- data.frame(x=x,y=y)
  data
}

#' @title Draw lines or circles
#' @param mapping Aesthetic mapping
#' @param data Dataset
#' @param typexkcdline "segment" or "circunference"
#' @param mask Logical
#' @param ... Additional arguments
#' @export
#' @import ggplot2
xkcdline <- function(mapping, data, typexkcdline="segment", mask = TRUE, ...) {
  if(typexkcdline == "segment" ){
    fun <- "pointssegment"
    requiredaesthetics <-  c("x","y","xend","yend")
  } else if(typexkcdline == "circunference" ) {
    fun <- "pointscircunference"
    requiredaesthetics <-  c("x","y","diameter")
  } else stop("typexkcdline must be segment or circle")
  
  nsegments <- dim(data)[1]
  datafun <- data
  argList<-list(...)
  fcn <- get(fun, mode = "function")
  argsfcntt <-  names(formals(fcn))
  argsfcn <- argsfcntt[ argsfcntt != "..."]
  
  for( i in intersect(argsfcn, names(argList))) {
    if(!(is.null(argList[i])==TRUE)){
      if(length(argList[[i]]) == 1 ) datafun[, i] <- unlist(rep(argList[[i]],nsegments))
      if(length(argList[[i]]) == nsegments ) datafun[, i] <- argList[[i]]
    }
  }
  
  listofinterpolates <- list()
  for(j in 1:nrow(datafun)) {
    row <- datafun[j,]
    listofinterpolates[[j]] <- do.call(fcn, as.list(row))
  }
  
  listofinterpolateswithillustrativedata <- lapply(1:nsegments,
                                                   function(i) {
                                                     dti <- listofinterpolates[[i]]
                                                     illustrativevariables <- names(datafun)[ ! names(datafun) %in% names(dti) ]
                                                     dti[, illustrativevariables] <- datafun[i, illustrativevariables]
                                                     dti}
  )
  
  listofpaths <- lapply(listofinterpolateswithillustrativedata,
                          function(x, mapping, mask, ...) {
                          pathmask <- NULL
                          if(mask) {
                            argList <- list(...)
                            for(i in intersect(c("color","colour"), names(argList)))
                              argList[i] <- NULL
                            # Determine user-specified linewidth (support legacy 'size')
                            user_lw <- NULL
                            if ("linewidth" %in% names(argList)) user_lw <- argList$linewidth
                            if (is.null(user_lw) && "size" %in% names(argList)) {
                              user_lw <- argList$size
                              argList$size <- NULL
                            }
                            if (is.null(user_lw)) user_lw <- 0.8

                            argList$mapping <- aes(x = x, y = y)
                            argList$data <- x
                            # mask should be thicker than the main line
                            argList$linewidth <- max(user_lw * 2, 1)
                            argList$colour <- "white"
                            pathmask <- do.call("geom_path", argList)
                          }
                          # Main path uses the original '...' so user can specify aesthetics
                          c(pathmask, geom_path(aes(x = x, y = y), data = x, ...))
                        },
                        mapping = mapping,
                        mask= mask
  )
  listofpaths
}

if (getRversion() >= "2.15.1") utils::globalVariables(c("x", "y"))