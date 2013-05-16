## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-03 Fri 21:32 emilio on emilio-laptop2>
## ============================================================


xkcdrect <- function(mapping, data, ...) {

  requiredaesthetics <-  c("xmin","ymin",
                           "xmax","ymax")
  
  rect1 <-  geom_rect(mapping, data, ...)
  
  defaultmapdat <- createdefaultmappinganddata(mapping, data, requiredaesthetics) 
  data <-defaultmapdat$data
  mapping <- defaultmapdat$mapping
  
  
  xrange <- range(min(data$xmin, data$xmax), max(data$xmin, data$xmax))
  yrange <- range(min(data$ymin, data$ymax), max(data$ymin, data$ymax))
  borderxjitteramount <- diff(xrange)/100
  borderyjitteramount <- diff(yrange)/100

  bordercolour <- "white"
  bordersize <- 3
  
  argList <- list(...)
  if( "colour" %in% names(argList) ) bordercolour <- argList[["colour"]]
  if( "color" %in% names(argList) ) bordercolour <- argList[["color"]]
  if( "size" %in% names(argList) ) bordersize <- argList[["size"]]
  if( "xjitteramount" %in% names(argList) ) borderxjitteramount <- argList[["xjitteramount"]]
  if( "yjitteramount" %in% names(argList) ) borderyjitteramount <- argList[["yjitteramount"]]


  ## To avoid  Notes when R CMD check, use a with
  mappu <- with(data, mappingjoin(aes(xbegin=xmin,ybegin=ymax, xend=xmax, yend=ymax), mapping))
  mappr <- with(data, mappingjoin(aes(xbegin=xmax,ybegin=ymin, xend=xmax, yend=ymax), mapping))
  mappl <- with(data, mappingjoin(aes(xbegin=xmin,ybegin=ymin, xend=xmin, yend=ymax), mapping))
  mappb <- with(data, mappingjoin(aes(xbegin=xmin,ybegin=ymin, xend=xmax, yend=ymin), mapping))
  
  upperline <- xkcdline(mappu,
                        data, colour=bordercolour,
                        yjitteramount=borderyjitteramount, mask = FALSE, size=bordersize, ...)
  
  rightline <-  xkcdline(mappr,
                        data, colour=bordercolour,
                        xjitteramount=borderxjitteramount, mask = FALSE, size=bordersize, ...)
                        
  leftline <-  xkcdline(mappl,
                        data, colour=bordercolour,
                        xjitteramount=borderxjitteramount, mask = FALSE, size=bordersize, ...)
                        
  bottomline <-  xkcdline(mappb,
                        data, colour=bordercolour,
                        yjitteramount=borderyjitteramount, mask = FALSE, size=bordersize, ...)
  
  list(rect1, upperline , rightline, leftline, bottomline)
}

