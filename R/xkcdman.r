# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-03 Fri 20:41 emilio on emilio-laptop2>
# =====================================================================


xkcdman <- function(mapping, data, ...) {
  
  requiredaesthetics <-  c("x","y",
                           "scale",
                           "ratioxy",
                           "angleofspine",
                           "anglerighthumerus",
                           "anglelefthumerus",
                           "anglerightradius",
                           "angleleftradius",
                           "anglerightleg",
                           "angleleftleg",                            
                           "angleofneck")

  
  ## We transform the data to get a default mapping
  defaultmapdat <- createdefaultmappinganddata(mapping, data, requiredaesthetics) 
  data <-defaultmapdat$data
  mapping <- defaultmapdat$mapping
  
  centerofhead <- cbind(data$x,data$y)
  diameterofhead <-  data$scale
  lengthofspine <- diameterofhead 
  lengthofleg <- lengthofspine * 1.2
  lengthofhumerus <- lengthofspine * 0.6
  lengthofradius <- lengthofspine * 0.5
  beginspine <- centerofhead + (diameterofhead / 2) * cbind( cos(data$angleofneck) * data$ratioxy, sin( data$angleofneck))
  endspine <- beginspine + lengthofspine * cbind( cos( data$angleofspine) * data$ratioxy , sin(data$angleofspine))
  endrighthumerus <- beginspine + lengthofhumerus * cbind( cos( data$anglerighthumerus) * data$ratioxy, sin(data$anglerighthumerus))
  endlefthumerus <- beginspine + lengthofhumerus * cbind( cos( data$anglelefthumerus)* data$ratioxy, sin(data$anglelefthumerus))
  
  bone <- function(begin, distance, angle, ratioxy, mapping, data, ... ) {
    end <- cbind( begin[,1] + distance * cos( angle ) * ratioxy, begin[,2] + distance * sin(angle) )
    data$xbegin <- begin[,1]
    data$ybegin <- begin[,2]
    data$xend <- end[,1]
    data$yend <- end[,2]
    
    ttmapping <- unlist(mapping)
    ttmapping$xbegin <- parse(text = "xbegin")[[1]]
    ttmapping$ybegin <- parse(text = "ybegin")[[1]]
    ttmapping$xend <- parse(text = "xend")[[1]]
    ttmapping$yend <- parse(text = "yend")[[1]]
    newmapping <- structure(ttmapping, class = "uneval")
 
    xkcdline(mapping=newmapping, data=data, ...)
  }
  
  head <- function(centerofhead, diameter, ratioxy , mapping, data,...) {
    data$diameter <- diameter
    
    ttmapping <- unlist(mapping)
    ttmapping$diameter <- parse(text = "diameter")[[1]]
    newmapping <- structure(ttmapping, class = "uneval")
    xkcdline(mapping = newmapping, data =data, typexkcdline="circunference", ...) 
  }
  
  c(head(centerofhead=centerofhead, diameter = diameterofhead, ratioxy = data$ratioxy, mapping = mapping, data = data, ...),
    bone(begin = beginspine, distance = lengthofspine, angle = data$angleofspine, ratioxy = data$ratioxy, mapping =mapping, data = data, ...  ),
    bone(begin = beginspine, distance = lengthofhumerus, angle = data$anglerighthumerus, ratioxy = data$ratioxy, mapping =mapping, data = data, ...) , # right humerus
    bone(begin = endrighthumerus, distance = lengthofradius, angle = data$anglerightradius , ratioxy = data$ratioxy, mapping =mapping, data = data, ...),
    bone(begin = beginspine, distance = lengthofhumerus, angle = data$anglelefthumerus, ratioxy = data$ratioxy, mapping =mapping, data = data, ...),
    bone(begin = endlefthumerus, distance = lengthofradius, angle = data$angleleftradius, ratioxy = data$ratioxy, mapping =mapping, data = data, ...),
    bone(begin = endspine, distance = lengthofleg, angle = data$angleleftleg, ratioxy = data$ratioxy, mapping =mapping, data = data, ...), # Leg
    bone(begin = endspine, distance = lengthofleg, angle = data$anglerightleg, ratioxy= data$ratioxy, mapping =mapping, data = data, ...)
    ) #Leg
}


