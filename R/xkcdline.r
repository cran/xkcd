# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-03 Fri 20:46 emilio on emilio-laptop2>
# =====================================================================


xkcdline <- function(mapping, data, typexkcdline="segment", mask = TRUE, ...) {

  if(typexkcdline == "segment" ){
    fun <- "pointssegment"
    ## Required variable in the aesthetics function for segment
    requiredaesthetics <-  c("xbegin","ybegin","xend","yend")
   
  } else if(typexkcdline == "circunference" ) {
    fun <- "pointscircunference"
    requiredaesthetics <-  c("x","y","diameter")
   
  } else stop("typexkcdline must be segment or circle")


  ## We transform the data to get a default mapping
  segementmapdat <- createdefaultmappinganddata(mapping, data, requiredaesthetics) 
  data <- segementmapdat$data
  mapping <- segementmapdat$mapping
 
  nsegments <- dim(data)[1]

  ## Are arguments of fun in the ellipsis?
  ## Yes, try to add to the data base
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

  ## Now, calculate the interpolates for each segment
  listofinterpolates <- doforeachrow(datafun, fun, FALSE, ...)
  listofinterpolateswithillustrativedata <- lapply(1:nsegments,
                                                   function(i) {
                                                     dti <- listofinterpolates[[i]]
                                                     illustrativevariables <- names(datafun)[ ! names(datafun) %in% names(dti) ]
                                                    dti[, illustrativevariables] <- datafun[i, illustrativevariables]
                                                   dti}
                                                   )

  ##print(listofinterpolateswithillustrativedata)
  if(typexkcdline == "segment" ){
    ## the mapping is xbegin, ybegin,...
    ## but we need x,y [functions pointssegment returns x,y and geom_path requires x,y]
    ## mapping <- mappingjoin(aes(x=x,y=y), mapping) # R CMD check gives NOTES
    mapping <- with(data, mappingjoin(aes(x=x,y=y), mapping))
    }

  
  listofpaths <- lapply(listofinterpolateswithillustrativedata,
                        function(x, mapping, mask, ...) {
                          pathmask <- NULL
                          ##print(mapping)
                          if(mask) {
                            ## Plot a white line widther that the original line
                            ## We must check the color, colour or size
                            ## and change them to white and a greater width
                            argList<-list(...)
                
                            for(i in intersect(c("color","colour"), names(argList)))
                              argList[i] <- NULL
                            argList$mapping <- mapping
                            argList$data <- x
                            argList$colour <- "white"
                            if(is.null(argList$size)==TRUE) argList$size <- 3
                            if(argList$size <= 3 ) argList$size <- 3
                            else  argList$size <- argList$size *2
                            ##print(argList)
                            pathmask <- do.call("geom_path",argList)
                            ##pathmask <- geom_path(mapping = mapping, data = x, colour="white",size=8)
                            }
                          c(pathmask,
                            geom_path(mapping = mapping, data = x, ...))
                       },
                        mapping = mapping,
                        mask = mask,
                        ... = ...
                       )
  listofpaths  
}

