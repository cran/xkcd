# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-02 Thu 23:47 emilio on emilio-laptop2>
# =====================================================================

## ============================================================
## createdefaultmappinganddata 
## ============================================================

## Create a aes with the default names. It changes the names of the data base
## 
## Therefore, we get a data frame with the default names of the mapping
## and a new mapping with the names by default
## For instance
## mapping <- aes(x= x1 +y1, y = y1) -> mapping <- aes(x= x, y = y)
## data[ , c("x1","y1","color")]  -> data[, c("x","y","x1","y1","color")]
createdefaultmappinganddata <- function(mapping, data, mandatoryarguments =c("x","y")) {
  
  ## Check the names of the aes
  nm <- names(mapping)
  positionswithoutname <- (1:length(nm))[nm==""]
  failsthisarguments <- mandatoryarguments[ !(mandatoryarguments %in% nm) ]
  if(length(failsthisarguments) != length(positionswithoutname))
    stop(paste("Argumenst of aes are ", paste(mandatoryarguments, collapse=", "),".",sep=""))
  names(mapping)[positionswithoutname] <- failsthisarguments
  ## New names
    namesmapping <- names(mapping)

  ## Create a new data
  ## For each name of the mapping, evaluate it and create a new data base
  ## with the names of the mapping.
  dataaes <- as.data.frame(lapply(mapping, function(xnamedataxkcdveryrare.327) with(data, eval(xnamedataxkcdveryrare.327))))
  ## Add the rest of variables of the data base

  variablestocbind <- names(data)[!(names(data) %in% namesmapping)] 
  dataaes[, variablestocbind] <- data[,variablestocbind]
  ## Now, it creates a new mapping with the default variables x=x, y=x, yend=yend, and so on.
  ## See the definition of the function ggplot2::aes_string
  parsed <- lapply(namesmapping, function(x) parse(text = x)[[1]])
  names(parsed) <- namesmapping
  newmapping <- structure(parsed, class = "uneval")
  list(mapping = newmapping, data = dataaes)
  }

