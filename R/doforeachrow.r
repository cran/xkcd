# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-02 Thu 16:00 emilio on emilio-despacho>
# =====================================================================

## ============================================================
## doforeachrow
## ============================================================

## Apply a FUN to each row of the DATA
## If the arguments of the FUN are in the DATA and in the ELLIPSIS
## only use the variable of the DATA
##
## If doitalsoforoptargs = TRUE, then try to get the row of the ELLIPSIS variable
## when applying the function FUN to each row of the DATA.
## Otherwise, use the original ELLIPSIS variable
## when calling the function FUN
doforeachrow <- function(data, fun, doitalsoforoptargs, ...) {
  ## Do not pass the variables of the ELLIPSIS
  ## that are they are in the DATA
  argList <- list(...)
  for( i in intersect(names(data), names(argList) ) )
    argList[i] <- NULL
  if(doitalsoforoptargs) {
    ## If there are variable of ELLIPSIS with the same length than the data base
    ## copy them to the data base and delete them from the ELLIPSIS
    for( i in  names(argList) ) {
      if(!(is.null(argList[i])==TRUE)){
        if(length(argList[[i]]) == 1
           | length(argList[[i]]) == dim(data)[1] ) {
          data[,i] <-  argList[[i]] 
          argList[i] <- NULL
        }
      }
    }
  }
  ##print(data)
  ## Now, apply for each row the FUN
  lapply(1:(dim(data)[1]),
         function(i, data, fun,  argList) {
           largstopass <- as.list(data[i,,])
           mylistofargs <- c(largstopass, argList)
           ## Arguments of the function?
           fcn <- get(fun, mode = "function")
           argsfcntt <-  names(formals(fcn))
           if( "..." %in% argsfcntt ) do.call(fun, mylistofargs)
           else { ## we can only pass the arguments of the function
             for( i in names(mylistofargs)[ !(names(mylistofargs) %in% argsfcntt )])
               mylistofargs[i] <- NULL
             do.call(fun, mylistofargs)
           }
         },
         data = data,
         fun = fun,
         argList = unlist(argList)
         )
}

