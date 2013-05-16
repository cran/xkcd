## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2013-05-03 Fri 11:45 emilio on emilio-laptop2>
## ============================================================

mappingjoin <- function(x,y) {
  nm1 <- names(x)
  nm2 <- names(y)
  for( i in intersect(nm1,nm2)) y[[i]] <- NULL
  parsed <- lapply(c(x,y), function(x) parse(text = x)[[1]])
  structure(parsed, class = "uneval")
}

