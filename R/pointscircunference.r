# Emilio Torres Manzanera
# University of Oviedo
# Time-stamp: <2013-05-02 Thu 09:27 emilio on emilio-despacho>
# =====================================================================

pointscircunference <- function(x =0, y=0, diameter = 1, ratioxy=1, npoints = 16, alpha=  runif(1, 0, pi/2)){
    require(Hmisc) # bezier
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
    ##return(data.frame(x = xx, y = yy))
    return(data.frame(bezier(x = xx, y =yy,evaluation=60)))
}



