## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2018-05-23 12:36 emilio on emilio-despacho>
## ============================================================

#' @title Draw a stick figure
#'
#' @description
#' Draws one stick figure per row of `data`. The figure is built from eight
#' fuzzy elements -- a circular head plus seven bones (spine, two humeri, two
#' radii and two legs) -- which are emitted as just two [geom_xkcdpath()]
#' layers.
#'
#' @details
#' The following aesthetics are required, supplied through `mapping` or as
#' columns of `data`: `x` and `y` (centre of the head), `scale` (overall size),
#' `ratioxy` (the x/y scale ratio, which keeps the figure from being distorted
#' when the axes have different units), and the angles `angleofspine`,
#' `anglerighthumerus`, `anglelefthumerus`, `anglerightradius`,
#' `angleleftradius`, `anglerightleg`, `angleleftleg` and `angleofneck`.
#'
#' @param mapping Aesthetic mapping
#' @param data Dataset
#' @param seed Optional integer for a reproducible figure. See [geom_xkcdpath()].
#' @param ... Optional arguments passed on to [geom_xkcdpath()].
#' @seealso [geom_xkcdpath()]
#' @export
#' @import ggplot2
xkcdman <- function(mapping, data, seed = NULL, ...) {

  centerofhead <- cbind(data$x, data$y)
  diameterofhead <- data$scale
  lengthofspine <- diameterofhead
  lengthofleg <- lengthofspine * 1.2
  lengthofhumerus <- lengthofspine * 0.6
  lengthofradius <- lengthofspine * 0.5

  ## Walk outwards from the head: neck -> spine -> limbs
  beginspine <- centerofhead + (diameterofhead / 2) *
    cbind(cos(data$angleofneck) * data$ratioxy, sin(data$angleofneck))
  endspine <- beginspine + lengthofspine *
    cbind(cos(data$angleofspine) * data$ratioxy, sin(data$angleofspine))
  endrighthumerus <- beginspine + lengthofhumerus *
    cbind(cos(data$anglerighthumerus) * data$ratioxy, sin(data$anglerighthumerus))
  endlefthumerus <- beginspine + lengthofhumerus *
    cbind(cos(data$anglelefthumerus) * data$ratioxy, sin(data$anglelefthumerus))

  ## One bone: a data frame of segments, carrying the other columns along so
  ## user aesthetics (colour, linewidth, ...) survive.
  bone <- function(begin, distance, angle) {
    end <- cbind(begin[, 1] + distance * cos(angle) * data$ratioxy,
                 begin[, 2] + distance * sin(angle))
    out <- data
    out$x <- begin[, 1]
    out$y <- begin[, 2]
    out$xend <- end[, 1]
    out$yend <- end[, 2]
    out$diameter <- NA_real_
    out
  }

  bones <- rbind(
    bone(beginspine, lengthofspine, data$angleofspine),
    bone(beginspine, lengthofhumerus, data$anglerighthumerus),
    bone(endrighthumerus, lengthofradius, data$anglerightradius),
    bone(beginspine, lengthofhumerus, data$anglelefthumerus),
    bone(endlefthumerus, lengthofradius, data$angleleftradius),
    bone(endspine, lengthofleg, data$anglerightleg),
    bone(endspine, lengthofleg, data$angleleftleg)
  )

  head <- data
  head$x <- centerofhead[, 1]
  head$y <- centerofhead[, 2]
  head$diameter <- diameterofhead
  head$xend <- NA_real_
  head$yend <- NA_real_

  ## Angles are consumed above; dropping them keeps ggplot2 from warning about
  ## unknown aesthetics further down the stack.
  anglecols <- c("angleofspine", "anglerighthumerus", "anglelefthumerus",
                 "anglerightradius", "angleleftradius", "anglerightleg",
                 "angleleftleg", "angleofneck", "scale")
  bones[anglecols] <- NULL
  head[anglecols] <- NULL

  bonemapping <- mapping_for(mapping, c("x", "y", "xend", "yend", "ratioxy"))
  headmapping <- mapping_for(mapping, c("x", "y", "diameter", "ratioxy"))

  list(
    geom_xkcdpath(mapping = bonemapping, data = bones, seed = seed, ...),
    geom_xkcdpath(mapping = headmapping, data = head, seed = seed, ...)
  )
}
