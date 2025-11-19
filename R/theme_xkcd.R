## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2018-05-23 17:38 emilio on emilio-despacho>
## ============================================================

#' Creates an XKCD theme
#'
#' This function creates an XKCD theme, applying the 'xkcd' font if available.
#'
#' @return A \code{\link[ggplot2]{theme}} object.
#' @import ggplot2
#' @note
#' The "xkcd" font must be installed and registered with \code{extrafont} for the
#' full effect. See the vignette \code{vignette("xkcd-intro")} for installation instructions.
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'xkcd' font is installed and registered:
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'      geom_point() +
#'      theme_xkcd()
#' p
#' }
theme_xkcd <- function(){
  # Define the base theme elements common to both cases
  base_theme <- theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank(),
    strip.background = element_blank()
  )
  
  # Check for the font and apply the text element
  if ("xkcd" %in% extrafont::fonts()) {
    base_theme + theme(
      text = element_text(size = 16, family = "xkcd")
    )
  } else {
    # Using message() instead of warning() is often preferred for theme
    # setup issues, but keeping the original warning() style for consistency.
    warning("The 'xkcd' font is not installed or registered with extrafont. ",
            "See vignette(\"xkcd-intro\") for instructions. ",
            "Using default text font.",
            call. = FALSE)
    
    # Fallback theme using the default ggplot2 font
    base_theme + theme(
      text = element_text(size = 16)
    )
  }
}