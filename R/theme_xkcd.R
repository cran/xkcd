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
#' The "xkcd" font must be installed on your system for the full effect. Modern
#' graphics devices pick it up directly; registering it with \code{extrafont} is
#' only needed for pdf/postscript output. See the vignette
#' \code{vignette("xkcd-intro")} for installation instructions.
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
  if (xkcd_font_available()) {
    base_theme + theme(
      text = element_text(size = 16, family = "xkcd")
    )
  } else {
    # Using message() instead of warning() is often preferred for theme
    # setup issues, but keeping the original warning() style for consistency.
    warning("The 'xkcd' font was not found. Install it on your system, or ",
            "register it with extrafont::font_import() for pdf/postscript output. ",
            "See vignette(\"xkcd-intro\") for instructions. ",
            "Using default text font.",
            call. = FALSE)

    # Fallback theme using the default ggplot2 font
    base_theme + theme(
      text = element_text(size = 16)
    )
  }
}

## Is the xkcd font usable?
##
## Modern graphics devices (ragg, quartz, svglite, cairo) resolve font families
## straight from the operating system and never consult extrafont's database, so
## checking extrafont alone reports the font as missing on machines where it is
## installed and working. Ask the OS first, then fall back to extrafont for users
## who registered the font that way (and for pdf/postscript output).
##
## The lookup scans the system font directories, so cache it for the session.
xkcd_font_cache <- new.env(parent = emptyenv())

xkcd_font_available <- function() {
  if (!is.null(xkcd_font_cache$available)) return(xkcd_font_cache$available)

  found <- FALSE

  if (requireNamespace("systemfonts", quietly = TRUE)) {
    # match_fonts() always returns some path, falling back to a default face when
    # the family is unknown, so compare the resolved family name rather than
    # treating any result as a hit.
    found <- tryCatch(
      identical(tolower(systemfonts::font_info("xkcd")$family[1]), "xkcd"),
      error = function(e) FALSE
    )
  }

  if (!found) {
    found <- tryCatch("xkcd" %in% extrafont::fonts(),
                      error = function(e) FALSE)
  }

  xkcd_font_cache$available <- found
  found
}