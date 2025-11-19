## GeomXkcdPath: ggproto implementation
## This Geom expands each input row into a jittered/bezier-smoothed path
## (segment or circle) and draws it using ggplot2's GeomPath. It provides
## proper integration with ggplot2 aesthetics and uses 'linewidth' for
## line thickness.

#' GeomXkcdPath: fuzzy path/circle geom (XKCD style)
#'
#' A ggplot2 geom that draws jittered, smoothed paths or fuzzy circles. It
#' expects aesthetics like `x`, `y`, and either `xend`/`yend` (for segments) or
#' `diameter` (for circles). Additional aesthetics (colour, alpha,
#' linewidth, linetype) are respected.
#'
#' @param mapping Aesthetic mapping.
#' @param data Data frame.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment.
#' @param ... Other arguments passed on to layer().
#' @param xjitteramount Horizontal jitter amount for segments.
#' @param yjitteramount Vertical jitter amount for segments.
#' @param mask Logical; if TRUE draws a thicker white mask path under the main path.
#' @param show.legend Show legend.
#' @param inherit.aes Whether to inherit aesthetics from the plot.
#' @export
#' @importFrom grid gList grobTree gpar polylineGrob
geom_xkcdpath <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ..., xjitteramount = 0.01,
                          yjitteramount = 0.01, mask = TRUE, show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXkcdPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xjitteramount = xjitteramount,
                  yjitteramount = yjitteramount,
                  mask = mask,
                  ...)
  )
}

GeomXkcdPath <- ggplot2::ggproto(
  "GeomXkcdPath",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.8, linetype = 1, alpha = 1,
                             xend = NA, yend = NA, diameter = NA),
  extra_params = c("xjitteramount", "yjitteramount", "mask", "na.rm"),
  draw_panel = function(self, data, panel_params, coord, xjitteramount = 0.01,
                        yjitteramount = 0.01, mask = TRUE, ...) {
    if (nrow(data) == 0) return(grid::nullGrob())

    # Expand each row into path points
    paths <- lapply(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      # use provided ratioxy or default 1
      rxy <- if ("ratioxy" %in% names(row)) row$ratioxy else 1

      if (!is.null(row$diameter) && !is.na(row$diameter)) {
        df <- pointscircunference(x = row$x, y = row$y, diameter = row$diameter,
                                  ratioxy = rxy)
      } else if (!is.null(row$xend) && !is.na(row$xend) && !is.null(row$yend) && !is.na(row$yend)) {
        df <- pointssegment(x = row$x, y = row$y, xend = row$xend, yend = row$yend,
                            xjitteramount = xjitteramount, yjitteramount = yjitteramount)
      } else {
        df <- data.frame(x = row$x, y = row$y)
      }

      # Copy aesthetics from the original row into the expanded path
      aesth_cols <- setdiff(names(row), c("x", "y", "xend", "yend", "diameter", "PANEL", "group"))
      for (nm in aesth_cols) df[[nm]] <- row[[nm]]
      df$group <- i
      df
    })

    pathdata <- do.call(rbind, paths)
    if (nrow(pathdata) == 0) return(grid::nullGrob())

    # Draw mask (white thicker path) if requested
    grobs <- list()
    if (isTRUE(mask)) {
      maskdata <- pathdata
      # ensure linewidth present
      if (!"linewidth" %in% names(maskdata)) maskdata$linewidth <- 1
      maskdata$linewidth <- pmax(maskdata$linewidth * 2, 1)
      maskdata$colour <- "white"
      grob_mask <- ggplot2::GeomPath$draw_panel(maskdata, panel_params, coord)
      grobs[[length(grobs) + 1]] <- grob_mask
    }

    # Main path
    grob_main <- ggplot2::GeomPath$draw_panel(pathdata, panel_params, coord)
    grobs[[length(grobs) + 1]] <- grob_main

    grid::grobTree(do.call(grid::gList, grobs))
  },

  draw_key = ggplot2::draw_key_path
)

# Declare globals used in aes to avoid R CMD check NOTES
if (getRversion() >= "2.15.1") utils::globalVariables(c("x", "y", "group", "xend", "yend", "diameter"))
